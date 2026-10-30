# Lexer.
#
# The lexer turns text into a flat list of `ink_token` objects. It knows
# nothing about the AST. Because `#` and `:` mean different things in
# different places, scanning is context sensitive. The lexer tracks one of
# three modes:
#
# * "selector": top level, before a `{`. `#id`, `.class`, `:pseudo`, `*`, `>`.
# * "property": inside a `{ }` block, where a property name is expected.
# * "value": after a declaration's `:` (or after a `$variable` name) up to the
#   terminating `;` or `}`. `#ff0000` is a colour here.
#
# Token types
#   trivia:      whitespace, comment
#   selectors:   ident, star, class (".x"), id ("#x"), pseudo (":x"), gt (">")
#   values:      ident, variable ("$x"), hex_color, number
#   punctuation: lbrace, rbrace, colon, semicolon, comma
#   other:       eof (always last), error (always last, stops the lexer)
#
# Every token is `list(type, value, start, end)` with locations as described
# in location.R. `value` is the raw source text of the token. Error tokens
# also carry `code` and `message`; the lexer stops after the first one so the
# parser can report whichever problem comes first in the source.

# Splits `text` into tokens and signals `ink_parse_error` for the first
# lexical error. `source` is only used to label that error.
ink_tokenize <- function(text, source = "<text>") {
  source <- check_source(source)
  tokens <- ink_lex(text)
  last <- tokens[[length(tokens)]]
  if (identical(last$type, "error")) {
    abort_error_token(last, source)
  }
  tokens
}

# Like ink_tokenize() but never signals: a lexical error becomes a final
# token of type "error".
#
# `mode` is the lexer's starting mode. Stylesheets start in "selector"; the
# compiler lexes a bare declaration value with mode = "value".
ink_lex <- function(text, mode = "selector") {
  src <- lexer_input(normalize_text(text))
  tokens <- list()
  state <- list(mode = mode, in_block = FALSE)
  i <- 1L
  while (i <= src$n) {
    res <- scan_token(src, i, state$mode)
    tokens[[length(tokens) + 1L]] <- build_token(src, i, res)
    if (identical(res$type, "error")) {
      return(tokens)
    }
    state <- advance_lex_state(state, res$type)
    i <- res$end
  }
  eof <- src$locate(src$n)
  tokens[[length(tokens) + 1L]] <- new_ink_token("eof", "", eof, eof)
  tokens
}

new_ink_token <- function(type, value, start, end) {
  structure(
    list(type = type, value = value, start = start, end = end),
    class = "ink_token"
  )
}

build_token <- function(src, i, res) {
  value <- paste(src$chars[i:(res$end - 1L)], collapse = "")
  start <- src$locate(i - 1L)
  end <- src$locate(res$end - 1L)
  token <- new_ink_token(res$type, value, start, end)
  if (identical(res$type, "error")) {
    token$code <- res$code
    token$message <- res$message
  }
  token
}

# Per-character lookup tables, so scanning never has to inspect a regex.
lexer_input <- function(text) {
  cp <- utf8ToInt(text)
  chars <- intToUtf8(cp, multiple = TRUE)
  n <- length(cp)
  alpha <- (cp >= 65L & cp <= 90L) | (cp >= 97L & cp <= 122L)
  digit <- cp >= 48L & cp <= 57L
  ident_start <- alpha | cp == 95L
  list(
    chars = chars,
    n = n,
    space = cp %in% c(9L, 10L, 12L, 13L, 32L),
    digit = digit,
    ident_start = ident_start,
    ident_char = ident_start | digit | cp == 45L,
    unit_char = alpha | chars == "%",
    # Index of every `*` that is immediately followed by `/`.
    comment_close = which(chars[-n] == "*" & chars[-1L] == "/"),
    locate = make_locator(chars)
  )
}

# After a token, decides how the following text is lexed.
advance_lex_state <- function(state, type) {
  mode <- state$mode
  if (type == "lbrace" && mode == "selector") {
    state$mode <- "property"
    state$in_block <- TRUE
  } else if (type == "rbrace") {
    state$mode <- "selector"
    state$in_block <- FALSE
  } else if (type == "variable") {
    state$mode <- "value"
  } else if (type == "colon" && mode == "property") {
    state$mode <- "value"
  } else if (type == "semicolon" && mode == "value") {
    state$mode <- if (state$in_block) "property" else "selector"
  }
  state
}

# Scanner results: a token type plus the (exclusive) index where it ends, or
# an error with the same shape. Every token starts at the scan index `i`.
scan_ok <- function(type, end) {
  list(type = type, end = end)
}

scan_fail <- function(code, message, end) {
  list(type = "error", end = end, code = code, message = message)
}

scan_token <- function(src, i, mode) {
  if (src$space[[i]]) {
    return(scan_ok("whitespace", scan_while(src$space, i)))
  }
  ch <- src$chars[[i]]
  if (ch == "/" && char_at(src, i + 1L) == "*") {
    return(scan_comment(src, i))
  }
  if (ch == "@") {
    return(scan_fail(
      "unsupported_at_rule",
      "At-rules such as '@import' are not supported",
      scan_while(src$ident_char, i + 1L)
    ))
  }
  switch(mode,
    selector = scan_selector_token(src, i),
    property = scan_property_token(src, i),
    value = scan_value_token(src, i)
  )
}

scan_selector_token <- function(src, i) {
  if (src$ident_start[[i]]) {
    return(scan_ok("ident", scan_while(src$ident_char, i)))
  }
  switch(src$chars[[i]],
    "*" = scan_ok("star", i + 1L),
    "." = scan_sigil(src, i, "class", "invalid_selector", "a class name"),
    "#" = scan_sigil(src, i, "id", "invalid_selector", "an ID name"),
    "$" = scan_sigil(src, i, "variable", "invalid_variable", "a variable name"),
    ":" = scan_pseudo(src, i),
    scan_punct(src, i, c("{", "}", ";", ",", ">"))
  )
}

scan_property_token <- function(src, i) {
  if (src$ident_start[[i]]) {
    return(scan_ok("ident", scan_while(src$ident_char, i)))
  }
  switch(src$chars[[i]],
    ":" = scan_ok("colon", i + 1L),
    "$" = scan_sigil(src, i, "variable", "invalid_variable", "a variable name"),
    "." = ,
    "#" = ,
    "*" = ,
    "&" = ,
    ">" = scan_fail(
      "unsupported_nesting",
      "Nested rules are not supported; a closing '}' may be missing",
      i + 1L
    ),
    scan_punct(src, i, c("{", "}", ";"))
  )
}

scan_value_token <- function(src, i) {
  if (src$ident_start[[i]]) {
    return(scan_ok("ident", scan_while(src$ident_char, i)))
  }
  number_end <- scan_number(src, i)
  if (!is.null(number_end)) {
    return(scan_ok("number", number_end))
  }
  switch(src$chars[[i]],
    ":" = scan_ok("colon", i + 1L),
    "$" = scan_sigil(src, i, "variable", "invalid_variable", "a variable name"),
    "#" = scan_hex_color(src, i),
    scan_punct(src, i, c("{", "}", ";", ","))
  )
}

# Accepts the punctuation character at `i` if it is in `allowed`; anything
# else is an error.
scan_punct <- function(src, i, allowed) {
  types <- c(
    "{" = "lbrace", "}" = "rbrace", ";" = "semicolon",
    "," = "comma", ">" = "gt"
  )
  ch <- src$chars[[i]]
  if (ch %in% allowed) {
    return(scan_ok(types[[ch]], i + 1L))
  }
  scan_unexpected(src, i)
}

scan_unexpected <- function(src, i) {
  ch <- src$chars[[i]]
  switch(ch,
    "\"" = ,
    "'" = scan_fail("unsupported_string", "Strings are not supported", i + 1L),
    "(" = ,
    ")" = scan_fail(
      "unsupported_function",
      "Functions and parentheses are not supported",
      i + 1L
    ),
    "!" = scan_fail(
      "unsupported_important",
      "'!important' and other '!' flags are not supported",
      i + 1L
    ),
    scan_fail(
      "unexpected_character",
      paste("Unexpected character", describe_char(ch)),
      i + 1L
    )
  )
}

# `.name`, `#name` and `$name`: a sigil that must be followed by an identifier.
scan_sigil <- function(src, i, type, code, what) {
  if (flag_at(src$ident_start, i + 1L)) {
    return(scan_ok(type, scan_while(src$ident_char, i + 1L)))
  }
  scan_fail(
    code,
    sprintf("Expected %s after '%s'", what, src$chars[[i]]),
    i + 1L
  )
}

# In selector context `:focus` is a pseudo-class, while a `:` that is not
# followed by a name is left for the parser to report.
scan_pseudo <- function(src, i) {
  if (flag_at(src$ident_start, i + 1L)) {
    return(scan_ok("pseudo", scan_while(src$ident_char, i + 1L)))
  }
  if (char_at(src, i + 1L) == ":") {
    return(scan_fail(
      "invalid_selector",
      "Pseudo-elements ('::') are not supported",
      i + 2L
    ))
  }
  scan_ok("colon", i + 1L)
}

# `#ff0000` in value context. The digit count is checked here because the
# colour is a single lexical unit.
scan_hex_color <- function(src, i) {
  end <- scan_while(src$ident_char, i + 1L)
  value <- paste(src$chars[i:(end - 1L)], collapse = "")
  valid <- grepl(
    "^#([0-9A-Fa-f]{3,4}|[0-9A-Fa-f]{6}|[0-9A-Fa-f]{8})$",
    value,
    perl = TRUE
  )
  if (valid) {
    return(scan_ok("hex_color", end))
  }
  scan_fail(
    "invalid_hex_color",
    sprintf(
      "Invalid hex color '%s'; expected 3, 4, 6 or 8 hexadecimal digits",
      value
    ),
    end
  )
}

# An optionally signed integer or decimal followed by an optional unit made
# of letters or `%` (`1`, `-2.5`, `.5`, `50%`, `10vh`). Returns the end index,
# or NULL if there is no number at `i`.
scan_number <- function(src, i) {
  j <- i
  if (src$chars[[j]] %in% c("+", "-")) {
    j <- j + 1L
  }
  if (flag_at(src$digit, j)) {
    j <- scan_while(src$digit, j)
    if (char_at(src, j) == "." && flag_at(src$digit, j + 1L)) {
      j <- scan_while(src$digit, j + 1L)
    }
  } else if (char_at(src, j) == "." && flag_at(src$digit, j + 1L)) {
    j <- scan_while(src$digit, j + 1L)
  } else {
    return(NULL)
  }
  scan_while(src$unit_char, j)
}

scan_comment <- function(src, i) {
  closes <- src$comment_close
  close <- closes[findInterval(i + 1L, closes) + 1L]
  if (is.na(close)) {
    return(scan_fail(
      "unterminated_comment",
      "Unterminated comment; expected a closing '*/'",
      src$n + 1L
    ))
  }
  scan_ok("comment", close + 2L)
}

# Index of the first position at or after `from` where `flags` is FALSE.
scan_while <- function(flags, from) {
  j <- from
  n <- length(flags)
  while (j <= n && flags[[j]]) {
    j <- j + 1L
  }
  j
}

flag_at <- function(flags, j) {
  j <= length(flags) && flags[[j]]
}

char_at <- function(src, j) {
  if (j <= src$n) src$chars[[j]] else ""
}

describe_char <- function(ch) {
  cp <- utf8ToInt(ch)
  if (cp > 32L && cp < 127L) {
    sprintf("'%s'", ch)
  } else {
    sprintf("U+%04X", cp)
  }
}
