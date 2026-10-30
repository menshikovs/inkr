#' Parse an inkr stylesheet
#'
#' `ink_parse()` parses stylesheet source text and `ink_read()` parses a
#' stylesheet file. Both return an [ink_stylesheet_ast] that preserves the
#' stylesheet as written, in source order, with source locations. Neither
#' resolves variables nor validates property values.
#'
#' @section Supported syntax:
#' * Comments: `/* ... */` (not nestable).
#' * Variable declarations at the top level: `$name: value;`.
#' * Rules: `selectors { property: value; ... }`. Rules and stylesheets may be
#'   empty. Every declaration must end with `;`.
#' * Selectors: type (`Button`), class (`.primary`), ID (`#results`),
#'   universal (`*`), compound (`Button.primary:focus`), pseudo-class
#'   (`:focus`), descendant (`A B`) and child (`A > B`) combinators, and
#'   selector lists (`A, B`).
#' * Identifiers use ASCII letters, digits, `_` and `-`, and start with a
#'   letter or `_`.
#'
#' Values are sequences of identifiers, numbers (with an optional unit or
#' `%`), `#hex` colours, `$variable` references and commas. Strings,
#' functions, nesting, at-rules (such as `@import`) and `!important` are not
#' supported and produce an error.
#'
#' @section Errors:
#' Malformed input signals an error of class `ink_parse_error`; the first
#' problem found is reported and parsing stops. The condition has these
#' fields, in addition to the usual `message`:
#'
#' * `code`: a stable machine-readable string such as `"missing_semicolon"`,
#'   `"missing_closing_brace"`, `"unterminated_comment"` or `"empty_value"`.
#' * `detail`: the human-readable explanation, without the location prefix
#'   that `message` carries.
#' * `source`, `line`, `column`, `offset`: where the problem was found. For a
#'   missing token this is the position where it was expected.
#' * `span`: a list with `start` and `end` locations of the relevant text
#'   (see [ink_stylesheet_ast] for the location conventions).
#'
#' `ink_read()` signals an error of class `ink_read_error` if the file cannot
#' be read.
#'
#' @param text Stylesheet source: a character vector. Several elements are
#'   treated as lines of one stylesheet.
#' @param source Name used for the stylesheet in the AST and in error
#'   messages.
#' @param path Path to a stylesheet file, normally with the `.inkr`
#'   extension. It must be UTF-8 encoded; a leading byte order mark is
#'   ignored.
#' @return An [ink_stylesheet_ast].
#' @examples
#' ink_parse("
#'   $primary: #7c3aed;
#'
#'   Button.primary:focus {
#'     color: $primary;
#'     padding: 1 2;
#'   }
#' ")
#'
#' ink_read(system.file("examples", "basic.inkr", package = "inkr"))
#'
#' # Syntax errors are structured conditions.
#' err <- tryCatch(ink_parse("Button { color: red }"), ink_parse_error = identity)
#' err$code
#' c(err$line, err$column)
#' @export
ink_parse <- function(text, source = "<text>") {
  source <- check_source(source)
  p <- new_parser(ink_lex(text), source)
  new_ink_stylesheet_ast(parse_statements(p), source)
}

#' @rdname ink_parse
#' @export
ink_read <- function(path) {
  text <- read_stylesheet_file(path)
  ink_parse(text, source = path)
}

read_stylesheet_file <- function(path) {
  if (!is.character(path) || length(path) != 1L || is.na(path)) {
    stop("`path` must be a single string.", call. = FALSE)
  }
  if (dir.exists(path)) {
    ink_abort_read(path, "it is a directory")
  }
  if (!file.exists(path)) {
    ink_abort_read(path, "the file does not exist")
  }
  bytes <- tryCatch(
    suppressWarnings(readBin(path, "raw", n = file.size(path))),
    error = function(e) ink_abort_read(path, conditionMessage(e))
  )
  if (any(bytes == as.raw(0L))) {
    ink_abort_read(path, "the file contains NUL bytes and is not text")
  }
  if (length(bytes) >= 3L && identical(bytes[1:3], as.raw(c(0xEF, 0xBB, 0xBF)))) {
    bytes <- bytes[-(1:3)]
  }
  text <- rawToChar(bytes)
  Encoding(text) <- "UTF-8"
  if (!validUTF8(text)) {
    ink_abort_read(path, "the file is not valid UTF-8")
  }
  text
}

# Parser state ---------------------------------------------------------------
#
# A recursive-descent parser over the lexer's tokens. Whitespace and comments
# are dropped up front; each remaining token records whether whitespace
# (`space_before`) or any trivia (`trivia_before`) preceded it, which is all
# the grammar needs: whitespace separates descendant selectors and word-like
# value tokens, and nothing else is significant.
#
# The parser stops at the first problem. Lexical errors arrive as an "error"
# token and are signalled when the parser reaches them.

new_parser <- function(tokens, source) {
  p <- new.env(parent = emptyenv())
  p$tokens <- drop_trivia(tokens)
  p$pos <- 1L
  p$source <- source
  p
}

drop_trivia <- function(tokens) {
  kept <- list()
  space <- FALSE
  trivia <- FALSE
  for (token in tokens) {
    if (token$type == "whitespace") {
      space <- TRUE
      trivia <- TRUE
    } else if (token$type == "comment") {
      trivia <- TRUE
    } else {
      token$space_before <- space
      token$trivia_before <- trivia
      kept[[length(kept) + 1L]] <- token
      space <- FALSE
      trivia <- FALSE
    }
  }
  kept
}

parser_peek <- function(p) {
  token <- p$tokens[[p$pos]]
  if (identical(token$type, "error")) {
    abort_error_token(token, p$source)
  }
  token
}

# Consumes and returns the current token. The final "eof" token is never
# consumed.
parser_next <- function(p) {
  token <- parser_peek(p)
  if (token$type != "eof") {
    p$pos <- p$pos + 1L
  }
  token
}

parser_abort <- function(p, code, detail, start, end = start) {
  ink_abort_parse(code, detail, p$source, start, end)
}

parser_abort_at <- function(p, code, detail, token) {
  parser_abort(p, code, detail, token$start, token$end)
}

describe_token <- function(token) {
  if (token$type == "eof") "end of input" else sprintf("'%s'", token$value)
}

# Statements -----------------------------------------------------------------

parse_statements <- function(p) {
  statements <- list()
  while (parser_peek(p)$type != "eof") {
    statements[[length(statements) + 1L]] <- parse_statement(p)
  }
  statements
}

parse_statement <- function(p) {
  if (parser_peek(p)$type == "variable") {
    parse_variable_declaration(p)
  } else {
    parse_rule(p)
  }
}

parse_variable_declaration <- function(p) {
  name <- parser_next(p)
  owner <- sprintf("variable '%s'", name$value)
  colon <- expect_colon(p, name, owner)
  value <- parse_value(p, colon, owner)
  new_ink_variable_declaration(
    name = substring(name$value, 2L),
    value = value$text,
    value_span = value$span,
    span = ink_span(name$start, value$end)
  )
}

parse_rule <- function(p) {
  selectors <- parse_selector_list(p)
  open <- expect_open_brace(p)
  block <- parse_declaration_block(p, open)
  new_ink_rule(
    selectors = selectors,
    declarations = block$declarations,
    span = ink_span(selectors$span$start, block$close$end)
  )
}

expect_open_brace <- function(p) {
  token <- parser_peek(p)
  if (token$type == "lbrace") {
    return(parser_next(p))
  }
  if (token$type == "eof") {
    parser_abort_at(
      p, "unexpected_end_of_input",
      "Unexpected end of input; expected '{' to start the rule", token
    )
  }
  parser_abort_at(
    p, "missing_opening_brace",
    sprintf("Expected '{' to start the rule, found %s", describe_token(token)),
    token
  )
}

# Parses declarations up to and including the closing brace.
parse_declaration_block <- function(p, open) {
  declarations <- list()
  repeat {
    token <- parser_peek(p)
    if (token$type == "rbrace") {
      return(list(declarations = declarations, close = parser_next(p)))
    }
    if (token$type == "eof") {
      parser_abort(
        p, "missing_closing_brace",
        sprintf(
          "Missing '}' to close the rule opened at %s",
          format_location(open$start)
        ),
        open$start, token$end
      )
    }
    declarations[[length(declarations) + 1L]] <- parse_declaration(p)
  }
}

# Declarations ---------------------------------------------------------------

parse_declaration <- function(p) {
  name <- parse_property_name(p)
  owner <- sprintf("property '%s'", name$value)
  following <- parser_peek(p)
  if (following$type == "lbrace") {
    parser_abort(
      p, "unsupported_nesting",
      sprintf(
        "Nested rules are not supported; '%s' looks like a selector (is a closing '}' missing?)",
        name$value
      ),
      name$start, following$end
    )
  }
  colon <- expect_colon(p, name, owner)
  value <- parse_value(p, colon, owner)
  new_ink_property_declaration(
    name = name$value,
    value = value$text,
    value_span = value$span,
    span = ink_span(name$start, value$end)
  )
}

parse_property_name <- function(p) {
  token <- parser_peek(p)
  if (token$type == "ident") {
    return(parser_next(p))
  }
  if (token$type == "variable") {
    parser_abort_at(
      p, "invalid_variable",
      "Variables can only be declared at the top level of a stylesheet",
      token
    )
  }
  if (token$type == "eof") {
    parser_abort_at(
      p, "unexpected_end_of_input",
      "Unexpected end of input; expected a property name", token
    )
  }
  parser_abort_at(
    p, "unexpected_token",
    sprintf("Expected a property name, found %s", describe_token(token)),
    token
  )
}

# `name` is the token before the colon (a property name or `$variable`).
expect_colon <- function(p, name, owner) {
  token <- parser_peek(p)
  if (token$type == "colon") {
    return(parser_next(p))
  }
  if (token$type == "eof") {
    parser_abort_at(
      p, "unexpected_end_of_input",
      sprintf("Unexpected end of input; expected ':' after %s", owner), token
    )
  }
  parser_abort(
    p, "missing_colon",
    sprintf("Missing ':' after %s", owner),
    name$end
  )
}

# Collects value tokens up to the `;` that ends the declaration. Returns the
# raw text, the span of the value, and the end of the whole declaration.
parse_value <- function(p, colon, owner) {
  parts <- list()
  repeat {
    token <- parser_peek(p)
    switch(token$type,
      semicolon = {
        if (length(parts) == 0L) {
          abort_empty_value(p, colon, token, owner)
        }
        parser_next(p)
        return(value_result(parts, token))
      },
      rbrace = {
        if (length(parts) == 0L) {
          abort_empty_value(p, colon, token, owner)
        }
        abort_missing_semicolon(p, parts[[length(parts)]]$end, owner)
      },
      lbrace = parser_abort_at(
        p, "missing_semicolon",
        sprintf("Unexpected '{' in the value of %s; is a ';' missing?", owner),
        token
      ),
      eof = parser_abort_at(
        p, "unexpected_end_of_input",
        sprintf("Unexpected end of input in the value of %s; expected ';'", owner),
        token
      ),
      colon = abort_colon_in_value(p, parts, colon, token, owner),
      parts[[length(parts) + 1L]] <- parser_next(p)
    )
  }
}

value_result <- function(parts, semicolon) {
  list(
    text = join_value_tokens(parts),
    span = ink_span(parts[[1L]]$start, parts[[length(parts)]]$end),
    end = semicolon$end
  )
}

# Source text of the tokens, with whitespace and comments between them
# collapsed to a single space.
join_value_tokens <- function(parts) {
  values <- vapply(parts, function(token) token$value, "")
  spaced <- vapply(parts[-1L], function(token) token$trivia_before, logical(1))
  paste0(c("", ifelse(spaced, " ", "")), values, collapse = "")
}

abort_empty_value <- function(p, colon, terminator, owner) {
  parser_abort(
    p, "empty_value",
    sprintf("Empty value for %s", owner),
    colon$end, terminator$start
  )
}

abort_missing_semicolon <- function(p, at, owner) {
  parser_abort(
    p, "missing_semicolon",
    sprintf("Missing ';' after the value of %s", owner),
    at
  )
}

# A `:` inside a value almost always means the previous declaration lost its
# `;`: the word before the colon is really the next property (or variable).
abort_colon_in_value <- function(p, parts, colon, token, owner) {
  count <- length(parts)
  last_type <- if (count > 0L) parts[[count]]$type else ""
  if (!(last_type %in% c("ident", "variable"))) {
    parser_abort_at(p, "unexpected_token", "Unexpected ':' in a value", token)
  }
  if (count == 1L) {
    abort_empty_value(p, colon, parts[[1L]], owner)
  }
  abort_missing_semicolon(p, parts[[count - 1L]]$end, owner)
}
