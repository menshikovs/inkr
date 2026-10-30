#' Typed property values
#'
#' [ink_compile()] turns raw declaration values into small S3 objects. They are
#' plain lists with a class, validated when created. Equal values are
#' `identical()`: input is normalized, so `#fff` and `#ffffff`, or
#' `bold underline` and `underline bold`, give the same object.
#'
#' `format()` and `print()` give a compact, deterministic text form.
#'
#' @section `ink_color`:
#' A list with a `kind` field:
#' * `kind = "rgb"`: also `r`, `g`, `b` and `a`, integers in 0-255; `a = 255L`
#'   is opaque. Written as `#rgb`, `#rgba`, `#rrggbb` or `#rrggbbaa` (hex digits
#'   in either case). Short forms are expanded (`#f80` is `#ff8800`). Formatted
#'   as `#rrggbb`, or `#rrggbbaa` when not opaque.
#' * `kind = "ansi"`: also `name` and `index`. One of `black`, `red`, `green`,
#'   `yellow`, `blue`, `magenta`, `cyan`, `white` (indexes 0-7) or the
#'   `bright-` variant of each (indexes 8-15). ANSI colors are *not* RGB
#'   colors: how they look depends on the terminal theme.
#' * `kind = "default"`: the terminal's default color, written `default`.
#' * `kind = "transparent"`: no explicit fill, written `transparent`. It is not
#'   black.
#'
#' Color keywords are lowercase. Any color value is accepted by any
#' color-typed property.
#'
#' @section `ink_spacing`:
#' A list with one field, `values`: an integer vector
#' `c(top, right, bottom, left)`. Written with 1 to 4 non-negative integers
#' using CSS expansion: `1` is all sides; `1 2` is vertical, horizontal;
#' `1 2 3` is top, horizontal, bottom; `1 2 3 4` is top, right, bottom, left.
#' Negative numbers, fractions, units and `auto` are rejected. This
#' restriction also applies to `margin`.
#'
#' @section `ink_border`:
#' A list with `style` (`"none"`, `"single"`, `"double"`, `"round"` or
#' `"heavy"`) and `color` (an `ink_color`, or `NULL` for `none`). Written as
#' `none` or `<style> <color>`, for example `round #7c3aed`. A color is
#' required for every style except `none`.
#'
#' @section `ink_length`:
#' A list with `kind` (`"cells"` or `"auto"`) and `cells` (a non-negative
#' integer, `NA` for `auto`). Percentages, `fr`, viewport units, `calc()` and
#' negative numbers are not supported.
#'
#' @section `ink_text_style`:
#' A list with one field, `flags`: a character vector, without duplicates, in
#' the fixed order `bold`, `dim`, `italic`, `underline`, `reverse`, `strike`.
#' `none` gives an empty vector and cannot be combined with other flags.
#'
#' @section `ink_keyword`:
#' A list with one field, `keyword`: `"inherit"` or `"initial"`. These CSS-wide
#' keywords are valid for every property. They are kept as markers; nothing is
#' resolved against a parent element at compile time.
#'
#' @param x A typed value.
#' @param ... Ignored.
#' @return `format()` returns a string; `print()` returns `x` invisibly.
#' @name ink_values
#' @seealso [ink_compile()], [ink_registry()]
#' @examples
#' sheet <- ink_compile(ink_parse("Button { border: round #7c3aed; padding: 1 2; }"))
#' decls <- sheet$rules[[1]]$declarations
#' decls[[1]]$value
#' decls[[2]]$value$values
NULL

# Value tokens ---------------------------------------------------------------
#
# A declaration value is re-lexed (in the lexer's "value" mode) into simple
# `list(type, value)` tokens. Variable substitution is done on this token
# list, never on text, so `$a` can never clobber the start of `$accent`.

VALUE_TOKEN_TYPES <- c("ident", "variable", "hex_color", "number", "comma")

# Internal condition for a value that does not fit its type. The compiler
# converts it into an `ink_compile_error` with a source location.
abort_value_problem <- function(code, expected) {
  stop(structure(
    class = c("ink_value_problem", "error", "condition"),
    list(message = expected, call = NULL, code = code, expected = expected)
  ))
}

value_tokens <- function(text, allow_variables = TRUE) {
  tokens <- list()
  for (token in ink_lex(text, mode = "value")) {
    type <- token$type
    if (type %in% c("whitespace", "comment", "eof")) {
      next
    }
    if (type == "error") {
      code <- if (token$code == "invalid_hex_color") "invalid_color" else "invalid_property_value"
      abort_value_problem(code, token$message)
    }
    if (!(type %in% VALUE_TOKEN_TYPES)) {
      abort_value_problem(
        "invalid_property_value",
        sprintf("Unexpected '%s' in a value.", token$value)
      )
    }
    if (type == "variable" && !allow_variables) {
      abort_value_problem(
        "invalid_property_value",
        sprintf("Variable '%s' cannot be used here.", token$value)
      )
    }
    tokens[[length(tokens) + 1L]] <- list(type = type, value = token$value)
  }
  tokens
}

# Source text of value tokens (a comma is attached to the token before it).
tokens_text <- function(tokens) {
  if (length(tokens) == 0L) {
    return("")
  }
  values <- vapply(tokens, function(token) token$value, "")
  is_comma <- values == ","
  paste0(ifelse(seq_along(values) == 1L | is_comma, "", " "), values, collapse = "")
}

# Parses `text` as a value of `type`, without variables or CSS-wide keywords.
# Used for registry initial values; signals `ink_value_problem`.
value_from_text <- function(type, text) {
  parse_typed_value(type, value_tokens(text, allow_variables = FALSE))
}

# Type registry --------------------------------------------------------------

VALUE_TYPES <- c("color", "spacing", "border", "length", "text_style")

VALUE_CLASSES <- c(
  color = "ink_color", spacing = "ink_spacing", border = "ink_border",
  length = "ink_length", text_style = "ink_text_style"
)

parse_typed_value <- function(type, tokens) {
  switch(type,
    color = parse_color_tokens(tokens),
    spacing = parse_spacing_tokens(tokens),
    border = parse_border_tokens(tokens),
    length = parse_length_tokens(tokens),
    text_style = parse_text_style_tokens(tokens),
    stop("Internal error: unknown value type '", type, "'.", call. = FALSE)
  )
}

# Shared by spacing and length: a token that is a non-negative whole number
# with no sign, fraction or unit, small enough to be an R integer.
is_cell_count <- function(token) {
  token$type == "number" &&
    grepl("^[0-9]+$", token$value) &&
    as.numeric(token$value) <= .Machine$integer.max
}

# CSS-wide keywords ----------------------------------------------------------

CSS_WIDE_KEYWORDS <- c("inherit", "initial")

new_ink_keyword <- function(keyword) {
  stopifnot(keyword %in% CSS_WIDE_KEYWORDS)
  structure(list(keyword = keyword), class = "ink_keyword")
}

# Returns an `ink_keyword` if the whole value is a CSS-wide keyword, else NULL.
as_css_wide_keyword <- function(tokens) {
  if (length(tokens) == 1L && tokens[[1L]]$type == "ident" &&
    tokens[[1L]]$value %in% CSS_WIDE_KEYWORDS) {
    return(new_ink_keyword(tokens[[1L]]$value))
  }
  NULL
}

#' @export
#' @noRd
format.ink_keyword <- function(x, ...) x$keyword

# Printing -------------------------------------------------------------------

print_value <- function(x) {
  cat("<", class(x)[[1L]], "> ", format(x), "\n", sep = "")
  invisible(x)
}

#' @export
#' @noRd
print.ink_keyword <- function(x, ...) print_value(x)
