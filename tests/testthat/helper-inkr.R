# Helpers shared by the test files. testthat sources helper-*.R first.

# Types / values of tokens, optionally without whitespace and comments.
token_types <- function(tokens) {
  vapply(tokens, function(token) token$type, "")
}

token_values <- function(tokens) {
  vapply(tokens, function(token) token$value, "")
}

significant <- function(tokens) {
  tokens[!(token_types(tokens) %in% c("whitespace", "comment", "eof"))]
}

# Compact (line, column, offset) vector for comparing locations.
loc <- function(location) {
  c(line = location$line, column = location$column, offset = location$offset)
}

# The source text covered by a span (ASCII input only).
span_text <- function(text, span) {
  substr(text, span$start$offset + 1L, span$end$offset)
}

# The condition raised when parsing `text`, or a test failure if none is.
parse_error <- function(text, ...) {
  tryCatch(
    {
      ink_parse(text, ...)
      NULL
    },
    ink_parse_error = function(e) e
  )
}

# Asserts that parsing `text` fails with `code` at `line`:`column`.
expect_parse_error <- function(text, code, line, column) {
  err <- parse_error(text)
  testthat::expect_s3_class(err, "ink_parse_error")
  if (!is.null(err)) {
    testthat::expect_equal(err$code, code)
    testthat::expect_equal(c(err$line, err$column), c(line, column))
  }
  invisible(err)
}

# Drops every source location from an AST so two parses can be compared on
# meaning alone.
strip_locations <- function(x) {
  if (is.list(x)) {
    x[c("span", "value_span")] <- NULL
    cls <- class(x)
    x <- lapply(x, strip_locations)
    class(x) <- cls
  }
  x
}

# Parsed statements of `text`.
statements <- function(text) {
  ink_parse(text)$statements
}

first_rule <- function(text) {
  statements(text)[[1L]]
}

# The single compound selector of a rule such as `Button.primary { }`.
first_compound <- function(text) {
  first_rule(text)$selectors$selectors[[1L]]$compounds[[1L]]
}

# Compilation helpers -------------------------------------------------------

compile_text <- function(text, ...) {
  ink_compile(ink_parse(text), ...)
}

# The `ink_compile_error` raised when compiling `text`, or NULL.
compile_error <- function(text, ..., source = "<text>") {
  tryCatch(
    {
      ink_compile(ink_parse(text, source = source), ...)
      NULL
    },
    ink_compile_error = function(e) e
  )
}

expect_compile_error <- function(text, code, line, column) {
  err <- compile_error(text)
  testthat::expect_s3_class(err, "ink_compile_error")
  if (!is.null(err)) {
    testthat::expect_equal(err$code, code)
    testthat::expect_equal(c(err$line, err$column), c(line, column))
  }
  invisible(err)
}

# The typed value of the first declaration of the first rule of `text`.
compiled_value <- function(text) {
  compile_text(text)$rules[[1L]]$declarations[[1L]]$value
}

# Parses `text` as a bare value of `type` (the internal value parser).
value <- function(type, text) {
  value_from_text(type, text)
}

# The `ink_value_problem` raised when parsing `text` as `type`, or NULL.
value_problem <- function(type, text) {
  tryCatch(
    {
      value_from_text(type, text)
      NULL
    },
    ink_value_problem = function(e) e
  )
}

expect_value_problem <- function(type, text, code) {
  problem <- value_problem(type, text)
  testthat::expect_s3_class(problem, "ink_value_problem")
  if (!is.null(problem)) {
    testthat::expect_equal(problem$code, code, info = text)
  }
  invisible(problem)
}
