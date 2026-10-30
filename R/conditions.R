# Syntax errors are signalled as conditions of class `ink_parse_error`.
#
# Fields:
# * `code`: stable, machine-readable string (see the list below).
# * `message`: "<source>:<line>:<column>: <detail>".
# * `detail`: the human-readable explanation without the location prefix.
# * `source`: the source name (file path or "<text>").
# * `line`, `column`, `offset`: primary location (see location.R).
# * `span`: `list(start, end)` of the relevant source text.
#
# Error codes:
#   Lexical:  unterminated_comment, unexpected_character, invalid_selector,
#             invalid_variable, invalid_hex_color
#   Unsupported syntax: unsupported_at_rule, unsupported_string,
#             unsupported_function, unsupported_important, unsupported_nesting
#   Syntactic: unexpected_token, unexpected_end_of_input,
#             unexpected_closing_brace, missing_opening_brace,
#             missing_closing_brace, missing_colon, missing_semicolon,
#             empty_value, invalid_selector, invalid_variable

new_ink_parse_error <- function(code, detail, source, start, end = start) {
  structure(
    class = c("ink_parse_error", "error", "condition"),
    list(
      message = sprintf("%s:%s: %s", source, format_location(start), detail),
      call = NULL,
      code = code,
      detail = detail,
      source = source,
      line = start$line,
      column = start$column,
      offset = start$offset,
      span = ink_span(start, end)
    )
  )
}

ink_abort_parse <- function(code, detail, source, start, end = start) {
  stop(new_ink_parse_error(code, detail, source, start, end))
}

# Re-signals the lexical error carried by a token of type "error".
abort_error_token <- function(token, source) {
  ink_abort_parse(token$code, token$message, source, token$start, token$end)
}

# Errors for files that cannot be read (as opposed to malformed stylesheets).
new_ink_read_error <- function(path, detail) {
  structure(
    class = c("ink_read_error", "error", "condition"),
    list(
      message = sprintf("Cannot read stylesheet '%s': %s", path, detail),
      call = NULL,
      path = path
    )
  )
}

ink_abort_read <- function(path, detail) {
  stop(new_ink_read_error(path, detail))
}
