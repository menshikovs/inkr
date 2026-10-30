# Source locations.
#
# Conventions, shared by tokens, AST nodes and syntax errors:
#
# * A location is a plain list `list(line, column, offset)`.
# * `line` is 1-based. Lines are separated by "\n" only; "\r" is ordinary
#   whitespace, so "\r\n" files report the same lines as "\n" files.
# * `column` is 1-based and counts characters (Unicode code points, a tab is
#   one character) from the start of the line.
# * `offset` is 0-based: the number of characters that precede the location.
#   For ASCII-only input the character at `offset` is `substr(text, offset + 1,
#   offset + 1)`.
# * A location identifies the *gap before* a character, so it is also defined
#   one past the last character (end of input).
# * A span is `list(start, end)` where `end` is *exclusive*: it is the
#   location right after the last character of the spanned text. A span that
#   ends with a newline therefore ends at column 1 of the next line, and an
#   empty span has `start` equal to `end`.

ink_loc <- function(line, column, offset) {
  list(line = as.integer(line), column = as.integer(column), offset = as.integer(offset))
}

ink_span <- function(start, end = start) {
  list(start = start, end = end)
}

# Returns a function mapping a 0-based offset to a location, for the text made
# of the character vector `chars`.
make_locator <- function(chars) {
  line_starts <- c(0L, which(chars == "\n"))
  function(offset) {
    line <- findInterval(offset, line_starts)
    ink_loc(line, offset - line_starts[[line]] + 1L, offset)
  }
}

format_location <- function(loc) {
  sprintf("%d:%d", loc$line, loc$column)
}
