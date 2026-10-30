# Lengths (width, height). See ?ink_values for the representation.

LENGTH_EXPECTED <- "Expected a non-negative integer (terminal cells) or 'auto'."

new_ink_length_cells <- function(cells) {
  stopifnot(is.numeric(cells), length(cells) == 1L, !is.na(cells), cells >= 0L, cells == round(cells))
  structure(list(kind = "cells", cells = as.integer(cells)), class = "ink_length")
}

new_ink_length_auto <- function() {
  structure(list(kind = "auto", cells = NA_integer_), class = "ink_length")
}

parse_length_tokens <- function(tokens) {
  if (length(tokens) != 1L) {
    abort_value_problem("invalid_length", LENGTH_EXPECTED)
  }
  token <- tokens[[1L]]
  if (token$type == "ident" && token$value == "auto") {
    return(new_ink_length_auto())
  }
  if (is_cell_count(token)) {
    return(new_ink_length_cells(as.integer(token$value)))
  }
  abort_value_problem("invalid_length", LENGTH_EXPECTED)
}

#' @export
#' @noRd
format.ink_length <- function(x, ...) {
  if (x$kind == "auto") "auto" else as.character(x$cells)
}

#' @export
#' @noRd
print.ink_length <- function(x, ...) print_value(x)
