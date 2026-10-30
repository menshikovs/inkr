# Spacing (padding, margin). See ?ink_values for the representation.

SPACING_EXPECTED <- "Expected 1 to 4 non-negative integers."

new_ink_spacing <- function(top, right, bottom, left) {
  sides <- c(top, right, bottom, left)
  stopifnot(is.numeric(sides), length(sides) == 4L, !anyNA(sides), all(sides >= 0L), all(sides == round(sides)))
  structure(
    list(values = c(top = as.integer(top), right = as.integer(right), bottom = as.integer(bottom), left = as.integer(left))),
    class = "ink_spacing"
  )
}

# CSS shorthand: 1 value = all sides; 2 = vertical, horizontal;
# 3 = top, horizontal, bottom; 4 = top, right, bottom, left.
parse_spacing_tokens <- function(tokens) {
  count <- length(tokens)
  if (count < 1L || count > 4L) {
    abort_value_problem("invalid_spacing", SPACING_EXPECTED)
  }
  if (!all(vapply(tokens, is_cell_count, logical(1)))) {
    abort_value_problem("invalid_spacing", SPACING_EXPECTED)
  }
  numbers <- as.integer(vapply(tokens, function(token) token$value, ""))
  sides <- numbers[switch(count, c(1L, 1L, 1L, 1L), c(1L, 2L, 1L, 2L), c(1L, 2L, 3L, 2L), 1:4)]
  new_ink_spacing(sides[[1L]], sides[[2L]], sides[[3L]], sides[[4L]])
}

#' @export
#' @noRd
format.ink_spacing <- function(x, ...) {
  paste0(names(x$values), "=", x$values, collapse = " ")
}

#' @export
#' @noRd
print.ink_spacing <- function(x, ...) print_value(x)
