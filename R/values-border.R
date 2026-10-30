# Borders. See ?ink_values for the representation.

BORDER_STYLES <- c("none", "single", "double", "round", "heavy")

BORDER_EXPECTED <- paste(
  "Expected 'none', or a border style (single, double, round, heavy)",
  "followed by a color, for example 'round red'."
)

new_ink_border <- function(style, color = NULL) {
  stopifnot(length(style) == 1L, style %in% BORDER_STYLES)
  if (style == "none") {
    stopifnot(is.null(color))
  } else {
    stopifnot(inherits(color, "ink_color"))
  }
  # `list(color = NULL)` keeps the element, so the structure is always the same.
  structure(list(style = style, color = color), class = "ink_border")
}

parse_border_tokens <- function(tokens) {
  if (length(tokens) == 0L || tokens[[1L]]$type != "ident") {
    abort_value_problem("invalid_border", BORDER_EXPECTED)
  }
  style <- tokens[[1L]]$value
  if (!(style %in% BORDER_STYLES)) {
    abort_value_problem(
      "invalid_border",
      sprintf(
        "Unknown border style '%s'. Supported styles: %s.",
        style, paste(BORDER_STYLES, collapse = ", ")
      )
    )
  }
  if (style == "none") {
    if (length(tokens) > 1L) {
      abort_value_problem("invalid_border", "'none' cannot be followed by a color.")
    }
    return(new_ink_border("none"))
  }
  if (length(tokens) == 1L) {
    abort_value_problem(
      "invalid_border",
      sprintf("A '%s' border needs a color, for example '%s red'.", style, style)
    )
  }
  if (length(tokens) > 2L) {
    abort_value_problem("invalid_border", BORDER_EXPECTED)
  }
  color <- tryCatch(
    parse_color_tokens(tokens[2L]),
    ink_value_problem = function(problem) {
      abort_value_problem("invalid_border", paste("Invalid border color.", problem$expected))
    }
  )
  new_ink_border(style, color)
}

#' @export
#' @noRd
format.ink_border <- function(x, ...) {
  if (x$style == "none") "none" else paste(x$style, format(x$color))
}

#' @export
#' @noRd
print.ink_border <- function(x, ...) print_value(x)
