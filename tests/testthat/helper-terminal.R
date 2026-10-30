# Helpers for the terminal adapter tests.

# Compile `css`, compute the style of one element and adapt it.
terminal_of <- function(css, type = "Button", classes = character(), parent = NULL,
                        capabilities = ink_terminal_capabilities(), matte = NULL,
                        registry = ink_registry()) {
  sheet <- compile_text(css, registry = registry)
  element <- ink_element(type, classes = classes, parent = parent)
  ink_terminal_style(ink_compute(sheet, element), capabilities, matte)
}

caps <- function(depth = "truecolor", unicode = TRUE, text_styles = TEXT_STYLE_FLAGS) {
  ink_terminal_capabilities(depth, unicode, text_styles)
}

terminal_error <- function(expr) {
  tryCatch(
    {
      expr
      NULL
    },
    ink_terminal_error = function(e) e
  )
}

expect_terminal_error <- function(expr, code) {
  err <- terminal_error(expr)
  testthat::expect_s3_class(err, "ink_terminal_error")
  if (!is.null(err)) {
    testthat::expect_equal(err$code, code)
    testthat::expect_true(nzchar(err$message))
  }
  invisible(err)
}

# Wrapped text with the ESC character shown as "\\033" so expectations are
# readable.
wrapped <- function(css, text = "Save", capabilities = caps(), reset = TRUE, matte = NULL) {
  ink_ansi_wrap(text, terminal_of(css, capabilities = capabilities, matte = matte), reset = reset)
}

esc <- "\033"

# Independent reference quantizers for the differential tests: the nearest entry
# of an explicit palette table, lowest index on a tie. They share nothing with
# the package's quantizers except the published palette definitions.
reference_palette_256 <- function() {
  levels <- c(0, 95, 135, 175, 215, 255)
  cube <- expand.grid(b = levels, g = levels, r = levels)[, c("r", "g", "b")]
  gray <- 8 + 10 * (0:23)
  rbind(as.matrix(cube), cbind(gray, gray, gray))
}

reference_nearest <- function(rgb, palette, first_index) {
  distance <- rowSums((palette - matrix(rgb, nrow(palette), 3L, byrow = TRUE))^2)
  as.integer(first_index + which(distance == min(distance))[[1L]] - 1L)
}
