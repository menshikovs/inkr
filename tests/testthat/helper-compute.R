# Helpers for cascade and computed-style tests.

# Compile `css` (optionally with a registry) and compute the style of `element`.
compute_css <- function(css, element, registry = ink_registry()) {
  ink_compute(compile_text(css, registry = registry), element)
}

# The formatted computed value of one property.
shown <- function(style, property = "color") {
  format(style$values[[property]])
}

# The ink_compute_error raised by `expr`, or NULL.
compute_error <- function(expr) {
  tryCatch(
    {
      expr
      NULL
    },
    ink_compute_error = function(e) e
  )
}

expect_compute_error <- function(expr, code) {
  err <- compute_error(expr)
  testthat::expect_s3_class(err, "ink_compute_error")
  if (!is.null(err)) {
    testthat::expect_equal(err$code, code)
    testthat::expect_true(nzchar(err$message))
  }
  invisible(err)
}

# A registry with one extra property.
registry_with <- function(name, type, initial, inherits = FALSE) {
  ink_register_property(ink_registry(), name, type, initial, inherits)
}

# A chain of `depth` "Filler" elements below `root` (default: a Screen).
filler_chain <- function(depth, root = ink_element("Screen")) {
  parent <- root
  for (i in seq_len(depth)) {
    parent <- ink_element("Filler", parent = parent)
  }
  parent
}

# True if no value in a computed style is an unresolved keyword.
has_no_keywords <- function(style) {
  !any(vapply(style$values, inherits, logical(1), what = "ink_keyword"))
}
