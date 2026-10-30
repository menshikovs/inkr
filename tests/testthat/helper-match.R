# Helpers for element and matching tests.

# Builds a parent chain and returns its leaf. Arguments run from the root to the
# leaf; each is a type string or a list of ink_element() arguments, e.g.
# tree("Screen", list(type = "Button", classes = "primary")).
tree <- function(...) {
  parent <- NULL
  for (spec in list(...)) {
    if (is.character(spec)) {
      spec <- list(type = spec)
    }
    parent <- do.call(ink_element, c(spec, list(parent = parent)))
  }
  parent
}

# A chain `depth` elements deep: n-1 "Filler" ancestors above a leaf of `leaf`.
deep_chain <- function(depth, leaf = "Leaf", root = NULL) {
  parent <- root
  for (i in seq_len(depth - 1L)) {
    parent <- ink_element("Filler", parent = parent)
  }
  ink_element(leaf, parent = parent)
}

# The first complex selector / compound selector of a one-rule stylesheet text.
selector_of <- function(text) {
  ink_parse(paste(text, "{}"))$statements[[1L]]$selectors$selectors[[1L]]
}

compound_of <- function(text) {
  selector_of(text)$compounds[[1L]]
}

# Does the selector match the element? (internal matcher)
selector_matches <- function(text, element) {
  ink_match_complex(selector_of(text), element)
}

compound_matches_element <- function(text, element) {
  ink_match_compound(compound_of(text), element)
}

# Match a stylesheet text against an element.
match_css <- function(css, element) {
  ink_match(compile_text(css), element)
}

rule_indices <- function(result) {
  vapply(result$matches, function(m) m$rule_index, 0L)
}

# The ink_match_error raised by `expr`, or NULL.
match_error <- function(expr) {
  tryCatch(
    {
      expr
      NULL
    },
    ink_match_error = function(e) e
  )
}

expect_match_error <- function(expr, code) {
  err <- match_error(expr)
  testthat::expect_s3_class(err, "ink_match_error")
  if (!is.null(err)) {
    testthat::expect_equal(err$code, code)
    testthat::expect_true(nzchar(err$message))
  }
  invisible(err)
}
