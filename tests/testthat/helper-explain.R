# Helpers for ink_explain() tests.

explain_css <- function(css, element, property = "color", registry = ink_registry(), source = "<text>") {
  sheet <- ink_compile(ink_parse(css, source = source), registry = registry)
  ink_explain(sheet, element, property)
}

# The ink_explain_error raised by `expr`, or NULL.
explain_error <- function(expr) {
  tryCatch(
    {
      expr
      NULL
    },
    ink_explain_error = function(e) e
  )
}

expect_explain_error <- function(expr, code) {
  err <- explain_error(expr)
  testthat::expect_s3_class(err, "ink_explain_error")
  if (!is.null(err)) {
    testthat::expect_equal(err$code, code)
    testthat::expect_true(nzchar(err$message))
  }
  invisible(err)
}

reasons <- function(explanation) {
  vapply(explanation$candidates, function(c) if (is.null(c$reason)) "winner" else c$reason, "")
}

candidate_values <- function(explanation) {
  vapply(explanation$candidates, function(c) format(c$value), "")
}

# Every property of `element`: compare the explanation with the computed style.
expect_explanations_consistent <- function(sheet, element) {
  style <- ink_compute(sheet, element)
  for (property in names(style$values)) {
    explained <- ink_explain(sheet, element, property)
    testthat::expect_identical(explained$value, style$values[[property]], info = property)
    origin <- style$provenance[[property]]
    testthat::expect_identical(explained$origin, origin$origin, info = property)
    testthat::expect_identical(explained$resolution$origin, origin$origin, info = property)
    testthat::expect_identical(explained$resolution$source, origin$source, info = property)
    testthat::expect_identical(explained$resolution$keyword, origin$keyword, info = property)
    winner <- explained$winner
    if (is.null(winner)) {
      testthat::expect_null(origin$rule_index, info = property)
    } else {
      testthat::expect_identical(winner$rule_index, origin$rule_index, info = property)
      testthat::expect_identical(winner$declaration_index, origin$declaration_index, info = property)
      testthat::expect_identical(winner$selector_index, origin$selector_index, info = property)
      testthat::expect_identical(winner$specificity, origin$specificity, info = property)
      testthat::expect_identical(winner$span, origin$span, info = property)
    }
  }
  invisible(style)
}

# Plain restatement of "outranks": higher (id, class, type), then rule, then
# declaration, compared left to right. Deliberately independent of the package's
# own comparison functions.
outranks <- function(a, b) {
  x <- c(a$specificity[["id"]], a$specificity[["class"]], a$specificity[["type"]], a$rule_index, a$declaration_index)
  y <- c(b$specificity[["id"]], b$specificity[["class"]], b$specificity[["type"]], b$rule_index, b$declaration_index)
  differing <- which(x != y)
  length(differing) > 0L && x[[differing[[1L]]]] > y[[differing[[1L]]]]
}
