# The golden file was captured with the Task 05 engine, before the selector
# optimization (see golden/golden-results.R). Every case must still produce
# exactly the same matches, computed styles (values and provenance) and
# explanations.

golden_expected <- function() {
  path <- test_path("golden", "golden-task05.rds")
  skip_if(!file.exists(path), "golden results are not available")
  readRDS(path)
}

test_that("the golden file covers the intended cases", {
  expected <- golden_expected()
  expect_length(expected, 43L)
  expect_true(all(c("spec-example", "mixed-combinators", "strict-ancestors", "keywords") %in% names(expected)))
  snapshots <- sum(lengths(expected))
  expect_gt(snapshots, 400L)
  matched <- sum(vapply(expected, function(case) sum(vapply(case, function(s) length(s$match), 0L)), 0L))
  expect_gt(matched, 300L)
})

test_that("every golden case is reproduced exactly", {
  expected <- golden_expected()
  for (case in golden_cases()) {
    actual <- golden_snapshot(case)
    expect_identical(actual, expected[[case$name]], info = case$name)
  }
})

test_that("golden results agree between matches, computed styles and explanations", {
  expected <- golden_expected()
  for (case in expected) {
    for (snapshot in case) {
      expect_s3_class(snapshot$style, "ink_computed_style")
      expect_true(all(vapply(snapshot$match, inherits, NA, what = "ink_matched_rule")))
      expect_identical(snapshot$explanations[[1L]]$value, snapshot$style$values$color)
      expect_identical(snapshot$explanations[[2L]]$value, snapshot$style$values$background)
    }
  }
})
