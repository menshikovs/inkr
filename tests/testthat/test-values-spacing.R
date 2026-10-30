sides <- function(text) {
  value("spacing", text)$values
}

test_that("one to four values expand CSS-style", {
  expect_equal(sides("1"), c(top = 1L, right = 1L, bottom = 1L, left = 1L))
  expect_equal(sides("1 2"), c(top = 1L, right = 2L, bottom = 1L, left = 2L))
  expect_equal(sides("1 2 3"), c(top = 1L, right = 2L, bottom = 3L, left = 2L))
  expect_equal(sides("1 2 3 4"), c(top = 1L, right = 2L, bottom = 3L, left = 4L))
})

test_that("spacing is an ink_spacing with an integer vector", {
  spacing <- value("spacing", "1 2")
  expect_s3_class(spacing, "ink_spacing")
  expect_type(spacing$values, "integer")
  expect_named(spacing$values, c("top", "right", "bottom", "left"))
})

test_that("boundary values are accepted", {
  expect_equal(unname(sides("0")), rep(0L, 4L))
  expect_equal(unname(sides("2147483647")), rep(2147483647L, 4L))
  expect_equal(unname(sides("007")), rep(7L, 4L))
})

test_that("equivalent spellings are identical", {
  expect_identical(value("spacing", "1"), value("spacing", "1 1 1 1"))
  expect_identical(value("spacing", "1 2"), value("spacing", "1 2 1 2"))
  expect_identical(value("spacing", "1 2 3"), value("spacing", "1 2 3 2"))
})

test_that("whitespace is ignored", {
  expect_identical(value("spacing", "  1   2\n3 "), value("spacing", "1 2 3"))
})

test_that("invalid spacing is rejected", {
  invalid <- c(
    "", "-1", "1.5", ".5", "+1", "1 2 3 4 5", "auto", "1 two", "1px", "50%",
    "1, 2", "2147483648", "red", "#fff", "1 -1", "inherit"
  )
  for (text in invalid) {
    expect_value_problem("spacing", text, "invalid_spacing")
  }
})

test_that("the problem says what is expected", {
  expect_equal(
    value_problem("spacing", "1 two")$expected,
    "Expected 1 to 4 non-negative integers."
  )
})

test_that("spacing formats as named sides", {
  expect_equal(format(value("spacing", "1 2")), "top=1 right=2 bottom=1 left=2")
  expect_output(print(value("spacing", "0")), "<ink_spacing>", fixed = TRUE)
})

test_that("the constructor rejects negative or fractional sides", {
  expect_error(new_ink_spacing(-1, 0, 0, 0))
  expect_error(new_ink_spacing(0.5, 0, 0, 0))
})

test_that("margin follows the same restriction as padding", {
  expect_equal(compiled_value("A { margin: 1 2; }")$values[["right"]], 2L)
  expect_compile_error("A { margin: -1; }", "invalid_spacing", 1, 13)
  expect_compile_error("A { margin: auto; }", "invalid_spacing", 1, 13)
})
