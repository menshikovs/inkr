test_that("none needs no color", {
  none <- value("border", "none")
  expect_s3_class(none, "ink_border")
  expect_equal(none$style, "none")
  expect_null(none$color)
  expect_true("color" %in% names(none))
})

test_that("every style combines with a color", {
  for (style in c("single", "double", "round", "heavy")) {
    border <- value("border", paste(style, "red"))
    expect_equal(border$style, style)
    expect_s3_class(border$color, "ink_color")
    expect_equal(border$color$name, "red")
  }
})

test_that("style and color are separate typed fields", {
  border <- value("border", "double #ff0000")
  expect_equal(border$style, "double")
  expect_s3_class(border$color, "ink_color")
  expect_equal(border$color$kind, "rgb")
  expect_equal(border$color$r, 255L)
  expect_identical(border$color, value("color", "#ff0000"))
})

test_that("any color value is accepted", {
  expect_equal(value("border", "single bright-blue")$color$name, "bright-blue")
  expect_equal(value("border", "single default")$color$kind, "default")
  expect_equal(value("border", "single #fff")$color$r, 255L)
  expect_identical(value("border", "round #fff"), value("border", "round #ffffff"))
})

test_that("whitespace is ignored", {
  expect_identical(value("border", "  round \n  red "), value("border", "round red"))
})

test_that("a color is required for styles other than none", {
  for (style in c("single", "double", "round", "heavy")) {
    problem <- value_problem("border", style)
    expect_s3_class(problem, "ink_value_problem")
    expect_equal(problem$code, "invalid_border")
    expect_match(problem$expected, "needs a color", fixed = TRUE)
  }
})

test_that("malformed borders are rejected", {
  invalid <- c(
    "", "dashed", "dashed red", "red", "red single", "none red", "single red blue",
    "single reddish", "round 1", "1", "#fff", "single, red",
    "Single red", "inherit"
  )
  for (text in invalid) {
    expect_value_problem("border", text, "invalid_border")
  }
})

test_that("an unknown style lists the supported ones", {
  problem <- value_problem("border", "dashed red")
  expect_match(problem$expected, "Unknown border style 'dashed'", fixed = TRUE)
  expect_match(problem$expected, "none, single, double, round, heavy", fixed = TRUE)
})

test_that("a bad color is reported as a border problem", {
  problem <- value_problem("border", "single reddish")
  expect_equal(problem$code, "invalid_border")
  expect_match(problem$expected, "Invalid border color", fixed = TRUE)
})

test_that("borders format as style and color", {
  expect_equal(format(value("border", "round #7C3AED")), "round #7c3aed")
  expect_equal(format(value("border", "none")), "none")
  expect_output(print(value("border", "heavy blue")), "<ink_border> heavy blue", fixed = TRUE)
})

test_that("the constructor enforces the color rule", {
  expect_error(new_ink_border("single"))
  expect_error(new_ink_border("none", value("color", "red")))
  expect_error(new_ink_border("dotted", value("color", "red")))
})

test_that("borders compile with variables", {
  border <- compiled_value("$primary: #7c3aed;\nButton { border: round $primary; }")
  expect_equal(border$style, "round")
  expect_equal(border$color$g, 58L)
  expect_compile_error("Button { border: single; }", "invalid_border", 1, 18)
})
