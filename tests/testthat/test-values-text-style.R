flags <- function(text) {
  value("text_style", text)$flags
}

test_that("single flags are accepted", {
  for (flag in c("bold", "dim", "italic", "underline", "reverse", "strike")) {
    expect_equal(flags(flag), flag)
  }
})

test_that("none gives no flags", {
  none <- value("text_style", "none")
  expect_s3_class(none, "ink_text_style")
  expect_identical(none$flags, character())
  expect_equal(format(none), "none")
})

test_that("flags are normalized into a fixed order", {
  expect_equal(flags("underline bold"), c("bold", "underline"))
  expect_equal(
    flags("strike reverse underline italic dim bold"),
    c("bold", "dim", "italic", "underline", "reverse", "strike")
  )
  expect_identical(value("text_style", "bold underline"), value("text_style", "underline bold"))
})

test_that("duplicates are removed", {
  expect_equal(flags("bold bold"), "bold")
  expect_equal(flags("italic bold italic bold"), c("bold", "italic"))
})

test_that("whitespace is ignored", {
  expect_identical(
    value("text_style", "  bold \n\t underline "),
    value("text_style", "bold underline")
  )
})

test_that("none cannot be combined with other flags", {
  expect_value_problem("text_style", "none bold", "invalid_text_style")
  expect_value_problem("text_style", "bold none", "invalid_text_style")
  expect_value_problem("text_style", "none none", "invalid_text_style")
})

test_that("unknown flags and other tokens are rejected", {
  for (text in c("", "blink", "Bold", "bold blink", "1", "#fff", "bold, dim", "inherit")) {
    expect_value_problem("text_style", text, "invalid_text_style")
  }
  expect_match(
    value_problem("text_style", "blink")$expected,
    "Unknown text style 'blink'",
    fixed = TRUE
  )
})

test_that("text styles format as the flag list", {
  expect_equal(format(value("text_style", "underline bold")), "bold underline")
  expect_output(print(value("text_style", "bold")), "<ink_text_style> bold", fixed = TRUE)
})

test_that("the constructor rejects unknown flags", {
  expect_error(new_ink_text_style("blink"))
  expect_identical(new_ink_text_style(c("dim", "bold", "bold"))$flags, c("bold", "dim"))
})
