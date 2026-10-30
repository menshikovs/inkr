test_that("zero padding and margin are preserved", {
  style <- terminal_of("Button { color: red; }")
  zero <- c(top = 0L, right = 0L, bottom = 0L, left = 0L)
  expect_identical(style$padding, zero)
  expect_identical(style$margin, zero)
})

test_that("padding keeps the four normalized sides", {
  expect_identical(terminal_of("Button { padding: 1 2; }")$padding, c(top = 1L, right = 2L, bottom = 1L, left = 2L))
  expect_identical(terminal_of("Button { padding: 3; }")$padding, c(top = 3L, right = 3L, bottom = 3L, left = 3L))
  expect_identical(terminal_of("Button { padding: 1 2 3; }")$padding, c(top = 1L, right = 2L, bottom = 3L, left = 2L))
})

test_that("mixed edge values stay in their positions", {
  style <- terminal_of("Button { padding: 4 3 2 1; margin: 0 5 0 7; }")
  expect_identical(style$padding, c(top = 4L, right = 3L, bottom = 2L, left = 1L))
  expect_identical(style$margin, c(top = 0L, right = 5L, bottom = 0L, left = 7L))
})

test_that("padding and margin are independent", {
  style <- terminal_of("Button { padding: 1; margin: 2; }")
  expect_equal(unname(style$padding), rep(1L, 4L))
  expect_equal(unname(style$margin), rep(2L, 4L))
})

test_that("spacing is the computed spacing, unchanged and in cells", {
  computed <- compute_css("Button { padding: 1 2; margin: 3 4; }", ink_element("Button"))
  style <- ink_terminal_style(computed)
  expect_identical(style$padding, computed$values$padding$values)
  expect_identical(style$margin, computed$values$margin$values)
  expect_type(style$padding, "integer")
})

test_that("spacing does not depend on the terminal profile", {
  css <- "Button { padding: 1 2 3 4; margin: 5 6; }"
  base <- terminal_of(css)
  for (profile in list(caps("ansi256", FALSE), caps("ansi16"), caps("none", TRUE, character()))) {
    other <- terminal_of(css, capabilities = profile)
    expect_identical(other$padding, base$padding)
    expect_identical(other$margin, base$margin)
  }
})

test_that("width and height keep cell counts", {
  style <- terminal_of("Button { width: 30; height: 5; }")
  expect_s3_class(style$width, "ink_length")
  expect_equal(style$width$kind, "cells")
  expect_equal(style$width$cells, 30L)
  expect_equal(style$height$cells, 5L)
})

test_that("auto stays auto and is never resolved", {
  style <- terminal_of("Button { width: auto; }")
  expect_equal(style$width$kind, "auto")
  expect_true(is.na(style$width$cells))
  expect_equal(terminal_of("Button { color: red; }")$height$kind, "auto")
  expect_equal(format(terminal_of("Button { width: 12; }")$width), "12")
})

test_that("width and height are the computed lengths, untouched", {
  computed <- compute_css("Button { width: 40; height: auto; }", ink_element("Button"))
  style <- ink_terminal_style(computed)
  expect_identical(style$width, computed$values$width)
  expect_identical(style$height, computed$values$height)
})

test_that("no layout is computed: sizes do not change with padding, borders or the profile", {
  plain <- terminal_of("Button { width: 20; }")
  padded <- terminal_of("Button { width: 20; padding: 5; border: heavy red; margin: 3; }")
  expect_identical(plain$width, padded$width)
  expect_equal(padded$width$cells, 20L)
  expect_named(plain, c(
    "foreground", "background", "text_styles", "border", "padding", "margin",
    "width", "height", "extra_properties", "capabilities", "matte"
  ))
  expect_false(any(c("content_width", "box", "x", "y", "viewport", "inner_width") %in% names(padded)))
})
