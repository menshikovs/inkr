attrs <- function(declaration, capabilities = caps()) {
  terminal_of(sprintf("Button { text-style: %s; }", declaration), capabilities = capabilities)$text_styles
}

test_that("every individual flag is carried through", {
  for (flag in c("bold", "dim", "italic", "underline", "reverse", "strike")) {
    result <- attrs(flag)
    expect_equal(result$requested, flag)
    expect_equal(result$enabled, flag)
  }
})

test_that("multiple flags keep the fixed order", {
  expect_equal(attrs("bold underline")$enabled, c("bold", "underline"))
  expect_equal(attrs("underline bold")$enabled, c("bold", "underline"))
  expect_equal(
    attrs("strike reverse underline italic dim bold")$enabled,
    c("bold", "dim", "italic", "underline", "reverse", "strike")
  )
})

test_that("duplicate flags appear once", {
  expect_equal(attrs("bold bold underline bold")$enabled, c("bold", "underline"))
})

test_that("none means no attributes", {
  result <- attrs("none")
  expect_identical(result$requested, character())
  expect_identical(result$enabled, character())
  expect_identical(terminal_of("Button { padding: 1; }")$text_styles$enabled, character())
})

test_that("unsupported attributes are omitted but remembered as requested", {
  profile <- caps(text_styles = c("bold", "underline"))
  result <- attrs("bold italic underline strike", profile)
  expect_equal(result$requested, c("bold", "italic", "underline", "strike"))
  expect_equal(result$enabled, c("bold", "underline"))
  expect_equal(setdiff(result$requested, result$enabled), c("italic", "strike"))
})

test_that("a terminal without attributes enables none", {
  result <- attrs("bold underline", caps(text_styles = character()))
  expect_equal(result$requested, c("bold", "underline"))
  expect_identical(result$enabled, character())
})

test_that("attributes do not depend on color depth or Unicode", {
  for (profile in list(caps("none"), caps("ansi16", FALSE), caps("ansi256"))) {
    expect_equal(attrs("bold dim", profile)$enabled, c("bold", "dim"))
  }
})

test_that("the SGR codes are the standard ones", {
  expect_equal(
    SGR_ATTRIBUTES,
    c(bold = 1L, dim = 2L, italic = 3L, underline = 4L, reverse = 7L, strike = 9L)
  )
  expect_named(SGR_ATTRIBUTES, TEXT_STYLE_FLAGS)
})

test_that("the style is computed from the cascade and inheritance, then adapted", {
  css <- "Screen { text-style: italic; } Button { text-style: bold; }"
  sheet <- compile_text(css)
  inherited <- ink_terminal_style(ink_compute(sheet, ink_element("Panel", parent = ink_element("Screen"))))
  expect_equal(inherited$text_styles$enabled, "italic")
  own <- ink_terminal_style(ink_compute(sheet, ink_element("Button", parent = ink_element("Screen"))))
  expect_equal(own$text_styles$enabled, "bold")
})

test_that("constructing a terminal style emits no escape sequences", {
  style <- terminal_of("Button { text-style: bold underline; color: red; }")
  text <- unlist(rapply(unclass(style), as.character, how = "unlist"))
  expect_false(any(grepl("\033", text, fixed = TRUE)))
})
