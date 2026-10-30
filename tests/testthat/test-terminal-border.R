glyph_names <- c("top_left", "top_right", "bottom_left", "bottom_right", "horizontal", "vertical")

border_of <- function(style, unicode = TRUE, color = "red", depth = "truecolor") {
  css <- if (style == "none") "Button { border: none; }" else sprintf("Button { border: %s %s; }", style, color)
  terminal_of(css, capabilities = caps(depth, unicode))$border
}

test_that("every Unicode border style has exactly the specified glyphs", {
  expected <- list(
    single = c(top_left = "┌", top_right = "┐", bottom_left = "└", bottom_right = "┘", horizontal = "─", vertical = "│"),
    double = c(top_left = "╔", top_right = "╗", bottom_left = "╚", bottom_right = "╝", horizontal = "═", vertical = "║"),
    round = c(top_left = "╭", top_right = "╮", bottom_left = "╰", bottom_right = "╯", horizontal = "─", vertical = "│"),
    heavy = c(top_left = "┏", top_right = "┓", bottom_left = "┗", bottom_right = "┛", horizontal = "━", vertical = "┃")
  )
  for (style in names(expected)) {
    border <- border_of(style)
    expect_identical(border$glyphs, expected[[style]], info = style)
    expect_equal(border$style, style)
    expect_true(border$unicode)
  }
})

test_that("the glyphs are the intended box-drawing characters", {
  expect_equal(utf8ToInt(border_of("single")$glyphs[["top_left"]]), 0x250CL)
  expect_equal(utf8ToInt(border_of("double")$glyphs[["vertical"]]), 0x2551L)
  expect_equal(utf8ToInt(border_of("round")$glyphs[["bottom_right"]]), 0x256FL)
  expect_equal(utf8ToInt(border_of("heavy")$glyphs[["horizontal"]]), 0x2501L)
})

test_that("the ASCII fallback is plain for every style", {
  for (style in c("single", "double", "round", "heavy")) {
    border <- border_of(style, unicode = FALSE)
    expect_identical(
      border$glyphs,
      c(top_left = "+", top_right = "+", bottom_left = "+", bottom_right = "+", horizontal = "-", vertical = "|"),
      info = style
    )
    expect_false(border$unicode)
    expect_true(all(utf8ToInt(paste(border$glyphs, collapse = "")) < 128L))
  }
})

test_that("every border style has a complete glyph set with one character each", {
  for (unicode in c(TRUE, FALSE)) {
    for (style in c("single", "double", "round", "heavy")) {
      glyphs <- border_of(style, unicode)$glyphs
      expect_named(glyphs, glyph_names)
      expect_true(all(nchar(glyphs) == 1L))
      expect_false(anyNA(glyphs))
    }
  }
  expect_setequal(names(UNICODE_BORDER_GLYPHS), c("single", "double", "round", "heavy"))
  expect_setequal(setdiff(BORDER_STYLES, "none"), names(UNICODE_BORDER_GLYPHS))
})

test_that("none has no glyphs and no color", {
  for (unicode in c(TRUE, FALSE)) {
    border <- border_of("none", unicode)
    expect_equal(border$style, "none")
    expect_null(border$glyphs)
    expect_null(border$color)
  }
  expect_equal(terminal_of("Button { padding: 1; }")$border$style, "none")
})

test_that("the border color is separate from the glyphs", {
  red <- border_of("single", color = "#ff0000")
  blue <- border_of("single", color = "#0000ff")
  expect_identical(red$glyphs, blue$glyphs)
  expect_false(identical(red$color, blue$color))
  expect_equal(unname(red$color$rgb), c(255L, 0L, 0L))
  expect_equal(unname(blue$color$rgb), c(0L, 0L, 255L))
  expect_s3_class(red$color, "ink_terminal_color")
  expect_false(any(vapply(red$glyphs, function(g) grepl("\033", g, fixed = TRUE), NA)))
})

test_that("border colors honor the color depth", {
  expect_equal(border_of("round", color = "#7c3aed", depth = "ansi256")$color$index, 99L)
  expect_equal(border_of("round", color = "#7c3aed", depth = "ansi16")$color$index, 12L)
  expect_equal(border_of("round", color = "green", depth = "ansi16")$color$index, 2L)
  expect_equal(border_of("round", color = "#7c3aed", depth = "none")$color$kind, "none")
  expect_equal(border_of("round", color = "default")$color$kind, "default")
})

test_that("all five specified examples adapt", {
  css <- c(
    "border: none;", "border: single red;", "border: double blue;",
    "border: round green;", "border: heavy #7c3aed;"
  )
  styles <- vapply(css, function(declaration) terminal_of(sprintf("Button { %s }", declaration))$border$style, "")
  expect_equal(unname(styles), c("none", "single", "double", "round", "heavy"))
})

test_that("a border is a definition only: nothing is drawn, written or positioned", {
  border <- border_of("round")
  expect_named(border, c("style", "glyphs", "color", "unicode"))
  expect_silent(border_of("heavy"))
  expect_false(any(grepl("\033", unlist(border$glyphs), fixed = TRUE)))
})
