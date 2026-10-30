fg <- function(css_color, depth = "truecolor", ...) {
  terminal_of(sprintf("Button { color: %s; }", css_color), capabilities = caps(depth), ...)$foreground
}

bg <- function(css_color, depth = "truecolor", ...) {
  terminal_of(sprintf("Button { background: %s; }", css_color), capabilities = caps(depth), ...)$background
}

test_that("truecolor keeps opaque RGB exactly", {
  color <- fg("#7c3aed")
  expect_equal(color$kind, "rgb")
  expect_equal(color$source, "rgb")
  expect_equal(unname(color$rgb), c(124L, 58L, 237L))
  expect_equal(unname(bg("#000000")$rgb), c(0L, 0L, 0L))
  expect_equal(unname(bg("#ffffff")$rgb), c(255L, 255L, 255L))
})

test_that("short and long hex forms give the same terminal color", {
  expect_identical(fg("#f80"), fg("#ff8800"))
  expect_identical(fg("#f80f"), fg("#ff8800"))
  expect_identical(fg("#FFF"), fg("#ffffff"))
  expect_identical(fg("#ffffffff"), fg("#fff"))
  expect_equal(unname(fg("#f80")$rgb), c(255L, 136L, 0L))
})

test_that("RGB colors are quantized to the xterm 256 palette", {
  color <- fg("#7c3aed", "ansi256")
  expect_equal(color$kind, "palette256")
  expect_equal(color$index, 99L)
  expect_equal(unname(color$rgb), c(124L, 58L, 237L))
  expect_equal(fg("#ff0000", "ansi256")$index, 196L)
  expect_equal(fg("#000000", "ansi256")$index, 16L)
  expect_equal(fg("#ffffff", "ansi256")$index, 231L)
  expect_equal(fg("#808080", "ansi256")$index, 244L)
  expect_equal(fg("#080808", "ansi256")$index, 232L)
  expect_equal(fg("#eeeeee", "ansi256")$index, 255L)
})

test_that("RGB colors are quantized to the 16 ANSI colors", {
  expect_equal(fg("#7c3aed", "ansi16")$kind, "palette16")
  expect_equal(fg("#7c3aed", "ansi16")$index, 12L)
  expect_equal(fg("#ff0000", "ansi16")$index, 9L)
  expect_equal(fg("#000000", "ansi16")$index, 0L)
  expect_equal(fg("#ffffff", "ansi16")$index, 15L)
  expect_equal(fg("#cd0000", "ansi16")$index, 1L)
  expect_equal(fg("#808080", "ansi16")$index, 8L)
})

test_that("named ANSI colors keep their ANSI meaning at every depth", {
  for (depth in c("truecolor", "ansi256", "ansi16")) {
    color <- fg("red", depth)
    expect_equal(color$kind, "palette16", info = depth)
    expect_equal(color$source, "ansi", info = depth)
    expect_equal(color$index, 1L, info = depth)
    expect_null(color$rgb)
  }
  expect_equal(fg("bright-blue")$index, 12L)
  expect_equal(bg("black")$index, 0L)
  expect_equal(fg("bright-white")$index, 15L)
  indexes <- vapply(c(ANSI_BASE_COLORS, paste0("bright-", ANSI_BASE_COLORS)), function(n) fg(n)$index, 0L)
  expect_equal(unname(indexes), 0:15)
})

test_that("an ANSI named color is not the same as its RGB look-alike", {
  expect_false(identical(fg("red"), fg("#ff0000")))
  expect_equal(fg("red", "truecolor")$kind, "palette16")
  expect_equal(fg("#ff0000", "truecolor")$kind, "rgb")
})

test_that("color depth none produces no color instruction and no black", {
  for (style in list(
    terminal_of("Button { color: #7c3aed; background: #181825; border: single red; }", capabilities = caps("none")),
    terminal_of("Button { color: red; background: blue; }", capabilities = caps("none")),
    terminal_of("Button { color: default; background: transparent; }", capabilities = caps("none"))
  )) {
    expect_equal(style$foreground$kind, "none")
    expect_equal(style$background$kind, "none")
    expect_equal(style$foreground$reason, "color_depth_none")
    expect_null(style$foreground$rgb)
    expect_null(style$background$rgb)
  }
  style <- terminal_of("Button { border: single red; }", capabilities = caps("none"))
  expect_equal(style$border$color$kind, "none")
  expect_equal(style$border$style, "single")
})

test_that("the terminal default color is an instruction, not RGB", {
  for (depth in c("truecolor", "ansi256", "ansi16")) {
    color <- fg("default", depth)
    expect_equal(color$kind, "default")
    expect_equal(color$source, "default")
    expect_null(color$rgb)
    expect_null(color$index)
    expect_equal(bg("default", depth)$kind, "default")
  }
})

test_that("transparent is no fill, distinct from default and from black", {
  transparent <- bg("transparent")
  expect_equal(transparent$kind, "none")
  expect_equal(transparent$source, "transparent")
  expect_equal(transparent$reason, "transparent")
  expect_false(identical(transparent, bg("default")))
  expect_false(identical(transparent, bg("#000000")))
  expect_false(identical(transparent, bg("black")))
  expect_equal(fg("transparent")$kind, "none")
})

test_that("the initial colors are default text and no background", {
  style <- terminal_of("Button { padding: 1; }")
  expect_equal(style$foreground$kind, "default")
  expect_equal(style$background$kind, "none")
  expect_equal(style$background$source, "transparent")
})

test_that("each depth gives a different but deterministic result", {
  results <- lapply(c("truecolor", "ansi256", "ansi16"), function(d) fg("#336699", d))
  expect_equal(vapply(results, function(r) r$kind, ""), c("rgb", "palette256", "palette16"))
  expect_identical(fg("#336699", "ansi256"), fg("#336699", "ansi256"))
  expect_equal(fg("#336699", "ansi256")$index, 60L)
})

test_that("border colors follow the same conversion", {
  style <- terminal_of("Button { border: single #7c3aed; }", capabilities = caps("ansi256"))
  expect_equal(style$border$color$kind, "palette256")
  expect_equal(style$border$color$index, 99L)
  style <- terminal_of("Button { border: double red; }", capabilities = caps("ansi16"))
  expect_equal(style$border$color$index, 1L)
  style <- terminal_of("Button { border: heavy transparent; }")
  expect_equal(style$border$color$kind, "none")
})
