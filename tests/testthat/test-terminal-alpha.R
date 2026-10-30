fg_rgb <- function(css, ..., depth = "truecolor") {
  terminal_of(css, capabilities = caps(depth), ...)$foreground
}

test_that("alpha 255 is an ordinary opaque color", {
  color <- fg_rgb("Button { color: #ff0000ff; }")
  expect_equal(color$kind, "rgb")
  expect_equal(unname(color$rgb), c(255L, 0L, 0L))
})

test_that("a fully transparent background draws nothing and needs no matte", {
  background <- terminal_of("Button { background: #ff000000; }")$background
  expect_equal(background$kind, "none")
  expect_equal(background$reason, "transparent")
  expect_equal(background$source, "rgb")
  expect_null(background$rgb)
})

test_that("a fully transparent background is not black and not default", {
  clear <- terminal_of("Button { background: #00000000; }")$background
  expect_false(identical(clear, terminal_of("Button { background: #000000; }")$background))
  expect_false(identical(clear, terminal_of("Button { background: default; }")$background))
})

test_that("partial alpha is composited over a known opaque background", {
  # 0x80 = 128/255 of red over opaque blue.
  style <- terminal_of("Button { color: #ff000080; background: #0000ff; }")
  expect_equal(unname(style$foreground$rgb), c(128L, 0L, 127L))
  expect_equal(unname(style$background$rgb), c(0L, 0L, 255L))
})

test_that("the compositing equation is alpha * foreground + (1 - alpha) * background, rounded", {
  expect_equal(composite_rgb(c(255, 0, 0), 128, c(0, 0, 255)), c(128L, 0L, 127L))
  expect_equal(composite_rgb(c(200, 100, 0), 255, c(0, 0, 0)), c(200L, 100L, 0L))
  expect_equal(composite_rgb(c(200, 100, 0), 0, c(10, 20, 30)), c(10L, 20L, 30L))
  expect_equal(composite_rgb(c(255, 255, 255), 51, c(0, 0, 0)), c(51L, 51L, 51L))
  expect_equal(composite_rgb(c(0, 0, 0), 51, c(255, 255, 255)), c(204L, 204L, 204L))
  # Results are rounded half up to whole channel values.
  expect_equal(composite_rgb(c(1, 1, 1), 128, c(0, 0, 0)), c(1L, 1L, 1L))
  expect_equal(composite_rgb(c(1, 1, 1), 127, c(0, 0, 0)), c(0L, 0L, 0L))
  expect_equal(composite_rgb(c(1, 1, 1), 100, c(0, 0, 0)), c(0L, 0L, 0L))
})

test_that("alpha 0 for a foreground shows the background", {
  style <- terminal_of("Button { color: #ff000000; background: #112233; }")
  expect_equal(unname(style$foreground$rgb), c(17L, 34L, 51L))
})

test_that("intermediate alpha lies between the two colors, monotonically", {
  alphas <- c(0, 32, 64, 128, 192, 224, 255)
  reds <- vapply(alphas, function(a) {
    css <- sprintf("Button { color: #ff0000%02x; background: #000000; }", a)
    fg_rgb(css)$rgb[[1L]]
  }, 0L)
  expect_true(all(diff(reds) >= 0L))
  expect_equal(reds[[1L]], 0L)
  expect_equal(reds[[length(reds)]], 255L)
})

test_that("a partially transparent background is composited over the matte", {
  style <- terminal_of("Button { background: #ffffff80; }", matte = "#000000")
  expect_equal(style$background$kind, "rgb")
  expect_equal(unname(style$background$rgb), c(128L, 128L, 128L))
  expect_equal(unname(style$matte), c(0L, 0L, 0L))
})

test_that("a partially transparent foreground uses the matte when the background is not concrete", {
  for (background in c("transparent", "default", "red")) {
    css <- sprintf("Button { color: #ffffff80; background: %s; }", background)
    style <- terminal_of(css, matte = "#000000")
    expect_equal(unname(style$foreground$rgb), c(128L, 128L, 128L), info = background)
  }
})

test_that("an opaque background wins over the matte as the underlying color", {
  style <- terminal_of("Button { color: #ffffff80; background: #0000ff; }", matte = "#ff0000")
  expect_equal(unname(style$foreground$rgb), c(128L, 128L, 255L))
})

test_that("a composited background underlies the foreground", {
  style <- terminal_of("Button { color: #ffffff80; background: #ffffff80; }", matte = "#000000")
  # The background is 128 grey; the foreground is white at half over that.
  expect_equal(unname(style$background$rgb), c(128L, 128L, 128L))
  expect_equal(unname(style$foreground$rgb), c(192L, 192L, 192L))
})

test_that("an unknown underlying background is an error, never a guess", {
  cases <- c(
    "Button { background: #7c3aed80; }",
    "Button { color: #ffffff80; }",
    "Button { color: #ffffff80; background: transparent; }",
    "Button { color: #ffffff80; background: default; }",
    "Button { color: #ffffff80; background: blue; }",
    "Button { color: #ffffff80; background: #12345600; }",
    "Button { border: single #ffffff80; }"
  )
  for (css in cases) {
    err <- expect_terminal_error(terminal_of(css), "unknown_alpha_background")
    expect_match(err$message, "matte", fixed = TRUE)
  }
})

test_that("a border color with alpha is composited too", {
  style <- terminal_of("Button { background: #000000; border: double #ffffff80; }")
  expect_equal(unname(style$border$color$rgb), c(128L, 128L, 128L))
  expect_terminal_error(terminal_of("Button { border: double #ffffff80; }"), "unknown_alpha_background")
})

test_that("quantization happens after compositing", {
  composited <- terminal_of("Button { color: #ffffff80; }", matte = "#000000", capabilities = caps("ansi256"))$foreground
  # The composited grey is (128, 128, 128), which is palette entry 244.
  expect_equal(unname(composited$rgb), c(128L, 128L, 128L))
  expect_equal(composited$index, 244L)
  sixteen <- terminal_of("Button { color: #ffffff80; }", matte = "#000000", capabilities = caps("ansi16"))$foreground
  expect_equal(sixteen$index, 8L)
  # Quantizing the uncomposited white would have given a different answer.
  expect_equal(terminal_of("Button { color: #ffffff; }", capabilities = caps("ansi256"))$foreground$index, 231L)
})

test_that("no color is displayed at depth none, so nothing is composited", {
  style <- terminal_of("Button { color: #ffffff80; background: #7c3aed80; }", capabilities = caps("none"))
  expect_equal(style$foreground$kind, "none")
  expect_equal(style$background$kind, "none")
})

test_that("ANSI and default colors need no compositing", {
  style <- terminal_of("Button { color: red; background: default; }")
  expect_equal(style$foreground$kind, "palette16")
  expect_equal(style$background$kind, "default")
})

test_that("the matte must be an opaque RGB color", {
  computed <- compute_css("Button { color: red; }", ink_element("Button"))
  good <- ink_terminal_style(computed, matte = "#181825")
  expect_equal(unname(good$matte), c(24L, 24L, 37L))
  expect_identical(ink_terminal_style(computed, matte = "#181825"), ink_terminal_style(computed, matte = "#181825ff"))
  expect_identical(ink_terminal_style(computed, matte = "#fff")$matte, c(255L, 255L, 255L))
  expect_identical(ink_terminal_style(computed, matte = new_ink_color_rgb(1, 2, 3))$matte, c(1L, 2L, 3L))
  for (bad in list("red", "bright-blue", "default", "transparent", "#ffffff80", "#ff", "nonsense", 5, NA_character_, TRUE, list("#fff"), c("#fff", "#000"), "")) {
    expect_terminal_error(ink_terminal_style(computed, matte = bad), "invalid_matte")
  }
  expect_terminal_error(ink_terminal_style(computed, matte = new_ink_color_ansi("red")), "invalid_matte")
  expect_terminal_error(ink_terminal_style(computed, matte = new_ink_color_default()), "invalid_matte")
  expect_terminal_error(ink_terminal_style(computed, matte = new_ink_color_rgb(1, 2, 3, 100)), "invalid_matte")
})

test_that("alpha boundary values behave consistently", {
  for (alpha in c(0L, 1L, 254L, 255L)) {
    css <- sprintf("Button { color: #ff0000%02x; background: #000000; }", alpha)
    style <- terminal_of(css)
    expect_equal(style$foreground$kind, "rgb")
    expect_true(style$foreground$rgb[[1L]] >= 0L && style$foreground$rgb[[1L]] <= 255L)
  }
  expect_equal(fg_rgb("Button { color: #ff000001; background: #000000; }")$rgb[[1L]], 1L)
  expect_equal(fg_rgb("Button { color: #ff0000fe; background: #000000; }")$rgb[[1L]], 254L)
})
