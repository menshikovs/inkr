test_that("the 256-color palette constants are the xterm definitions", {
  expect_equal(CUBE_LEVELS, c(0, 95, 135, 175, 215, 255))
  expect_equal(GRAY_LEVELS[c(1, 2, 24)], c(8, 18, 238))
  expect_length(GRAY_LEVELS, 24L)
})

test_that("well-known colors map to well-known indices", {
  expect_equal(nearest_ansi256(c(0, 0, 0)), 16L)
  expect_equal(nearest_ansi256(c(255, 255, 255)), 231L)
  expect_equal(nearest_ansi256(c(255, 0, 0)), 196L)
  expect_equal(nearest_ansi256(c(0, 255, 0)), 46L)
  expect_equal(nearest_ansi256(c(0, 0, 255)), 21L)
  expect_equal(nearest_ansi256(c(255, 255, 0)), 226L)
  expect_equal(nearest_ansi256(c(95, 135, 175)), 67L)
  expect_equal(nearest_ansi256(c(135, 95, 255)), 99L)
})

test_that("greys prefer the grey ramp when it is nearer", {
  expect_equal(nearest_ansi256(c(8, 8, 8)), 232L)
  expect_equal(nearest_ansi256(c(128, 128, 128)), 244L)
  expect_equal(nearest_ansi256(c(238, 238, 238)), 255L)
  # 98 = 8 + 10 * 9 is exactly grey step 9 (index 241).
  expect_equal(nearest_ansi256(c(98, 98, 98)), 241L)
})

test_that("channels snap to the lower level on a tie", {
  # 115 is exactly between 95 and 135.
  expect_equal(nearest_ansi256(c(115, 0, 0)), 52L)
  expect_equal(nearest_ansi256(c(0, 115, 0)), 22L)
  expect_equal(nearest_ansi256(c(0, 0, 115)), 17L)
})

test_that("the color cube wins a tie against the grey ramp", {
  ties <- 0L
  for (value in 0:255) {
    cube_distance <- min(abs(CUBE_LEVELS - value))
    gray_distance <- min(abs(GRAY_LEVELS - value))
    if (cube_distance == gray_distance) {
      ties <- ties + 1L
      level <- which.min(abs(CUBE_LEVELS - value))
      expect_equal(nearest_ansi256(c(value, value, value)), 16L + 43L * (level - 1L), info = as.character(value))
    }
  }
  expect_gt(ties, 0L)
})

test_that("the result always lies in the usable range 16-255", {
  grid <- expand.grid(r = seq(0, 255, 17), g = seq(0, 255, 17), b = seq(0, 255, 17))
  indices <- vapply(seq_len(nrow(grid)), function(i) nearest_ansi256(unlist(grid[i, ])), 0L)
  expect_true(all(indices >= 16L & indices <= 255L))
})

test_that("it agrees with a brute-force nearest search over the whole palette", {
  palette <- reference_palette_256()
  expect_equal(nrow(palette), 240L)
  grid <- expand.grid(r = seq(0, 255, 15), g = seq(0, 255, 15), b = seq(0, 255, 15))
  set.seed(9)
  random <- matrix(sample(0:255, 3000L, replace = TRUE), ncol = 3L)
  colors <- rbind(as.matrix(grid), random)
  disagreements <- 0L
  for (i in seq_len(nrow(colors))) {
    rgb <- as.numeric(colors[i, ])
    if (nearest_ansi256(rgb) != reference_nearest(rgb, palette, 16L)) disagreements <- disagreements + 1L
  }
  expect_equal(disagreements, 0L)
})

test_that("quantization is deterministic", {
  rgb <- c(37, 201, 99)
  expect_identical(nearest_ansi256(rgb), nearest_ansi256(rgb))
  expect_identical(
    vapply(1:3, function(i) nearest_ansi256(c(124, 58, 237)), 0L),
    rep(99L, 3L)
  )
})

test_that("a style converted to 256 colors carries the index and the source RGB", {
  style <- terminal_of("Button { color: #5f87af; }", capabilities = caps("ansi256"))
  expect_equal(style$foreground$kind, "palette256")
  expect_equal(style$foreground$index, 67L)
  expect_equal(unname(style$foreground$rgb), c(95L, 135L, 175L))
})
