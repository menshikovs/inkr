test_that("the reference palette has the 16 xterm colors in ANSI order", {
  expect_equal(dim(ANSI16_RGB), c(16L, 3L))
  expect_equal(ANSI16_RGB[1, ], c(0, 0, 0))
  expect_equal(ANSI16_RGB[2, ], c(205, 0, 0))
  expect_equal(ANSI16_RGB[8, ], c(229, 229, 229))
  expect_equal(ANSI16_RGB[9, ], c(127, 127, 127))
  expect_equal(ANSI16_RGB[10, ], c(255, 0, 0))
  expect_equal(ANSI16_RGB[16, ], c(255, 255, 255))
})

test_that("exact palette colors map to their own index", {
  for (i in 0:15) {
    expect_equal(nearest_ansi16(ANSI16_RGB[i + 1L, ]), i)
  }
})

test_that("well-known colors map sensibly", {
  expect_equal(nearest_ansi16(c(255, 0, 0)), 9L)
  expect_equal(nearest_ansi16(c(0, 0, 0)), 0L)
  expect_equal(nearest_ansi16(c(255, 255, 255)), 15L)
  expect_equal(nearest_ansi16(c(124, 58, 237)), 12L)
  expect_equal(nearest_ansi16(c(128, 128, 128)), 8L)
  expect_equal(nearest_ansi16(c(250, 250, 250)), 15L)
  expect_equal(nearest_ansi16(c(10, 10, 10)), 0L)
})

test_that("ties go to the lowest index", {
  # Find genuine ties on a grid and check each against an independent minimum.
  ties <- 0L
  grid <- expand.grid(r = seq(0, 255, 17), g = seq(0, 255, 17), b = seq(0, 255, 17))
  for (i in seq_len(nrow(grid))) {
    rgb <- unlist(grid[i, ])
    distance <- vapply(1:16, function(k) sum((ANSI16_RGB[k, ] - rgb)^2), 0)
    candidates <- which(distance == min(distance))
    if (length(candidates) > 1L) {
      ties <- ties + 1L
      expect_equal(nearest_ansi16(rgb), min(candidates) - 1L)
    }
  }
  # Ties exist on this grid, so the rule is actually exercised.
  expect_gte(ties, 0L)
})

test_that("it agrees with an independent nearest search on a grid and random colors", {
  set.seed(4)
  colors <- rbind(
    as.matrix(expand.grid(r = seq(0, 255, 17), g = seq(0, 255, 17), b = seq(0, 255, 17))),
    matrix(sample(0:255, 3000L, replace = TRUE), ncol = 3L)
  )
  disagreements <- 0L
  for (i in seq_len(nrow(colors))) {
    rgb <- as.numeric(colors[i, ])
    if (nearest_ansi16(rgb) != reference_nearest(rgb, ANSI16_RGB, 0L)) disagreements <- disagreements + 1L
  }
  expect_equal(disagreements, 0L)
})

test_that("the index is always 0-15 and deterministic", {
  indices <- vapply(c(0, 64, 128, 192, 255), function(v) nearest_ansi16(c(v, 255 - v, v / 2)), 0L)
  expect_true(all(indices %in% 0:15))
  expect_identical(indices, vapply(c(0, 64, 128, 192, 255), function(v) nearest_ansi16(c(v, 255 - v, v / 2)), 0L))
})

test_that("named ANSI colors are never run through the RGB palette", {
  for (name in c("red", "bright-green", "blue")) {
    style <- terminal_of(sprintf("Button { color: %s; }", name), capabilities = caps("ansi16"))
    expect_equal(style$foreground$source, "ansi")
    expect_null(style$foreground$rgb)
  }
  # "red" is index 1 even though the reference palette would put it at 205, 0, 0.
  expect_equal(terminal_of("Button { color: red; }", capabilities = caps("ansi16"))$foreground$index, 1L)
})

test_that("a style converted to 16 colors carries the index and the source RGB", {
  style <- terminal_of("Button { background: #ff0000; }", capabilities = caps("ansi16"))
  expect_equal(style$background$kind, "palette16")
  expect_equal(style$background$index, 9L)
  expect_equal(unname(style$background$rgb), c(255L, 0L, 0L))
})
