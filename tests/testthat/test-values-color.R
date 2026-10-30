test_that("hex colors of every length are accepted", {
  c3 <- value("color", "#f80")
  expect_equal(c(c3$r, c3$g, c3$b, c3$a), c(255L, 136L, 0L, 255L))
  c4 <- value("color", "#f808")
  expect_equal(c(c4$r, c4$g, c4$b, c4$a), c(255L, 136L, 0L, 136L))
  c6 <- value("color", "#7c3aed")
  expect_equal(c(c6$r, c6$g, c6$b, c6$a), c(124L, 58L, 237L, 255L))
  c8 <- value("color", "#7C3AED80")
  expect_equal(c(c8$r, c8$g, c8$b, c8$a), c(124L, 58L, 237L, 128L))
})

test_that("short hex forms normalize to their long equivalents", {
  expect_identical(value("color", "#fff"), value("color", "#ffffff"))
  expect_identical(value("color", "#ffff"), value("color", "#ffffffff"))
  expect_identical(value("color", "#f808"), value("color", "#ff880088"))
  expect_identical(value("color", "#ABCDEF"), value("color", "#abcdef"))
})

test_that("alpha is an explicit channel and formats only when not opaque", {
  expect_equal(value("color", "#00000000")$a, 0L)
  expect_equal(value("color", "#000")$a, 255L)
  expect_equal(format(value("color", "#7C3AED")), "#7c3aed")
  expect_equal(format(value("color", "#7C3AED80")), "#7c3aed80")
  expect_equal(format(value("color", "#f80")), "#ff8800")
  expect_equal(format(value("color", "#0000")), "#00000000")
})

test_that("channels are integers", {
  color <- value("color", "#102030")
  expect_type(c(color$r, color$g, color$b, color$a), "integer")
})

test_that("all sixteen ANSI colors are accepted", {
  base <- c("black", "red", "green", "yellow", "blue", "magenta", "cyan", "white")
  for (i in seq_along(base)) {
    normal <- value("color", base[[i]])
    expect_equal(normal$kind, "ansi")
    expect_equal(normal$name, base[[i]])
    expect_equal(normal$index, i - 1L)
    bright <- value("color", paste0("bright-", base[[i]]))
    expect_equal(bright$kind, "ansi")
    expect_equal(bright$name, paste0("bright-", base[[i]]))
    expect_equal(bright$index, i + 7L)
  }
})

test_that("default and transparent are distinct from every other color", {
  expect_equal(value("color", "default")$kind, "default")
  expect_equal(value("color", "transparent")$kind, "transparent")
  expect_false(identical(value("color", "transparent"), value("color", "#00000000")))
  expect_false(identical(value("color", "transparent"), value("color", "black")))
  expect_false(identical(value("color", "transparent"), value("color", "#000")))
  expect_false(identical(value("color", "default"), value("color", "white")))
})

test_that("ANSI colors stay distinct from RGB colors", {
  expect_false(identical(value("color", "red"), value("color", "#ff0000")))
  expect_equal(value("color", "red")$kind, "ansi")
  expect_null(value("color", "red")$r)
  expect_null(value("color", "#ff0000")$name)
})

test_that("colors have class ink_color and a readable format", {
  for (text in c("#fff", "red", "bright-blue", "default", "transparent")) {
    expect_s3_class(value("color", text), "ink_color")
  }
  expect_equal(format(value("color", "bright-blue")), "bright-blue")
  expect_equal(format(value("color", "default")), "default")
  expect_output(print(value("color", "red")), "<ink_color> red", fixed = TRUE)
})

test_that("whitespace around a color is ignored", {
  expect_identical(value("color", "  red \n"), value("color", "red"))
  expect_identical(value("color", "\t#fff "), value("color", "#fff"))
  expect_identical(value("color", "/* c */ red"), value("color", "red"))
})

test_that("invalid colors are rejected", {
  invalid <- c(
    "", "reddish", "Red", "RED", "bright", "bright-", "bright-gray", "grey",
    "rgb", "1", "50%", "red blue", "red, blue", "#ff", "#fffff", "#fffffffff",
    "#gggggg", "inherit", "initial", "auto", "none"
  )
  for (text in invalid) {
    expect_value_problem("color", text, "invalid_color")
  }
})

test_that("a variable is not allowed in a bare value", {
  expect_value_problem("color", "$primary", "invalid_property_value")
})

test_that("color problems explain what is expected", {
  problem <- value_problem("color", "reddish")
  expect_match(problem$expected, "Unknown color 'reddish'", fixed = TRUE)
  expect_match(problem$expected, "bright-", fixed = TRUE)
})

test_that("the color constructors validate their input", {
  expect_error(new_ink_color_rgb(256, 0, 0))
  expect_error(new_ink_color_rgb(-1, 0, 0))
  expect_error(new_ink_color_rgb(0.5, 0, 0))
  expect_error(new_ink_color_ansi("grey"))
})
