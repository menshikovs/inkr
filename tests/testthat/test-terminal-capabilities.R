test_that("the defaults describe a full-featured terminal", {
  capabilities <- ink_terminal_capabilities()
  expect_s3_class(capabilities, "ink_terminal_capabilities")
  expect_named(capabilities, c("color_depth", "unicode", "text_styles"))
  expect_equal(capabilities$color_depth, "truecolor")
  expect_true(capabilities$unicode)
  expect_equal(capabilities$text_styles, c("bold", "dim", "italic", "underline", "reverse", "strike"))
})

test_that("every color depth is accepted", {
  for (depth in c("truecolor", "ansi256", "ansi16", "none")) {
    expect_equal(ink_terminal_capabilities(color_depth = depth)$color_depth, depth)
  }
})

test_that("invalid color depths are rejected", {
  for (depth in list("rainbow", "TRUECOLOR", "24bit", "", NA_character_, 256, NULL, c("none", "ansi16"), TRUE, character())) {
    err <- expect_terminal_error(ink_terminal_capabilities(color_depth = depth), "invalid_color_depth")
    expect_equal(err$argument, "color_depth")
  }
  expect_match(terminal_error(ink_terminal_capabilities("rainbow"))$message, "truecolor", fixed = TRUE)
})

test_that("unicode must be TRUE or FALSE", {
  expect_false(ink_terminal_capabilities(unicode = FALSE)$unicode)
  for (bad in list(NA, "yes", 1, NULL, c(TRUE, FALSE), logical())) {
    err <- expect_terminal_error(ink_terminal_capabilities(unicode = bad), "invalid_capabilities")
    expect_equal(err$argument, "unicode")
  }
})

test_that("text styles are validated, deduplicated and put in the fixed order", {
  capabilities <- ink_terminal_capabilities(text_styles = c("underline", "bold", "bold", "strike"))
  expect_equal(capabilities$text_styles, c("bold", "underline", "strike"))
  expect_identical(ink_terminal_capabilities(text_styles = character())$text_styles, character())
  for (bad in list("blink", "none", NA_character_, 1, NULL, list("bold"), c("bold", "Bold"))) {
    err <- expect_terminal_error(ink_terminal_capabilities(text_styles = bad), "invalid_capabilities")
    expect_equal(err$argument, "text_styles")
  }
  expect_match(terminal_error(ink_terminal_capabilities(text_styles = "blink"))$message, "'blink'", fixed = TRUE)
})

test_that("capabilities are deterministic and independent of the environment", {
  keys <- c("TERM", "COLORTERM", "NO_COLOR", "CLICOLOR", "FORCE_COLOR")
  old <- Sys.getenv(keys, unset = NA)
  on.exit({
    for (key in keys) {
      if (is.na(old[[key]])) Sys.unsetenv(key) else do.call(Sys.setenv, structure(list(old[[key]]), names = key))
    }
  })
  baseline <- ink_terminal_capabilities()
  Sys.setenv(TERM = "dumb", COLORTERM = "", NO_COLOR = "1", CLICOLOR = "0", FORCE_COLOR = "0")
  expect_identical(ink_terminal_capabilities(), baseline)
  Sys.setenv(TERM = "xterm-256color", COLORTERM = "truecolor")
  expect_identical(ink_terminal_capabilities(), baseline)
  expect_identical(ink_terminal_capabilities("ansi16"), ink_terminal_capabilities("ansi16"))
})

test_that("creating and printing capabilities writes nothing but the print output", {
  expect_silent(ink_terminal_capabilities("none", FALSE, "bold"))
  output <- capture.output(result <- print(ink_terminal_capabilities("ansi16", FALSE, c("bold", "dim"))))
  expect_identical(result, ink_terminal_capabilities("ansi16", FALSE, c("bold", "dim")))
  expect_equal(
    output,
    "<ink_terminal_capabilities> color: ansi16 | unicode: no (ASCII) | text styles: bold dim"
  )
  expect_false(any(grepl("\033", output, fixed = TRUE)))
  expect_match(capture.output(print(ink_terminal_capabilities(text_styles = character()))), "text styles: none", fixed = TRUE)
})

test_that("capabilities objects are checked wherever they are accepted", {
  style <- compute_css("Button { color: red; }", ink_element("Button"))
  for (bad in list(NULL, "truecolor", list(color_depth = "truecolor", unicode = TRUE, text_styles = character()), 1)) {
    expect_terminal_error(ink_terminal_style(style, capabilities = bad), "invalid_capabilities")
  }
  damaged <- ink_terminal_capabilities()
  damaged$color_depth <- "rainbow"
  expect_terminal_error(ink_terminal_style(style, capabilities = damaged), "invalid_color_depth")
  damaged <- ink_terminal_capabilities()
  damaged$unicode <- NA
  expect_terminal_error(ink_terminal_style(style, capabilities = damaged), "invalid_capabilities")
})
