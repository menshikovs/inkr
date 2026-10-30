computed <- compute_css("Button { color: red; }", ink_element("Button"))
style <- ink_terminal_style(computed)

test_that("a value that is not a computed style is rejected", {
  for (bad in list(NULL, list(), "style", 1, computed$values, ink_registry(), style, ink_element("Button"))) {
    err <- expect_terminal_error(ink_terminal_style(bad), "invalid_computed_style")
    expect_equal(err$argument, "style")
  }
})

test_that("a computed style missing or damaging a built-in property is rejected, not repaired", {
  for (name in names(BUILTIN_TERMINAL_PROPERTIES)) {
    broken <- computed
    broken$values[[name]] <- NULL
    err <- expect_terminal_error(ink_terminal_style(broken), "invalid_computed_style")
    expect_match(err$message, name, fixed = TRUE)
    broken <- computed
    broken$values[[name]] <- "oops"
    expect_terminal_error(ink_terminal_style(broken), "invalid_computed_style")
  }
  broken <- computed
  broken$values <- "none"
  expect_terminal_error(ink_terminal_style(broken), "invalid_computed_style")
})

test_that("an unresolved keyword is rejected", {
  broken <- computed
  broken$values$color <- new_ink_keyword("inherit")
  expect_terminal_error(ink_terminal_style(broken), "invalid_computed_style")
  broken <- computed
  broken$values[["row-stripe"]] <- new_ink_keyword("initial")
  expect_terminal_error(ink_terminal_style(broken), "invalid_computed_style")
})

test_that("malformed typed values are rejected", {
  broken <- computed
  broken$values$border <- structure(list(style = "dotted", color = NULL), class = "ink_border")
  expect_terminal_error(ink_terminal_style(broken), "invalid_computed_style")
  broken <- computed
  broken$values$padding <- structure(list(values = c(top = -1L, right = 0L, bottom = 0L, left = 0L)), class = "ink_spacing")
  expect_terminal_error(ink_terminal_style(broken), "invalid_computed_style")
  broken <- computed
  broken$values[["text-style"]] <- structure(list(flags = "blink"), class = "ink_text_style")
  expect_terminal_error(ink_terminal_style(broken), "invalid_computed_style")
})

test_that("malformed colors are invalid color values", {
  bad_colors <- list(
    structure(list(kind = "rgb", r = 300L, g = 0L, b = 0L, a = 255L), class = "ink_color"),
    structure(list(kind = "rgb", r = 1L, g = 0L, b = 0L, a = NA_integer_), class = "ink_color"),
    structure(list(kind = "ansi", name = "red", index = 99L), class = "ink_color"),
    structure(list(kind = "plaid"), class = "ink_color")
  )
  for (color in bad_colors) {
    broken <- computed
    broken$values$color <- color
    expect_terminal_error(ink_terminal_style(broken), "invalid_color_value")
    broken <- computed
    broken$values$background <- color
    expect_terminal_error(ink_terminal_style(broken), "invalid_color_value")
  }
  broken <- compute_css("Button { border: single red; }", ink_element("Button"))
  broken$values$border$color <- bad_colors[[1L]]
  expect_terminal_error(ink_terminal_style(broken), "invalid_color_value")
})

test_that("capability problems are reported with their own codes", {
  expect_terminal_error(ink_terminal_style(computed, capabilities = "none"), "invalid_capabilities")
  expect_terminal_error(ink_terminal_style(computed, capabilities = NULL), "invalid_capabilities")
  expect_terminal_error(ink_terminal_capabilities(color_depth = "x"), "invalid_color_depth")
})

test_that("text must be one valid string", {
  for (bad in list(NULL, NA_character_, 1, TRUE, c("a", "b"), character(), list("a"), factor("a"))) {
    err <- expect_terminal_error(ink_ansi_wrap(bad, style), "invalid_text")
    expect_equal(err$argument, "text")
  }
  invalid_utf8 <- rawToChar(as.raw(c(0x61, 0xff, 0x62)))
  expect_terminal_error(ink_ansi_wrap(invalid_utf8, style), "invalid_text")
})

test_that("the style given to ink_ansi_wrap must be a terminal style", {
  for (bad in list(NULL, computed, list(), "style", 1, ink_terminal_capabilities())) {
    err <- expect_terminal_error(ink_ansi_wrap("x", bad), "invalid_terminal_style")
    expect_equal(err$argument, "style")
  }
})

test_that("a damaged terminal style is rejected", {
  broken <- style
  broken$foreground <- "red"
  expect_terminal_error(ink_ansi_wrap("x", broken), "invalid_terminal_style")
  broken <- style
  broken$foreground$kind <- "magenta"
  expect_terminal_error(ink_ansi_wrap("x", broken), "invalid_terminal_style")
  broken <- ink_terminal_style(compute_css("Button { color: #ff0000; }", ink_element("Button")))
  broken$foreground$rgb <- c(999L, 0L, 0L)
  expect_terminal_error(ink_ansi_wrap("x", broken), "invalid_terminal_style")
  broken <- ink_terminal_style(compute_css("Button { color: #ff0000; }", ink_element("Button")), caps("ansi256"))
  broken$foreground$index <- 300L
  expect_terminal_error(ink_ansi_wrap("x", broken), "invalid_terminal_style")
  broken <- style
  broken$text_styles$enabled <- "blink"
  expect_terminal_error(ink_ansi_wrap("x", broken), "invalid_terminal_style")
})

test_that("reset must be TRUE or FALSE", {
  for (bad in list(NA, "yes", 1, NULL, c(TRUE, FALSE), logical())) {
    err <- expect_terminal_error(ink_ansi_wrap("x", style, reset = bad), "invalid_reset")
    expect_equal(err$argument, "reset")
  }
})

test_that("alpha without a background is an error naming the remedy", {
  alpha <- compute_css("Button { color: #ffffff80; }", ink_element("Button"))
  err <- expect_terminal_error(ink_terminal_style(alpha), "unknown_alpha_background")
  expect_match(err$message, "matte", fixed = TRUE)
  expect_s3_class(ink_terminal_style(alpha, matte = "#000000"), "ink_terminal_style")
})

test_that("terminal errors are ink_terminal_error and nothing else", {
  err <- terminal_error(ink_terminal_style(NULL))
  expect_s3_class(err, "ink_terminal_error")
  expect_s3_class(err, "error")
  for (other in c("ink_compute_error", "ink_compile_error", "ink_parse_error", "ink_match_error")) {
    expect_false(inherits(err, other))
  }
  expect_named(unclass(err), c("message", "call", "code", "detail", "argument"))
  expect_equal(err$message, err$detail)
})

test_that("errors from earlier layers keep their own classes", {
  expect_error(compile_text("Button { color: reddish; }"), class = "ink_compile_error")
  expect_error(ink_compute(NULL, ink_element("A")), class = "ink_compute_error")
})

test_that("random bad input only produces ink_terminal_error", {
  junk <- list(NULL, NA, 1L, 2.5, "x", "", TRUE, character(), list(), list(1), factor("a"),
               mean, quote(x), new.env(), c(a = "b"), matrix(1:4, 2))
  for (bad in junk) {
    expect_terminal_error(ink_terminal_style(bad), "invalid_computed_style")
    expect_terminal_error(ink_terminal_style(computed, capabilities = bad), "invalid_capabilities")
    expect_terminal_error(ink_ansi_wrap("x", bad), "invalid_terminal_style")
  }
})

test_that("failed calls do not change their inputs", {
  before_computed <- computed
  before_style <- style
  terminal_error(ink_terminal_style(computed, matte = "nonsense"))
  terminal_error(ink_ansi_wrap("x", style, reset = NA))
  terminal_error(ink_ansi_wrap(NA_character_, style))
  expect_identical(computed, before_computed)
  expect_identical(style, before_style)
})

test_that("no global terminal state is touched", {
  options_before <- options()
  env_before <- Sys.getenv()
  ink_terminal_style(computed, caps("ansi256", FALSE), matte = "#000000")
  ink_ansi_wrap("x", ink_terminal_style(computed))
  expect_identical(options(), options_before)
  expect_identical(Sys.getenv(), env_before)
})
