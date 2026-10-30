integration_css <- "
  $primary: #7c3aed;

  Screen {
      color: white;
      background: #181825;
  }

  Button {
      padding: 1 2;
      border: round $primary;
  }

  Button.primary {
      background: $primary;
      text-style: bold;
  }
"

integration <- function(...) {
  sheet <- compile_text(integration_css)
  screen <- ink_element("Screen")
  button <- ink_element("Button", classes = "primary", parent = screen)
  list(sheet = sheet, screen = screen, button = button, computed = ink_compute(sheet, button))
}

test_that("ink_terminal_style returns an ink_terminal_style with the documented fields", {
  terminal <- ink_terminal_style(integration()$computed)
  expect_s3_class(terminal, "ink_terminal_style")
  expect_named(
    terminal,
    c(
      "foreground", "background", "text_styles", "border", "padding", "margin",
      "width", "height", "extra_properties", "capabilities", "matte"
    )
  )
  expect_s3_class(terminal$foreground, "ink_terminal_color")
  expect_s3_class(terminal$capabilities, "ink_terminal_capabilities")
})

test_that("the full pipeline: parse, compile, match, compute, adapt", {
  ast <- ink_parse(integration_css)
  sheet <- ink_compile(ast)
  screen <- ink_element("Screen")
  button <- ink_element("Button", classes = "primary", parent = screen)
  terminal <- ink_terminal_style(ink_compute(sheet, button))

  # The foreground is inherited from the Screen: the ANSI colour white.
  expect_equal(terminal$foreground$kind, "palette16")
  expect_equal(terminal$foreground$source, "ansi")
  expect_equal(terminal$foreground$index, 7L)
  # The background comes from Button.primary through the variable.
  expect_equal(terminal$background$kind, "rgb")
  expect_equal(unname(terminal$background$rgb), c(124L, 58L, 237L))
  expect_equal(terminal$text_styles$enabled, "bold")
  expect_equal(terminal$padding, c(top = 1L, right = 2L, bottom = 1L, left = 2L))
  expect_equal(terminal$border$style, "round")
  expect_equal(unname(terminal$border$glyphs[c("top_left", "top_right", "bottom_left", "bottom_right")]),
               c("╭", "╮", "╰", "╯"))
  expect_equal(unname(terminal$border$color$rgb), c(124L, 58L, 237L))
  expect_equal(terminal$width$kind, "auto")
})

test_that("the same element without the Screen parent inherits nothing", {
  sheet <- compile_text(integration_css)
  terminal <- ink_terminal_style(ink_compute(sheet, ink_element("Button", classes = "primary")))
  expect_equal(terminal$foreground$kind, "default")
  expect_equal(terminal$foreground$source, "default")
})

test_that("an unstyled element adapts to default colors, no fill and no attributes", {
  terminal <- terminal_of("")
  expect_equal(terminal$foreground$kind, "default")
  expect_equal(terminal$background$kind, "none")
  expect_equal(terminal$background$reason, "transparent")
  expect_length(terminal$text_styles$enabled, 0L)
  expect_equal(terminal$border$style, "none")
  expect_null(terminal$border$glyphs)
  expect_equal(terminal$padding, c(top = 0L, right = 0L, bottom = 0L, left = 0L))
  expect_length(terminal$extra_properties, 0L)
})

test_that("conversion does not modify the computed style", {
  computed <- integration()$computed
  before <- computed
  ink_terminal_style(computed)
  ink_terminal_style(computed, caps("ansi16", FALSE, "bold"), matte = "#000000")
  expect_identical(computed, before)
})

test_that("one computed style can be adapted for several profiles", {
  computed <- integration()$computed
  profiles <- list(caps("truecolor"), caps("ansi256"), caps("ansi16"), caps("none"))
  styles <- lapply(profiles, function(p) ink_terminal_style(computed, p))
  kinds <- vapply(styles, function(s) s$background$kind, "")
  expect_equal(kinds, c("rgb", "palette256", "palette16", "none"))
  expect_identical(computed, integration()$computed)
})

test_that("adapting is deterministic and equal inputs give equal outputs", {
  computed <- integration()$computed
  expect_identical(ink_terminal_style(computed), ink_terminal_style(computed))
  a <- terminal_of("Button { color: #fff; background: #ffffff; }")
  b <- terminal_of("Button{color:#ffffff;background:#fff;}")
  expect_identical(a, b)
})

test_that("computed styles from ink_compute and ink_session_compute are accepted alike", {
  sheet <- compile_text(integration_css)
  session <- ink_session(sheet)
  ink_session_set(session, "screen", type = "Screen")
  ink_session_set(session, "save", type = "Button", classes = "primary", parent_key = "screen")
  from_session <- ink_terminal_style(ink_session_compute(session, "save"))
  from_stateless <- ink_terminal_style(ink_compute(sheet, integration()$button))
  expect_identical(from_session, from_stateless)
})

test_that("a session can be adapted again after a change", {
  sheet <- compile_text(integration_css)
  session <- ink_session(sheet)
  ink_session_set(session, "b", type = "Button", classes = "primary")
  before <- ink_terminal_style(ink_session_compute(session, "b"))
  ink_session_set(session, "b", classes = character())
  after <- ink_terminal_style(ink_session_compute(session, "b"))
  expect_equal(before$background$kind, "rgb")
  expect_equal(after$background$kind, "none")
})

test_that("printing summarizes the style without escape sequences", {
  terminal <- ink_terminal_style(integration()$computed)
  output <- capture.output(result <- print(terminal))
  expect_identical(result, terminal)
  expect_equal(output[[1L]], "<ink_terminal_style> truecolor, unicode")
  expect_true(any(grepl("^  background  rgb\\(124, 58, 237\\)$", output)))
  expect_true(any(grepl("^  text styles bold$", output)))
  expect_true(any(grepl("round", output)))
  expect_true(any(grepl("padding     top=1 right=2 bottom=1 left=2", output, fixed = TRUE)))
  expect_false(any(grepl("\033", output, fixed = TRUE)))
  none <- capture.output(print(ink_terminal_style(integration()$computed, caps("none", FALSE, "bold"))))
  expect_true(any(grepl("color_depth_none", none, fixed = TRUE)))
  expect_true(any(grepl("+-+", none, fixed = TRUE)))
})

test_that("printing notes attributes the terminal cannot show", {
  output <- capture.output(print(terminal_of(
    "Button { text-style: bold underline; }", capabilities = caps(text_styles = "bold")
  )))
  expect_true(any(grepl("omitted: underline", output, fixed = TRUE)))
})

test_that("adapting writes nothing to the console", {
  computed <- integration()$computed
  expect_silent(ink_terminal_style(computed))
  expect_silent(ink_terminal_style(computed, caps("ansi256", FALSE)))
  expect_silent(ink_ansi_wrap("text", ink_terminal_style(computed)))
})
