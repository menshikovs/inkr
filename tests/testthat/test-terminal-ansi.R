w <- function(declarations, text = "Save", depth = "truecolor", reset = TRUE, unicode = TRUE, text_styles = TEXT_STYLE_FLAGS) {
  css <- sprintf("Button { %s }", declarations)
  ink_ansi_wrap(
    text, terminal_of(css, capabilities = caps(depth, unicode, text_styles)), reset = reset
  )
}

test_that("bold red text under truecolor is the specified sequence", {
  expect_identical(w("color: #ff0000; text-style: bold;"), "\033[1;38;2;255;0;0mSave\033[0m")
})

test_that("a truecolor foreground", {
  expect_identical(w("color: #7c3aed;"), "\033[38;2;124;58;237mSave\033[0m")
})

test_that("a truecolor background", {
  expect_identical(w("background: #00ff00;"), "\033[48;2;0;255;0mSave\033[0m")
})

test_that("foreground and background come after the attributes, foreground first", {
  expect_identical(
    w("color: #ff0000; background: #0000ff; text-style: bold underline;"),
    "\033[1;4;38;2;255;0;0;48;2;0;0;255mSave\033[0m"
  )
})

test_that("256-color sequences use the 38;5 and 48;5 forms", {
  expect_identical(w("color: #7c3aed;", depth = "ansi256"), "\033[38;5;99mSave\033[0m")
  expect_identical(w("background: #7c3aed;", depth = "ansi256"), "\033[48;5;99mSave\033[0m")
  expect_identical(w("color: #ff0000; background: #000000;", depth = "ansi256"), "\033[38;5;196;48;5;16mSave\033[0m")
})

test_that("16-color sequences use the 30-37 / 90-97 and 40-47 / 100-107 ranges", {
  expect_identical(w("color: #ff0000;", depth = "ansi16"), "\033[91mSave\033[0m")
  expect_identical(w("background: #ff0000;", depth = "ansi16"), "\033[101mSave\033[0m")
  expect_identical(w("color: red;", depth = "ansi16"), "\033[31mSave\033[0m")
  expect_identical(w("color: black;"), "\033[30mSave\033[0m")
  expect_identical(w("color: white;"), "\033[37mSave\033[0m")
  expect_identical(w("background: blue;"), "\033[44mSave\033[0m")
  expect_identical(w("color: bright-blue;"), "\033[94mSave\033[0m")
  expect_identical(w("background: bright-white;"), "\033[107mSave\033[0m")
  expect_identical(w("color: bright-black; background: bright-red;", depth = "truecolor"), "\033[90;101mSave\033[0m")
})

test_that("named ANSI colors give the same sequence at every non-none depth", {
  for (depth in c("truecolor", "ansi256", "ansi16")) {
    expect_identical(w("color: green;", depth = depth), "\033[32mSave\033[0m")
  }
})

test_that("no-color mode emits no color instruction", {
  expect_identical(w("color: red; background: blue;", depth = "none"), "Save")
  expect_identical(w("color: #7c3aed; background: #181825; text-style: bold;", depth = "none"), "\033[1mSave\033[0m")
  expect_false(grepl("38;|48;|3[0-7]m", w("color: #7c3aed; text-style: bold;", depth = "none")))
})

test_that("the terminal default color is SGR 39 / 49 when the stylesheet asks for it", {
  expect_identical(w("color: default;"), "\033[39mSave\033[0m")
  expect_identical(w("background: default;"), "\033[49mSave\033[0m")
  expect_identical(w("color: default; background: default;"), "\033[39;49mSave\033[0m")
  expect_identical(w("color: default; text-style: bold;"), "\033[1;39mSave\033[0m")
  expect_identical(w("color: default; background: default; text-style: underline;"), "\033[4;39;49mSave\033[0m")
  expect_identical(w("color: red; background: default;"), "\033[31;49mSave\033[0m")
  expect_identical(w("color: initial;"), "\033[39mSave\033[0m")
})

test_that("an unset color is also `default` but sends nothing", {
  # color and background are never declared: no 39, no 49.
  expect_identical(w("padding: 1;"), "Save")
  expect_identical(w("background: #00ff00;"), "\033[48;2;0;255;0mSave\033[0m")
  expect_identical(w("text-style: bold;"), "\033[1mSave\033[0m")
  expect_identical(w("color: #ff0000;"), "\033[38;2;255;0;0mSave\033[0m")
  unset <- terminal_of("Button { padding: 1; }")
  expect_equal(unset$foreground$kind, "default")
  expect_false(unset$foreground$explicit)
  expect_true(terminal_of("Button { color: default; }")$foreground$explicit)
})

test_that("an inherited default counts as asked for", {
  sheet <- compile_text("Screen { color: default; }")
  inherited <- ink_terminal_style(ink_compute(sheet, ink_element("Button", parent = ink_element("Screen"))))
  expect_true(inherited$foreground$explicit)
  expect_identical(ink_ansi_wrap("x", inherited), "\033[39mx\033[0m")
  unrelated <- ink_terminal_style(ink_compute(sheet, ink_element("Button")))
  expect_false(unrelated$foreground$explicit)
  expect_identical(ink_ansi_wrap("x", unrelated), "x")
})

test_that("default colors are not sent when colors are off", {
  expect_identical(w("color: default;", depth = "none"), "Save")
  expect_identical(w("color: default; text-style: bold;", depth = "none"), "\033[1mSave\033[0m")
})

test_that("a transparent background adds nothing, unlike default and black", {
  expect_identical(w("color: red; background: transparent;"), "\033[31mSave\033[0m")
  expect_identical(w("color: red; background: default;"), "\033[31;49mSave\033[0m")
  expect_identical(w("color: red; background: #000000;"), "\033[31;48;2;0;0;0mSave\033[0m")
})

test_that("each attribute has its standard code", {
  codes <- c(bold = 1, dim = 2, italic = 3, underline = 4, reverse = 7, strike = 9)
  for (flag in names(codes)) {
    expect_identical(w(sprintf("text-style: %s;", flag)), sprintf("\033[%dmSave\033[0m", codes[[flag]]))
  }
})

test_that("bold and underline together", {
  expect_identical(w("text-style: bold underline;"), "\033[1;4mSave\033[0m")
  expect_identical(w("text-style: underline bold;"), "\033[1;4mSave\033[0m")
})

test_that("many attributes are joined in the fixed order", {
  expect_identical(
    w("text-style: strike reverse underline italic dim bold;"),
    "\033[1;2;3;4;7;9mSave\033[0m"
  )
})

test_that("unsupported attributes are not emitted", {
  expect_identical(w("text-style: bold italic;", text_styles = "bold"), "\033[1mSave\033[0m")
  expect_identical(w("text-style: bold italic;", text_styles = character()), "Save")
  expect_identical(w("color: red; text-style: italic;", text_styles = character()), "\033[31mSave\033[0m")
})

test_that("reset = TRUE appends the reset, reset = FALSE does not", {
  expect_identical(w("color: red;", reset = TRUE), "\033[31mSave\033[0m")
  expect_identical(w("color: red;", reset = FALSE), "\033[31mSave")
  expect_identical(w("text-style: bold;"), "\033[1mSave\033[0m")
  expect_identical(w("text-style: bold;", reset = FALSE), "\033[1mSave")
})

test_that("reset defaults to TRUE", {
  style <- terminal_of("Button { color: red; }")
  expect_identical(ink_ansi_wrap("x", style), ink_ansi_wrap("x", style, reset = TRUE))
})

test_that("a plain style returns the text unchanged, with no escape sequences", {
  style <- terminal_of("Button { padding: 1; border: round red; width: 4; }")
  expect_identical(ink_ansi_wrap("Save", style), "Save")
  expect_identical(ink_ansi_wrap("Save", style, reset = FALSE), "Save")
  expect_identical(ink_ansi_wrap("Save", terminal_of("")), "Save")
})

test_that("an empty string is returned as is", {
  expect_identical(w("color: red; text-style: bold;", text = ""), "")
  expect_identical(w("color: red;", text = "", reset = FALSE), "")
  expect_identical(w("", text = ""), "")
})

test_that("border, padding, margin and sizes do not appear in the output", {
  plain <- w("color: red;")
  decorated <- w("color: red; border: double blue; padding: 2; margin: 3; width: 20; height: 4;")
  expect_identical(decorated, plain)
})

test_that("ordinary text is preserved exactly, including non-ASCII", {
  text <- "café ✔ 中文 \U0001F600"
  expect_identical(w("color: red;", text = text), paste0("\033[31m", text, "\033[0m"))
  expect_identical(w("color: red;", text = "a b  c"), "\033[31ma b  c\033[0m")
})

test_that("tab and newline are kept, and a multi-line text is one span", {
  expect_identical(w("color: red;", text = "a\tb\nc"), "\033[31ma\tb\nc\033[0m")
})

test_that("control characters in the text cannot inject terminal commands", {
  hostile <- "x\033[2Jy\033]0;title\007z\033[H"
  result <- w("color: red;", text = hostile)
  expect_identical(result, "\033[31mx[2Jy]0;titlez[H\033[0m")
  # Exactly the two escapes the function itself added.
  expect_equal(lengths(regmatches(result, gregexpr("\033", result, fixed = TRUE))), 2L)
})

test_that("all other C0, DEL and C1 control characters are removed", {
  controls <- c(1:8, 11:31, 127L)
  text <- paste0("a", intToUtf8(controls, multiple = FALSE), "b")
  expect_identical(w("color: red;", text = text), "\033[31mab\033[0m")
  c1 <- paste0("a", intToUtf8(0x80:0x9f, multiple = FALSE), "b")
  expect_identical(w("color: red;", text = c1), "\033[31mab\033[0m")
  expect_identical(w("color: red;", text = "a\u009b2Jb"), "\033[31ma2Jb\033[0m")
  expect_identical(w("color: red;", text = "line\r\nnext"), "\033[31mline\nnext\033[0m")
})

test_that("the characters just outside the removed ranges are kept", {
  expect_identical(w("color: red;", text = " ¡~ "), "\033[31m ¡~ \033[0m")
})

test_that("text that is only control characters becomes empty and unstyled", {
  expect_identical(w("color: red;", text = "\033\007"), "")
  expect_identical(ink_ansi_wrap("\033[2J", terminal_of("")), "[2J")
})

test_that("a plain style also sanitizes the text", {
  expect_identical(ink_ansi_wrap("a\033[31mb", terminal_of("")), "a[31mb")
})

test_that("the only escape sequences ever produced are SGR sequences", {
  set.seed(6)
  styles <- list(
    terminal_of("Button { color: #123456; background: #abcdef; text-style: bold dim; }"),
    terminal_of("Button { color: red; }", capabilities = caps("ansi16")),
    terminal_of("Button { color: default; text-style: underline; }"),
    terminal_of("")
  )
  pool <- c("a", "Z", " ", "\033", "[", "2", "J", "H", ";", "m", "\007", "\r", "\n", "\t", "\u009b", "\u0090", "é")
  problems <- character()
  for (i in seq_len(300L)) {
    text <- paste(sample(pool, sample(0:12, 1L), replace = TRUE), collapse = "")
    for (style in styles) {
      result <- ink_ansi_wrap(text, style, reset = i %% 2L == 0L)
      escapes <- lengths(regmatches(result, gregexpr("\033", result, fixed = TRUE)))
      sgr <- lengths(regmatches(result, gregexpr("\033\\[[0-9;]*m", result)))
      if (escapes != sgr) problems <- c(problems, encodeString(result))
      if (grepl(paste0("\007", "|\r|", "\u009b", "|", "\u0090"), result)) problems <- c(problems, encodeString(result))
    }
  }
  # Every escape in every output starts a complete SGR sequence, and no raw
  # control character from the input survives.
  expect_equal(problems, character())
})

test_that("no cursor, clear or title sequences are ever emitted", {
  result <- w("color: red; background: blue; text-style: bold underline reverse;", text = "Hello")
  expect_false(grepl("\033\\[[0-9;]*[ABCDEFGHJKSTfhlnsu]", result))
  expect_false(grepl("\033\\]", result))
  expect_true(grepl("^\033\\[[0-9;]*m.*\033\\[0m$", result))
})

test_that("the output is deterministic and does not modify the style", {
  style <- terminal_of("Button { color: #ff0000; text-style: bold; }")
  before <- style
  expect_identical(ink_ansi_wrap("Save", style), ink_ansi_wrap("Save", style))
  expect_identical(style, before)
})

test_that("wrapping prints nothing", {
  expect_silent(ink_ansi_wrap("Save", terminal_of("Button { color: red; }")))
})

test_that("the same style under different profiles gives different, fixed sequences", {
  css <- "Button { color: #7c3aed; text-style: bold; }"
  expect_identical(ink_ansi_wrap("x", terminal_of(css, capabilities = caps("truecolor"))), "\033[1;38;2;124;58;237mx\033[0m")
  expect_identical(ink_ansi_wrap("x", terminal_of(css, capabilities = caps("ansi256"))), "\033[1;38;5;99mx\033[0m")
  expect_identical(ink_ansi_wrap("x", terminal_of(css, capabilities = caps("ansi16"))), "\033[1;94mx\033[0m")
  expect_identical(ink_ansi_wrap("x", terminal_of(css, capabilities = caps("none"))), "\033[1mx\033[0m")
})

test_that("a session-computed style wraps the same as a stateless one", {
  sheet <- compile_text("Button { color: #ff0000; text-style: bold; }")
  session <- ink_session(sheet)
  ink_session_set(session, "b", type = "Button")
  from_session <- ink_ansi_wrap("Save", ink_terminal_style(ink_session_compute(session, "b")))
  expect_identical(from_session, "\033[1;38;2;255;0;0mSave\033[0m")
})
