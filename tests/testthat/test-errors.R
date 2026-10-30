test_that("syntax errors are ink_parse_error conditions", {
  err <- parse_error("Button {")
  expect_s3_class(err, "ink_parse_error")
  expect_s3_class(err, "error")
  expect_s3_class(err, "condition")
  expect_error(ink_parse("Button {"), class = "ink_parse_error")
})

test_that("the condition carries code, message, source, location and span", {
  err <- parse_error("A {\n  b: c\n}", source = "demo.inkr")
  expect_named(
    unclass(err),
    c(
      "message", "call", "code", "detail", "source",
      "line", "column", "offset", "span"
    )
  )
  expect_type(err$code, "character")
  expect_equal(err$source, "demo.inkr")
  expect_equal(err$line, 2L)
  expect_equal(err$column, 7L)
  expect_equal(err$offset, 10L)
  expect_named(err$span, c("start", "end"))
  expect_equal(loc(err$span$start), c(line = 2, column = 7, offset = 10))
  expect_match(conditionMessage(err), "^demo.inkr:2:7: ")
  expect_match(conditionMessage(err), err$detail, fixed = TRUE)
})

test_that("the error message names the default source", {
  expect_match(conditionMessage(parse_error("}")), "^<text>:1:1: ")
})

test_that("every error has a non-empty code and detail", {
  texts <- c("A {", "A { b }", "}", "A { b: ; }", "/*", "A { b: % }", "$", "A >")
  for (text in texts) {
    err <- parse_error(text)
    expect_s3_class(err, "ink_parse_error")
    expect_true(nzchar(err$code), info = text)
    expect_match(err$code, "^[a-z_]+$", info = text)
    expect_true(nzchar(err$detail), info = text)
  }
})

test_that("a missing semicolon between declarations is detected", {
  text <- "Button {\n    color: red\n    padding: 1;\n}"
  err <- expect_parse_error(text, "missing_semicolon", 2, 15)
  # The error is where the `;` belongs: right after `red`.
  expect_equal(span_text(text, err$span), "")
  expect_equal(substr(text, err$offset - 2L, err$offset), "red")
})

test_that("a missing semicolon before the closing brace is detected", {
  expect_parse_error("Button { color: red }", "missing_semicolon", 1, 20)
  expect_parse_error("Button {\n  color: red\n}", "missing_semicolon", 2, 13)
})

test_that("a missing semicolon after a variable is detected", {
  expect_parse_error("$a: 1\n$b: 2;", "missing_semicolon", 1, 6)
  expect_parse_error("$a: 1", "unexpected_end_of_input", 1, 6)
})

test_that("a missing semicolon before a rule is detected", {
  expect_parse_error("$a: 1\nButton {\n}", "missing_semicolon", 2, 8)
})

test_that("a missing closing brace is reported at the opening brace", {
  text <- "Button {\n  color: red;\n"
  err <- expect_parse_error(text, "missing_closing_brace", 1, 8)
  expect_equal(err$span$end$line, 3L)
  expect_parse_error("A {}\nB { c: d;", "missing_closing_brace", 2, 3)
  expect_parse_error("A {", "missing_closing_brace", 1, 3)
})

test_that("a missing closing brace before the next rule is nesting", {
  expect_parse_error(
    "Button {\n  color: red;\nInput {\n  color: blue;\n}",
    "unsupported_nesting", 3, 1
  )
})

test_that("a missing opening brace is detected", {
  expect_parse_error("Button\n  color: red;\n}", "missing_opening_brace", 2, 8)
  expect_parse_error("Button color:red; }", "missing_opening_brace", 1, 17)
  expect_parse_error("A }", "missing_opening_brace", 1, 3)
})

test_that("a missing colon is detected where the colon belongs", {
  text <- "Button { color red; }"
  err <- expect_parse_error(text, "missing_colon", 1, 15)
  expect_equal(substr(text, err$offset - 4L, err$offset), "color")
  expect_parse_error("A { color; }", "missing_colon", 1, 10)
  expect_parse_error("A {\n  color\n  padding: 1;\n}", "missing_colon", 2, 8)
  expect_parse_error("$a red;", "missing_colon", 1, 3)
})

test_that("an empty value is detected", {
  expect_parse_error("Button { color: ; }", "empty_value", 1, 16)
  expect_parse_error("Button { color: }", "empty_value", 1, 16)
  expect_parse_error("Button { color:/* nothing */; }", "empty_value", 1, 16)
  expect_parse_error("$a: ;", "empty_value", 1, 4)
  expect_parse_error("$a:;", "empty_value", 1, 4)
  expect_parse_error("A {\n  b:\n  c: 1;\n}", "empty_value", 2, 5)
})

test_that("an invalid selector is detected", {
  expect_parse_error("Button > { }", "invalid_selector", 1, 10)
  expect_parse_error("Button, { }", "invalid_selector", 1, 9)
  expect_parse_error(", Button { }", "invalid_selector", 1, 1)
  expect_parse_error("> Button { }", "invalid_selector", 1, 1)
  expect_parse_error("{ color: red; }", "invalid_selector", 1, 1)
  expect_parse_error("A > > B { }", "invalid_selector", 1, 5)
  expect_parse_error("Button#a#b { }", "invalid_selector", 1, 9)
  expect_parse_error("*Button { }", "invalid_selector", 1, 2)
  expect_parse_error("Button* { }", "invalid_selector", 1, 7)
  expect_parse_error(".a* { }", "invalid_selector", 1, 3)
  expect_parse_error("Button::before { }", "invalid_selector", 1, 7)
  expect_parse_error(". { }", "invalid_selector", 1, 1)
  expect_parse_error("Button # { }", "invalid_selector", 1, 8)
})

test_that("an invalid variable declaration is detected", {
  expect_parse_error("$: red;", "invalid_variable", 1, 1)
  expect_parse_error("$1: red;", "invalid_variable", 1, 1)
  expect_parse_error("$ a: red;", "invalid_variable", 1, 1)
  expect_parse_error("Button { $a: 1; }", "invalid_variable", 1, 10)
  expect_parse_error("A { color: $; }", "invalid_variable", 1, 12)
})

test_that("variables cannot be declared inside rules", {
  err <- parse_error("A {\n  $x: 1;\n}")
  expect_equal(err$code, "invalid_variable")
  expect_match(err$detail, "top level")
})

test_that("an unterminated comment is detected", {
  expect_parse_error("Button {}\n/* oops", "unterminated_comment", 2, 1)
  expect_parse_error("/*", "unterminated_comment", 1, 1)
  expect_parse_error("A { /* x\n}", "unterminated_comment", 1, 5)
})

test_that("unexpected end of input is detected", {
  expect_parse_error("Button", "unexpected_end_of_input", 1, 7)
  expect_parse_error("Button,", "unexpected_end_of_input", 1, 8)
  expect_parse_error("Button >", "unexpected_end_of_input", 1, 9)
  expect_parse_error("Button { color", "unexpected_end_of_input", 1, 15)
  expect_parse_error("Button { color:", "unexpected_end_of_input", 1, 16)
  expect_parse_error("Button {\n color: red", "unexpected_end_of_input", 2, 12)
  expect_parse_error("$a", "unexpected_end_of_input", 1, 3)
  expect_parse_error("$a:", "unexpected_end_of_input", 1, 4)
})

test_that("stray punctuation is reported", {
  expect_parse_error("}", "unexpected_closing_brace", 1, 1)
  expect_parse_error("A {} }", "unexpected_closing_brace", 1, 6)
  expect_parse_error(";", "unexpected_token", 1, 1)
  expect_parse_error("A {};", "unexpected_token", 1, 5)
  expect_parse_error("A { ; }", "unexpected_token", 1, 5)
  expect_parse_error("A { : red; }", "unexpected_token", 1, 5)
  expect_parse_error("A { b: 1 : 2; }", "unexpected_token", 1, 10)
  expect_parse_error(": red;", "unexpected_token", 1, 1)
})

test_that("nesting is reported explicitly", {
  err <- expect_parse_error("A {\n  B {\n  }\n}", "unsupported_nesting", 2, 3)
  expect_equal(c(err$span$end$line, err$span$end$column), c(2L, 6L))
  expect_parse_error("A { .b { } }", "unsupported_nesting", 1, 5)
  expect_parse_error("A { &:hover { } }", "unsupported_nesting", 1, 5)
})

test_that("unsupported syntax is reported explicitly", {
  expect_parse_error("@import 'x.inkr';", "unsupported_at_rule", 1, 1)
  expect_parse_error("A { content: 'x'; }", "unsupported_string", 1, 14)
  expect_parse_error("A { color: rgb(1, 2, 3); }", "unsupported_function", 1, 15)
  expect_parse_error("A { color: red !important; }", "unsupported_important", 1, 16)
})

test_that("a lexical error is reported where it occurs", {
  expect_parse_error("A { b: 1%2$; }", "invalid_variable", 1, 11)
  expect_parse_error("Button + Input { }", "unexpected_character", 1, 8)
  expect_parse_error("A { b: #12; }", "invalid_hex_color", 1, 8)
})

test_that("the first error in the source wins over later lexical errors", {
  # Missing semicolon on line 2 comes before the unterminated comment.
  expect_parse_error("A {\n  b: c\n  d: e;\n}\n/* never closed", "missing_semicolon", 2, 7)
  # And the reverse: the comment is first.
  expect_parse_error("/* never closed\n A { b: c\n d: e;}", "unterminated_comment", 1, 1)
})

test_that("only the first error is reported and no recovery happens", {
  err <- parse_error("A { b }\nC { d }")
  expect_equal(c(err$line, err$column), c(1L, 6L))
})

test_that("error locations are accurate after multi-line comments and CRLF", {
  text <- "/* line one\nline two */\r\nA {\r\n  b: c\r\n}"
  expect_parse_error(text, "missing_semicolon", 4, 7)
})

test_that("malformed input never fails with an unrelated error", {
  pieces <- c(
    "A", "Button", ".c", "#i", ":p", "*", ">", ",", " ", "\n", "{", "}", ";",
    ":", "$v", "$", "#", "#fff", "1", "-2.5", "50%", "/*", "*/", "/* c */",
    "@", "'", "\"", "(", ")", "!", "&", "%", "\\", "\u00e9", "\t", "\r", "color",
    "text-style", "--", "_x"
  )
  set.seed(1)
  problems <- character()
  parse_errors <- 0L
  for (i in seq_len(1500L)) {
    n <- sample.int(14L, 1L)
    text <- paste(sample(pieces, n, replace = TRUE), collapse = "")
    outcome <- tryCatch(
      ink_parse(text),
      ink_parse_error = function(e) e,
      error = function(e) e
    )
    if (inherits(outcome, "ink_parse_error")) {
      parse_errors <- parse_errors + 1L
      span <- outcome$span
      sane <- span$start$offset <= span$end$offset &&
        outcome$offset <= nchar(text) && outcome$line >= 1L && outcome$column >= 1L
      if (!sane) problems <- c(problems, encodeString(text))
    } else if (!inherits(outcome, "ink_stylesheet_ast")) {
      problems <- c(problems, encodeString(text))
    }
  }
  expect_equal(problems, character())
  # The generator must exercise both outcomes.
  expect_gt(parse_errors, 100L)
  expect_lt(parse_errors, 1500L)
})

test_that("statement spans are non-empty, ordered and never overlap", {
  text <- "/* c */ $a: 1;\nA, B > C { d: e; f: g; }\n\n$h: i j;\n* {}\n"
  ast <- ink_parse(text)
  starts <- vapply(ast$statements, function(s) s$span$start$offset, 0L)
  ends <- vapply(ast$statements, function(s) s$span$end$offset, 0L)
  expect_true(all(starts < ends))
  expect_true(all(ends[-length(ends)] <= starts[-1L]))
})

test_that("invalid arguments are ordinary errors, not syntax errors", {
  expect_error(ink_parse(42), "character vector")
  expect_error(ink_parse(NA_character_), "missing values")
  expect_error(ink_parse(NULL), "character vector")
  expect_error(ink_parse("A {}", source = c("a", "b")), "single string")
  expect_error(ink_parse("A {}", source = NULL), "single string")
  err <- tryCatch(ink_parse(42), error = identity)
  expect_false(inherits(err, "ink_parse_error"))
})

test_that("text must be valid UTF-8", {
  bad <- rawToChar(as.raw(c(0x41, 0xff, 0x42)))
  expect_error(ink_parse(bad), "UTF-8")
})
