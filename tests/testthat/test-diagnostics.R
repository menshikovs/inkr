# Regression tests for diagnostics corrected during the v0.1 stabilization.

test_that("a stray colon inside a value is reported as such, not as an empty value", {
  err <- expect_parse_error("A { color: red: blue; }", "unexpected_token", 1, 15)
  expect_match(err$detail, "':'", fixed = TRUE)
  expect_match(err$detail, "'red'", fixed = TRUE)
  expect_match(err$detail, "property 'color'", fixed = TRUE)
  expect_false(identical(err$code, "empty_value"))
})

test_that("the same applies to variables", {
  err <- expect_parse_error("$a: red: blue;", "unexpected_token", 1, 8)
  expect_match(err$detail, "variable '$a'", fixed = TRUE)
})

test_that("two declarations run together on one line are still not an empty value", {
  expect_parse_error("A { color: padding: 1; }", "unexpected_token", 1, 19)
})

test_that("a value that is genuinely empty before the next declaration is still empty", {
  expect_parse_error("A {\n  color:\n  padding: 1;\n}", "empty_value", 2, 9)
  expect_parse_error("$a:\n$b: 2;", "empty_value", 1, 4)
  expect_parse_error("A { color: ; }", "empty_value", 1, 11)
  expect_parse_error("A { color: }", "empty_value", 1, 11)
})

test_that("a missing semicolon before the next declaration is still a missing semicolon", {
  expect_parse_error("A {\n  color: red\n  padding: 1;\n}", "missing_semicolon", 2, 13)
  expect_parse_error("A { color: red padding: 1; }", "missing_semicolon", 1, 15)
  expect_parse_error("$a: red\n$b: blue;", "missing_semicolon", 1, 8)
})

test_that("a colon after something that is not a word keeps its diagnostic", {
  expect_parse_error("A { b: 1 : 2; }", "unexpected_token", 1, 10)
  expect_parse_error("A { b: #fff : 2; }", "unexpected_token", 1, 13)
  expect_parse_error("A { b: red blue : 2; }", "missing_semicolon", 1, 11)
})

test_that("valid declarations still parse", {
  ast <- ink_parse("A { color: red; padding: 1 2; border: round red; }\n$a: red; B { color: $a; }")
  expect_length(ast$statements, 3L)
  expect_equal(ast$statements[[1L]]$declarations[[3L]]$value, "round red")
})

test_that("print limits are validated with a clear message", {
  ast <- ink_parse("A { } B { }")
  sheet <- ink_compile(ast)
  element <- ink_element("A")
  objects <- list(
    ast = ast, compiled = sheet, match = ink_match(sheet, element),
    explanation = ink_explain(sheet, element, "color")
  )
  for (name in names(objects)) {
    for (bad in list(-1, NA, NA_real_, "a", c(1, 2), NULL, TRUE, list(1))) {
      err <- tryCatch(capture.output(print(objects[[name]], n = bad)), error = identity)
      expect_s3_class(err, "error")
      expect_match(conditionMessage(err), "`n` must be a single non-negative number", fixed = TRUE, info = name)
    }
    for (good in list(0, 1, 2.5, 5L, Inf)) {
      expect_no_error(capture.output(print(objects[[name]], n = good)))
    }
  }
})

test_that("a print limit of zero shows the header but no entries", {
  ast <- ink_parse("A { } B { }")
  output <- capture.output(print(ast, n = 0))
  expect_match(output[[1L]], "2 statements", fixed = TRUE)
  expect_true(any(grepl("and 2 more", output, fixed = TRUE)))
  expect_false(any(grepl("\\[1\\]", output)))
})

test_that("a path is shown once, and with both ends, when the print limit is tiny", {
  explanation <- ink_explain(compile_text("B { padding: 1; }"), ink_element("A"), "color")
  output <- capture.output(print(explanation, n = 0))
  expect_equal(sum(output == "  A"), 1L)

  long <- ink_explain(compile_text("Screen { color: red; }"), filler_chain(30L), "color")
  for (limit in list(0, 1, 2, 2.9)) {
    shown <- capture.output(print(long, n = limit))
    expect_true(any(grepl("elements omitted", shown, fixed = TRUE)))
    expect_equal(sum(shown == "  Screen"), 1L)
    expect_equal(sum(shown == "  Filler"), if (limit >= 3) limit - 1 else 1L)
  }
  expect_equal(sum(capture.output(print(long, n = Inf)) == "  Filler"), 30L)
})
