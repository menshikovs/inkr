test_that("compile errors are ink_compile_error conditions, not parse errors", {
  err <- compile_error("Button { padding: 1 two; }")
  expect_s3_class(err, "ink_compile_error")
  expect_s3_class(err, "error")
  expect_false(inherits(err, "ink_parse_error"))
  expect_error(compile_text("Button { padding: 1 two; }"), class = "ink_compile_error")
})

test_that("the condition carries code, message, source, location and span", {
  text <- "Button {\n    padding: 1 two;\n}"
  err <- compile_error(text, source = "app.inkr")
  expect_named(
    unclass(err),
    c(
      "message", "call", "code", "detail", "source", "line", "column",
      "offset", "span", "property", "variable", "chain"
    )
  )
  expect_equal(err$code, "invalid_spacing")
  expect_equal(err$source, "app.inkr")
  expect_equal(c(err$line, err$column, err$offset), c(2L, 14L, 22L))
  expect_equal(err$property, "padding")
  expect_null(err$variable)
  expect_equal(span_text(text, err$span), "1 two")
  expect_equal(
    conditionMessage(err),
    "app.inkr:2:14: Invalid padding value '1 two'. Expected 1 to 4 non-negative integers."
  )
  expect_equal(
    err$detail,
    "Invalid padding value '1 two'. Expected 1 to 4 non-negative integers."
  )
})

test_that("the error points at the value span from the AST", {
  text <- "A {\n  color:\n     reddish;\n}"
  err <- compile_error(text)
  expect_equal(c(err$line, err$column), c(3L, 6L))
  expect_equal(span_text(text, err$span), "reddish")
})

test_that("each type reports its own error code", {
  expect_compile_error("A { color: reddish; }", "invalid_color", 1, 12)
  expect_compile_error("A { background: #fff red; }", "invalid_color", 1, 17)
  expect_compile_error("A { padding: -1; }", "invalid_spacing", 1, 14)
  expect_compile_error("A { padding: 1 2 3 4 5; }", "invalid_spacing", 1, 14)
  expect_compile_error("A { padding: 1.5; }", "invalid_spacing", 1, 14)
  expect_compile_error("A { padding: auto; }", "invalid_spacing", 1, 14)
  expect_compile_error("A { border: dashed red; }", "invalid_border", 1, 13)
  expect_compile_error("A { border: round; }", "invalid_border", 1, 13)
  expect_compile_error("A { width: -3; }", "invalid_length", 1, 12)
  expect_compile_error("A { height: 1.5; }", "invalid_length", 1, 13)
  expect_compile_error("A { text-style: bold none; }", "invalid_text_style", 1, 17)
  expect_compile_error("A { text-style: blink; }", "invalid_text_style", 1, 17)
})

test_that("commas in values are invalid for every built-in type", {
  for (text in c(
    "A { color: red, blue; }", "A { padding: 1, 2; }", "A { border: round, red; }",
    "A { width: 1, 2; }", "A { text-style: bold, dim; }"
  )) {
    err <- compile_error(text)
    expect_s3_class(err, "ink_compile_error")
    expect_match(err$code, "^invalid_")
  }
})

test_that("an error in a substituted value shows the expansion", {
  text <- "$two: two;\nButton { padding: 1 $two; }"
  err <- compile_error(text)
  expect_equal(err$code, "invalid_spacing")
  expect_equal(c(err$line, err$column), c(2L, 19L))
  expect_match(err$detail, "'1 $two' (expands to '1 two')", fixed = TRUE)
})

test_that("an invalid variable value is reported where it is used", {
  err <- expect_compile_error("$c: reddish;\nA { color: $c; }", "invalid_color", 2, 12)
  expect_equal(err$property, "color")
  # Unused variables have no type to check against, so they compile.
  expect_s3_class(compile_text("$c: reddish;"), "ink_compiled_stylesheet")
})

test_that("error fields identify the property, variable and chain", {
  err <- compile_error("A { color: $nope; }")
  expect_equal(err$property, "color")
  expect_null(err$chain)

  err <- compile_error("$a: $nope;")
  expect_equal(err$variable, "a")
  expect_null(err$property)

  err <- compile_error("$a: $b;\n$b: $a;")
  expect_equal(err$variable, "a")
  expect_equal(err$chain, c("a", "b", "a"))
})

test_that("a circular reference points at the declaration of the first variable", {
  text <- "$x: 1;\n  $a: $b;\n$b: $a;"
  err <- compile_error(text)
  expect_equal(c(err$line, err$column), c(2L, 3L))
  expect_equal(span_text(text, err$span), "$a: $b;")
})

test_that("the first error wins; variables are checked before rules", {
  err <- compile_error("A { color: reddish; }\n$a: $nope;")
  expect_equal(err$code, "undefined_variable")

  err <- compile_error("A { color: reddish; padding: x; }")
  expect_equal(err$code, "invalid_color")

  err <- compile_error("A { colour: red; color: reddish; }")
  expect_equal(err$code, "unknown_property")
})

test_that("parse errors are not replaced by compile errors", {
  expect_error(ink_parse("Button { color: red }"), class = "ink_parse_error")
  expect_error(ink_compile(ink_parse("Button { color: red; }")), NA)
  err <- tryCatch(compile_text("Button { color: red }"), error = identity)
  expect_s3_class(err, "ink_parse_error")
})

test_that("error codes are drawn from the documented set", {
  documented <- c(
    "undefined_variable", "circular_variable", "variable_too_large",
    "unknown_property", "invalid_property_value", "invalid_color",
    "invalid_spacing", "invalid_border", "invalid_length", "invalid_text_style",
    "invalid_registry_definition"
  )
  texts <- c(
    "A { color: $x; }", "$a: $a;", "A { nope: 1; }", "A { color: 1; }",
    "A { padding: x; }", "A { border: x; }", "A { width: x; }", "A { text-style: x; }"
  )
  for (text in texts) {
    expect_true(compile_error(text)$code %in% documented, info = text)
  }
})

test_that("malformed values never crash with an unstructured error", {
  pieces <- c(
    "red", "bright-red", "#fff", "#12345678", "1", "-1", "0", "2.5", "50%", "auto",
    "none", "bold", "round", "single", "default", "transparent", "inherit",
    "initial", "$v", "$w", "$missing", ",", "x", "1px", "heavy", "dim"
  )
  properties <- names(ink_registry()$properties)
  set.seed(42)
  problems <- character()
  failures <- 0L
  for (i in seq_len(1500L)) {
    value <- paste(sample(pieces, sample.int(5L, 1L), replace = TRUE), collapse = " ")
    text <- sprintf("$v: red;\n$w: $v 1;\nA { %s: %s; }", sample(properties, 1L), value)
    outcome <- tryCatch(
      ink_compile(ink_parse(text)),
      ink_compile_error = function(e) e,
      error = function(e) e
    )
    if (inherits(outcome, "ink_compile_error")) {
      failures <- failures + 1L
      sane <- nzchar(outcome$code) && outcome$line >= 1L && outcome$span$start$offset <= outcome$span$end$offset
      if (!sane) problems <- c(problems, text)
    } else if (!inherits(outcome, "ink_compiled_stylesheet")) {
      problems <- c(problems, text)
    }
  }
  expect_equal(problems, character())
  expect_gt(failures, 100L)
  expect_lt(failures, 1500L)
})

test_that("values that merely look like code are plain tokens", {
  err <- compile_error("A { color: Sys_setenv; }")
  expect_equal(err$code, "invalid_color")
  expect_error(compile_text("A { color: system('x'); }"), class = "ink_parse_error")
})
