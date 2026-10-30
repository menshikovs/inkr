resolved <- function(text) {
  vapply(compile_text(text)$variables, function(v) v$value, "")
}

test_that("variables may be used before they are declared", {
  sheet <- compile_text("Button { color: $p; }\n$p: red;")
  value <- sheet$rules[[1L]]$declarations[[1L]]$value
  expect_equal(value$kind, "ansi")
  expect_equal(value$name, "red")
})

test_that("variables can refer to other variables, in any order", {
  expect_equal(
    resolved("$a: $b;\n$b: $c;\n$c: green;"),
    c(a = "green", b = "green", c = "green")
  )
  expect_equal(
    resolved("$c: green;\n$b: $c;\n$a: $b;"),
    c(c = "green", b = "green", a = "green")
  )
  # The example from the specification.
  sheet <- compile_text("$primary: $accent;\n$accent: #7c3aed;\nButton { color: $primary; }")
  expect_equal(format(sheet$rules[[1L]]$declarations[[1L]]$value), "#7c3aed")
})

test_that("a diamond of references is not a cycle", {
  text <- "$top: $left $right;\n$left: $base;\n$right: $base;\n$base: 1;"
  expect_equal(resolved(text)[["top"]], "1 1")
})

test_that("an undefined variable is an error", {
  err <- expect_compile_error("Button { color: $nope; }", "undefined_variable", 1, 17)
  expect_match(err$detail, "$nope", fixed = TRUE)
  expect_equal(err$property, "color")
  expect_null(err$variable)
})

test_that("an undefined variable inside a variable is reported even if unused", {
  err <- expect_compile_error("$a: red;\n$b: $nope;", "undefined_variable", 2, 5)
  expect_equal(err$variable, "b")
  expect_match(err$detail, "variable '$b'", fixed = TRUE)
})

test_that("variable names are case-sensitive", {
  expect_compile_error("$Primary: red;\nButton { color: $primary; }", "undefined_variable", 2, 17)
  expect_equal(
    resolved("$Primary: red;\n$primary: blue;"),
    c(Primary = "red", primary = "blue")
  )
})

test_that("a circular reference reports the chain", {
  err <- expect_compile_error("$a: $b;\n$b: $c;\n$c: $a;", "circular_variable", 1, 1)
  expect_equal(err$chain, c("a", "b", "c", "a"))
  expect_equal(err$detail, "Circular variable reference: $a -> $b -> $c -> $a")
  expect_match(conditionMessage(err), "$a -> $b -> $c -> $a", fixed = TRUE)
})

test_that("a two-variable cycle and a self-reference are detected", {
  err <- expect_compile_error("$a: $b;\n$b: $a;", "circular_variable", 1, 1)
  expect_equal(err$chain, c("a", "b", "a"))

  err <- expect_compile_error("$a: $a;", "circular_variable", 1, 1)
  expect_equal(err$chain, c("a", "a"))
  expect_equal(err$detail, "Circular variable reference: $a -> $a")

  err <- expect_compile_error("$a: 1 $a 2;", "circular_variable", 1, 1)
  expect_equal(err$chain, c("a", "a"))
})

test_that("the chain starts at the cycle, not at the variable that led to it", {
  err <- expect_compile_error("$x: $a;\n$a: $b;\n$b: $a;", "circular_variable", 2, 1)
  expect_equal(err$chain, c("a", "b", "a"))
})

test_that("cycles are reported even when no rule uses them", {
  expect_compile_error("$a: $b;\n$b: $a;\nButton { color: red; }", "circular_variable", 1, 1)
})

test_that("the last declaration of a variable wins", {
  sheet <- compile_text("$primary: red;\n$primary: blue;\nButton { color: $primary; }")
  expect_equal(format(sheet$rules[[1L]]$declarations[[1L]]$value), "blue")
  expect_named(sheet$variables, "primary")
  expect_equal(sheet$variables$primary$value, "blue")
  expect_equal(sheet$variables$primary$span$start$line, 2L)
})

test_that("overridden definitions stay in the AST and do not take part in resolution", {
  ast <- ink_parse("$a: $missing;\n$a: red;\n$b: $b;\n$b: blue;")
  expect_length(ast$statements, 4L)
  sheet <- ink_compile(ast)
  expect_equal(vapply(sheet$variables, function(v) v$value, ""), c(a = "red", b = "blue"))
})

test_that("effective variables are listed in the source order of their last declaration", {
  expect_named(compile_text("$a: 1;\n$b: 2;\n$a: 3;")$variables, c("b", "a"))
})

test_that("variables are substituted inside multi-part values", {
  sheet <- compile_text("$p: round red;\nButton { border: $p; }")
  border <- sheet$rules[[1L]]$declarations[[1L]]$value
  expect_equal(border$style, "round")
  expect_equal(border$color$name, "red")

  sheet <- compile_text("$pad: 1 2;\nButton { padding: $pad 3; }")
  expect_equal(
    sheet$rules[[1L]]$declarations[[1L]]$value$values,
    c(top = 1L, right = 2L, bottom = 3L, left = 2L)
  )

  sheet <- compile_text("$style: round;\n$c: #fff;\nButton { border: $style $c; }")
  expect_equal(sheet$rules[[1L]]$declarations[[1L]]$value$color$r, 255L)
})

test_that("substitution is token-aware and safe against prefix collisions", {
  text <- "$a: red;\n$accent: blue;\nButton { color: $accent; }"
  expect_equal(format(compiled_value(text)), "blue")

  text <- "$a: red;\n$ab: green;\n$abc: blue;\nButton { color: $abc; background: $ab; }"
  sheet <- compile_text(text)
  expect_equal(format(sheet$rules[[1L]]$declarations[[1L]]$value), "blue")
  expect_equal(format(sheet$rules[[1L]]$declarations[[2L]]$value), "green")

  text <- "$a: 1;\n$a1: 2;\nButton { padding: $a1 $a; }"
  expect_equal(compiled_value(text)$values[["top"]], 2L)
  expect_equal(compiled_value(text)$values[["right"]], 1L)
})

test_that("variable values are not evaluated as code", {
  expect_error(
    compile_text("$a: Sys.setenv(INKR_TEST = 'x');"),
    class = "ink_parse_error"
  )
  expect_equal(Sys.getenv("INKR_TEST"), "")
  sheet <- compile_text("$a: system;")
  expect_equal(sheet$variables$a$value, "system")
})

test_that("resolved variables record the raw and the substituted value", {
  sheet <- compile_text("$a: $b  /* c */ x;\n$b: 1;")
  expect_equal(sheet$variables$a$raw, "$b x")
  expect_equal(sheet$variables$a$value, "1 x")
  expect_s3_class(sheet$variables$a, "ink_resolved_variable")
})

test_that("resolved variable text keeps hex case and commas", {
  expect_equal(resolved("$c: #FFF;")[["c"]], "#FFF")
  expect_equal(resolved("$l: 1, 2;")[["l"]], "1, 2")
})

test_that("deep variable chains resolve without recursion limits", {
  n <- 3000L
  lines <- c(
    sprintf("$v%d: $v%d;", seq_len(n - 1L), seq_len(n - 1L) + 1L),
    sprintf("$v%d: red;", n),
    "Button { color: $v1; }"
  )
  sheet <- compile_text(paste(lines, collapse = "\n"))
  expect_equal(format(sheet$rules[[1L]]$declarations[[1L]]$value), "red")
  expect_length(sheet$variables, n)
})

test_that("a long cycle is detected with its full chain", {
  n <- 2000L
  lines <- sprintf("$v%d: $v%d;", seq_len(n), c(seq_len(n)[-1L], 1L))
  err <- compile_error(paste(lines, collapse = "\n"))
  expect_equal(err$code, "circular_variable")
  expect_length(err$chain, n + 1L)
  expect_equal(err$chain[[1L]], err$chain[[n + 1L]])
})

test_that("exponential growth through nested variables is rejected", {
  lines <- c("$v0: 1;", sprintf("$v%d: $v%d $v%d;", 1:12, 0:11, 0:11))
  err <- compile_error(paste(lines, collapse = "\n"))
  expect_equal(err$code, "variable_too_large")
  expect_equal(err$variable, "v10")

  ok <- compile_text(paste(c("$v0: 1;", sprintf("$v%d: $v%d $v%d;", 1:9, 0:8, 0:8)), collapse = "\n"))
  expect_equal(ok$variables$v9$value, paste(rep("1", 512L), collapse = " "))
})

test_that("stylesheets with only variables compile", {
  sheet <- compile_text("$a: red;\n$b: 1 2;")
  expect_length(sheet$rules, 0L)
  expect_length(sheet$variables, 2L)
})

test_that("a variable can hold a CSS-wide keyword", {
  value <- compiled_value("$k: inherit;\nButton { color: $k; }")
  expect_s3_class(value, "ink_keyword")
  expect_equal(value$keyword, "inherit")
})
