test_that("ink_compile returns an ink_compiled_stylesheet", {
  sheet <- compile_text("Button { color: red; }")
  expect_s3_class(sheet, "ink_compiled_stylesheet")
  expect_named(sheet, c("rules", "variables", "registry", "source"))
  expect_equal(sheet$source, "<text>")
  expect_s3_class(sheet$registry, "ink_registry")
  expect_equal(ink_compile(ink_parse("A {}", source = "x.inkr"))$source, "x.inkr")
})

test_that("an empty stylesheet compiles", {
  for (text in c("", "  \n", "/* nothing */")) {
    sheet <- compile_text(text)
    expect_length(sheet$rules, 0L)
    expect_length(sheet$variables, 0L)
  }
})

test_that("a single rule compiles to typed declarations", {
  sheet <- compile_text("Button { color: red; padding: 1 2; }")
  expect_length(sheet$rules, 1L)
  rule <- sheet$rules[[1L]]
  expect_s3_class(rule, "ink_compiled_rule")
  expect_named(rule, c("selectors", "declarations", "span"))
  expect_s3_class(rule$declarations[[1L]], "ink_compiled_declaration")
  expect_named(
    rule$declarations[[1L]],
    c("name", "type", "value", "raw", "span", "value_span")
  )
  expect_s3_class(rule$declarations[[1L]]$value, "ink_color")
  expect_s3_class(rule$declarations[[2L]]$value, "ink_spacing")
})

test_that("an empty rule compiles", {
  sheet <- compile_text("Button {}")
  expect_length(sheet$rules, 1L)
  expect_length(sheet$rules[[1L]]$declarations, 0L)
})

test_that("multiple rules keep source order", {
  sheet <- compile_text("A { width: 1; }\nB { width: 2; }\n$v: 3;\nC { width: $v; }")
  types <- vapply(
    sheet$rules,
    function(rule) rule$selectors$selectors[[1L]]$compounds[[1L]]$type,
    ""
  )
  expect_equal(types, c("A", "B", "C"))
  expect_equal(sheet$rules[[3L]]$declarations[[1L]]$value$cells, 3L)
})

test_that("declarations keep their order, type and raw text", {
  sheet <- compile_text("A {\n  width: 5;\n  color: $c;\n  border: none;\n}\n$c: red;")
  declarations <- sheet$rules[[1L]]$declarations
  expect_equal(vapply(declarations, function(d) d$name, ""), c("width", "color", "border"))
  expect_equal(vapply(declarations, function(d) d$type, ""), c("length", "color", "border"))
  expect_equal(vapply(declarations, function(d) d$raw, ""), c("5", "$c", "none"))
})

test_that("duplicate declarations are all kept, in order", {
  sheet <- compile_text("Button {\n  color: red;\n  color: blue;\n  color: inherit;\n}")
  declarations <- sheet$rules[[1L]]$declarations
  expect_length(declarations, 3L)
  expect_equal(format(declarations[[1L]]$value), "red")
  expect_equal(format(declarations[[2L]]$value), "blue")
  expect_s3_class(declarations[[3L]]$value, "ink_keyword")
  expect_equal(
    vapply(declarations, function(d) d$span$start$line, 0L),
    c(2L, 3L, 4L)
  )
})

test_that("variables are resolved inside declarations", {
  sheet <- compile_text("$primary: #7c3aed;\nButton { background: $primary; border: round $primary; }")
  declarations <- sheet$rules[[1L]]$declarations
  expect_equal(declarations[[1L]]$value$r, 124L)
  expect_equal(declarations[[2L]]$value$color$r, 124L)
  expect_equal(declarations[[1L]]$raw, "$primary")
})

test_that("selectors are preserved for later matching", {
  text <- "Container > Button.primary:focus, #sidebar * {}"
  ast <- ink_parse(text)
  sheet <- ink_compile(ast)
  expect_identical(sheet$rules[[1L]]$selectors, ast$statements[[1L]]$selectors)
  expect_s3_class(sheet$rules[[1L]]$selectors, "ink_selector_list")
})

test_that("source positions are preserved", {
  text <- "$a: red;\n\nButton {\n  color: $a;\n  padding: 1;\n}"
  ast <- ink_parse(text)
  sheet <- ink_compile(ast)
  expect_identical(sheet$rules[[1L]]$span, ast$statements[[2L]]$span)
  expect_identical(
    sheet$rules[[1L]]$declarations[[1L]]$span,
    ast$statements[[2L]]$declarations[[1L]]$span
  )
  expect_identical(
    sheet$rules[[1L]]$declarations[[2L]]$value_span,
    ast$statements[[2L]]$declarations[[2L]]$value_span
  )
  expect_identical(sheet$variables$a$span, ast$statements[[1L]]$span)
  expect_equal(span_text(text, sheet$rules[[1L]]$declarations[[1L]]$value_span), "$a")
})

test_that("the AST is not modified", {
  ast <- ink_parse("$a: red;\n$a: blue;\nButton { color: $a; padding: 1 2; }")
  before <- ast
  ink_compile(ast)
  ink_compile(ast)
  expect_identical(ast, before)
  expect_length(ast$statements, 3L)
  expect_equal(ast$statements[[3L]]$declarations[[1L]]$value, "$a")
})

test_that("compilation is deterministic", {
  text <- "$a: red;\n$b: $a;\nButton { color: $b; border: round $a; }\nInput { width: 3; }"
  expect_identical(compile_text(text), compile_text(text))
  expect_identical(ink_compile(ink_parse(text)), ink_compile(ink_parse(text)))
})

test_that("equivalent input compiles to equivalent values", {
  a <- compile_text("Button { color: #fff; padding: 1; text-style: underline bold; }")
  b <- compile_text("Button{color:#ffffff;padding:1 1 1 1;text-style:bold underline;}")
  values <- function(sheet) lapply(sheet$rules[[1L]]$declarations, function(d) d$value)
  expect_identical(values(a), values(b))
})

test_that("inherit and initial are kept as markers for every property", {
  for (property in names(ink_registry()$properties)) {
    for (keyword in c("inherit", "initial")) {
      value <- compiled_value(sprintf("A { %s: %s; }", property, keyword))
      expect_s3_class(value, "ink_keyword")
      expect_equal(value$keyword, keyword)
    }
  }
  value <- compiled_value("A { color: inherit; }")
  expect_equal(format(value), "inherit")
  expect_output(print(value), "<ink_keyword> inherit", fixed = TRUE)
})

test_that("keywords work for custom properties and are not resolved", {
  registry <- ink_register_property(ink_registry(), "gap", "length", "0")
  sheet <- compile_text("A { gap: initial; }", registry = registry)
  expect_equal(sheet$rules[[1L]]$declarations[[1L]]$value$keyword, "initial")
})

test_that("a keyword cannot be mixed with other tokens", {
  expect_compile_error("A { color: inherit red; }", "invalid_color", 1, 12)
  expect_compile_error("A { padding: 1 initial; }", "invalid_spacing", 1, 14)
})

test_that("compiling does not apply the cascade or fill in initial values", {
  sheet <- compile_text("A { color: red; }\nA { color: blue; }")
  expect_length(sheet$rules, 2L)
  expect_length(sheet$rules[[1L]]$declarations, 1L)
})

test_that("the specified workflow works", {
  ast <- ink_parse("
    $primary: #7c3aed;

    Button.primary {
      background: $primary;
      padding: 1 2;
      border: round $primary;
      text-style: bold;
    }
  ")
  sheet <- ink_compile(ast)
  expect_true(inherits(sheet, "ink_compiled_stylesheet"))
  values <- lapply(sheet$rules[[1L]]$declarations, function(d) d$value)
  expect_s3_class(values[[1L]], "ink_color")
  expect_s3_class(values[[2L]], "ink_spacing")
  expect_s3_class(values[[3L]], "ink_border")
  expect_s3_class(values[[4L]], "ink_text_style")
  expect_false(any(vapply(values, is.character, NA)))
})

test_that("the example stylesheets compile", {
  for (file in c("basic.inkr", "variables.inkr")) {
    path <- system.file("examples", file, package = "inkr")
    skip_if(path == "", "installed examples are not available")
    sheet <- ink_compile(ink_read(path))
    expect_s3_class(sheet, "ink_compiled_stylesheet")
    expect_equal(sheet$source, path)
    expect_gt(length(sheet$rules), 3L)
  }
})

test_that("ink_compile requires an AST", {
  expect_error(ink_compile("Button {}"), "ink_stylesheet_ast")
  expect_error(ink_compile(NULL), "ink_stylesheet_ast")
  expect_error(ink_compile(list(statements = list())), "ink_stylesheet_ast")
  err <- tryCatch(ink_compile("x"), error = identity)
  expect_false(inherits(err, "ink_compile_error"))
})

test_that("print summarizes the compiled stylesheet", {
  sheet <- compile_text(
    "$primary: #7c3aed;\nButton.primary { background: $primary; padding: 1 2; border: round $primary; }"
  )
  output <- capture.output(result <- print(sheet))
  expect_identical(result, sheet)
  expect_match(output[[1L]], "<ink_compiled_stylesheet> <text>: 1 rule, 1 variable", fixed = TRUE)
  expect_true(any(grepl("$primary = #7c3aed", output, fixed = TRUE)))
  expect_true(any(grepl("Button.primary", output, fixed = TRUE)))
  expect_true(any(grepl("background: #7c3aed", output, fixed = TRUE)))
  expect_true(any(grepl("border: round #7c3aed", output, fixed = TRUE)))
  expect_true(any(grepl("padding: top=1 right=2 bottom=1 left=2", output, fixed = TRUE)))
})

test_that("print handles empty and long stylesheets", {
  expect_match(capture.output(print(compile_text("")))[[1L]], "0 rules, 0 variables", fixed = TRUE)
  long <- compile_text(paste(rep("A { width: 1; }", 5L), collapse = "\n"))
  output <- capture.output(print(long, n = 2L))
  expect_true(any(grepl("and 3 more rules", output, fixed = TRUE)))
})
