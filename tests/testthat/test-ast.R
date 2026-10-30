example_text <- paste(
  "$primary: #7c3aed;",
  "",
  "Button.primary:focus {",
  "    color: $primary;",
  "    padding: 1 2;",
  "}",
  sep = "\n"
)

test_that("nodes have the documented classes", {
  ast <- ink_parse(example_text)
  expect_s3_class(ast, "ink_stylesheet_ast")

  variable <- ast$statements[[1L]]
  expect_s3_class(variable, "ink_variable_declaration")
  expect_s3_class(variable, "ink_node")

  rule <- ast$statements[[2L]]
  expect_s3_class(rule, "ink_rule")
  expect_s3_class(rule$selectors, "ink_selector_list")
  expect_s3_class(rule$selectors$selectors[[1L]], "ink_complex_selector")
  expect_s3_class(
    rule$selectors$selectors[[1L]]$compounds[[1L]],
    "ink_compound_selector"
  )
  expect_s3_class(rule$declarations[[1L]], "ink_property_declaration")
  expect_true(all(vapply(
    list(rule, rule$selectors, rule$declarations[[1L]]),
    inherits, logical(1), what = "ink_node"
  )))
})

test_that("node fields are stable", {
  ast <- ink_parse(example_text)
  expect_named(ast, c("statements", "source"))
  expect_named(ast$statements[[1L]], c("name", "value", "value_span", "span"))

  rule <- ast$statements[[2L]]
  expect_named(rule, c("selectors", "declarations", "span"))
  expect_named(rule$selectors, c("selectors", "span"))
  expect_named(rule$selectors$selectors[[1L]], c("compounds", "combinators", "span"))
  expect_named(
    rule$declarations[[1L]],
    c("name", "value", "value_span", "span")
  )
  expect_named(
    rule$selectors$selectors[[1L]]$compounds[[1L]],
    c("type", "id", "classes", "pseudo_classes", "span")
  )
})

test_that("absent parts of a compound selector keep their fields", {
  compound <- first_compound(".a {}")
  expect_true("type" %in% names(compound))
  expect_true("id" %in% names(compound))
  expect_null(compound$type)
  expect_null(compound$id)
  expect_identical(compound$pseudo_classes, character())
})

test_that("the example rule is represented as documented", {
  rule <- ink_parse(example_text)$statements[[2L]]
  compound <- rule$selectors$selectors[[1L]]$compounds[[1L]]

  expect_equal(compound$type, "Button")
  expect_equal(compound$classes, "primary")
  expect_equal(compound$pseudo_classes, "focus")
  expect_null(compound$id)

  expect_length(rule$declarations, 2L)
  expect_equal(rule$declarations[[1L]]$name, "color")
  expect_equal(rule$declarations[[1L]]$value, "$primary")
  expect_equal(rule$declarations[[2L]]$name, "padding")
  expect_equal(rule$declarations[[2L]]$value, "1 2")
})

test_that("statements keep their original order", {
  text <- "A {}\n$x: 1;\nB {}\n$y: 2;\nC {}"
  ast <- ink_parse(text)
  starts <- vapply(ast$statements, function(node) node$span$start$offset, 0L)
  expect_equal(starts, sort(starts))
  expect_equal(
    vapply(ast$statements, function(node) substr(text, node$span$start$offset + 1L, node$span$start$offset + 2L), ""),
    c("A ", "$x", "B ", "$y", "C ")
  )
})

test_that("complex selectors keep combinator relationships", {
  complex <- first_rule("A B > C {}")$selectors$selectors[[1L]]
  expect_s3_class(complex, "ink_complex_selector")
  expect_equal(complex$combinators, c("descendant", "child"))
  expect_length(complex$combinators, length(complex$compounds) - 1L)

  single <- first_rule("A {}")$selectors$selectors[[1L]]
  expect_identical(single$combinators, character())
})

test_that("raw values are preserved, not interpreted", {
  rule <- first_rule(
    "A { color: #7C3AED; width: 50%; x: $unknown; y: 1.50; z: +3; }"
  )
  values <- vapply(rule$declarations, function(d) d$value, "")
  expect_equal(values, c("#7C3AED", "50%", "$unknown", "1.50", "+3"))
})

test_that("unknown properties and values are accepted", {
  rule <- first_rule("A { not-a-real-property: nonsense 42 nonsense; }")
  expect_equal(rule$declarations[[1L]]$name, "not-a-real-property")
  expect_equal(rule$declarations[[1L]]$value, "nonsense 42 nonsense")
})

test_that("node spans cover exactly their source text", {
  text <- example_text
  ast <- ink_parse(text)

  variable <- ast$statements[[1L]]
  expect_equal(span_text(text, variable$span), "$primary: #7c3aed;")
  expect_equal(span_text(text, variable$value_span), "#7c3aed")

  rule <- ast$statements[[2L]]
  expect_equal(
    span_text(text, rule$span),
    "Button.primary:focus {\n    color: $primary;\n    padding: 1 2;\n}"
  )
  expect_equal(span_text(text, rule$selectors$span), "Button.primary:focus")
  expect_equal(
    span_text(text, rule$selectors$selectors[[1L]]$compounds[[1L]]$span),
    "Button.primary:focus"
  )
  expect_equal(span_text(text, rule$declarations[[1L]]$span), "color: $primary;")
  expect_equal(span_text(text, rule$declarations[[1L]]$value_span), "$primary")
  expect_equal(span_text(text, rule$declarations[[2L]]$span), "padding: 1 2;")
  expect_equal(span_text(text, rule$declarations[[2L]]$value_span), "1 2")
})

test_that("spans of selector parts are precise", {
  text <- "Container   >  Button.a, #b {}"
  list <- first_rule(text)$selectors
  expect_equal(span_text(text, list$span), "Container   >  Button.a, #b")

  complex <- list$selectors
  expect_equal(span_text(text, complex[[1L]]$span), "Container   >  Button.a")
  expect_equal(span_text(text, complex[[2L]]$span), "#b")
  expect_equal(span_text(text, complex[[1L]]$compounds[[2L]]$span), "Button.a")
})

test_that("locations carry line, column and offset", {
  text <- "A {}\n\n  B { color: red; }"
  rule <- ink_parse(text)$statements[[2L]]
  expect_equal(loc(rule$span$start), c(line = 3, column = 3, offset = 8))
  expect_equal(loc(rule$span$end), c(line = 3, column = 20, offset = 25))

  declaration <- rule$declarations[[1L]]
  expect_equal(loc(declaration$span$start), c(line = 3, column = 7, offset = 12))
  expect_equal(loc(declaration$value_span$start), c(line = 3, column = 14, offset = 19))
  expect_equal(loc(declaration$value_span$end), c(line = 3, column = 17, offset = 22))
})

test_that("a multiline rule spans several lines", {
  rule <- first_rule("A {\n  b: c;\n}")
  expect_equal(rule$span$start$line, 1L)
  expect_equal(rule$span$end$line, 3L)
  expect_equal(rule$span$end$column, 2L)
})

test_that("spans of different parses of the same text are identical", {
  expect_identical(ink_parse(example_text), ink_parse(example_text))
})

test_that("changing whitespace changes only locations", {
  tight <- ink_parse("A{b:c;}")
  loose <- ink_parse("A  {\n  b :  c  ;\n}")
  expect_false(identical(tight, loose))
  expect_identical(strip_locations(tight), strip_locations(loose))
})

test_that("print shows a summary of the statements", {
  ast <- ink_parse(example_text)
  output <- capture.output(result <- print(ast))
  expect_identical(result, ast)
  expect_match(output[[1L]], "<ink_stylesheet_ast>", fixed = TRUE)
  expect_match(output[[1L]], "2 statements", fixed = TRUE)
  expect_true(any(grepl("$primary: #7c3aed;", output, fixed = TRUE)))
  expect_true(any(grepl("Button.primary:focus { 2 declarations }", output, fixed = TRUE)))
})

test_that("print formats selectors faithfully", {
  ast <- ink_parse("A B > C.d#e:f, * {}")
  output <- capture.output(print(ast))
  expect_true(any(grepl("A B > C#e.d:f, *", output, fixed = TRUE)))
})

test_that("print handles empty stylesheets and truncates long ones", {
  expect_match(capture.output(print(ink_parse("")))[[1L]], "0 statements", fixed = TRUE)
  expect_match(capture.output(print(ink_parse("A{}")))[[1L]], "1 statement$")

  long <- ink_parse(paste(rep("A {}", 5L), collapse = "\n"))
  output <- capture.output(print(long, n = 2L))
  expect_length(output, 4L)
  expect_match(output[[4L]], "and 3 more", fixed = TRUE)
})

test_that("AST constructors reject malformed nodes", {
  expect_error(new_ink_stylesheet_ast(list(1), "<text>"))
  expect_error(new_ink_complex_selector(list(), character(), NULL))
  compound <- first_compound("A {}")
  expect_error(new_ink_complex_selector(list(compound), "child", NULL))
  expect_error(new_ink_complex_selector(list(compound, compound), "sibling", NULL))
})
