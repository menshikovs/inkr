test_that("ink_parse returns an ink_stylesheet_ast", {
  ast <- ink_parse("Button { color: red; }")
  expect_s3_class(ast, "ink_stylesheet_ast")
  expect_equal(ast$source, "<text>")
  expect_equal(ink_parse("A {}", source = "x.inkr")$source, "x.inkr")
})

test_that("empty and comment-only stylesheets are valid", {
  for (text in c("", "   \n\t\n", "/* nothing */", "/* a */ /* b */\n")) {
    ast <- ink_parse(text)
    expect_s3_class(ast, "ink_stylesheet_ast")
    expect_length(ast$statements, 0L)
  }
  expect_length(ink_parse(character())$statements, 0L)
})

test_that("a single rule is parsed", {
  rule <- first_rule("Button { color: red; }")
  expect_s3_class(rule, "ink_rule")
  expect_length(rule$declarations, 1L)
  expect_equal(rule$declarations[[1L]]$name, "color")
  expect_equal(rule$declarations[[1L]]$value, "red")
})

test_that("multiple rules are kept in source order", {
  ast <- ink_parse("A { a: 1; }\nB { b: 2; }\nC { c: 3; }")
  types <- vapply(
    ast$statements,
    function(rule) rule$selectors$selectors[[1L]]$compounds[[1L]]$type,
    ""
  )
  expect_equal(types, c("A", "B", "C"))
})

test_that("an empty rule is valid", {
  for (text in c("A {}", "A { }", "A {\n}", "A { /* nothing */ }")) {
    rule <- first_rule(text)
    expect_s3_class(rule, "ink_rule")
    expect_length(rule$declarations, 0L)
  }
})

test_that("global variables are parsed", {
  node <- first_rule("$primary: #7c3aed;")
  expect_s3_class(node, "ink_variable_declaration")
  expect_equal(node$name, "primary")
  expect_equal(node$value, "#7c3aed")
})

test_that("multiple variables and rules interleave in source order", {
  text <- "$a: 1;\n$b: red;\nA { x: $a; }\n$c: 2 3;"
  nodes <- statements(text)
  expect_equal(
    vapply(nodes, function(node) class(node)[[1L]], ""),
    c(
      "ink_variable_declaration", "ink_variable_declaration",
      "ink_rule", "ink_variable_declaration"
    )
  )
  expect_equal(nodes[[2L]]$name, "b")
  expect_equal(nodes[[4L]]$value, "2 3")
})

test_that("variable declarations tolerate whitespace around the colon", {
  expect_equal(first_rule("$a:red;")$value, "red")
  expect_equal(first_rule("$a   :   red   ;")$value, "red")
})

test_that("type, class, id and universal selectors are parsed", {
  type <- first_compound("Button {}")
  expect_equal(type$type, "Button")
  expect_null(type$id)
  expect_equal(type$classes, character())

  class <- first_compound(".primary {}")
  expect_null(class$type)
  expect_equal(class$classes, "primary")

  id <- first_compound("#results {}")
  expect_null(id$type)
  expect_equal(id$id, "results")

  universal <- first_compound("* {}")
  expect_equal(universal$type, "*")
})

test_that("compound selectors combine parts in one node", {
  compound <- first_compound("Button.primary.big#ok:focus:hover {}")
  expect_equal(compound$type, "Button")
  expect_equal(compound$classes, c("primary", "big"))
  expect_equal(compound$id, "ok")
  expect_equal(compound$pseudo_classes, c("focus", "hover"))

  expect_equal(first_compound("*.a:b {}")$type, "*")
  expect_equal(first_compound(".a.b {}")$classes, c("a", "b"))
  expect_equal(first_compound("#x.a {}")$classes, "a")
})

test_that("pseudo-classes are parsed without being interpreted", {
  for (name in c("focus", "selected", "disabled", "anything-goes", "x_1")) {
    compound <- first_compound(sprintf("Button:%s {}", name))
    expect_equal(compound$pseudo_classes, name)
  }
  expect_equal(first_compound(":focus {}")$pseudo_classes, "focus")
})

test_that("the descendant combinator is whitespace between compounds", {
  complex <- first_rule("Container Button {}")$selectors$selectors[[1L]]
  expect_length(complex$compounds, 2L)
  expect_equal(complex$combinators, "descendant")
  expect_equal(complex$compounds[[1L]]$type, "Container")
  expect_equal(complex$compounds[[2L]]$type, "Button")

  tabbed <- first_rule("A\n\t B {}")$selectors$selectors[[1L]]
  expect_equal(tabbed$combinators, "descendant")
})

test_that("the child combinator works with and without spaces", {
  for (text in c("A > B {}", "A>B {}", "A >B {}", "A> B {}", "A\n>\nB {}")) {
    complex <- first_rule(text)$selectors$selectors[[1L]]
    expect_equal(complex$combinators, "child", info = text)
    expect_equal(complex$compounds[[2L]]$type, "B", info = text)
  }
})

test_that("combinators in a chain are kept in order", {
  complex <- first_rule("A B > C D > #e.f {}")$selectors$selectors[[1L]]
  expect_equal(complex$combinators, c("descendant", "child", "descendant", "child"))
  expect_equal(
    vapply(complex$compounds, function(compound) compound$type %||% "-", ""),
    c("A", "B", "C", "D", "-")
  )
})

test_that("selector lists are parsed", {
  list <- first_rule("Button, Input,\n  .a > B, #c {}")$selectors
  expect_s3_class(list, "ink_selector_list")
  expect_length(list$selectors, 4L)
  expect_equal(list$selectors[[3L]]$combinators, "child")
  expect_equal(list$selectors[[4L]]$compounds[[1L]]$id, "c")
})

test_that("a rule can have many declarations", {
  rule <- first_rule("A { color: red; padding: 1 2; text-style: bold; }")
  expect_equal(
    vapply(rule$declarations, function(d) d$name, ""),
    c("color", "padding", "text-style")
  )
  expect_equal(
    vapply(rule$declarations, function(d) d$value, ""),
    c("red", "1 2", "bold")
  )
})

test_that("values may contain whitespace", {
  rule <- first_rule("A { border: round  $primary;\n margin:\n 0\n 1\n; }")
  expect_equal(rule$declarations[[1L]]$value, "round $primary")
  expect_equal(rule$declarations[[2L]]$value, "0 1")
})

test_that("values keep every kind of token as written", {
  rule <- first_rule("A { x: #7C3AED $v 50% -1.5 name, 2px; }")
  expect_equal(rule$declarations[[1L]]$value, "#7C3AED $v 50% -1.5 name, 2px")
})

test_that("the last declaration still needs its semicolon", {
  expect_parse_error("A { color: red }", "missing_semicolon", 1, 15)
  expect_length(ink_parse("A { color: red; }")$statements, 1L)
})

test_that("keyword-looking property names and values are plain identifiers", {
  rule <- first_rule("A { color: color; background: background; }")
  expect_equal(rule$declarations[[1L]]$value, "color")
})

test_that("a variable reference inside a value is not resolved", {
  ast <- ink_parse("$p: red;\nA { color: $p; }")
  expect_equal(ast$statements[[2L]]$declarations[[1L]]$value, "$p")
})

test_that("ink_parse accepts a character vector of lines", {
  ast <- ink_parse(c("A {", "  color: red;", "}"))
  expect_equal(ast$statements[[1L]]$span$end$line, 3L)
})

test_that("whitespace and comments do not change the parsed meaning", {
  pairs <- list(
    c("A.b > C:d, E { x: 1 2; }", "  A.b>C:d ,E{x:1  2 ;}"),
    c(
      "$a: 1 2;\nA B { c: d; }",
      paste0(
        "/* head */ $a:/* in */1/* x */2 ;\n/* y */A /* z */ B/* w */{",
        "/* v */c/* u */:/* t */d/* s */;/* r */}/* q */"
      )
    ),
    c("A, B { }", "A,\n\n   B\n{\n}\n")
  )
  for (pair in pairs) {
    expect_equal(
      strip_locations(ink_parse(pair[[1L]])),
      strip_locations(ink_parse(pair[[2L]])),
      info = pair[[1L]]
    )
  }
})

test_that("a comment between compounds does not act as a descendant combinator", {
  expect_parse_error("A/* c */B {}", "invalid_selector", 1, 9)
  complex <- first_rule("A /* c */ B {}")$selectors$selectors[[1L]]
  expect_equal(complex$combinators, "descendant")
})

test_that("comments are allowed everywhere trivia is", {
  text <- "/* a */ A /* b */ { /* c */ x /* d */ : /* e */ 1 /* f */ ; /* g */ } /* h */"
  rule <- first_rule(text)
  expect_equal(rule$declarations[[1L]]$name, "x")
  expect_equal(rule$declarations[[1L]]$value, "1")
})

test_that("CRLF line endings parse like LF", {
  lf <- "A {\n  b: c;\n}\n"
  crlf <- "A {\r\n  b: c;\r\n}\r\n"
  expect_equal(
    strip_locations(ink_parse(crlf)),
    strip_locations(ink_parse(lf))
  )
  expect_equal(ink_parse(crlf)$statements[[1L]]$declarations[[1L]]$span$start$line, 2L)
})

test_that("the example stylesheet parses", {
  path <- system.file("examples", "basic.inkr", package = "inkr")
  skip_if(path == "", "installed examples are not available")
  ast <- ink_read(path)
  expect_s3_class(ast, "ink_stylesheet_ast")
  expect_gt(length(ast$statements), 10L)
  expect_equal(ast$source, path)
})
