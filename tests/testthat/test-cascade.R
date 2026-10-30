registry <- ink_registry()

cascade_for <- function(css, element) {
  sheet <- compile_text(css)
  ink_cascade(ink_match(sheet, element), sheet$registry)
}

test_that("no matching declarations give an empty cascade", {
  expect_length(ink_cascade(list(), registry), 0L)
  expect_length(cascade_for("Input { color: red; }", ink_element("Button")), 0L)
  expect_length(cascade_for("", ink_element("Button")), 0L)
  expect_length(cascade_for("Button {}", ink_element("Button")), 0L)
})

test_that("one matching declaration wins by default", {
  winners <- cascade_for("Button { color: red; }", ink_element("Button"))
  expect_named(winners, "color")
  winner <- winners$color
  expect_equal(winner$property, "color")
  expect_equal(format(winner$value), "red")
  expect_equal(winner$rule_index, 1L)
  expect_equal(winner$declaration_index, 1L)
  expect_equal(winner$selector_index, 1L)
  expect_identical(winner$specificity, c(id = 0L, class = 0L, type = 1L))
})

test_that("winners carry the declaration's source span", {
  sheet <- compile_text("Button {\n  color: red;\n  padding: 1;\n}")
  winners <- ink_cascade(ink_match(sheet, ink_element("Button")), sheet$registry)
  expect_identical(winners$padding$span, sheet$rules[[1L]]$declarations[[2L]]$span)
  expect_identical(winners$color$span$start$line, 2L)
})

test_that("each property is decided independently", {
  css <- "Button { color: white; background: blue; padding: 1; }\nButton.primary { color: yellow; }"
  winners <- cascade_for(css, ink_element("Button", classes = "primary"))
  expect_equal(format(winners$color$value), "yellow")
  expect_equal(winners$color$rule_index, 2L)
  expect_equal(format(winners$background$value), "blue")
  expect_equal(winners$background$rule_index, 1L)
  expect_equal(winners$padding$rule_index, 1L)
})

test_that("a more specific selector beats a less specific one", {
  winners <- cascade_for(
    "Button { color: red; }\n.primary { color: blue; }",
    ink_element("Button", classes = "primary")
  )
  expect_equal(format(winners$color$value), "blue")
})

test_that("an earlier, more specific rule beats a later one", {
  winners <- cascade_for(
    "#save { color: red; }\nButton { color: blue; }",
    ink_element("Button", id = "save")
  )
  expect_equal(format(winners$color$value), "red")
  expect_equal(winners$color$rule_index, 1L)
})

test_that("equal specificity goes to the later rule", {
  winners <- cascade_for(
    "Button { color: red; }\nButton { color: blue; }",
    ink_element("Button")
  )
  expect_equal(format(winners$color$value), "blue")
  expect_equal(winners$color$rule_index, 2L)
})

test_that("a later declaration in the same rule wins", {
  winners <- cascade_for("Button { color: red; color: green; }", ink_element("Button"))
  expect_equal(format(winners$color$value), "green")
  expect_equal(winners$color$declaration_index, 2L)
  winners <- cascade_for("Button { color: red; width: 3; color: green; color: blue; }", ink_element("Button"))
  expect_equal(format(winners$color$value), "blue")
  expect_equal(winners$color$declaration_index, 4L)
})

test_that("selector lists use the specificity of the selector that matched", {
  css <- "Button, #save { color: red; }\nButton.primary { color: blue; }"
  with_id <- cascade_for(css, ink_element("Button", id = "save", classes = "primary"))
  expect_equal(format(with_id$color$value), "red")
  expect_identical(with_id$color$specificity, c(id = 1L, class = 0L, type = 0L))
  expect_equal(with_id$color$selector_index, 2L)

  without_id <- cascade_for(css, ink_element("Button", classes = "primary"))
  expect_equal(format(without_id$color$value), "blue")
})

test_that("several matching selectors in one rule still count once", {
  sheet <- compile_text("Button, .primary, Button.primary { color: red; padding: 1; }")
  matches <- ink_match(sheet, ink_element("Button", classes = "primary"))
  candidates <- collect_declarations(matches$matches, sheet$registry)
  expect_length(candidates, 2L)
  winners <- ink_cascade(matches, sheet$registry)
  expect_equal(winners$color$selector_index, 3L)
  expect_identical(winners$color$specificity, c(id = 0L, class = 1L, type = 1L))
})

test_that("candidates are collected in source order, one per declaration", {
  sheet <- compile_text("A { color: red; width: 1; }\nB { color: blue; }\nA { height: 2; }")
  matches <- ink_match(sheet, ink_element("A"))
  candidates <- collect_declarations(matches$matches, sheet$registry)
  expect_equal(vapply(candidates, function(c) c$property, ""), c("color", "width", "height"))
  expect_equal(vapply(candidates, function(c) c$rule_index, 0L), c(1L, 1L, 3L))
  expect_equal(vapply(candidates, function(c) c$declaration_index, 0L), c(1L, 2L, 1L))
})

test_that("winners come back in registry order", {
  sheet <- compile_text("Button { height: 2; color: red; padding: 1; background: blue; }")
  winners <- ink_cascade(ink_match(sheet, ink_element("Button")), sheet$registry)
  expect_equal(names(winners), c("color", "background", "padding", "height"))
})

test_that("the result does not depend on the order candidates are visited in", {
  css <- paste(
    "Button { color: red; width: 1; }", "#a { color: blue; }", "Button.x { color: green; }",
    "Button { color: white; width: 2; }", ".x, .y { width: 3; color: black; }", sep = "\n"
  )
  sheet <- compile_text(css)
  matches <- ink_match(sheet, ink_element("Button", id = "a", classes = c("x", "y")))$matches
  forward <- ink_cascade(matches, sheet$registry)
  backward <- ink_cascade(rev(matches), sheet$registry)
  expect_identical(forward, backward)
})

test_that("the cascade accepts a match result and does not resolve keywords", {
  sheet <- compile_text("Button { color: inherit; background: initial; }")
  result <- ink_match(sheet, ink_element("Button"))
  winners <- ink_cascade(result, sheet$registry)
  expect_s3_class(winners$color$value, "ink_keyword")
  expect_equal(winners$color$value$keyword, "inherit")
  expect_equal(winners$background$value$keyword, "initial")
})

test_that("source order of the stylesheet is not changed by the cascade", {
  sheet <- compile_text("B { color: red; }\nA { color: blue; }")
  before <- sheet
  ink_cascade(ink_match(sheet, ink_element("A")), sheet$registry)
  expect_identical(sheet, before)
})

test_that("the cascade rejects declarations for unregistered properties", {
  sheet <- compile_text("Button { color: red; }")
  sheet$rules[[1L]]$declarations[[1L]]$name <- "bogus"
  matches <- ink_match(sheet, ink_element("Button"))
  expect_compute_error(ink_cascade(matches, sheet$registry), "invalid_cascade_result")
  expect_compute_error(ink_cascade(list(42), registry), "invalid_cascade_result")
})

test_that("priority comparison orders by specificity, then rule, then declaration", {
  make <- function(spec, rule, decl) {
    list(specificity = spec, rule_index = rule, declaration_index = decl)
  }
  low <- c(id = 0L, class = 0L, type = 1L)
  high <- c(id = 0L, class = 1L, type = 0L)
  expect_equal(compare_priority(make(high, 1L, 1L), make(low, 9L, 9L)), 1L)
  expect_equal(compare_priority(make(low, 9L, 9L), make(high, 1L, 1L)), -1L)
  expect_equal(compare_priority(make(low, 2L, 1L), make(low, 1L, 9L)), 1L)
  expect_equal(compare_priority(make(low, 1L, 2L), make(low, 1L, 1L)), 1L)
  expect_equal(compare_priority(make(low, 1L, 1L), make(low, 1L, 1L)), 0L)
})
