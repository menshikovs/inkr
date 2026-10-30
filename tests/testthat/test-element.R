test_that("a minimal element has the documented fields", {
  element <- ink_element("Button")
  expect_s3_class(element, "ink_element")
  expect_named(element, c("type", "id", "classes", "states", "parent"))
  expect_equal(element$type, "Button")
  expect_null(element$id)
  expect_identical(element$classes, character())
  expect_identical(element$states, character())
  expect_null(element$parent)
})

test_that("elements are not AST nodes", {
  element <- ink_element("Button")
  expect_false(inherits(element, "ink_node"))
  expect_identical(class(element), "ink_element")
})

test_that("an element with all fields keeps them", {
  screen <- ink_element(type = "Screen", id = "main")
  container <- ink_element("Container", id = "content", classes = "panel", parent = screen)
  button <- ink_element(
    "Button",
    id = "save", classes = c("primary", "rounded"), states = "focus", parent = container
  )
  expect_equal(button$type, "Button")
  expect_equal(button$id, "save")
  expect_equal(button$classes, c("primary", "rounded"))
  expect_equal(button$states, "focus")
  expect_equal(button$parent$id, "content")
  expect_equal(button$parent$parent$id, "main")
  expect_null(button$parent$parent$parent)
})

test_that("parent chains can be walked", {
  leaf <- tree("A", "B", "C", "D")
  types <- character()
  current <- leaf
  while (!is.null(current)) {
    types <- c(types, current$type)
    current <- current$parent
  }
  expect_equal(types, c("D", "C", "B", "A"))
})

test_that("empty class and state vectors are allowed", {
  element <- ink_element("Button", classes = character(), states = character())
  expect_length(element$classes, 0L)
  expect_length(element$states, 0L)
})

test_that("duplicate classes and states are removed, keeping first-occurrence order", {
  element <- ink_element(
    "Button",
    classes = c("b", "a", "b", "c", "a"),
    states = c("focus", "disabled", "focus")
  )
  expect_equal(element$classes, c("b", "a", "c"))
  expect_equal(element$states, c("focus", "disabled"))
})

test_that("names and attributes are not kept on stored values", {
  element <- ink_element(c(t = "Button"), id = c(i = "x"), classes = c(a = "one"), states = c(b = "two"))
  expect_null(names(element$type))
  expect_null(names(element$id))
  expect_null(names(element$classes))
  expect_null(names(element$states))
})

test_that("identifiers are case-sensitive", {
  expect_false(identical(ink_element("button")$type, ink_element("Button")$type))
  element <- ink_element("Button", id = "Save", classes = c("Primary", "primary"))
  expect_equal(element$id, "Save")
  expect_equal(element$classes, c("Primary", "primary"))
})

test_that("unusual but valid identifiers are accepted", {
  element <- ink_element("_Widget-2", id = "a_b-c", classes = c("_x", "y1"), states = "is-open")
  expect_equal(element$type, "_Widget-2")
  expect_equal(element$states, "is-open")
})

test_that("an invalid type is rejected", {
  invalid <- list("", NA_character_, 1, NULL, c("A", "B"), "*", "#a", ".a", ":a", " Button",
                  "Button ", "Button\n", "1Button", "a b", "café", factor("Button"), TRUE, list("A"))
  for (type in invalid) {
    expect_match_error(ink_element(type), "invalid_element_type")
  }
  expect_match_error(ink_element(), "invalid_element_type")
  err <- match_error(ink_element("*"))
  expect_match(err$message, "universal", fixed = TRUE)
  expect_equal(err$argument, "type")
})

test_that("an invalid id is rejected", {
  for (id in list("", "#save", ".save", NA_character_, 1, c("a", "b"), "a b", "1a", "a\n", factor("x"), TRUE)) {
    expect_match_error(ink_element("Button", id = id), "invalid_element_id")
  }
  err <- match_error(ink_element("Button", id = "#save"))
  expect_match(err$message, "without the '#'", fixed = TRUE)
  expect_equal(err$argument, "id")
  expect_null(ink_element("Button", id = NULL)$id)
})

test_that("invalid classes are rejected", {
  invalid <- list(NA_character_, c("ok", NA), "", ".primary", "has space", 1, NULL, list("a"),
                  factor("a"), c("ok", "1bad"), "a\n", TRUE)
  for (classes in invalid) {
    expect_match_error(ink_element("Button", classes = classes), "invalid_element_classes")
  }
  err <- match_error(ink_element("Button", classes = c("ok", ".primary")))
  expect_match(err$message, "'.primary'", fixed = TRUE)
  expect_equal(err$argument, "classes")
})

test_that("invalid states are rejected", {
  invalid <- list(NA_character_, "", ":focus", "has space", 1, NULL, list("a"), c("focus", "-x"))
  for (states in invalid) {
    expect_match_error(ink_element("Button", states = states), "invalid_element_states")
  }
  expect_equal(match_error(ink_element("Button", states = ":focus"))$argument, "states")
})

test_that("an invalid parent is rejected", {
  ast_node <- ink_parse("Button {}")$statements[[1L]]
  for (parent in list(list(), "Screen", 1, TRUE, ast_node, ink_parse("Button {}"))) {
    expect_match_error(ink_element("Button", parent = parent), "invalid_element_parent")
  }
  err <- match_error(ink_element("Button", parent = ast_node))
  expect_match(err$message, "AST node", fixed = TRUE)
  expect_equal(err$argument, "parent")

  broken <- ink_element("Screen")
  broken$type <- 42
  expect_match_error(ink_element("Button", parent = broken), "invalid_element_parent")
})

test_that("constructing an element does not modify its parent", {
  parent <- ink_element("Screen", id = "main", classes = "a")
  before <- parent
  child <- ink_element("Button", parent = parent)
  expect_identical(parent, before)
  expect_identical(child$parent, before)
})

test_that("elements store parents only, never children", {
  parent <- ink_element("Screen")
  child <- ink_element("Button", parent = parent)
  expect_named(parent, c("type", "id", "classes", "states", "parent"))
  expect_null(parent$children)
  expect_length(child$parent$classes, 0L)
})

test_that("sibling elements share a parent without interfering", {
  parent <- ink_element("Container")
  a <- ink_element("Button", id = "a", parent = parent)
  b <- ink_element("Button", id = "b", parent = parent)
  expect_equal(c(a$id, b$id), c("a", "b"))
  expect_identical(a$parent, b$parent)
})

test_that("elements format like selectors and print their path", {
  button <- tree(list(type = "Screen", id = "main"), "Container",
                 list(type = "Button", id = "save", classes = c("primary", "rounded"), states = "focus"))
  expect_equal(format(button), "Button#save.primary.rounded:focus")
  expect_equal(format(ink_element("Button")), "Button")
  output <- capture.output(result <- print(button))
  expect_identical(result, button)
  expect_equal(output[[1L]], "<ink_element> Button#save.primary.rounded:focus")
  expect_equal(output[[2L]], "  path: Screen#main > Container > Button#save.primary.rounded:focus")
  expect_length(capture.output(print(ink_element("Button"))), 1L)
})

test_that("long parent chains can be built", {
  leaf <- deep_chain(2000L)
  depth <- 0L
  current <- leaf
  while (!is.null(current)) {
    depth <- depth + 1L
    current <- current$parent
  }
  expect_equal(depth, 2000L)
})
