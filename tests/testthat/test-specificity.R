spec <- function(text) {
  ink_specificity(selector_of(text))
}

tuple <- function(id, class, type) {
  c(id = id, class = class, type = type)
}

test_that("specificity is a named integer vector", {
  result <- spec("Button")
  expect_identical(result, c(id = 0L, class = 0L, type = 1L))
  expect_type(result, "integer")
  expect_named(result, c("id", "class", "type"))
})

test_that("the documented examples have the documented tuples", {
  expect_identical(spec("*"), c(id = 0L, class = 0L, type = 0L))
  expect_identical(spec("Button"), c(id = 0L, class = 0L, type = 1L))
  expect_identical(spec(".primary"), c(id = 0L, class = 1L, type = 0L))
  expect_identical(spec("#save"), c(id = 1L, class = 0L, type = 0L))
  expect_identical(spec("Button.primary"), c(id = 0L, class = 1L, type = 1L))
  expect_identical(spec("Button:focus"), c(id = 0L, class = 1L, type = 1L))
  expect_identical(spec("Button.primary:focus"), c(id = 0L, class = 2L, type = 1L))
  expect_identical(spec("#dialog Button"), c(id = 1L, class = 0L, type = 1L))
  expect_identical(spec("#dialog > Button"), c(id = 1L, class = 0L, type = 1L))
  expect_identical(spec("A B C"), c(id = 0L, class = 0L, type = 3L))
  expect_identical(spec("A.primary > B:focus"), c(id = 0L, class = 2L, type = 2L))
})

test_that("the universal selector and combinators count for nothing", {
  expect_identical(spec("* > *"), c(id = 0L, class = 0L, type = 0L))
  expect_identical(spec("* *"), c(id = 0L, class = 0L, type = 0L))
  expect_identical(spec("*.a"), c(id = 0L, class = 1L, type = 0L))
  expect_identical(spec("*:focus"), c(id = 0L, class = 1L, type = 0L))
  expect_identical(spec("* Button"), spec("Button"))
  expect_identical(spec("A > B"), spec("A B"))
})

test_that("pseudo-classes count like classes", {
  expect_identical(spec(":focus:selected:disabled"), c(id = 0L, class = 3L, type = 0L))
  expect_identical(spec(".a:focus"), spec(".a.b"))
})

test_that("every compound in a complex selector contributes", {
  expect_identical(spec("#a #b .c .d E F"), c(id = 2L, class = 2L, type = 2L))
  expect_identical(spec("A#a.x:s > B#b.y:t"), c(id = 2L, class = 4L, type = 2L))
})

test_that("specificity depends only on selector structure", {
  expect_identical(spec("Button.primary"), spec("Button.primary"))
  expect_identical(spec("Button.a.b"), spec("Button.b.a"))
  ast <- ink_parse("Button.primary { color: red; }\n\nButton.primary   { color: blue; }")
  first <- ink_specificity(ast$statements[[1L]]$selectors$selectors[[1L]])
  second <- ink_specificity(ast$statements[[2L]]$selectors$selectors[[1L]])
  expect_identical(first, second)
})

test_that("comparison is lexicographic", {
  less <- function(a, b) compare_specificity(a, b) == -1L
  expect_true(less(tuple(0L, 0L, 5L), tuple(0L, 1L, 0L)))
  expect_true(less(tuple(0L, 9L, 9L), tuple(1L, 0L, 0L)))
  expect_true(less(tuple(0L, 0L, 1L), tuple(0L, 0L, 2L)))
  expect_equal(compare_specificity(tuple(1L, 2L, 3L), tuple(1L, 2L, 3L)), 0L)
  expect_equal(compare_specificity(tuple(1L, 0L, 0L), tuple(0L, 99L, 99L)), 1L)
  expect_equal(compare_specificity(tuple(0L, 2L, 0L), tuple(0L, 1L, 50L)), 1L)
  expect_equal(compare_specificity(tuple(0L, 1L, 2L), tuple(0L, 1L, 3L)), -1L)
})

test_that("counts above nine do not overflow into the next component", {
  classes <- function(n) paste0(".c", seq_len(n), collapse = "")
  eleven_classes <- spec(classes(11L))
  one_id <- spec("#x")
  expect_identical(eleven_classes, tuple(0L, 11L, 0L))
  # With a 100/10/1 weighting eleven classes (110) would outrank one ID (100).
  expect_equal(compare_specificity(one_id, eleven_classes), 1L)
  expect_equal(compare_specificity(eleven_classes, one_id), -1L)

  twelve_types <- spec(paste(rep("T", 12L), collapse = " "))
  one_class <- spec(".a")
  expect_identical(twelve_types, tuple(0L, 0L, 12L))
  expect_equal(compare_specificity(one_class, twelve_types), 1L)

  expect_equal(compare_specificity(spec(classes(10L)), spec(classes(9L))), 1L)
  expect_equal(compare_specificity(spec(paste0("E", classes(10L))), spec(classes(10L))), 1L)
  # Ordinary vector comparison is not lexicographic: this is the trap.
  expect_true(all(tuple(1L, 0L, 0L) < tuple(2L, 1L, 1L)))
  expect_false(all(tuple(1L, 0L, 0L) > tuple(0L, 11L, 0L)))
  expect_equal(compare_specificity(tuple(1L, 0L, 0L), tuple(0L, 11L, 0L)), 1L)
})

test_that("ink_specificity only accepts well-formed complex selectors", {
  list_node <- ink_parse("A, B {}")$statements[[1L]]$selectors
  expect_match_error(ink_specificity(list_node), "invalid_selector")
  expect_match_error(ink_specificity(compound_of("A")), "invalid_selector")
  expect_match_error(ink_specificity("Button"), "invalid_selector")
  expect_match_error(ink_specificity(NULL), "invalid_selector")
  expect_match_error(ink_specificity(list()), "invalid_selector")
  broken <- selector_of("A.b")
  broken$compounds[[1L]]$classes <- NULL
  expect_match_error(ink_specificity(broken), "invalid_selector")
  broken <- selector_of("A B")
  broken$compounds <- list()
  expect_match_error(ink_specificity(broken), "invalid_selector")
})
