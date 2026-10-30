empty_sheet <- function() compile_text("")

test_that("invalid element constructor input signals ink_match_error", {
  expect_match_error(ink_element(type = ""), "invalid_element_type")
  expect_match_error(ink_element(type = NA_character_), "invalid_element_type")
  expect_match_error(ink_element(type = "Button", id = "#save"), "invalid_element_id")
  expect_match_error(ink_element(type = "Button", classes = NA_character_), "invalid_element_classes")
  expect_match_error(ink_element(type = "Button", parent = list()), "invalid_element_parent")
})

test_that("the matcher rejects a stylesheet that is not compiled", {
  element <- ink_element("Button")
  for (bad in list(123, NULL, "Button {}", list(), ink_parse("Button {}"), ink_registry())) {
    expect_match_error(ink_match(bad, element), "invalid_stylesheet")
  }
  err <- match_error(ink_match(123, element))
  expect_match(err$message, "ink_compiled_stylesheet", fixed = TRUE)
  expect_equal(err$argument, "sheet")
})

test_that("the matcher rejects an element that is not an element", {
  sheet <- empty_sheet()
  for (bad in list(list(), NULL, "Button", 1, ink_parse("Button {}")$statements[[1L]], sheet)) {
    expect_match_error(ink_match(sheet, bad), "invalid_element")
  }
  err <- match_error(ink_match(sheet, list()))
  expect_equal(err$argument, "element")
  expect_match(err$message, "ink_element", fixed = TRUE)
})

test_that("an element edited into an invalid shape is rejected", {
  sheet <- empty_sheet()
  element <- ink_element("Button", classes = "a")

  bad <- element
  bad$type <- 5
  expect_match_error(ink_match(sheet, bad), "invalid_element")

  bad <- element
  bad$classes <- NA_character_
  expect_match_error(ink_match(sheet, bad), "invalid_element")

  bad <- element
  bad$states <- list("x")
  expect_match_error(ink_match(sheet, bad), "invalid_element")

  bad <- element
  bad$id <- c("a", "b")
  expect_match_error(ink_match(sheet, bad), "invalid_element")

  bad <- element
  bad$parent <- "Screen"
  expect_match_error(ink_match(sheet, bad), "invalid_element")
})

test_that("an invalid ancestor is reported, even far up the chain", {
  sheet <- empty_sheet()
  leaf <- tree("Root", "Middle", "Leaf")
  leaf$parent$parent$type <- NA_character_
  err <- match_error(ink_match(sheet, leaf))
  expect_equal(err$code, "invalid_element")
  expect_match(err$message, "Ancestor 2", fixed = TRUE)
})

test_that("malformed rules and selectors are errors, not silent non-matches", {
  element <- ink_element("Button")

  sheet <- compile_text("Button { color: red; }")
  sheet$rules[[1L]] <- 42
  expect_match_error(ink_match(sheet, element), "invalid_stylesheet")

  sheet <- compile_text("Button { color: red; }")
  sheet$rules[[1L]]$selectors <- NULL
  expect_match_error(ink_match(sheet, element), "invalid_stylesheet")

  sheet <- compile_text("Button { color: red; }")
  sheet$rules <- "none"
  expect_match_error(ink_match(sheet, element), "invalid_stylesheet")

  sheet <- compile_text("Button { color: red; }")
  sheet$rules[[1L]]$selectors$selectors <- list()
  expect_match_error(ink_match(sheet, element), "invalid_stylesheet")

  sheet <- compile_text("Button { color: red; }")
  sheet$rules[[1L]]$selectors$selectors[[1L]]$combinators <- "child"
  expect_match_error(ink_match(sheet, element), "invalid_selector")

  sheet <- compile_text("Button { color: red; }")
  sheet$rules[[1L]]$selectors$selectors[[1L]]$compounds[[1L]]$classes <- 1
  expect_match_error(ink_match(sheet, element), "invalid_selector")

  sheet <- compile_text("Button { color: red; }")
  sheet$rules[[1L]]$selectors$selectors[[1L]] <- "Button"
  expect_match_error(ink_match(sheet, element), "invalid_selector")
})

test_that("matcher errors are ink_match_error and nothing else", {
  err <- match_error(ink_element(""))
  expect_s3_class(err, "ink_match_error")
  expect_s3_class(err, "error")
  expect_false(inherits(err, "ink_parse_error"))
  expect_false(inherits(err, "ink_compile_error"))
  expect_named(
    unclass(err),
    c("message", "call", "code", "detail", "argument")
  )
  expect_equal(err$message, err$detail)
  expect_error(ink_match(1, 2), class = "ink_match_error")
})

test_that("error messages name the problem", {
  expect_match(conditionMessage(match_error(ink_element("Button", id = "#save"))), "'#save'", fixed = TRUE)
  expect_match(conditionMessage(match_error(ink_element("Button", classes = c("a", "b c")))), "'b c'", fixed = TRUE)
  expect_match(conditionMessage(match_error(ink_element("Button", parent = "x"))), "ink_element", fixed = TRUE)
  expect_match(conditionMessage(match_error(ink_element())), "required", fixed = TRUE)
})

test_that("a failed call does not change its inputs", {
  sheet <- compile_text("Button { color: red; }")
  element <- ink_element("Button")
  sheet_before <- sheet
  element_before <- element
  match_error(ink_match(sheet, list()))
  match_error(ink_match(list(), element))
  expect_identical(sheet, sheet_before)
  expect_identical(element, element_before)
})

test_that("random bad inputs only ever produce ink_match_error", {
  set.seed(3)
  junk <- list(NULL, NA, 1L, 2.5, "x", "", TRUE, character(), list(), list(1), factor("a"),
               mean, quote(x), new.env(), c(a = "b"), matrix(1:4, 2))
  sheet <- empty_sheet()
  for (bad in junk) {
    expect_match_error(ink_match(bad, ink_element("A")), "invalid_stylesheet")
    expect_match_error(ink_match(sheet, bad), "invalid_element")
    expect_match_error(ink_specificity(bad), "invalid_selector")
    err <- match_error(ink_element(bad))
    if (!is.null(err)) expect_equal(err$code, "invalid_element_type")
  }
})
