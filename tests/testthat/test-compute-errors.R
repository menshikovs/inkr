sheet <- compile_text("Screen { color: red; }\nButton { padding: 1; }")
element <- tree("Screen", "Button")

test_that("a stylesheet that is not compiled is rejected", {
  for (bad in list(NULL, list(), "Button {}", 1, ink_parse("Button {}"), ink_registry(), ink_element("A"))) {
    expect_compute_error(ink_compute(bad, element), "invalid_compiled_stylesheet")
  }
  err <- compute_error(ink_compute(list(), element))
  expect_equal(err$argument, "sheet")
  expect_match(err$message, "ink_compiled_stylesheet", fixed = TRUE)
})

test_that("an element that is not an element is rejected", {
  for (bad in list(NULL, list(), "Button", 1, sheet, ink_parse("A {}")$statements[[1L]])) {
    expect_compute_error(ink_compute(sheet, bad), "invalid_element")
  }
  err <- compute_error(ink_compute(sheet, list()))
  expect_equal(err$argument, "element")
})

test_that("an element edited into an invalid shape is rejected", {
  bad <- element
  bad$type <- 5
  expect_compute_error(ink_compute(sheet, bad), "invalid_element")
  bad <- element
  bad$classes <- NA_character_
  expect_compute_error(ink_compute(sheet, bad), "invalid_element")
  bad <- element
  bad$parent <- "Screen"
  expect_compute_error(ink_compute(sheet, bad), "invalid_element")
})

test_that("an invalid ancestor is reported as an invalid parent chain", {
  leaf <- tree("Root", "Middle", "Leaf")
  bad <- leaf
  bad$parent$type <- 5
  err <- expect_compute_error(ink_compute(sheet, bad), "invalid_parent_chain")
  expect_match(err$message, "Ancestor 1", fixed = TRUE)

  bad <- leaf
  bad$parent$parent$states <- NA_character_
  err <- expect_compute_error(ink_compute(sheet, bad), "invalid_parent_chain")
  expect_match(err$message, "Ancestor 2", fixed = TRUE)

  bad <- leaf
  bad$parent$parent$parent <- "nope"
  expect_compute_error(ink_compute(sheet, bad), "invalid_parent_chain")
})

test_that("damaged compiled data is rejected with a structured error", {
  bad <- sheet
  bad$rules[[1L]] <- 42
  expect_compute_error(ink_compute(bad, element), "invalid_compiled_stylesheet")

  bad <- sheet
  bad$rules[[1L]]$selectors <- NULL
  expect_compute_error(ink_compute(bad, element), "invalid_compiled_stylesheet")

  bad <- sheet
  bad$rules[[1L]]$selectors$selectors[[1L]]$combinators <- "child"
  expect_compute_error(ink_compute(bad, element), "invalid_compiled_stylesheet")

  bad <- sheet
  bad$rules <- "none"
  expect_compute_error(ink_compute(bad, element), "invalid_compiled_stylesheet")
})

test_that("a damaged registry is rejected", {
  bad <- sheet
  bad$registry$properties$color$type <- "nonsense"
  err <- expect_compute_error(ink_compute(bad, element), "invalid_compiled_stylesheet")
  expect_match(err$message, "registry", fixed = TRUE)

  bad <- sheet
  bad$registry <- NULL
  expect_compute_error(ink_compute(bad, element), "invalid_compiled_stylesheet")

  bad <- sheet
  bad$registry <- unclass(sheet$registry)
  expect_compute_error(ink_compute(bad, element), "invalid_compiled_stylesheet")
})

test_that("a declaration for an unregistered property is an invalid cascade result", {
  bad <- sheet
  bad$rules[[1L]]$declarations[[1L]]$name <- "bogus"
  expect_compute_error(ink_compute(bad, element), "invalid_cascade_result")

  bad <- sheet
  bad$rules[[1L]]$declarations[[1L]] <- 42
  expect_compute_error(ink_compute(bad, element), "invalid_cascade_result")

  bad <- sheet
  bad$rules[[1L]]$declarations[[1L]]$value <- NULL
  expect_compute_error(ink_compute(bad, element), "invalid_cascade_result")
})

test_that("a value of the wrong type is an invalid computed value", {
  bad <- sheet
  bad$rules[[1L]]$declarations[[1L]]$value <- value("length", "3")
  err <- expect_compute_error(ink_compute(bad, element), "invalid_computed_value")
  expect_match(err$message, "ink_color", fixed = TRUE)

  bad <- sheet
  bad$rules[[1L]]$declarations[[1L]]$value <- "red"
  expect_compute_error(ink_compute(bad, element), "invalid_computed_value")
})

test_that("an unsupported keyword is an invalid computed value", {
  bad <- sheet
  bad$rules[[1L]]$declarations[[1L]]$value <- structure(list(keyword = "unset"), class = "ink_keyword")
  expect_compute_error(ink_compute(bad, element), "invalid_computed_value")
})

test_that("empty stylesheets, rule-less sheets and root elements are valid", {
  expect_s3_class(ink_compute(compile_text(""), ink_element("A")), "ink_computed_style")
  expect_s3_class(ink_compute(compile_text("$a: 1;"), ink_element("A")), "ink_computed_style")
  expect_s3_class(ink_compute(compile_text("A { color: red; }"), ink_element("B")), "ink_computed_style")
  expect_s3_class(ink_compute(sheet, ink_element("Screen")), "ink_computed_style")
})

test_that("compute errors are ink_compute_error and nothing else", {
  err <- compute_error(ink_compute(NULL, element))
  expect_s3_class(err, "ink_compute_error")
  expect_s3_class(err, "error")
  for (other in c("ink_match_error", "ink_compile_error", "ink_parse_error")) {
    expect_false(inherits(err, other))
  }
  expect_named(unclass(err), c("message", "call", "code", "detail", "argument"))
  expect_equal(err$message, err$detail)
  expect_error(ink_compute(1, 2), class = "ink_compute_error")
})

test_that("random bad input only produces ink_compute_error", {
  junk <- list(NULL, NA, 1L, 2.5, "x", "", TRUE, character(), list(), list(1), factor("a"),
               mean, quote(x), new.env(), c(a = "b"), matrix(1:4, 2))
  for (bad in junk) {
    expect_compute_error(ink_compute(bad, element), "invalid_compiled_stylesheet")
    expect_compute_error(ink_compute(sheet, bad), "invalid_element")
  }
})

test_that("failed computations do not change their inputs", {
  bad <- sheet
  bad$rules[[1L]]$declarations[[1L]]$name <- "bogus"
  sheet_before <- sheet
  element_before <- element
  bad_before <- bad
  compute_error(ink_compute(bad, element))
  compute_error(ink_compute(sheet, list()))
  expect_identical(sheet, sheet_before)
  expect_identical(element, element_before)
  expect_identical(bad, bad_before)
  # A good computation still works afterwards.
  expect_equal(shown(ink_compute(sheet, element)), "red")
})
