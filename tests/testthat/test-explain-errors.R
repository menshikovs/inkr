sheet <- compile_text("Screen { color: red; }\nButton { padding: 1; }")
element <- tree("Screen", "Button")

test_that("an unknown property is a structured error naming the known ones", {
  err <- expect_explain_error(ink_explain(sheet, element, "not-a-property"), "unknown_property")
  expect_equal(err$argument, "property")
  expect_match(err$message, "not-a-property", fixed = TRUE)
  expect_match(err$message, "background", fixed = TRUE)
  expect_explain_error(ink_explain(sheet, element, "Color"), "unknown_property")
  expect_explain_error(ink_explain(sheet, element, " color"), "unknown_property")
})

test_that("an invalid property argument is rejected", {
  for (bad in list("", NA_character_, c("color", "background"), 123, NULL, TRUE, character(), list("color"), factor("color"))) {
    err <- expect_explain_error(ink_explain(sheet, element, bad), "invalid_property")
    expect_equal(err$argument, "property")
  }
  expect_explain_error(ink_explain(sheet, element), "invalid_property")
  expect_explain_error(ink_explain(sheet), "invalid_element")
  expect_explain_error(ink_explain(), "invalid_stylesheet")
})

test_that("a stylesheet that is not compiled is rejected", {
  for (bad in list(NULL, list(), "Button {}", 1, ink_parse("Button {}"), ink_registry(), element)) {
    err <- expect_explain_error(ink_explain(bad, element, "color"), "invalid_stylesheet")
    expect_equal(err$argument, "sheet")
  }
})

test_that("an element that is not an element is rejected", {
  for (bad in list(NULL, list(), "Button", 1, sheet, ink_parse("A {}")$statements[[1L]])) {
    err <- expect_explain_error(ink_explain(sheet, bad, "color"), "invalid_element")
    expect_equal(err$argument, "element")
  }
})

test_that("an element or ancestor edited into an invalid shape is rejected", {
  bad <- element
  bad$type <- 5
  expect_explain_error(ink_explain(sheet, bad, "color"), "invalid_element")

  bad <- element
  bad$parent$classes <- NA_character_
  err <- expect_explain_error(ink_explain(sheet, bad, "color"), "invalid_element")
  expect_match(err$message, "Ancestor 1", fixed = TRUE)

  bad <- element
  bad$parent <- "nope"
  expect_explain_error(ink_explain(sheet, bad, "color"), "invalid_element")
})

test_that("damaged compiled data is an explain error", {
  bad <- sheet
  bad$rules[[1L]] <- 42
  expect_explain_error(ink_explain(bad, element, "color"), "invalid_stylesheet")

  bad <- sheet
  bad$rules[[1L]]$selectors$selectors[[1L]]$combinators <- "child"
  expect_explain_error(ink_explain(bad, element, "color"), "invalid_stylesheet")

  bad <- sheet
  bad$registry$properties$color$type <- "nonsense"
  expect_explain_error(ink_explain(bad, element, "color"), "invalid_stylesheet")

  bad <- sheet
  bad$registry <- NULL
  expect_explain_error(ink_explain(bad, element, "color"), "invalid_stylesheet")
})

test_that("inconsistent cascade data is an invalid explanation state", {
  bad <- sheet
  bad$rules[[1L]]$declarations[[1L]]$name <- "bogus"
  expect_explain_error(ink_explain(bad, element, "color"), "invalid_explanation_state")

  bad <- sheet
  bad$rules[[1L]]$declarations[[1L]]$value <- value("length", "3")
  expect_explain_error(ink_explain(bad, element, "color"), "invalid_explanation_state")
})

test_that("the consistency checks catch contradictory observations", {
  provenance <- list(origin = "declaration", rule_index = 1L, declaration_index = 1L, selector_index = 1L,
                     specificity = c(id = 0L, class = 0L, type = 1L))
  resolved <- list(provenance = list(color = provenance))
  resolution <- list(origin = "declaration")
  winner <- list(rule_index = 2L, declaration_index = 1L, selector_index = 1L, specificity = provenance$specificity)
  expect_explain_error(check_explanation_consistency(resolved, "color", resolution, winner), "invalid_explanation_state")
  expect_explain_error(check_explanation_consistency(resolved, "color", resolution, NULL), "invalid_explanation_state")
  winner$rule_index <- 1L
  expect_no_error(check_explanation_consistency(resolved, "color", resolution, winner))
})

test_that("a candidate that outranks the winner is reported, not explained away", {
  winner <- list(specificity = c(id = 0L, class = 0L, type = 1L), rule_index = 1L, declaration_index = 1L)
  stronger <- list(specificity = c(id = 1L, class = 0L, type = 0L), rule_index = 0L, declaration_index = 1L,
                   property = "color")
  expect_explain_error(override_reason(stronger, winner), "invalid_explanation_state")
  twin <- winner
  twin$property <- "color"
  expect_explain_error(override_reason(twin, winner), "invalid_explanation_state")
})

test_that("a property with no declarations, no parent and no rules is valid", {
  expect_s3_class(ink_explain(compile_text(""), ink_element("A"), "color"), "ink_style_explanation")
  expect_s3_class(ink_explain(compile_text("$a: 1;"), ink_element("A"), "width"), "ink_style_explanation")
  expect_s3_class(ink_explain(sheet, ink_element("Unmatched"), "padding"), "ink_style_explanation")
})

test_that("explain errors are ink_explain_error and nothing else", {
  err <- explain_error(ink_explain(sheet, element, "nope"))
  expect_s3_class(err, "ink_explain_error")
  expect_s3_class(err, "error")
  for (other in c("ink_match_error", "ink_compute_error", "ink_compile_error", "ink_parse_error")) {
    expect_false(inherits(err, other))
  }
  expect_named(unclass(err), c("message", "call", "code", "detail", "argument"))
  expect_equal(err$message, err$detail)
})

test_that("random bad input only produces ink_explain_error", {
  junk <- list(NULL, NA, 1L, 2.5, "x", "", TRUE, character(), list(), list(1), factor("a"),
               mean, quote(x), new.env(), c(a = "b"), matrix(1:4, 2))
  for (bad in junk) {
    expect_explain_error(ink_explain(bad, element, "color"), "invalid_stylesheet")
    expect_explain_error(ink_explain(sheet, bad, "color"), "invalid_element")
    expect_s3_class(explain_error(ink_explain(sheet, element, bad)), "ink_explain_error")
  }
})

test_that("failed explanations do not change their inputs", {
  bad <- sheet
  bad$rules[[1L]]$declarations[[1L]]$name <- "bogus"
  sheet_before <- sheet
  element_before <- element
  bad_before <- bad
  explain_error(ink_explain(bad, element, "color"))
  explain_error(ink_explain(sheet, element, "nope"))
  expect_identical(sheet, sheet_before)
  expect_identical(element, element_before)
  expect_identical(bad, bad_before)
  expect_equal(format(ink_explain(sheet, element, "color")$value), "red")
})
