button <- ink_element(
  "Button",
  id = "save", classes = c("primary", "rounded"), states = c("focus", "loading")
)

test_that("a type selector matches the same type only", {
  expect_true(compound_matches_element("Button", button))
  expect_false(compound_matches_element("Input", button))
})

test_that("type names are case-sensitive", {
  expect_false(compound_matches_element("button", button))
  expect_false(compound_matches_element("BUTTON", button))
  expect_true(compound_matches_element("button", ink_element("button")))
})

test_that("the universal selector matches every element", {
  expect_true(compound_matches_element("*", button))
  expect_true(compound_matches_element("*", ink_element("Anything")))
  expect_true(compound_matches_element("*", tree("A", "B")))
})

test_that("ID selectors compare the id exactly", {
  expect_true(compound_matches_element("#save", button))
  expect_false(compound_matches_element("#cancel", button))
  expect_false(compound_matches_element("#Save", button))
  expect_false(compound_matches_element("#sav", button))
  expect_false(compound_matches_element("#save", ink_element("Button")))
})

test_that("class selectors check membership", {
  expect_true(compound_matches_element(".primary", button))
  expect_true(compound_matches_element(".rounded", button))
  expect_false(compound_matches_element(".secondary", button))
  expect_false(compound_matches_element(".Primary", button))
  expect_false(compound_matches_element(".prim", button))
  expect_false(compound_matches_element(".primary", ink_element("Button")))
})

test_that("state selectors check membership, for any valid state name", {
  expect_true(compound_matches_element(":focus", button))
  expect_true(compound_matches_element(":loading", button))
  expect_false(compound_matches_element(":disabled", button))
  expect_false(compound_matches_element(":selected", button))
  expect_false(compound_matches_element(":Focus", button))
  expect_true(compound_matches_element("DataTable:loading", ink_element("DataTable", states = "loading")))
  expect_false(compound_matches_element("DataTable:loading", ink_element("DataTable")))
})

test_that("classes and states are separate namespaces", {
  element <- ink_element("Button", classes = "focus", states = "primary")
  expect_false(compound_matches_element(".primary", element))
  expect_false(compound_matches_element(":focus", element))
  expect_true(compound_matches_element(".focus", element))
  expect_true(compound_matches_element(":primary", element))
})

test_that("every class and every state of a compound must be present", {
  expect_true(compound_matches_element(".primary.rounded", button))
  expect_true(compound_matches_element(".rounded.primary", button))
  expect_false(compound_matches_element(".primary.rounded.big", button))
  expect_true(compound_matches_element(":focus:loading", button))
  expect_false(compound_matches_element(":focus:loading:disabled", button))
})

test_that("compound selectors need all conditions on the same element", {
  expect_true(compound_matches_element("Button.primary:focus", button))
  expect_true(compound_matches_element("Button#save.primary.rounded:focus:loading", button))
  expect_true(compound_matches_element("*.primary:focus", button))
  expect_false(compound_matches_element("Input.primary:focus", button))
  expect_false(compound_matches_element("Button.secondary:focus", button))
  expect_false(compound_matches_element("Button.primary:disabled", button))
  expect_false(compound_matches_element("Button#other.primary:focus", button))
  expect_false(compound_matches_element("Button.primary.rounded:focus:enabled", button))
})

test_that("a compound is a conjunction, not a set of alternatives", {
  only_class <- ink_element("Input", classes = "primary")
  expect_false(compound_matches_element("Button.primary", only_class))
  only_type <- ink_element("Button")
  expect_false(compound_matches_element("Button.primary", only_type))
})

test_that("compound matching ignores the parent chain", {
  parented <- ink_element("Button", classes = "primary", parent = ink_element("Screen", id = "main"))
  expect_true(compound_matches_element("Button.primary", parented))
  expect_false(compound_matches_element("#main", parented))
})

test_that("matching does not modify the element", {
  before <- button
  compound_matches_element("Button.primary:focus", button)
  expect_identical(button, before)
})

test_that("the compound matcher rejects invalid input instead of returning FALSE", {
  expect_match_error(ink_match_compound(list(), button), "invalid_selector")
  expect_match_error(ink_match_compound(NULL, button), "invalid_selector")
  expect_match_error(ink_match_compound(compound_of("Button"), list()), "invalid_element")
  broken <- compound_of("Button")
  broken$classes <- list("a")
  expect_match_error(ink_match_compound(broken, button), "invalid_selector")
})
