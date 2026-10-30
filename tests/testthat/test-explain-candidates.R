test_that("three competing declarations give three candidates", {
  css <- "Button { color: red; background: blue; }\n.primary { color: yellow; }\nButton.primary { color: green; }"
  explanation <- explain_css(css, ink_element("Button", classes = "primary"))
  expect_length(explanation$candidates, 3L)
  expect_equal(candidate_values(explanation), c("red", "yellow", "green"))
  expect_false("background" %in% vapply(explanation$candidates, function(c) c$property, ""))
})

test_that("candidates carry rule and declaration indices", {
  css <- "Button { width: 1; color: red; }\nButton { color: blue; color: green; }"
  explanation <- explain_css(css, ink_element("Button"))
  expect_equal(vapply(explanation$candidates, function(c) c$rule_index, 0L), c(1L, 2L, 2L))
  expect_equal(vapply(explanation$candidates, function(c) c$declaration_index, 0L), c(2L, 1L, 2L))
  expect_type(explanation$candidates[[1L]]$rule_index, "integer")
})

test_that("candidates keep source order and are not sorted by priority", {
  css <- "#save { color: red; }\nButton { color: blue; }\n.primary { color: green; }"
  explanation <- explain_css(css, ink_element("Button", id = "save", classes = "primary"))
  expect_equal(candidate_values(explanation), c("red", "blue", "green"))
  specificity_ids <- vapply(explanation$candidates, function(c) c$specificity[["id"]], 0L)
  expect_equal(specificity_ids, c(1L, 0L, 0L))
})

test_that("candidates preserve typed values, raw text and spans", {
  sheet <- compile_text("$c: #7c3aed;\nButton {\n  color: $c;\n}")
  explanation <- ink_explain(sheet, ink_element("Button"), "color")
  candidate <- explanation$candidates[[1L]]
  expect_s3_class(candidate$value, "ink_color")
  expect_identical(candidate$value, sheet$rules[[1L]]$declarations[[1L]]$value)
  expect_equal(candidate$raw, "$c")
  expect_identical(candidate$span, sheet$rules[[1L]]$declarations[[1L]]$span)
  expect_equal(candidate$span$start$line, 3L)
  expect_equal(format(explanation$value), "#7c3aed")
})

test_that("the selector node and indices point at the matching selector", {
  css <- "Button, #save, .primary { color: red; }"
  explanation <- explain_css(css, ink_element("Button", id = "save", classes = "primary"))
  candidate <- explanation$candidates[[1L]]
  expect_equal(candidate$matching_selector_indices, 1:3)
  expect_equal(candidate$selector_index, 2L)
  expect_s3_class(candidate$selector, "ink_complex_selector")
  expect_equal(format_complex_selector(candidate$selector), "#save")
})

test_that("selector lists contribute each declaration once", {
  css <- "Button, .primary, Button.primary { color: red; padding: 1; }"
  explanation <- explain_css(css, ink_element("Button", classes = "primary"))
  expect_length(explanation$candidates, 1L)
  expect_equal(explanation$candidates[[1L]]$matching_selector_indices, 1:3)
  expect_equal(explanation$candidates[[1L]]$selector_index, 3L)
})

test_that("candidates match what the matcher reports", {
  css <- "Button { color: red; }\n.x, Button.x { color: blue; }\nInput { color: green; }"
  sheet <- compile_text(css)
  element <- ink_element("Button", classes = "x")
  explanation <- ink_explain(sheet, element, "color")
  matched <- ink_match(sheet, element)$matches
  expect_equal(
    vapply(explanation$candidates, function(c) c$rule_index, 0L),
    vapply(matched, function(m) m$rule_index, 0L)
  )
  expect_identical(
    lapply(explanation$candidates, function(c) c$specificity),
    lapply(matched, function(m) m$specificity)
  )
  expect_identical(
    lapply(explanation$candidates, function(c) c$selector_index),
    lapply(matched, function(m) m$winning_selector_index)
  )
})

test_that("the winner is the candidate marked winner", {
  explanation <- explain_css("A { color: red; }\nA { color: blue; }\nA { color: green; }", ink_element("A"))
  winners <- Filter(function(c) c$status == "winner", explanation$candidates)
  expect_length(winners, 1L)
  expect_identical(explanation$winner, winners[[1L]])
  expect_equal(explanation$winner$rule_index, 3L)
})

test_that("many candidates are all kept", {
  css <- paste(sprintf("A { color: red; width: %d; }", 1:150), collapse = "\n")
  explanation <- explain_css(css, ink_element("A"))
  expect_length(explanation$candidates, 150L)
  expect_equal(explanation$winner$rule_index, 150L)
  expect_equal(sum(reasons(explanation) == "earlier_rule"), 149L)
})

test_that("keyword declarations are candidates with their keyword value", {
  css <- "Screen { color: red; }\nButton { color: inherit; }\nButton.x { color: initial; }"
  explanation <- explain_css(css, tree("Screen", list(type = "Button", classes = "x")))
  expect_equal(candidate_values(explanation), c("inherit", "initial"))
  expect_s3_class(explanation$candidates[[1L]]$value, "ink_keyword")
  expect_equal(explanation$candidates[[1L]]$raw, "inherit")
  expect_false(inherits(explanation$value, "ink_keyword"))
})
