example_css <- "
  Button {
    color: white;
  }

  .primary {
    background: blue;
  }

  Button.primary:focus {
    border: round red;
  }

  #save {
    color: green;
  }
"

example_button <- function() {
  ink_element(
    "Button",
    id = "save", classes = "primary", states = "focus",
    parent = ink_element("Screen", id = "main")
  )
}

specificity_of <- function(result) {
  lapply(result$matches, function(m) unname(m$specificity))
}

test_that("the specified workflow matches all four rules with their specificity", {
  result <- match_css(example_css, example_button())
  expect_s3_class(result, "ink_match_result")
  expect_equal(rule_indices(result), 1:4)
  expect_equal(
    specificity_of(result),
    list(c(0L, 0L, 1L), c(0L, 1L, 0L), c(0L, 2L, 1L), c(1L, 0L, 0L))
  )
})

test_that("matches stay in source order, not specificity order", {
  # Written from most to least specific, so sorting by specificity would
  # reverse the result.
  css <- "#save { color: red; }\nButton.primary { color: blue; }\nButton { color: green; }"
  result <- match_css(css, example_button())
  expect_equal(rule_indices(result), 1:3)
  ids <- vapply(result$matches, function(m) m$specificity[["id"]], 0L)
  expect_equal(ids, c(1L, 0L, 0L))
})

test_that("an empty stylesheet matches nothing", {
  result <- match_css("", ink_element("Button"))
  expect_s3_class(result, "ink_match_result")
  expect_length(result$matches, 0L)
})

test_that("a stylesheet with only variables matches nothing", {
  result <- match_css("$a: red;\n$b: 1;", ink_element("Button"))
  expect_length(result$matches, 0L)
})

test_that("an empty rule can still match", {
  result <- match_css("Button {}", ink_element("Button"))
  expect_equal(rule_indices(result), 1L)
  expect_length(result$matches[[1L]]$rule$declarations, 0L)
})

test_that("a valid selector that does not match is a normal result", {
  result <- match_css("Input { color: red; }\n.other { color: blue; }", ink_element("Button"))
  expect_s3_class(result, "ink_match_result")
  expect_length(result$matches, 0L)
  expect_no_error(capture.output(print(result)))
})

test_that("only matching rules are returned", {
  css <- "Button { color: red; }\nInput { color: blue; }\n.primary { color: green; }"
  result <- match_css(css, ink_element("Button", classes = "primary"))
  expect_equal(rule_indices(result), c(1L, 3L))
  result <- match_css(css, ink_element("Input"))
  expect_equal(rule_indices(result), 2L)
})

test_that("both rules appear when they set the same property", {
  css <- "Button { color: red; }\n.primary { color: blue; }"
  result <- match_css(css, ink_element("Button", classes = "primary"))
  expect_equal(rule_indices(result), 1:2)
  colors <- vapply(
    result$matches,
    function(m) format(m$rule$declarations[[1L]]$value),
    ""
  )
  expect_equal(colors, c("red", "blue"))
})

test_that("compiled rules, declarations and spans are returned unchanged", {
  sheet <- compile_text("A {\n  color: red;\n  color: blue;\n  padding: 1 2;\n}\nB { width: 3; }")
  result <- ink_match(sheet, ink_element("A"))
  matched <- result$matches[[1L]]
  expect_identical(matched$rule, sheet$rules[[1L]])
  expect_identical(matched$rule$declarations, sheet$rules[[1L]]$declarations)
  expect_length(matched$rule$declarations, 3L)
  expect_identical(matched$span, sheet$rules[[1L]]$span)
  expect_identical(matched$rule$selectors, sheet$rules[[1L]]$selectors)
})

test_that("matched rules have the documented fields", {
  result <- match_css(example_css, example_button())
  expect_named(result, c("element", "matches"))
  matched <- result$matches[[3L]]
  expect_s3_class(matched, "ink_matched_rule")
  expect_named(
    matched,
    c("rule_index", "rule", "selector_indices", "winning_selector_index", "specificity", "span")
  )
  expect_identical(matched$rule_index, 3L)
  expect_identical(matched$selector_indices, 1L)
  expect_identical(matched$winning_selector_index, 1L)
  expect_identical(matched$specificity, c(id = 0L, class = 2L, type = 1L))
  expect_identical(result$element, example_button())
})

test_that("a selector list that does not match is not a match", {
  result <- match_css("Button, #save { color: red; }", ink_element("Input"))
  expect_length(result$matches, 0L)
})

test_that("one selector of a list matching is enough, with that selector's specificity", {
  css <- "Button, #save { color: red; }"
  result <- match_css(css, ink_element("Button"))
  matched <- result$matches[[1L]]
  expect_equal(matched$selector_indices, 1L)
  expect_equal(matched$winning_selector_index, 1L)
  expect_identical(matched$specificity, c(id = 0L, class = 0L, type = 1L))

  result <- match_css(css, ink_element("Input", id = "save"))
  matched <- result$matches[[1L]]
  expect_equal(matched$selector_indices, 2L)
  expect_equal(matched$winning_selector_index, 2L)
  expect_identical(matched$specificity, c(id = 1L, class = 0L, type = 0L))
})

test_that("with several matching selectors the strongest one wins", {
  css <- "Button, #save { color: red; }"
  result <- match_css(css, ink_element("Button", id = "save"))
  expect_length(result$matches, 1L)
  matched <- result$matches[[1L]]
  expect_equal(matched$selector_indices, c(1L, 2L))
  expect_equal(matched$winning_selector_index, 2L)
  expect_identical(matched$specificity, c(id = 1L, class = 0L, type = 0L))
})

test_that("a stronger selector that does not match never contributes", {
  css <- ".a, .a.b, .a.b.c { color: red; }"
  result <- match_css(css, ink_element("X", classes = "a"))
  expect_identical(result$matches[[1L]]$specificity, c(id = 0L, class = 1L, type = 0L))
  expect_equal(result$matches[[1L]]$selector_indices, 1L)

  result <- match_css(css, ink_element("X", classes = c("a", "b")))
  expect_identical(result$matches[[1L]]$specificity, c(id = 0L, class = 2L, type = 0L))
  expect_equal(result$matches[[1L]]$selector_indices, 1:2)
  expect_equal(result$matches[[1L]]$winning_selector_index, 2L)
})

test_that("equal-specificity selectors pick the earliest as the winner", {
  result <- match_css(".a, .b { color: red; }", ink_element("X", classes = c("a", "b")))
  matched <- result$matches[[1L]]
  expect_equal(matched$selector_indices, 1:2)
  expect_equal(matched$winning_selector_index, 1L)
  result <- match_css(".a, .b { color: red; }", ink_element("X", classes = c("b", "a")))
  expect_equal(result$matches[[1L]]$winning_selector_index, 1L)
  result <- match_css(".a, .b { color: red; }", ink_element("X", classes = "b"))
  expect_equal(result$matches[[1L]]$winning_selector_index, 2L)
})

test_that("a rule appears once even if every selector in its list matches", {
  result <- match_css("A, A, A.x, * { color: red; }", ink_element("A", classes = "x"))
  expect_length(result$matches, 1L)
  expect_equal(result$matches[[1L]]$selector_indices, 1:4)
  expect_equal(result$matches[[1L]]$winning_selector_index, 3L)
  expect_equal(anyDuplicated(rule_indices(result)), 0L)
})

test_that("complex selectors are matched through the parent chain", {
  css <- "Screen Button { color: red; }\nContainer > Button { color: blue; }\nPanel > Button { color: green; }"
  button <- tree("Screen", "Container", "Button")
  result <- match_css(css, button)
  expect_equal(rule_indices(result), 1:2)
  expect_identical(result$matches[[2L]]$specificity, c(id = 0L, class = 0L, type = 2L))
})

test_that("the regression case matches through a stylesheet", {
  result <- match_css("A > B C { color: red; }", tree("A", "B", "B", "C"))
  expect_equal(rule_indices(result), 1L)
  expect_length(match_css("A > B C { color: red; }", tree("B", "B", "C"))$matches, 0L)
})

test_that("matching does not modify the stylesheet or the element", {
  sheet <- compile_text(example_css)
  element <- example_button()
  sheet_before <- sheet
  element_before <- element
  ink_match(sheet, element)
  ink_match(sheet, element)
  expect_identical(sheet, sheet_before)
  expect_identical(element, element_before)
})

test_that("repeated matching produces equivalent results", {
  sheet <- compile_text(example_css)
  element <- example_button()
  expect_identical(ink_match(sheet, element), ink_match(sheet, element))
})

test_that("matching depends on attributes and ancestry, not on identity", {
  sheet <- compile_text("Screen Button { color: red; }")
  with_parent <- ink_element("Button", parent = ink_element("Screen"))
  without_parent <- ink_element("Button")
  expect_length(ink_match(sheet, with_parent)$matches, 1L)
  expect_length(ink_match(sheet, without_parent)$matches, 0L)
  twin <- ink_element("Button", parent = ink_element("Screen"))
  expect_identical(ink_match(sheet, with_parent), ink_match(sheet, twin))
})

test_that("a large stylesheet is matched in source order", {
  css <- paste(sprintf(".c%d { width: %d; }", 1:300, 1:300), collapse = "\n")
  element <- ink_element("X", classes = c("c300", "c1", "c150"))
  result <- match_css(css, element)
  expect_equal(rule_indices(result), c(1L, 150L, 300L))
})

test_that("a deep tree can be matched against a stylesheet", {
  leaf <- deep_chain(1500L, "Leaf", root = ink_element("Top"))
  result <- match_css("Top Leaf { color: red; }\nTop > Leaf { color: blue; }", leaf)
  expect_equal(rule_indices(result), 1L)
})

test_that("print shows the matching rules", {
  result <- match_css(example_css, example_button())
  output <- capture.output(returned <- print(result))
  expect_identical(returned, result)
  expect_equal(output[[1L]], "<ink_match_result> 4 matching rules for Button#save.primary:focus")
  expect_true(any(grepl("[1] Button", output, fixed = TRUE)))
  expect_true(any(grepl("[3] Button.primary:focus", output, fixed = TRUE)))
  expect_true(any(grepl("specificity: (0, 2, 1)", output, fixed = TRUE)))
  expect_true(any(grepl("specificity: (1, 0, 0)", output, fixed = TRUE)))
  expect_true(any(grepl("rule: 4", output, fixed = TRUE)))
})

test_that("print shows the winning selector and the other matches of a list", {
  result <- match_css("Button, #save { color: red; }", ink_element("Button", id = "save"))
  output <- capture.output(print(result))
  expect_true(any(grepl("[1] #save", output, fixed = TRUE)))
  expect_true(any(grepl("selectors matched: 1, 2", output, fixed = TRUE)))
})

test_that("print handles singular, empty and long results", {
  one <- match_css("Button {}", ink_element("Button"))
  expect_match(capture.output(print(one))[[1L]], "1 matching rule for", fixed = TRUE)
  none <- match_css("Input {}", ink_element("Button"))
  expect_match(capture.output(print(none))[[1L]], "0 matching rules for", fixed = TRUE)
  many <- match_css(paste(rep("* {}", 5L), collapse = "\n"), ink_element("Button"))
  output <- capture.output(print(many, n = 2L))
  expect_true(any(grepl("and 3 more", output, fixed = TRUE)))
})
