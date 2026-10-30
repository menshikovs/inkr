example_css <- "
Button {
    color: white;
}

.primary {
    color: blue;
}

#save {
    color: green;
}
"

example_button <- function() ink_element("Button", id = "save", classes = "primary")

test_that("ink_explain returns an ink_style_explanation", {
  explanation <- explain_css(example_css, example_button())
  expect_s3_class(explanation, "ink_style_explanation")
  expect_named(
    explanation,
    c("element", "property", "type", "value", "origin", "winner", "candidates", "resolution", "source")
  )
  expect_equal(explanation$property, "color")
  expect_equal(explanation$type, "color")
  expect_equal(explanation$source, "<text>")
})

test_that("the specified example explains the winning declaration", {
  explanation <- explain_css(example_css, example_button())
  expect_equal(format(explanation$value), "green")
  expect_s3_class(explanation$value, "ink_color")
  expect_equal(explanation$origin, "declaration")
  winner <- explanation$winner
  expect_s3_class(winner, "ink_explanation_candidate")
  expect_equal(winner$status, "winner")
  expect_equal(winner$rule_index, 3L)
  expect_equal(winner$raw, "green")
  expect_identical(winner$specificity, c(id = 1L, class = 0L, type = 0L))
  expect_equal(format_complex_selector(winner$selector), "#save")
})

test_that("all applicable declarations are candidates, in source order", {
  explanation <- explain_css(example_css, example_button())
  expect_length(explanation$candidates, 3L)
  expect_equal(vapply(explanation$candidates, function(c) c$rule_index, 0L), 1:3)
  expect_equal(candidate_values(explanation), c("white", "blue", "green"))
  expect_equal(reasons(explanation), c("lower_specificity", "lower_specificity", "winner"))
  expect_equal(
    vapply(explanation$candidates, function(c) c$status, ""),
    c("overridden", "overridden", "winner")
  )
})

test_that("the value is the same typed object ink_compute produces", {
  sheet <- compile_text(example_css)
  element <- example_button()
  expect_identical(
    ink_explain(sheet, element, "color")$value,
    ink_compute(sheet, element)$values$color
  )
})

test_that("candidates are only for the requested property", {
  css <- "Button { color: red; background: blue; }\n.primary { color: yellow; }\nButton.primary { color: green; }"
  element <- ink_element("Button", classes = "primary")
  explanation <- explain_css(css, element, "color")
  expect_length(explanation$candidates, 3L)
  expect_equal(candidate_values(explanation), c("red", "yellow", "green"))
  background <- explain_css(css, element, "background")
  expect_length(background$candidates, 1L)
  expect_equal(candidate_values(background), "blue")
})

test_that("a rule that does not match contributes no candidate", {
  css <- "Button { color: red; }\nInput { color: blue; }\n.missing { color: green; }"
  explanation <- explain_css(css, ink_element("Button"))
  expect_length(explanation$candidates, 1L)
  expect_equal(explanation$winner$rule_index, 1L)
})

test_that("a single declaration wins without any overridden candidates", {
  explanation <- explain_css("Button { color: red; }", ink_element("Button"))
  expect_length(explanation$candidates, 1L)
  expect_equal(reasons(explanation), "winner")
  expect_identical(explanation$winner, explanation$candidates[[1L]])
})

test_that("an empty stylesheet is explained from the registry", {
  explanation <- explain_css("", ink_element("Button"))
  expect_s3_class(explanation, "ink_style_explanation")
  expect_length(explanation$candidates, 0L)
  expect_null(explanation$winner)
  expect_equal(explanation$origin, "initial")
  expect_identical(explanation$value, ink_registry()$properties$color$initial)
})

test_that("a stylesheet with only variables is explained from the registry", {
  explanation <- explain_css("$a: red;\n$b: 1;", ink_element("Button"), "padding")
  expect_length(explanation$candidates, 0L)
  expect_equal(explanation$origin, "initial")
})

test_that("a property nobody declares is explained, not an error", {
  explanation <- explain_css("Button { padding: 1; }", ink_element("Button"), "color")
  expect_length(explanation$candidates, 0L)
  expect_null(explanation$winner)
  expect_equal(explanation$origin, "initial")
  expect_equal(explanation$resolution$source, "registry")
  expect_equal(format(explanation$value), "default")
})

test_that("every candidate has the documented fields", {
  explanation <- explain_css(example_css, example_button())
  for (candidate in explanation$candidates) {
    expect_named(
      candidate,
      c(
        "property", "rule_index", "declaration_index", "selector_index",
        "matching_selector_indices", "specificity", "selector", "value", "raw",
        "span", "status", "reason"
      )
    )
  }
  expect_null(explanation$winner$reason)
})

test_that("explanations are deterministic", {
  sheet <- compile_text(example_css)
  element <- example_button()
  expect_identical(
    ink_explain(sheet, element, "color"),
    ink_explain(sheet, element, "color")
  )
})

test_that("explaining does not modify its inputs", {
  sheet <- compile_text(example_css)
  element <- tree("Screen", list(type = "Button", id = "save", classes = "primary"))
  sheet_before <- sheet
  element_before <- element
  ink_explain(sheet, element, "color")
  ink_explain(sheet, element$parent, "background")
  expect_identical(sheet, sheet_before)
  expect_identical(element, element_before)
})

test_that("ink_compute is unchanged by the existence of ink_explain", {
  sheet <- compile_text(example_css)
  element <- example_button()
  before <- ink_compute(sheet, element)
  ink_explain(sheet, element, "color")
  expect_identical(ink_compute(sheet, element), before)
  expect_null(before$explanation)
  expect_named(before, c("values", "provenance", "element"))
})

test_that("explanations agree with computed styles for a richer stylesheet", {
  css <- "
    $primary: #7c3aed;
    Screen { color: white; background: #181825; }
    Container { padding: 1; }
    Button { color: inherit; background: blue; border: single white; }
    Button.primary { background: $primary; }
    Button:focus { text-style: bold; }
    #save { border: round green; }
  "
  sheet <- compile_text(css)
  screen <- ink_element("Screen", id = "main")
  container <- ink_element("Container", classes = "panel", parent = screen)
  button <- ink_element("Button", id = "save", classes = "primary", states = "focus", parent = container)
  for (element in list(button, container, screen, ink_element("Button"))) {
    expect_explanations_consistent(sheet, element)
  }
})
