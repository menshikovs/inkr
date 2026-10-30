test_that("every built-in property can be explained", {
  css <- "Screen { color: red; background: blue; text-style: bold; padding: 1; margin: 2; border: single red; width: 5; height: 6; }"
  sheet <- compile_text(css)
  element <- tree("Screen", "Button")
  for (property in names(ink_registry()$properties)) {
    explanation <- ink_explain(sheet, element, property)
    expect_s3_class(explanation, "ink_style_explanation")
    expect_equal(explanation$property, property)
    expect_equal(explanation$type, ink_registry()$properties[[property]]$type)
  }
  expect_explanations_consistent(sheet, element)
  expect_explanations_consistent(sheet, element$parent)
})

test_that("the specified custom property example is explained", {
  registry <- ink_register_property(ink_registry(), "row-stripe", "color", "transparent", inherits = FALSE)
  sheet <- ink_compile(ink_parse("DataTable { row-stripe: #242424; }"), registry = registry)
  explanation <- ink_explain(sheet, ink_element("DataTable"), "row-stripe")
  expect_equal(explanation$origin, "declaration")
  expect_s3_class(explanation$value, "ink_color")
  expect_equal(format(explanation$value), "#242424")
  expect_length(explanation$candidates, 1L)
  expect_identical(explanation$value, ink_compute(sheet, ink_element("DataTable"))$values[["row-stripe"]])
})

test_that("a custom inheriting property is traced through ancestors", {
  registry <- registry_with("accent", "color", "bright-cyan", inherits = TRUE)
  explanation <- explain_css(
    "Screen { accent: magenta; }", tree("Screen", "Panel", "Button"), "accent", registry
  )
  expect_equal(explanation$origin, "inherited")
  expect_equal(format(explanation$value), "magenta")
  expect_equal(format(explanation$resolution$effective_source$element), "Screen")
})

test_that("a custom non-inheriting property is explained as initial", {
  registry <- registry_with("row-stripe", "color", "transparent")
  explanation <- explain_css(
    "Screen { row-stripe: red; }", tree("Screen", "Button"), "row-stripe", registry
  )
  expect_equal(explanation$origin, "initial")
  expect_equal(format(explanation$value), "transparent")
  expect_length(explanation$candidates, 0L)
})

test_that("a custom initial value is reported and kept typed", {
  registry <- registry_with("gap", "length", "3")
  explanation <- explain_css("Button { color: red; }", ink_element("Button"), "gap", registry)
  expect_equal(explanation$origin, "initial")
  expect_s3_class(explanation$value, "ink_length")
  expect_equal(explanation$value$cells, 3L)
  expect_identical(explanation$value, registry$properties$gap$initial)
  expect_identical(explanation$resolution$initial_value, registry$properties$gap$initial)
})

test_that("explicit inherit and initial work on custom properties", {
  registry <- registry_with("gap", "length", "1")
  registry <- ink_register_property(registry, "tone", "color", "white", inherits = TRUE)
  css <- "Screen { gap: 9; tone: red; }\nButton { gap: inherit; tone: initial; }"
  element <- tree("Screen", "Button")
  gap <- explain_css(css, element, "gap", registry)
  tone <- explain_css(css, element, "tone", registry)
  expect_equal(gap$origin, "explicit_inherit")
  expect_equal(format(gap$value), "9")
  expect_equal(tone$origin, "explicit_initial")
  expect_equal(format(tone$value), "white")
  sheet <- compile_text(css, registry = registry)
  expect_explanations_consistent(sheet, element)
})

test_that("custom properties of every type keep their typed values", {
  registry <- ink_registry()
  registry <- ink_register_property(registry, "p-spacing", "spacing", "1 2", TRUE)
  registry <- ink_register_property(registry, "p-border", "border", "round red", TRUE)
  registry <- ink_register_property(registry, "p-style", "text_style", "dim", TRUE)
  sheet <- compile_text(
    "Screen { p-spacing: 3; p-border: heavy blue; p-style: strike; }",
    registry = registry
  )
  element <- tree("Screen", "Button")
  expect_s3_class(ink_explain(sheet, element, "p-spacing")$value, "ink_spacing")
  expect_s3_class(ink_explain(sheet, element, "p-border")$value, "ink_border")
  expect_s3_class(ink_explain(sheet, element, "p-style")$value, "ink_text_style")
  expect_explanations_consistent(sheet, element)
})

test_that("the stylesheet's registry decides which properties exist", {
  registry <- registry_with("gap", "length", "0")
  with_gap <- compile_text("Button { gap: 1; }", registry = registry)
  plain <- compile_text("Button { color: red; }")
  expect_s3_class(ink_explain(with_gap, ink_element("Button"), "gap"), "ink_style_explanation")
  expect_explain_error(ink_explain(plain, ink_element("Button"), "gap"), "unknown_property")
})

test_that("inheritance flags are read from the registry", {
  registry <- ink_registry()
  registry$properties$padding$inherits <- TRUE
  explanation <- explain_css("Screen { padding: 2; }", tree("Screen", "Button"), "padding", registry)
  expect_equal(explanation$origin, "inherited")
  expect_equal(format(explanation$value), "top=2 right=2 bottom=2 left=2")
})

test_that("explaining leaves the registry unchanged", {
  registry <- registry_with("gap", "length", "0", inherits = TRUE)
  before <- registry
  sheet <- compile_text("Screen { gap: 2; }", registry = registry)
  ink_explain(sheet, tree("Screen", "Button"), "gap")
  expect_identical(registry, before)
  expect_identical(sheet$registry, before)
})
