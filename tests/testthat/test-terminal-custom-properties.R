stripe_registry <- function() {
  ink_register_property(ink_registry(), "row-stripe", "color", "transparent", inherits = FALSE)
}

test_that("a custom property does not break the adapter and is preserved", {
  registry <- stripe_registry()
  style <- terminal_of("DataTable { row-stripe: #242424; }", type = "DataTable", registry = registry)
  expect_s3_class(style, "ink_terminal_style")
  expect_named(style$extra_properties, "row-stripe")
  stripe <- style$extra_properties[["row-stripe"]]
  expect_s3_class(stripe, "ink_color")
  expect_equal(c(stripe$r, stripe$g, stripe$b, stripe$a), c(36L, 36L, 36L, 255L))
})

test_that("custom values keep their typed S3 representation, unconverted", {
  registry <- ink_registry()
  registry <- ink_register_property(registry, "gap", "length", "1")
  registry <- ink_register_property(registry, "inset", "spacing", "1 2")
  registry <- ink_register_property(registry, "frame", "border", "round red")
  registry <- ink_register_property(registry, "emphasis", "text_style", "bold")
  registry <- ink_register_property(registry, "accent", "color", "bright-red")
  style <- terminal_of(
    "DataTable { gap: 4; inset: 3; frame: heavy blue; emphasis: italic; accent: #ff0000; }",
    type = "DataTable", registry = registry, capabilities = caps("ansi16")
  )
  extra <- style$extra_properties
  expect_named(extra, c("gap", "inset", "frame", "emphasis", "accent"))
  expect_s3_class(extra$gap, "ink_length")
  expect_s3_class(extra$inset, "ink_spacing")
  expect_s3_class(extra$frame, "ink_border")
  expect_s3_class(extra$emphasis, "ink_text_style")
  # Colors in custom properties are *not* quantized: the host decides what they mean.
  expect_s3_class(extra$accent, "ink_color")
  expect_equal(extra$accent$kind, "rgb")
  expect_equal(extra$gap$cells, 4L)
})

test_that("extra properties are the very same objects as in the computed style", {
  registry <- stripe_registry()
  sheet <- compile_text("DataTable { row-stripe: #242424; }", registry = registry)
  computed <- ink_compute(sheet, ink_element("DataTable"))
  style <- ink_terminal_style(computed)
  expect_identical(style$extra_properties[["row-stripe"]], computed$values[["row-stripe"]])
})

test_that("an undeclared custom property keeps its registered initial value", {
  registry <- stripe_registry()
  style <- terminal_of("DataTable { color: red; }", type = "DataTable", registry = registry)
  expect_identical(style$extra_properties[["row-stripe"]], registry$properties[["row-stripe"]]$initial)
})

test_that("built-in properties are never in extra_properties", {
  style <- terminal_of("Button { color: red; padding: 1; }", registry = stripe_registry())
  expect_false(any(names(BUILTIN_TERMINAL_PROPERTIES) %in% names(style$extra_properties)))
  expect_named(style$extra_properties, "row-stripe")
})

test_that("a stylesheet without custom properties has no extras", {
  expect_length(terminal_of("Button { color: red; }")$extra_properties, 0L)
})

test_that("custom properties do not affect the colors, attributes or ANSI output", {
  plain <- terminal_of("Button { color: #ff0000; text-style: bold; }")
  extra <- terminal_of("Button { color: #ff0000; text-style: bold; row-stripe: blue; }", registry = stripe_registry())
  expect_identical(plain$foreground, extra$foreground)
  expect_identical(plain$text_styles, extra$text_styles)
  expect_identical(ink_ansi_wrap("x", plain), ink_ansi_wrap("x", extra))
})

test_that("custom properties pass through inheritance and sessions", {
  registry <- ink_register_property(ink_registry(), "tone", "color", "white", inherits = TRUE)
  sheet <- compile_text("Screen { tone: red; }", registry = registry)
  session <- ink_session(sheet)
  ink_session_set(session, "root", type = "Screen")
  ink_session_set(session, "child", type = "Button", parent_key = "root")
  style <- ink_terminal_style(ink_session_compute(session, "child"))
  expect_equal(style$extra_properties$tone$name, "red")
})

test_that("printing mentions the custom properties", {
  output <- capture.output(print(terminal_of("DataTable { }", type = "DataTable", registry = stripe_registry())))
  expect_true(any(grepl("other       row-stripe", output, fixed = TRUE)))
})
