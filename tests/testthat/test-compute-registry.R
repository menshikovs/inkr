test_that("the specified custom property example works", {
  registry <- ink_registry()
  registry <- ink_register_property(
    registry,
    name = "row-stripe", type = "color", initial = "transparent", inherits = FALSE
  )
  sheet <- ink_compile(ink_parse("DataTable { row-stripe: #242424; }"), registry = registry)
  style <- ink_compute(sheet, ink_element(type = "DataTable"))
  value <- style$values[["row-stripe"]]
  expect_s3_class(value, "ink_color")
  expect_equal(c(value$r, value$g, value$b, value$a), c(36L, 36L, 36L, 255L))
})

test_that("a custom inheriting property is inherited", {
  registry <- registry_with("accent", "color", "bright-cyan", inherits = TRUE)
  css <- "Screen { accent: magenta; }"
  style <- compute_css(css, tree("Screen", "Container", "Button"), registry)
  expect_equal(shown(style, "accent"), "magenta")
  expect_equal(style$provenance$accent$origin, "inherited")
})

test_that("a custom non-inheriting property is not inherited", {
  registry <- registry_with("row-stripe", "color", "transparent", inherits = FALSE)
  css <- "Screen { row-stripe: #242424; }"
  style <- compute_css(css, tree("Screen", "Button"), registry)
  expect_equal(shown(style, "row-stripe"), "transparent")
  expect_equal(style$provenance[["row-stripe"]]$origin, "initial")
})

test_that("a custom property's initial value is used when nothing is declared", {
  registry <- registry_with("gap", "length", "3")
  style <- compute_css("Button { color: red; }", ink_element("Button"), registry)
  expect_equal(shown(style, "gap"), "3")
  expect_s3_class(style$values$gap, "ink_length")
  expect_identical(style$values$gap, registry$properties$gap$initial)
})

test_that("explicit inherit works on custom properties, inheriting or not", {
  registry <- registry_with("gap", "length", "1")
  registry <- ink_register_property(registry, "tone", "color", "white", inherits = TRUE)
  css <- "Screen { gap: 9; tone: red; }\nButton { gap: inherit; tone: inherit; }"
  style <- compute_css(css, tree("Screen", "Button"), registry)
  expect_equal(shown(style, "gap"), "9")
  expect_equal(shown(style, "tone"), "red")
  root <- compute_css(css, ink_element("Button"), registry)
  expect_equal(shown(root, "gap"), "1")
  expect_equal(shown(root, "tone"), "white")
})

test_that("explicit initial works on custom properties, inheriting or not", {
  registry <- registry_with("gap", "length", "1")
  registry <- ink_register_property(registry, "tone", "color", "white", inherits = TRUE)
  css <- "Screen { gap: 9; tone: red; }\nButton { gap: initial; tone: initial; }"
  style <- compute_css(css, tree("Screen", "Button"), registry)
  expect_equal(shown(style, "gap"), "1")
  expect_equal(shown(style, "tone"), "white")
})

test_that("custom properties of every type participate", {
  registry <- ink_registry()
  registry <- ink_register_property(registry, "p-color", "color", "red", TRUE)
  registry <- ink_register_property(registry, "p-spacing", "spacing", "1 2", TRUE)
  registry <- ink_register_property(registry, "p-border", "border", "round red", TRUE)
  registry <- ink_register_property(registry, "p-length", "length", "auto", TRUE)
  registry <- ink_register_property(registry, "p-style", "text_style", "dim", TRUE)
  css <- "Screen { p-color: blue; p-spacing: 3; p-border: heavy green; p-length: 7; p-style: strike italic; }"
  style <- compute_css(css, tree("Screen", "Button"), registry)
  expect_equal(shown(style, "p-color"), "blue")
  expect_equal(shown(style, "p-spacing"), "top=3 right=3 bottom=3 left=3")
  expect_equal(shown(style, "p-border"), "heavy green")
  expect_equal(shown(style, "p-length"), "7")
  expect_equal(shown(style, "p-style"), "italic strike")
})

test_that("a computed style contains every registered property", {
  registry <- registry_with("a", "length", "0")
  registry <- ink_register_property(registry, "b", "color", "red", TRUE)
  style <- compute_css("Button { color: blue; }", ink_element("Button"), registry)
  expect_equal(sort(names(style$values)), sort(names(registry$properties)))
  expect_length(style$values, length(registry$properties))
  expect_equal(names(style$provenance), names(style$values))
})

test_that("registry order decides property order", {
  registry <- registry_with("zeta", "length", "0")
  registry <- ink_register_property(registry, "alpha", "length", "0")
  style <- compute_css("Button { alpha: 1; }", ink_element("Button"), registry)
  expect_equal(
    names(style$values),
    c(names(ink_registry()$properties), "zeta", "alpha")
  )
})

test_that("custom properties win the cascade like any other property", {
  registry <- registry_with("gap", "length", "0")
  css <- "Button { gap: 1; }\nButton.x { gap: 2; }\n#id { gap: 3; }\nButton { gap: 4; }"
  expect_equal(shown(compute_css(css, ink_element("Button"), registry), "gap"), "4")
  expect_equal(shown(compute_css(css, ink_element("Button", classes = "x"), registry), "gap"), "2")
  expect_equal(shown(compute_css(css, ink_element("Button", id = "id"), registry), "gap"), "3")
})

test_that("the stylesheet's registry is used, not the default one", {
  registry <- registry_with("gap", "length", "0")
  sheet <- compile_text("Button { gap: 5; }", registry = registry)
  style <- ink_compute(sheet, ink_element("Button"))
  expect_true("gap" %in% names(style$values))
  expect_false("gap" %in% names(compute_css("Button { color: red; }", ink_element("Button"))$values))
})

test_that("computing leaves the registry and its sources unchanged", {
  registry <- registry_with("gap", "length", "0", inherits = TRUE)
  before <- registry
  default_before <- ink_registry()
  sheet <- compile_text("Screen { gap: 2; }", registry = registry)
  sheet_before <- sheet
  ink_compute(sheet, tree("Screen", "Button"))
  expect_identical(registry, before)
  expect_identical(sheet, sheet_before)
  expect_identical(sheet$registry, before)
  expect_identical(ink_registry(), default_before)
})

test_that("built-in inheritance comes from the registry, not from the code", {
  # A registry whose `padding` inherits makes padding inherit.
  registry <- ink_registry()
  registry$properties$padding$inherits <- TRUE
  style <- compute_css("Screen { padding: 2; }", tree("Screen", "Button"), registry)
  expect_equal(shown(style, "padding"), "top=2 right=2 bottom=2 left=2")
  # And one where `color` does not inherit.
  registry <- ink_registry()
  registry$properties$color$inherits <- FALSE
  style <- compute_css("Screen { color: red; }", tree("Screen", "Button"), registry)
  expect_equal(shown(style, "color"), "default")
})
