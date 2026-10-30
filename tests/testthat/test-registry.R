registry_error <- function(expr) {
  tryCatch(
    {
      expr
      NULL
    },
    ink_compile_error = function(e) e
  )
}

test_that("ink_registry returns a registry with the built-in properties", {
  registry <- ink_registry()
  expect_s3_class(registry, "ink_registry")
  expect_named(
    registry$properties,
    c("color", "background", "text-style", "padding", "margin", "border", "width", "height")
  )
})

test_that("built-in types and inheritance follow the specification", {
  properties <- ink_registry()$properties
  types <- vapply(properties, function(p) p$type, "")
  inherits <- vapply(properties, function(p) p$inherits, NA)
  expect_equal(
    types,
    c(
      color = "color", background = "color", `text-style` = "text_style",
      padding = "spacing", margin = "spacing", border = "border",
      width = "length", height = "length"
    )
  )
  expect_equal(
    inherits,
    c(
      color = TRUE, background = FALSE, `text-style` = TRUE, padding = FALSE,
      margin = FALSE, border = FALSE, width = FALSE, height = FALSE
    )
  )
})

test_that("built-in initial values are valid typed values of the declared type", {
  properties <- ink_registry()$properties
  for (property in properties) {
    expect_s3_class(property$initial, VALUE_CLASSES[[property$type]])
    expect_s3_class(property, "ink_property")
  }
  initial <- function(name) format(properties[[name]]$initial)
  expect_equal(initial("color"), "default")
  expect_equal(initial("background"), "transparent")
  expect_equal(initial("text-style"), "none")
  expect_equal(initial("padding"), "top=0 right=0 bottom=0 left=0")
  expect_equal(initial("margin"), "top=0 right=0 bottom=0 left=0")
  expect_equal(initial("border"), "none")
  expect_equal(initial("width"), "auto")
  expect_equal(initial("height"), "auto")
})

test_that("a custom property can be registered and used", {
  registry <- ink_register_property(
    ink_registry(), "row-stripe",
    type = "color", initial = "transparent", inherits = FALSE
  )
  sheet <- compile_text("DataTable {\n  row-stripe: #242424;\n}", registry = registry)
  declaration <- sheet$rules[[1L]]$declarations[[1L]]
  expect_equal(declaration$name, "row-stripe")
  expect_equal(declaration$type, "color")
  expect_equal(format(declaration$value), "#242424")
  expect_identical(sheet$registry, registry)
})

test_that("registered properties keep their metadata", {
  registry <- ink_register_property(ink_registry(), "gap", "length", "2", inherits = TRUE)
  property <- registry$properties[["gap"]]
  expect_equal(property$type, "length")
  expect_true(property$inherits)
  expect_equal(property$initial$cells, 2L)
  expect_named(registry$properties, c(names(ink_registry()$properties), "gap"))
})

test_that("every value type can be registered", {
  registry <- ink_registry()
  registry <- ink_register_property(registry, "p-color", "color", "bright-red")
  registry <- ink_register_property(registry, "p-spacing", "spacing", "1 2")
  registry <- ink_register_property(registry, "p-border", "border", "round red")
  registry <- ink_register_property(registry, "p-length", "length", "auto")
  registry <- ink_register_property(registry, "p-style", "text_style", "underline bold")
  expect_equal(format(registry$properties[["p-style"]]$initial), "bold underline")
  expect_equal(registry$properties[["p-spacing"]]$initial$values[["right"]], 2L)
})

test_that("the initial value is normalized", {
  registry <- ink_register_property(ink_registry(), "x", "color", "#FFF")
  expect_identical(registry$properties$x$initial, value("color", "#ffffff"))
})

test_that("a typed initial value is accepted when the class matches", {
  registry <- ink_register_property(ink_registry(), "x", "color", value("color", "red"))
  expect_equal(format(registry$properties$x$initial), "red")
  err <- registry_error(ink_register_property(ink_registry(), "x", "color", value("length", "3")))
  expect_equal(err$code, "invalid_registry_definition")
})

test_that("registering does not modify the original registry", {
  original <- ink_registry()
  before <- original
  extended <- ink_register_property(original, "gap", "length", "0")
  expect_identical(original, before)
  expect_false("gap" %in% names(original$properties))
  expect_true("gap" %in% names(extended$properties))
  expect_false("gap" %in% names(ink_registry()$properties))
})

test_that("two extensions of one registry are independent", {
  base <- ink_registry()
  a <- ink_register_property(base, "a", "length", "0")
  b <- ink_register_property(base, "b", "length", "0")
  expect_false("b" %in% names(a$properties))
  expect_false("a" %in% names(b$properties))
})

test_that("invalid property names are rejected", {
  for (name in c("", "1abc", "-x", "has space", "a.b", "a:b", "café", "a;b", "$x")) {
    err <- registry_error(ink_register_property(ink_registry(), name, "color", "red"))
    expect_s3_class(err, "ink_compile_error")
    expect_equal(err$code, "invalid_registry_definition", info = name)
  }
  for (name in list(NA_character_, 1, NULL, c("a", "b"))) {
    err <- registry_error(ink_register_property(ink_registry(), name, "color", "red"))
    expect_equal(err$code, "invalid_registry_definition")
  }
})

test_that("valid but unusual property names are accepted", {
  for (name in c("_x", "x-y_z", "Row-Stripe", "a1")) {
    registry <- ink_register_property(ink_registry(), name, "color", "red")
    expect_true(name %in% names(registry$properties))
  }
})

test_that("unknown types are rejected", {
  for (type in list("colour", "COLOR", "text-style", "", NA_character_, 1, NULL)) {
    err <- registry_error(ink_register_property(ink_registry(), "x", type, "red"))
    expect_equal(err$code, "invalid_registry_definition")
  }
  err <- registry_error(ink_register_property(ink_registry(), "x", "colour", "red"))
  expect_match(err$detail, "Supported types: color, spacing, border, length, text_style", fixed = TRUE)
})

test_that("invalid initial values are rejected", {
  cases <- list(
    list("color", "reddish"),
    list("color", "$primary"),
    list("color", "inherit"),
    list("color", ""),
    list("spacing", "-1"),
    list("length", "50%"),
    list("border", "round"),
    list("text_style", "bold none"),
    list("color", 1),
    list("color", NA_character_),
    list("color", NULL)
  )
  for (case in cases) {
    err <- registry_error(ink_register_property(ink_registry(), "x", case[[1L]], case[[2L]]))
    expect_s3_class(err, "ink_compile_error")
    expect_equal(err$code, "invalid_registry_definition")
  }
})

test_that("invalid initial values explain the problem", {
  err <- registry_error(ink_register_property(ink_registry(), "gap", "spacing", "1 two"))
  expect_match(err$detail, "Invalid initial value '1 two' for spacing property 'gap'", fixed = TRUE)
  expect_match(err$detail, "Expected 1 to 4 non-negative integers.", fixed = TRUE)
  expect_null(err$line)
  expect_equal(conditionMessage(err), err$detail)
})

test_that("duplicate registrations are rejected, including built-ins", {
  err <- registry_error(ink_register_property(ink_registry(), "color", "color", "red"))
  expect_equal(err$code, "invalid_registry_definition")
  expect_match(err$detail, "already registered", fixed = TRUE)
  expect_equal(err$property, "color")

  once <- ink_register_property(ink_registry(), "x", "color", "red")
  err <- registry_error(ink_register_property(once, "x", "length", "0"))
  expect_equal(err$code, "invalid_registry_definition")
  # The built-in definition is untouched.
  expect_equal(ink_registry()$properties$color$type, "color")
})

test_that("inherits must be TRUE or FALSE", {
  for (flag in list(NA, "yes", 1, c(TRUE, FALSE), NULL)) {
    err <- registry_error(ink_register_property(ink_registry(), "x", "color", "red", inherits = flag))
    expect_equal(err$code, "invalid_registry_definition")
  }
})

test_that("registering requires a real registry", {
  for (bad in list(NULL, list(), "registry", list(properties = list()))) {
    err <- registry_error(ink_register_property(bad, "x", "color", "red"))
    expect_equal(err$code, "invalid_registry_definition")
  }
})

test_that("a tampered registry cannot be used silently", {
  ast <- ink_parse("Button { color: red; }")

  registry <- ink_registry()
  registry$properties$color$type <- "nonsense"
  expect_equal(registry_error(ink_compile(ast, registry))$code, "invalid_registry_definition")

  registry <- ink_registry()
  registry$properties$color$initial <- "red"
  expect_equal(registry_error(ink_compile(ast, registry))$code, "invalid_registry_definition")

  registry <- ink_registry()
  names(registry$properties)[[1L]] <- "colour"
  expect_equal(registry_error(ink_compile(ast, registry))$code, "invalid_registry_definition")

  registry <- ink_registry()
  registry$properties$color$inherits <- NA
  expect_equal(registry_error(ink_compile(ast, registry))$code, "invalid_registry_definition")

  registry <- ink_registry()
  registry$properties[[1L]] <- 42
  expect_equal(registry_error(ink_compile(ast, registry))$code, "invalid_registry_definition")

  registry <- unclass(ink_registry())
  expect_equal(registry_error(ink_compile(ast, registry))$code, "invalid_registry_definition")
  expect_equal(registry_error(ink_compile(ast, NULL))$code, "invalid_registry_definition")
})

test_that("properties outside the registry are rejected, not ignored", {
  err <- expect_compile_error("Button {\n  colour: red;\n}", "unknown_property", 2, 3)
  expect_equal(err$property, "colour")
  expect_equal(loc(err$span$end), c(line = 2, column = 9, offset = 17))
  expect_match(err$detail, "Unknown property 'colour'", fixed = TRUE)
  expect_match(err$detail, "color", fixed = TRUE)

  expect_compile_error("Button { row-stripe: red; }", "unknown_property", 1, 10)
  expect_compile_error("Button { Color: red; }", "unknown_property", 1, 10)
})

test_that("a stylesheet that needs a custom property fails with the default registry", {
  text <- "DataTable { row-stripe: #242424; }"
  expect_equal(compile_error(text)$code, "unknown_property")
  registry <- ink_register_property(ink_registry(), "row-stripe", "color", "transparent")
  expect_s3_class(compile_text(text, registry = registry), "ink_compiled_stylesheet")
})

test_that("the registry prints", {
  output <- capture.output(result <- print(ink_registry()))
  expect_identical(result, ink_registry())
  expect_match(output[[1L]], "<ink_registry> 8 properties", fixed = TRUE)
  expect_true(any(grepl("background", output) & grepl("transparent", output)))
  registry <- ink_register_property(ink_registry(), "gap", "length", "0", inherits = TRUE)
  expect_true(any(grepl("gap.*inherits", capture.output(print(registry)))))
})
