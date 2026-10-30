example_css <- "
  $primary: #7c3aed;

  Screen {
    color: white;
    background: #181825;
  }

  Container {
    padding: 1;
  }

  Button {
    color: inherit;
    background: blue;
    border: single white;
  }

  Button.primary {
    background: $primary;
  }

  Button:focus {
    text-style: bold;
  }

  #save {
    border: round green;
  }
"

example_tree <- function() {
  screen <- ink_element("Screen", id = "main")
  container <- ink_element("Container", classes = "panel", parent = screen)
  ink_element("Button", id = "save", classes = "primary", states = "focus", parent = container)
}

test_that("ink_compute returns an ink_computed_style", {
  style <- compute_css("Button { color: red; }", ink_element("Button"))
  expect_s3_class(style, "ink_computed_style")
  expect_named(style, c("values", "provenance", "element"))
  expect_type(style$values, "list")
})

test_that("the specified example picks the expected values", {
  style <- compute_css(example_css, example_tree())
  expect_equal(shown(style, "color"), "white")
  expect_equal(shown(style, "background"), "#7c3aed")
  expect_equal(shown(style, "border"), "round green")
  expect_equal(shown(style, "text-style"), "bold")
  # Container's padding is not inherited.
  expect_equal(shown(style, "padding"), "top=0 right=0 bottom=0 left=0")
})

test_that("the ID selector decides the color of the first example", {
  sheet <- ink_compile(ink_parse("
    Button { color: white; background: blue; }
    Button.primary { color: yellow; }
    #save { color: green; }
  "))
  style <- ink_compute(sheet, ink_element("Button", id = "save", classes = "primary"))
  expect_equal(shown(style, "color"), "green")
  expect_equal(shown(style, "background"), "blue")
})

test_that("each property comes from its own winning rule", {
  css <- "Button { color: white; background: blue; padding: 1; }\nButton.primary { color: yellow; }"
  style <- compute_css(css, ink_element("Button", classes = "primary"))
  expect_equal(shown(style, "color"), "yellow")
  expect_equal(shown(style, "background"), "blue")
  expect_equal(shown(style, "padding"), "top=1 right=1 bottom=1 left=1")
  expect_equal(
    vapply(style$provenance[c("color", "background", "padding")], function(o) o$rule_index, 0L),
    c(color = 2L, background = 1L, padding = 1L)
  )
})

test_that("every registered property is present, in registry order", {
  style <- compute_css("Button { color: red; }", ink_element("Button"))
  expect_equal(names(style$values), names(ink_registry()$properties))
  expect_equal(
    names(style$values),
    c("color", "background", "text-style", "padding", "margin", "border", "width", "height")
  )
  expect_equal(names(style$provenance), names(style$values))
  expect_equal(anyDuplicated(names(style$values)), 0L)
})

test_that("values are typed objects of the property's type", {
  style <- compute_css(example_css, example_tree())
  classes <- vapply(style$values, function(v) class(v)[[1L]], "")
  expect_equal(
    classes,
    c(
      color = "ink_color", background = "ink_color", `text-style` = "ink_text_style",
      padding = "ink_spacing", margin = "ink_spacing", border = "ink_border",
      width = "ink_length", height = "ink_length"
    )
  )
  expect_false(any(vapply(style$values, is.character, NA)))
})

test_that("no keywords, variables or ANSI codes remain", {
  style <- compute_css(example_css, example_tree())
  expect_true(has_no_keywords(style))
  text <- vapply(style$values, format, "")
  expect_false(any(grepl("\\$", text)))
  expect_false(any(grepl("\033", text, fixed = TRUE)))
})

test_that("properties without a declaration use the registry's initial values", {
  style <- compute_css("Button { color: red; }", ink_element("Button"))
  registry <- ink_registry()
  for (name in setdiff(names(registry$properties), "color")) {
    expect_identical(style$values[[name]], registry$properties[[name]]$initial, info = name)
  }
  expect_equal(shown(style, "background"), "transparent")
  expect_equal(shown(style, "width"), "auto")
  expect_equal(shown(style, "text-style"), "none")
})

test_that("an empty stylesheet yields all initial values", {
  style <- compute_css("", ink_element("Button"))
  registry <- ink_registry()
  expect_identical(style$values, lapply(registry$properties, function(p) p$initial))
  expect_true(all(vapply(style$provenance, function(o) o$origin, "") == "initial"))
})

test_that("a stylesheet with only variables yields all initial values", {
  style <- compute_css("$a: red;\n$b: 1 2;", ink_element("Button"))
  expect_identical(style$values, lapply(ink_registry()$properties, function(p) p$initial))
})

test_that("an element that matches nothing is valid", {
  style <- compute_css("Input { color: red; }\n.x { color: blue; }", ink_element("Button"))
  expect_equal(shown(style, "color"), "default")
  expect_equal(style$provenance$color$origin, "initial")
})

test_that("an element with many matching rules resolves them all", {
  css <- paste(
    "* { margin: 1; }", "Button { color: red; }", ".a { background: blue; }", ".b { width: 5; }",
    ":s { height: 6; }", "Button.a.b:s { text-style: bold; }", "#id { padding: 2; }",
    "Screen Button { border: single red; }", sep = "\n"
  )
  element <- tree("Screen", list(type = "Button", id = "id", classes = c("a", "b"), states = "s"))
  style <- compute_css(css, element)
  expect_equal(
    vapply(style$values, format, ""),
    c(
      color = "red", background = "blue", `text-style` = "bold",
      padding = "top=2 right=2 bottom=2 left=2", margin = "top=1 right=1 bottom=1 left=1",
      border = "single red", width = "5", height = "6"
    )
  )
})

test_that("repeated computation gives identical results", {
  sheet <- compile_text(example_css)
  element <- example_tree()
  expect_identical(ink_compute(sheet, element), ink_compute(sheet, element))
})

test_that("computing does not modify its inputs", {
  ast <- ink_parse(example_css)
  sheet <- ink_compile(ast)
  element <- example_tree()
  ast_before <- ast
  sheet_before <- sheet
  element_before <- element
  registry_before <- sheet$registry
  ink_compute(sheet, element)
  ink_compute(sheet, element$parent)
  expect_identical(ast, ast_before)
  expect_identical(sheet, sheet_before)
  expect_identical(element, element_before)
  expect_identical(sheet$registry, registry_before)
})

test_that("printing does not modify the style", {
  style <- compute_css(example_css, example_tree())
  before <- style
  capture.output(print(style))
  expect_identical(style, before)
})

test_that("print lists the registered properties and their values", {
  style <- compute_css(example_css, example_tree())
  output <- capture.output(result <- print(style))
  expect_identical(result, style)
  expect_equal(output[[1L]], "<ink_computed_style> Button#save.primary:focus")
  expect_true(any(grepl("^  color +white$", output)))
  expect_true(any(grepl("^  background +#7c3aed$", output)))
  expect_true(any(grepl("^  border +round green$", output)))
  expect_true(any(grepl("^  text-style +bold$", output)))
  expect_true(any(grepl("^  padding +top=0 right=0 bottom=0 left=0$", output)))
  expect_false(any(grepl("\033", output, fixed = TRUE)))
  expect_length(output, 2L + length(style$values))
})

test_that("print follows the registry, including custom properties", {
  registry <- registry_with("row-stripe", "color", "transparent")
  style <- compute_css("DataTable { row-stripe: #242424; }", ink_element("DataTable"), registry)
  output <- capture.output(print(style))
  expect_true(any(grepl("^  row-stripe +#242424$", output)))
  expect_length(output, 2L + 9L)
})

test_that("the cascade example stylesheet computes as documented", {
  path <- system.file("examples", "cascade.inkr", package = "inkr")
  skip_if(path == "", "installed examples are not available")
  sheet <- ink_compile(ink_read(path))
  screen <- ink_element("Screen", id = "main")
  panel <- ink_element("Container", classes = "panel", parent = screen)
  button <- ink_element("Button", id = "save", classes = "primary", states = "focus", parent = panel)
  style <- ink_compute(sheet, button)
  expect_equal(shown(style, "color"), "white")
  expect_equal(shown(style, "background"), "#7c3aed")
  expect_equal(shown(style, "border"), "round green")
  expect_equal(shown(style, "text-style"), "bold underline")
  expect_equal(shown(style, "padding"), "top=0 right=0 bottom=0 left=0")
  expect_equal(shown(style, "margin"), "top=0 right=1 bottom=0 left=1")

  plain <- ink_element("Button", classes = c("primary", "plain"), parent = screen)
  plain_style <- ink_compute(sheet, plain)
  expect_equal(shown(plain_style, "color"), "default")
  # Without the #save rule the later Button.primary rule wins.
  expect_equal(shown(plain_style, "background"), "#181825")
})

test_that("a deep tree computes without recursion limits", {
  css <- "Screen { color: green; background: blue; }\nFiller { padding: 1; }"
  leaf <- filler_chain(1200L)
  first <- compute_css(css, leaf)
  expect_equal(shown(first, "color"), "green")
  expect_equal(shown(first, "background"), "transparent")
  expect_equal(shown(first, "padding"), "top=1 right=1 bottom=1 left=1")
  expect_identical(first, compute_css(css, leaf))
})

test_that("inheritance through a deep tree comes from the root", {
  css <- "Screen { color: green; }"
  leaf <- filler_chain(1500L)
  style <- compute_css(css, leaf)
  expect_equal(shown(style, "color"), "green")
  expect_equal(style$provenance$color$origin, "inherited")
})

test_that("a deep tree with descendant selectors is matched at every level", {
  css <- "Screen Filler { text-style: bold; }\nScreen > Filler { text-style: italic; }"
  style <- compute_css(css, filler_chain(300L))
  # The target is not a direct child of the Screen, so only the first rule applies,
  # and text-style inherits from the parent, which is the same.
  expect_equal(shown(style, "text-style"), "bold")
})
