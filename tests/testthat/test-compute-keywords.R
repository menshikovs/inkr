test_that("initial on an inherited property ignores the parent", {
  css <- "Screen { color: red; }\nButton { color: initial; }"
  style <- compute_css(css, tree("Screen", "Button"))
  expect_equal(shown(style), "default")
  expect_equal(style$provenance$color$origin, "explicit_initial")
  expect_equal(style$provenance$color$source, "registry")
})

test_that("initial on non-inherited properties gives the registry value", {
  css <- "Button { background: initial; padding: initial; border: initial; width: initial; }"
  style <- compute_css(css, ink_element("Button"))
  expect_equal(shown(style, "background"), "transparent")
  expect_equal(shown(style, "padding"), "top=0 right=0 bottom=0 left=0")
  expect_equal(shown(style, "border"), "none")
  expect_equal(shown(style, "width"), "auto")
})

test_that("initial is not the same as a missing declaration", {
  css <- "Screen { color: red; }\nButton { color: initial; }\nInput { padding: 1; }"
  with_keyword <- compute_css(css, tree("Screen", "Button"))
  without <- compute_css(css, tree("Screen", "Input"))
  expect_equal(shown(with_keyword), "default")
  expect_equal(shown(without), "red")
})

test_that("initial is not transparent or default unless the registry says so", {
  registry <- registry_with("accent", "color", "bright-magenta", inherits = TRUE)
  css <- "Screen { accent: red; }\nButton { accent: initial; }"
  style <- compute_css(css, tree("Screen", "Button"), registry)
  expect_equal(shown(style, "accent"), "bright-magenta")
})

test_that("inherit on an inherited property takes the parent's value", {
  css <- "Screen { color: red; text-style: bold; }\nButton { color: inherit; text-style: inherit; }"
  style <- compute_css(css, tree("Screen", "Button"))
  expect_equal(shown(style), "red")
  expect_equal(shown(style, "text-style"), "bold")
})

test_that("inherit on a non-inherited property takes the parent's value", {
  css <- "Screen { border: heavy red; }\nButton { border: inherit; }"
  style <- compute_css(css, tree("Screen", "Button"))
  expect_equal(shown(style, "border"), "heavy red")
})

test_that("a stronger keyword declaration beats a weaker concrete one", {
  css <- "Screen { color: green; }\nButton { color: red; }\nButton.primary { color: inherit; }"
  style <- compute_css(css, tree("Screen", list(type = "Button", classes = "primary")))
  expect_equal(shown(style), "green")
  expect_equal(style$provenance$color$origin, "explicit_inherit")
  expect_equal(style$provenance$color$rule_index, 3L)
})

test_that("a stronger concrete declaration beats a weaker keyword", {
  css <- "Button.primary { color: red; }\nButton { color: inherit; }"
  style <- compute_css(css, tree(list(type = "Screen"), list(type = "Button", classes = "primary")))
  expect_equal(shown(style), "red")
  expect_equal(style$provenance$color$origin, "declaration")
})

test_that("an earlier but more specific keyword beats a later weaker declaration", {
  css <- "Button.primary { color: initial; }\nButton { color: blue; }"
  style <- compute_css(css, ink_element("Button", classes = "primary"))
  expect_equal(shown(style), "default")
  expect_equal(style$provenance$color$origin, "explicit_initial")
})

test_that("with equal specificity the later of a keyword and a value wins", {
  parent <- ink_element("Screen")
  element <- ink_element("Button", parent = parent)
  css <- "Screen { color: green; }\nButton { color: initial; }\nButton { color: red; }"
  expect_equal(shown(compute_css(css, element)), "red")
  css <- "Screen { color: green; }\nButton { color: red; }\nButton { color: initial; }"
  expect_equal(shown(compute_css(css, element)), "default")
  css <- "Screen { color: green; }\nButton { color: initial; color: inherit; }"
  expect_equal(shown(compute_css(css, element)), "green")
  css <- "Screen { color: green; }\nButton { color: inherit; color: initial; }"
  expect_equal(shown(compute_css(css, element)), "default")
})

test_that("keywords compete per property", {
  css <- "Screen { color: green; background: blue; }\nButton { color: inherit; background: red; }"
  style <- compute_css(css, tree("Screen", "Button"))
  expect_equal(shown(style, "color"), "green")
  expect_equal(shown(style, "background"), "red")
})

test_that("no keyword objects remain in any computed style", {
  keywords <- c("inherit", "initial")
  properties <- names(ink_registry()$properties)
  for (keyword in keywords) {
    css <- paste(sprintf("Button { %s: %s; }", properties, keyword), collapse = "\n")
    for (element in list(ink_element("Button"), tree("Screen", "Button"))) {
      style <- compute_css(css, element)
      expect_true(has_no_keywords(style))
      expect_true(all(vapply(style$values, function(v) !inherits(v, "ink_keyword"), NA)))
    }
  }
})

test_that("a keyword held in a variable behaves like the keyword", {
  css <- "$k: inherit;\nScreen { color: green; }\nButton { color: $k; }"
  style <- compute_css(css, tree("Screen", "Button"))
  expect_equal(shown(style), "green")
  expect_equal(style$provenance$color$origin, "explicit_inherit")
})

test_that("keywords record their winning declaration", {
  css <- "Screen { color: red; }\nButton {\n  width: 1;\n  color: initial;\n}"
  style <- compute_css(css, tree("Screen", "Button"))
  origin <- style$provenance$color
  expect_equal(origin$keyword, "initial")
  expect_equal(origin$rule_index, 2L)
  expect_equal(origin$declaration_index, 2L)
  expect_equal(origin$span$start$line, 4L)
})
