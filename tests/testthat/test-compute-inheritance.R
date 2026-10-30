test_that("a root element inherits nothing and uses initial values", {
  style <- compute_css("Button { padding: 1; }", ink_element("Button"))
  expect_equal(shown(style, "color"), "default")
  expect_equal(style$provenance$color$origin, "initial")
  expect_equal(style$provenance$color$source, "registry")
})

test_that("an inherited property is taken from the direct parent", {
  style <- compute_css("Screen { color: white; }", tree("Screen", "Button"))
  expect_equal(shown(style), "white")
  expect_equal(style$provenance$color$origin, "inherited")
  expect_equal(style$provenance$color$source, "parent")
})

test_that("the parent's computed value is inherited through undeclaring ancestors", {
  css <- "Screen { color: green; }\nButton { padding: 1; }"
  style <- compute_css(css, tree("Screen", "Container", "Panel", "Button"))
  expect_equal(shown(style), "green")
  expect_equal(shown(style, "padding"), "top=1 right=1 bottom=1 left=1")
})

test_that("the nearest ancestor's value wins", {
  css <- "Screen { color: white; }\nContainer { color: yellow; }"
  style <- compute_css(css, tree("Screen", "Container", "Button"))
  expect_equal(shown(style), "yellow")
  style <- compute_css(css, tree("Screen", "Container", "Panel", "Button"))
  expect_equal(shown(style), "yellow")
})

test_that("an element's own declaration beats inheritance", {
  css <- "Screen { color: white; }\nButton { color: red; }"
  style <- compute_css(css, tree("Screen", "Button"))
  expect_equal(shown(style), "red")
  expect_equal(style$provenance$color$origin, "declaration")
})

test_that("a low-specificity own declaration still beats an inherited value", {
  css <- "#top { color: white; }\n* { color: red; }"
  style <- compute_css(css, tree(list(type = "Screen", id = "top"), "Button"))
  expect_equal(shown(style), "red")
})

test_that("non-inherited properties are not inherited", {
  css <- "Screen { background: blue; padding: 2; margin: 3; border: single red; width: 10; height: 5; }"
  style <- compute_css(css, tree("Screen", "Button"))
  expect_equal(shown(style, "background"), "transparent")
  expect_equal(shown(style, "padding"), "top=0 right=0 bottom=0 left=0")
  expect_equal(shown(style, "margin"), "top=0 right=0 bottom=0 left=0")
  expect_equal(shown(style, "border"), "none")
  expect_equal(shown(style, "width"), "auto")
  expect_equal(shown(style, "height"), "auto")
  for (name in c("background", "padding", "margin", "border", "width", "height")) {
    expect_equal(style$provenance[[name]]$origin, "initial", info = name)
  }
})

test_that("only the inheriting built-ins color and text-style inherit", {
  css <- "Screen { color: red; text-style: bold; background: blue; padding: 1; }"
  style <- compute_css(css, tree("Screen", "Button"))
  origins <- vapply(style$provenance, function(o) o$origin, "")
  expect_equal(names(origins)[origins == "inherited"], c("color", "text-style"))
})

test_that("a parent's explicit padding does not reach the child, but its own does", {
  css <- "Container { padding: 1; }\nButton { margin: 2; }"
  style <- compute_css(css, tree("Container", "Button"))
  expect_equal(shown(style, "padding"), "top=0 right=0 bottom=0 left=0")
  expect_equal(shown(style, "margin"), "top=2 right=2 bottom=2 left=2")
})

test_that("explicit inherit works on an inherited property", {
  css <- "Screen { color: white; }\nButton { color: inherit; }"
  style <- compute_css(css, tree("Screen", "Button"))
  expect_equal(shown(style), "white")
  expect_equal(style$provenance$color$origin, "explicit_inherit")
  expect_equal(style$provenance$color$source, "parent")
})

test_that("explicit inherit forces inheritance of a non-inherited property", {
  css <- "Screen { color: white; background: blue; }\nButton { background: inherit; }"
  style <- compute_css(css, tree("Screen", "Button"))
  expect_equal(shown(style, "color"), "white")
  expect_equal(shown(style, "background"), "blue")
  expect_equal(style$provenance$background$origin, "explicit_inherit")
})

test_that("inherit works for every property type", {
  css <- paste(
    "Screen { color: red; background: green; text-style: bold; padding: 1 2; margin: 3;",
    "border: double blue; width: 40; height: 10; }",
    "Button { color: inherit; background: inherit; text-style: inherit; padding: inherit;",
    "margin: inherit; border: inherit; width: inherit; height: inherit; }",
    sep = "\n"
  )
  child <- compute_css(css, tree("Screen", "Button"))
  parent <- compute_css(css, tree("Screen"))
  expect_equal(shown(child, "padding"), "top=1 right=2 bottom=1 left=2")
  expect_equal(shown(child, "border"), "double blue")
  expect_equal(shown(child, "width"), "40")
  expect_equal(shown(child, "text-style"), "bold")
  expect_identical(child$values$border, compute_css("Screen { border: double blue; }", ink_element("Screen"))$values$border)
  expect_equal(shown(parent, "padding"), "top=1 right=2 bottom=1 left=2")
})

test_that("inherit on a root element resolves to the initial value", {
  style <- compute_css("Screen { color: inherit; background: inherit; }", ink_element("Screen"))
  expect_equal(shown(style, "color"), "default")
  expect_equal(shown(style, "background"), "transparent")
  expect_equal(style$provenance$color$origin, "explicit_inherit")
  expect_equal(style$provenance$color$source, "registry")
  expect_true(has_no_keywords(style))
})

test_that("inherit takes the parent's computed value, which may itself be inherited", {
  css <- "Screen { background: blue; }\nContainer { background: inherit; }\nButton { background: inherit; }"
  style <- compute_css(css, tree("Screen", "Container", "Button"))
  expect_equal(shown(style, "background"), "blue")
  # An undeclaring middle element breaks the chain for a non-inherited property.
  css <- "Screen { background: blue; }\nButton { background: inherit; }"
  style <- compute_css(css, tree("Screen", "Container", "Button"))
  expect_equal(shown(style, "background"), "transparent")
})

test_that("inheritance passes a value through a long chain", {
  css <- "Screen { color: green; text-style: underline; }"
  elements <- c("Screen", rep("Mid", 20L), "Button")
  style <- compute_css(css, do.call(tree, as.list(elements)))
  expect_equal(shown(style), "green")
  expect_equal(shown(style, "text-style"), "underline")
})

test_that("each level inherits from its own parent's computed value", {
  css <- "Screen { color: red; }\nA { color: green; }\nC { color: blue; }"
  leaf <- tree("Screen", "A", "B", "C", "D", "E")
  expect_equal(shown(compute_css(css, leaf)), "blue")
  expect_equal(shown(compute_css(css, leaf$parent$parent)), "blue")
  expect_equal(shown(compute_css(css, leaf$parent$parent$parent)), "green")
  expect_equal(shown(compute_css(css, leaf$parent$parent$parent$parent)), "green")
  expect_equal(shown(compute_css(css, leaf$parent$parent$parent$parent$parent)), "red")
})

test_that("computing a child agrees with computing its parent", {
  css <- "Screen { color: red; text-style: bold; }\nPanel { color: blue; }"
  leaf <- tree("Screen", "Panel", "Button")
  child <- compute_css(css, leaf)
  parent <- compute_css(css, leaf$parent)
  expect_identical(child$values$color, parent$values$color)
  expect_identical(child$values[["text-style"]], parent$values[["text-style"]])
})

test_that("an inherited value is the same typed object as the parent's", {
  css <- "Screen { color: #abcdef; }"
  parent <- compute_css(css, ink_element("Screen"))
  child <- compute_css(css, tree("Screen", "Button"))
  expect_identical(child$values$color, parent$values$color)
  expect_s3_class(child$values$color, "ink_color")
})

test_that("inheritance follows the element tree, not the stylesheet", {
  css <- "Panel { color: red; }\nScreen { color: blue; }"
  style <- compute_css(css, tree("Screen", "Panel", "Button"))
  expect_equal(shown(style), "red")
  style <- compute_css(css, tree("Panel", "Screen", "Button"))
  expect_equal(shown(style), "blue")
})
