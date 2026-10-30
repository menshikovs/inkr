css <- "
Screen {
  color: white;
  background: blue;
}

Button {
  width: 4;
  color: inherit;
  padding: initial;
}

Button, #save {
  border: round red;
}

Button.primary {
  background: green;
  background: yellow;
}
"

fixture <- function() {
  sheet <- compile_text(css)
  element <- tree("Screen", list(type = "Button", id = "save", classes = "primary"))
  list(sheet = sheet, element = element, style = ink_compute(sheet, element))
}

test_that("a direct declaration matches the computed provenance", {
  f <- fixture()
  explanation <- ink_explain(f$sheet, f$element, "width")
  origin <- f$style$provenance$width
  expect_equal(explanation$origin, "declaration")
  expect_equal(explanation$winner$rule_index, origin$rule_index)
  expect_equal(explanation$winner$declaration_index, origin$declaration_index)
  expect_equal(explanation$winner$selector_index, origin$selector_index)
  expect_identical(explanation$winner$specificity, origin$specificity)
  expect_identical(explanation$winner$span, origin$span)
  expect_equal(explanation$winner$rule_index, 2L)
  expect_equal(explanation$winner$declaration_index, 1L)
})

test_that("duplicate declarations report the winning declaration index", {
  f <- fixture()
  explanation <- ink_explain(f$sheet, f$element, "background")
  expect_equal(explanation$winner$rule_index, 4L)
  expect_equal(explanation$winner$declaration_index, 2L)
  expect_equal(format(explanation$value), "yellow")
  expect_equal(reasons(explanation), c("earlier_declaration", "winner"))
  expect_equal(explanation$winner$span$start$line, 19L)
})

test_that("a selector list reports the winning selector", {
  f <- fixture()
  explanation <- ink_explain(f$sheet, f$element, "border")
  expect_equal(explanation$winner$selector_index, 2L)
  expect_equal(explanation$winner$matching_selector_indices, 1:2)
  expect_identical(explanation$winner$specificity, c(id = 1L, class = 0L, type = 0L))
  expect_identical(explanation$winner$specificity, f$style$provenance$border$specificity)
})

test_that("an inherited value matches the computed provenance", {
  sheet <- compile_text("Screen { color: white; }")
  element <- tree("Screen", "Button")
  explanation <- ink_explain(sheet, element, "color")
  provenance <- ink_compute(sheet, element)$provenance$color
  expect_equal(explanation$origin, "inherited")
  expect_identical(explanation$resolution$path[[2L]]$provenance, provenance)
})

test_that("an initial value matches the computed provenance", {
  f <- fixture()
  explanation <- ink_explain(f$sheet, f$element, "margin")
  expect_equal(explanation$origin, "initial")
  expect_identical(explanation$resolution$path[[2L]]$provenance, f$style$provenance$margin)
})

test_that("explicit inherit and initial match the computed provenance", {
  f <- fixture()
  inherit <- ink_explain(f$sheet, f$element, "color")
  initial <- ink_explain(f$sheet, f$element, "padding")
  expect_equal(inherit$origin, "explicit_inherit")
  expect_equal(initial$origin, "explicit_initial")
  expect_equal(inherit$winner$declaration_index, f$style$provenance$color$declaration_index)
  expect_equal(initial$winner$declaration_index, f$style$provenance$padding$declaration_index)
  expect_identical(inherit$winner$span, f$style$provenance$color$span)
  expect_equal(inherit$resolution$keyword, "inherit")
  expect_equal(initial$resolution$keyword, "initial")
})

test_that("the target's step in the path is the computed provenance record", {
  f <- fixture()
  for (property in names(f$style$values)) {
    explanation <- ink_explain(f$sheet, f$element, property)
    target <- explanation$resolution$path[[length(explanation$resolution$path)]]
    expect_identical(target$provenance, f$style$provenance[[property]], info = property)
    expect_identical(target$value, f$style$values[[property]], info = property)
  }
})

test_that("every ancestor's step equals that ancestor's own computed provenance", {
  sheet <- compile_text("Screen { color: red; text-style: bold; }\nPanel { color: blue; }")
  leaf <- tree("Screen", "Panel", "Mid", "Button")
  explanation <- ink_explain(sheet, leaf, "color")
  chain <- list(leaf$parent$parent$parent, leaf$parent$parent, leaf$parent, leaf)
  for (i in seq_along(chain)) {
    own <- ink_compute(sheet, chain[[i]])
    step <- explanation$resolution$path[[i]]
    expect_identical(step$provenance, own$provenance$color, info = i)
    expect_identical(step$value, own$values$color, info = i)
  }
})

test_that("explanations agree with computed styles across many trees", {
  f <- fixture()
  elements <- list(
    f$element, f$element$parent, ink_element("Button"),
    tree("Screen", "Container", list(type = "Button", classes = "primary")),
    tree(list(type = "Screen", id = "save"), "Button")
  )
  for (element in elements) {
    expect_explanations_consistent(f$sheet, element)
  }
})
