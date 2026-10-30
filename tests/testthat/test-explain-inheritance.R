positions_in_chain <- function(explanation) {
  Filter(function(step) isTRUE(step$in_effective_chain), explanation$resolution$path)
}

labels <- function(steps) {
  vapply(steps, function(step) format(step$element), "")
}

test_that("a root element takes the registry's initial value", {
  explanation <- explain_css("Button { padding: 1; }", ink_element("Button"))
  expect_equal(explanation$origin, "initial")
  expect_equal(explanation$resolution$source, "registry")
  expect_identical(explanation$value, explanation$resolution$initial_value)
  expect_length(explanation$resolution$path, 1L)
  expect_equal(explanation$resolution$effective_source$position, 1L)
})

test_that("a direct parent supplies an inherited value", {
  explanation <- explain_css("Screen { color: white; }", tree("Screen", "Button"))
  expect_equal(explanation$origin, "inherited")
  expect_equal(explanation$resolution$source, "parent")
  expect_equal(format(explanation$value), "white")
  expect_length(explanation$candidates, 0L)
  expect_null(explanation$winner)
  source <- explanation$resolution$effective_source
  expect_equal(source$origin, "declaration")
  expect_equal(format(source$element), "Screen")
})

test_that("the trace follows every generation to the declaring ancestor", {
  css <- "Screen { color: green; }\nContainer { padding: 1; }"
  leaf <- tree(list(type = "Screen", id = "main"), "Container", "Panel", "Button")
  explanation <- explain_css(css, leaf)
  path <- explanation$resolution$path
  expect_length(path, 4L)
  expect_equal(labels(path), c("Screen#main", "Container", "Panel", "Button"))
  expect_equal(
    vapply(path, function(step) step$origin, ""),
    c("declaration", "inherited", "inherited", "inherited")
  )
  expect_equal(vapply(path, function(step) step$position, 0L), 1:4)
  expect_equal(vapply(path, function(step) step$supplied_by, 0L), c(1L, 1L, 1L, 1L))
  expect_true(all(vapply(path, function(step) step$in_effective_chain, NA)))
  for (step in path) expect_equal(format(step$value), "green")
  expect_equal(format(explanation$resolution$effective_source$element), "Screen#main")
})

test_that("each step's element is the actual element of the tree", {
  leaf <- tree("Screen", "Container", "Button")
  explanation <- explain_css("Screen { color: red; }", leaf)
  path <- explanation$resolution$path
  expect_identical(path[[3L]]$element, leaf)
  expect_identical(path[[2L]]$element, leaf$parent)
  expect_identical(path[[1L]]$element, leaf$parent$parent)
  expect_identical(explanation$element, leaf)
})

test_that("the nearest declaring ancestor is the effective source", {
  css <- "Screen { color: red; }\nContainer { color: blue; }"
  explanation <- explain_css(css, tree("Screen", "Container", "Button"))
  expect_equal(format(explanation$value), "blue")
  expect_equal(format(explanation$resolution$effective_source$element), "Container")
  path <- explanation$resolution$path
  # Screen's red is context, not part of the chain the value travelled through.
  expect_equal(vapply(path, function(step) step$in_effective_chain, NA), c(FALSE, TRUE, TRUE))
  expect_equal(labels(positions_in_chain(explanation)), c("Container", "Button"))
  expect_equal(format(path[[1L]]$value), "red")
  expect_equal(vapply(path, function(step) step$supplied_by, 0L), c(1L, 2L, 2L))
})

test_that("the source is unambiguous across several overriding generations", {
  css <- "A { color: red; }\nC { color: green; }\nE { color: blue; }"
  leaf <- tree("A", "B", "C", "D", "E", "F", "G")
  explanation <- explain_css(css, leaf)
  expect_equal(format(explanation$value), "blue")
  expect_equal(format(explanation$resolution$effective_source$element), "E")
  expect_equal(labels(positions_in_chain(explanation)), c("E", "F", "G"))
  expect_equal(
    vapply(explanation$resolution$path, function(step) step$supplied_by, 0L),
    c(1L, 1L, 3L, 3L, 5L, 5L, 5L)
  )
})

test_that("an inherited value with no declaring ancestor traces to the root's initial value", {
  explanation <- explain_css("Button { padding: 1; }", tree("Screen", "Container", "Button"))
  expect_equal(explanation$origin, "inherited")
  expect_equal(format(explanation$value), "default")
  source <- explanation$resolution$effective_source
  expect_equal(source$origin, "initial")
  expect_equal(format(source$element), "Screen")
  expect_equal(source$provenance$source, "registry")
  expect_equal(labels(positions_in_chain(explanation)), c("Screen", "Container", "Button"))
})

test_that("a non-inherited property is explained as initial, not traced through ancestors", {
  css <- "Screen { background: blue; }"
  explanation <- explain_css(css, tree("Screen", "Button"), "background")
  expect_equal(explanation$origin, "initial")
  expect_equal(format(explanation$value), "transparent")
  expect_equal(explanation$resolution$effective_source$position, 2L)
  expect_equal(labels(positions_in_chain(explanation)), "Button")
})

test_that("a child override beats inheritance and ends the trace at the child", {
  css <- "Screen { color: white; }\nButton { color: red; }"
  explanation <- explain_css(css, tree("Screen", "Button"))
  expect_equal(explanation$origin, "declaration")
  expect_equal(explanation$resolution$effective_source$position, 2L)
  expect_equal(labels(positions_in_chain(explanation)), "Button")
  expect_equal(explanation$winner$rule_index, 2L)
})

test_that("inherited values are explained for text-style as well as color", {
  explanation <- explain_css("Screen { text-style: bold; }", tree("Screen", "Mid", "Button"), "text-style")
  expect_equal(explanation$origin, "inherited")
  expect_s3_class(explanation$value, "ink_text_style")
  expect_equal(format(explanation$resolution$effective_source$element), "Screen")
})

test_that("an ancestor's own explanation agrees with its place in the path", {
  css <- "Screen { color: red; }\nPanel { color: blue; }"
  leaf <- tree("Screen", "Panel", "Mid", "Button")
  leaf_explanation <- explain_css(css, leaf)
  panel_explanation <- explain_css(css, leaf$parent$parent)
  step <- leaf_explanation$resolution$path[[2L]]
  expect_identical(step$value, panel_explanation$value)
  expect_identical(step$provenance, panel_explanation$resolution$path[[2L]]$provenance)
  expect_identical(step$origin, panel_explanation$origin)
})

test_that("deep parent chains are traced without recursion limits", {
  css <- "Screen { color: green; }"
  leaf <- filler_chain(1200L)
  explanation <- explain_css(css, leaf)
  expect_equal(explanation$origin, "inherited")
  expect_equal(format(explanation$value), "green")
  path <- explanation$resolution$path
  expect_length(path, 1201L)
  expect_equal(format(explanation$resolution$effective_source$element), "Screen")
  expect_equal(explanation$resolution$effective_source$position, 1L)
  expect_true(all(vapply(path, function(step) step$supplied_by == 1L, NA)))
  expect_true(all(vapply(path[-1L], function(step) step$origin == "inherited", NA)))
  expect_identical(explanation$value, ink_compute(compile_text(css), leaf)$values$color)
})

test_that("a deep tree with an override in the middle names the middle", {
  css <- "Screen { color: green; }\nMid { color: blue; }"
  top <- ink_element("Mid", parent = filler_chain(500L))
  leaf <- filler_chain(600L, root = top)
  explanation <- explain_css(css, leaf)
  expect_equal(format(explanation$value), "blue")
  expect_equal(explanation$resolution$effective_source$position, 502L)
  expect_equal(format(explanation$resolution$effective_source$element), "Mid")
})
