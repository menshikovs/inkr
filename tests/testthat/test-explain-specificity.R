winner_of <- function(css, element, property = "color") {
  explain_css(css, element, property)$winner
}

test_that("a class beats a type", {
  explanation <- explain_css(
    "Button { color: red; }\n.primary { color: blue; }",
    ink_element("Button", classes = "primary")
  )
  expect_equal(format(explanation$value), "blue")
  expect_equal(reasons(explanation), c("lower_specificity", "winner"))
})

test_that("an id beats a class, wherever it is written", {
  css <- ".primary { color: blue; }\n#save { color: red; }\n.primary { color: green; }"
  explanation <- explain_css(css, ink_element("Button", id = "save", classes = "primary"))
  expect_equal(format(explanation$value), "red")
  expect_equal(reasons(explanation), c("lower_specificity", "winner", "lower_specificity"))
})

test_that("an earlier, more specific rule beats a later one", {
  explanation <- explain_css(
    "#save { color: red; }\nButton { color: blue; }",
    ink_element("Button", id = "save")
  )
  expect_equal(format(explanation$value), "red")
  expect_equal(reasons(explanation), c("winner", "lower_specificity"))
})

test_that("compound selectors count every part", {
  css <- ".a { color: red; }\nButton.a { color: blue; }\nButton.a.b { color: green; }"
  explanation <- explain_css(css, ink_element("Button", classes = c("a", "b")))
  expect_equal(format(explanation$value), "green")
  expect_equal(reasons(explanation), c("lower_specificity", "lower_specificity", "winner"))
  expect_identical(explanation$winner$specificity, c(id = 0L, class = 2L, type = 1L))
})

test_that("pseudo-classes count like classes", {
  css <- ".a { color: red; }\n:focus:hover { color: blue; }"
  explanation <- explain_css(css, ink_element("X", classes = "a", states = c("focus", "hover")))
  expect_equal(format(explanation$value), "blue")
  expect_identical(explanation$winner$specificity, c(id = 0L, class = 2L, type = 0L))
})

test_that("descendant and child selectors contribute every compound", {
  css <- "Button { color: red; }\nScreen Button { color: blue; }\nScreen > Container Button { color: green; }"
  explanation <- explain_css(css, tree("Screen", "Container", "Button"))
  expect_equal(format(explanation$value), "green")
  expect_identical(
    lapply(explanation$candidates, function(c) c$specificity),
    list(
      c(id = 0L, class = 0L, type = 1L),
      c(id = 0L, class = 0L, type = 2L),
      c(id = 0L, class = 0L, type = 3L)
    )
  )
  css <- "Screen > Button { color: red; }\nContainer Button { color: blue; }"
  explanation <- explain_css(css, tree("Screen", "Container", "Button"))
  expect_equal(length(explanation$candidates), 1L)
  expect_equal(explanation$winner$rule_index, 2L)
})

test_that("selector lists use the specificity of the matching selector", {
  css <- "Button, #save { color: red; }\nButton.primary { color: blue; }"
  with_id <- explain_css(css, ink_element("Button", id = "save", classes = "primary"))
  expect_equal(format(with_id$value), "red")
  first <- with_id$candidates[[1L]]
  expect_identical(first$specificity, c(id = 1L, class = 0L, type = 0L))
  expect_equal(first$selector_index, 2L)
  expect_equal(first$matching_selector_indices, 1:2)
  expect_equal(with_id$candidates[[2L]]$reason, "lower_specificity")

  without_id <- explain_css(css, ink_element("Button", classes = "primary"))
  expect_equal(format(without_id$value), "blue")
  first <- without_id$candidates[[1L]]
  expect_identical(first$specificity, c(id = 0L, class = 0L, type = 1L))
  expect_equal(first$selector_index, 1L)
  expect_equal(first$matching_selector_indices, 1L)
  expect_equal(first$reason, "lower_specificity")
})

test_that("a strong selector that does not match is ignored", {
  css <- "Button, #other { color: red; }\n.primary { color: blue; }"
  explanation <- explain_css(css, ink_element("Button", classes = "primary"))
  expect_equal(format(explanation$value), "blue")
  expect_identical(explanation$candidates[[1L]]$specificity, c(id = 0L, class = 0L, type = 1L))
})

test_that("equal-specificity matching selectors use the earliest as the diagnostic winner", {
  css <- ".a, .b { color: red; }"
  explanation <- explain_css(css, ink_element("X", classes = c("a", "b")))
  expect_equal(explanation$winner$selector_index, 1L)
  expect_equal(explanation$winner$matching_selector_indices, 1:2)
  explanation <- explain_css(css, ink_element("X", classes = c("b")))
  expect_equal(explanation$winner$selector_index, 2L)
})

test_that("equal specificity from different rules is decided by order, not specificity", {
  css <- "Button.primary { color: red; }\nButton:focus { color: blue; }"
  explanation <- explain_css(css, ink_element("Button", classes = "primary", states = "focus"))
  expect_equal(format(explanation$value), "blue")
  expect_equal(reasons(explanation), c("earlier_rule", "winner"))
})

test_that("specificity counts above nine are compared lexicographically", {
  classes <- function(n) paste0(".c", seq_len(n), collapse = "")
  element <- ink_element("X", id = "x", classes = paste0("c", 1:12))
  css <- sprintf("%s { color: blue; }\n#x { color: red; }", classes(11L))
  explanation <- explain_css(css, element)
  expect_equal(format(explanation$value), "red")
  expect_equal(reasons(explanation), c("lower_specificity", "winner"))
  expect_identical(explanation$candidates[[1L]]$specificity, c(id = 0L, class = 11L, type = 0L))

  css <- sprintf("%s { color: red; }\n%s { color: blue; }", classes(10L), classes(9L))
  explanation <- explain_css(css, element)
  expect_equal(format(explanation$value), "red")
  expect_equal(reasons(explanation), c("winner", "lower_specificity"))
})

test_that("the recorded specificity is the one ink_specificity reports", {
  ast <- ink_parse("#dialog > Button.primary:focus { color: red; }")
  sheet <- ink_compile(ast)
  element <- tree(list(type = "Screen", id = "dialog"), list(type = "Button", classes = "primary", states = "focus"))
  explanation <- ink_explain(sheet, element, "color")
  expected <- ink_specificity(ast$statements[[1L]]$selectors$selectors[[1L]])
  expect_identical(explanation$winner$specificity, expected)
})
