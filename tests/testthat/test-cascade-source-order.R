button <- ink_element("Button")

test_that("the later of two equal rules wins", {
  expect_equal(shown(compute_css("Button { color: red; }\nButton { color: blue; }", button)), "blue")
  expect_equal(shown(compute_css("Button { color: blue; }\nButton { color: red; }", button)), "red")
})

test_that("the later of two declarations in a rule wins", {
  expect_equal(shown(compute_css("Button { color: red; color: green; }", button)), "green")
  expect_equal(shown(compute_css("Button { color: green; color: red; }", button)), "red")
})

test_that("rule position outranks declaration position", {
  css <- "Button { width: 1; width: 2; width: 3; color: red; }\nButton { color: blue; }"
  style <- compute_css(css, button)
  expect_equal(shown(style), "blue")
  # The winning declaration is the first of its rule, yet it wins: the rule is later.
  expect_equal(style$provenance$color$rule_index, 2L)
  expect_equal(style$provenance$color$declaration_index, 1L)
  expect_equal(shown(style, "width"), "3")
  expect_equal(style$provenance$width$declaration_index, 3L)
})

test_that("many duplicate declarations resolve to the last one", {
  css <- paste0("Button {", paste(sprintf("width: %d;", 1:50), collapse = " "), "}")
  style <- compute_css(css, button)
  expect_equal(shown(style, "width"), "50")
  expect_equal(style$provenance$width$declaration_index, 50L)
})

test_that("source order only matters between equal specificities", {
  css <- "Button.primary { color: red; }\nButton { color: blue; }"
  expect_equal(shown(compute_css(css, ink_element("Button", classes = "primary"))), "red")
  expect_equal(shown(compute_css(css, button)), "blue")
})

test_that("rules that do not match never take part", {
  css <- "Button { color: red; }\nInput { color: blue; }\n.missing { color: green; }"
  expect_equal(shown(compute_css(css, button)), "red")
})

test_that("a later rule for another property does not disturb the first", {
  css <- "Button { color: red; }\nButton { background: blue; }"
  style <- compute_css(css, button)
  expect_equal(shown(style), "red")
  expect_equal(shown(style, "background"), "blue")
  expect_equal(style$provenance$color$rule_index, 1L)
  expect_equal(style$provenance$background$rule_index, 2L)
})

test_that("a long stylesheet is resolved by position", {
  css <- paste(sprintf("Button { width: %d; }", 1:200), collapse = "\n")
  expect_equal(shown(compute_css(css, button), "width"), "200")
})
