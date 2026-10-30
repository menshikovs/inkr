test_that("an id rule beats a type rule that comes later", {
  style <- compute_css(
    "#save { color: red; }\nButton { color: blue; }",
    ink_element("Button", id = "save")
  )
  expect_equal(shown(style), "red")
})

test_that("an id rule beats a type rule that comes earlier", {
  style <- compute_css(
    "Button { color: blue; }\n#save { color: red; }",
    ink_element("Button", id = "save")
  )
  expect_equal(shown(style), "red")
})

test_that("equal specificity from a class and a state goes to the later rule", {
  css <- "Button.primary { color: red; }\nButton:focus { color: blue; }"
  element <- ink_element("Button", classes = "primary", states = "focus")
  expect_equal(shown(compute_css(css, element)), "blue")
  reversed <- "Button:focus { color: blue; }\nButton.primary { color: red; }"
  expect_equal(shown(compute_css(reversed, element)), "red")
})

test_that("more classes beat fewer, wherever the rules are", {
  css <- ".a.b { color: red; }\n.a { color: blue; }"
  element <- ink_element("X", classes = c("a", "b"))
  expect_equal(shown(compute_css(css, element)), "red")
  css <- ".a { color: blue; }\n.a.b { color: red; }"
  expect_equal(shown(compute_css(css, element)), "red")
})

test_that("classes beat any number of types", {
  css <- ".a { color: red; }\nA B C D E F { color: blue; }"
  element <- tree("A", "B", "C", "D", "E", list(type = "F", classes = "a"))
  expect_equal(shown(compute_css(css, element)), "red")
})

test_that("specificity is compared lexicographically beyond nine classes", {
  classes <- function(n) paste0(".c", seq_len(n), collapse = "")
  element <- ink_element("X", id = "x", classes = paste0("c", 1:12))

  # One id outranks eleven classes, even though 11 * 10 > 100.
  css <- sprintf("#x { color: red; }\n%s { color: blue; }", classes(11L))
  expect_equal(shown(compute_css(css, element)), "red")
  css <- sprintf("%s { color: blue; }\n#x { color: red; }", classes(11L))
  expect_equal(shown(compute_css(css, element)), "red")

  # Ten classes beat nine, wherever they are.
  css <- sprintf("%s { color: red; }\n%s { color: blue; }", classes(10L), classes(9L))
  expect_equal(shown(compute_css(css, element)), "red")

  # Twelve types do not beat a single class.
  deep <- tree(list(type = "A"), "A", "A", "A", "A", "A", "A", "A", "A", "A", "A",
               list(type = "A", classes = "k"))
  css <- ".k { color: red; }\nA A A A A A A A A A A A { color: blue; }"
  expect_equal(shown(compute_css(css, deep)), "red")
})

test_that("pseudo-classes are counted like classes in the cascade", {
  element <- ink_element("X", classes = "a", states = c("s", "t"))
  css <- ".a { color: red; }\n:s:t { color: blue; }"
  expect_equal(shown(compute_css(css, element)), "blue")
  css <- ".a { color: red; }\n:s { color: blue; }"
  expect_equal(shown(compute_css(css, element)), "blue")
})

test_that("a selector list uses the specificity of the selector that matched", {
  css <- "Button, #save { color: red; }\nButton.primary { color: blue; }"
  expect_equal(
    shown(compute_css(css, ink_element("Button", id = "save", classes = "primary"))),
    "red"
  )
  expect_equal(
    shown(compute_css(css, ink_element("Button", classes = "primary"))),
    "blue"
  )
  # A strong selector of the list that does not match must not help the rule.
  css <- "Button, #other { color: red; }\n.primary { color: blue; }"
  expect_equal(
    shown(compute_css(css, ink_element("Button", classes = "primary"))),
    "blue"
  )
})

test_that("complex selectors take their specificity from every compound", {
  css <- "Button { color: red; }\nScreen Button { color: blue; }\nScreen > Container Button { color: green; }"
  element <- tree("Screen", "Container", "Button")
  expect_equal(shown(compute_css(css, element)), "green")
  css <- "Screen Button { color: blue; }\n.row { color: red; }"
  element <- tree("Screen", list(type = "Button", classes = "row"))
  expect_equal(shown(compute_css(css, element)), "red")
})

test_that("the specificity recorded for the winner is the selector's", {
  style <- compute_css("#dialog Button { color: red; }", tree(list(type = "Screen", id = "dialog"), "Button"))
  expect_identical(style$provenance$color$specificity, c(id = 1L, class = 0L, type = 1L))
})
