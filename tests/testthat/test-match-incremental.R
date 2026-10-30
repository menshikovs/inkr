matches_for <- function(css, leaf) {
  match_chain(prepare_selectors(compile_text(css)), element_chain(leaf))
}

matched_rules_of <- function(css, leaf) {
  vapply(matches_for(css, leaf), function(m) m$rule_index, 0L)
}

test_that("the critical case: A > B C matches C below A > B > B", {
  expect_equal(matched_rules_of("A > B C { width: 1; }", tree("A", "B", "B", "C")), 1L)
  expect_equal(matched_rules_of("A > B C { width: 1; }", tree("A", "B", "C")), 1L)
})

test_that("only a more distant ancestor may be the valid candidate", {
  css <- "A > B C { width: 1; }"
  # Nearest B has a B parent (invalid), the outer B has the A parent (valid).
  expect_equal(matched_rules_of(css, tree("A", "B", "B", "C")), 1L)
  expect_equal(matched_rules_of(css, tree("A", "B", "B", "B", "C")), 1L)
  # Nearest B valid, and also when other things sit in between.
  expect_equal(matched_rules_of(css, tree("A", "B", "X", "Y", "C")), 1L)
  # No B has an A parent.
  expect_equal(matched_rules_of(css, tree("B", "B", "C")), integer())
  expect_equal(matched_rules_of(css, tree("A", "X", "B", "C")), integer())
  expect_equal(matched_rules_of(css, tree("A", "X", "B", "B", "C")), integer())
  expect_equal(matched_rules_of(css, tree("B", "A", "C")), integer())
})

test_that("child combinators need the immediate parent's state", {
  expect_equal(matched_rules_of("A > B > C { width: 1; }", tree("A", "B", "C")), 1L)
  expect_equal(matched_rules_of("A > B > C { width: 1; }", tree("A", "X", "B", "C")), integer())
  expect_equal(matched_rules_of("A > B > C { width: 1; }", tree("A", "B", "X", "C")), integer())
  expect_equal(matched_rules_of("A B > C { width: 1; }", tree("A", "X", "B", "C")), 1L)
  expect_equal(matched_rules_of("A B > C { width: 1; }", tree("A", "B", "X", "C")), integer())
  expect_equal(matched_rules_of("A B > C { width: 1; }", tree("B", "A", "B", "C")), 1L)
})

test_that("descendant combinators accept any depth", {
  css <- "Screen Button { width: 1; }"
  expect_equal(matched_rules_of(css, tree("Screen", "Button")), 1L)
  expect_equal(matched_rules_of(css, tree("Screen", "A", "B", "C", "D", "Button")), 1L)
  expect_equal(matched_rules_of(css, tree("A", "Button")), integer())
  expect_equal(matched_rules_of(css, tree("Button", "Screen")), integer())
})

test_that("an element is never its own ancestor", {
  expect_equal(matched_rules_of("A A { width: 1; }", ink_element("A")), integer())
  expect_equal(matched_rules_of("A A { width: 1; }", tree("A", "A")), 1L)
  expect_equal(matched_rules_of("A A { width: 1; }", tree("A", "X", "A")), 1L)
  expect_equal(matched_rules_of("A > A { width: 1; }", ink_element("A")), integer())
  expect_equal(matched_rules_of("A > A { width: 1; }", tree("A", "X", "A")), integer())
  expect_equal(matched_rules_of("A A A { width: 1; }", tree("A", "A")), integer())
  expect_equal(matched_rules_of("A A A { width: 1; }", tree("A", "A", "A")), 1L)
  expect_equal(matched_rules_of("* * { width: 1; }", ink_element("A")), integer())
  expect_equal(matched_rules_of("* > * { width: 1; }", tree("A", "B")), 1L)
})

test_that("a state computed for the parent is what the child continues from", {
  prepared <- prepare_selectors(compile_text("A > B C { width: 1; }"))
  a <- match_element(prepared, ink_element("A"), NULL)
  b <- match_element(prepared, ink_element("B"), a$state)
  # B has an A parent, so the prefix `A > B` has now matched here and below.
  expect_true(any(b$state$self))
  inner_b <- match_element(prepared, ink_element("B"), b$state)
  # The inner B has a B parent: its own `self` is empty but `any` remembers.
  expect_false(inner_b$state$self[[2L]])
  expect_true(inner_b$state$any[[2L]])
  c <- match_element(prepared, ink_element("C"), inner_b$state)
  expect_length(c$matches, 1L)
})

test_that("matching state does not leak between unrelated branches", {
  prepared <- prepare_selectors(compile_text("A B { width: 1; }"))
  a <- match_element(prepared, ink_element("A"), NULL)
  under_a <- match_element(prepared, ink_element("B"), a$state)
  beside_a <- match_element(prepared, ink_element("B"), NULL)
  expect_length(under_a$matches, 1L)
  expect_length(beside_a$matches, 0L)
  # The first call did not alter the state object it was given.
  again <- match_element(prepared, ink_element("B"), a$state)
  expect_length(again$matches, 1L)
})

test_that("universal selectors, ids, classes and states take part in combinators", {
  css <- "#main .row:selected { width: 1; }\n* > .row { width: 2; }\n.panel * { width: 3; }"
  leaf <- tree(
    list(type = "Screen", id = "main"), list(type = "Panel", classes = "panel"),
    list(type = "Row", classes = "row", states = "selected")
  )
  expect_equal(matched_rules_of(css, leaf), 1:3)
  expect_equal(matched_rules_of(css, ink_element("Row", classes = "row")), integer())
})

test_that("selector lists keep per-selector specificity", {
  css <- "Button, #save { width: 1; }"
  expect_identical(
    matches_for(css, ink_element("Button", id = "save"))[[1L]]$specificity,
    c(id = 1L, class = 0L, type = 0L)
  )
  expect_identical(
    matches_for(css, ink_element("Button"))[[1L]]$specificity,
    c(id = 0L, class = 0L, type = 1L)
  )
})

test_that("matches come back in stylesheet order", {
  css <- "#a { width: 1; }\nA { width: 2; }\n.x { width: 3; }\n* { width: 4; }"
  expect_equal(matched_rules_of(css, ink_element("A", id = "a", classes = "x")), 1:4)
})

test_that("the optimized matcher equals the reference matcher on the golden-style sheets", {
  sheet <- compile_text("A > B C { width: 1; }\nA B > C { width: 2; }\nA > B > C { width: 3; }\n* C { width: 4; }\nB C, A > B { width: 5; }")
  for (leaf in list(tree("A", "B", "B", "C"), tree("A", "B", "C"), tree("B", "B", "C"),
                    tree("A", "X", "B", "C"), tree("C"), tree("A", "B"))) {
    chain <- element_chain(leaf)
    expect_identical(match_rules(sheet, chain), reference_match_rules(sheet, chain))
  }
})

test_that("ink_match uses the optimized matcher and still agrees with the reference", {
  sheet <- compile_text("A > B C { width: 1; }\nA B { width: 2; }")
  leaf <- tree("A", "B", "B", "C")
  expect_identical(ink_match(sheet, leaf)$matches, reference_match_rules(sheet, element_chain(leaf)))
})

test_that("deep chains are matched iteratively", {
  sheet <- compile_text("Screen Filler { width: 1; }\nScreen > Filler { width: 2; }\nFiller > Filler > Leaf { width: 3; }")
  leaf <- deep_chain(3000L, "Leaf", root = ink_element("Screen"))
  matches <- match_chain(prepare_selectors(sheet), element_chain(leaf))
  expect_equal(vapply(matches, function(m) m$rule_index, 0L), 3L)
  filler <- deep_chain(3000L, "Filler", root = ink_element("Screen"))
  expect_equal(
    vapply(match_chain(prepare_selectors(sheet), element_chain(filler)), function(m) m$rule_index, 0L),
    1L
  )
})
