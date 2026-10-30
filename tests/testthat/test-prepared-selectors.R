test_that("prepare_selectors builds an ink_prepared_selectors", {
  prepared <- prepare_selectors(compile_text("Button { color: red; }\n.a, #b { color: blue; }"))
  expect_s3_class(prepared, "ink_prepared_selectors")
  expect_equal(prepared$n_rules, 2L)
  expect_equal(prepared$n_selectors, 3L)
})

test_that("rule and selector indices are kept, in stylesheet order", {
  prepared <- prepare_selectors(compile_text("A { width: 1; }\nB, C, D { width: 2; }\nE { width: 3; }"))
  expect_equal(prepared$rule_index, c(1L, 2L, 2L, 2L, 3L))
  expect_equal(prepared$selector_index, c(1L, 1L, 2L, 3L, 1L))
})

test_that("specificity tuples are exactly those of ink_specificity", {
  sheet <- compile_text("#a > B.c:d, E F { width: 1; }\n* { width: 2; }\n.x.y.z { width: 3; }")
  prepared <- prepare_selectors(sheet)
  expected <- unlist(
    lapply(sheet$rules, function(rule) lapply(rule$selectors$selectors, ink_specificity)),
    recursive = FALSE
  )
  expect_identical(prepared$specificity, expected)
})

test_that("the original rules and ASTs are retained, not replaced", {
  sheet <- compile_text("Screen > Container Button.primary:focus { color: red; }")
  prepared <- prepare_selectors(sheet)
  expect_identical(prepared$rules, sheet$rules)
  expect_identical(prepared$last[[1L]], sheet$rules[[1L]]$selectors$selectors[[1L]]$compounds[[3L]])
  leaf <- ink_element("Button", classes = "primary", states = "focus",
    parent = ink_element("Container", parent = ink_element("Screen")))
  expect_length(match_chain(prepared, element_chain(leaf)), 1L)
})

test_that("a selector is split into compounds and combinators", {
  prepared <- prepare_selectors(compile_text("Screen > Container Button.primary { color: red; }"))
  # Two prefix compounds (Screen, Container) and one rightmost compound.
  expect_equal(prepared$n_slots, 2L)
  expect_equal(prepared$slot_first, c(TRUE, FALSE))
  expect_equal(prepared$slot_child, c(FALSE, TRUE))
  expect_equal(prepared$slot_src, c(1L, 1L))
  expect_equal(prepared$parent_slot, 2L)
  expect_false(prepared$last_child)
  expect_equal(format_compound_selector(prepared$prefix_compounds[[1L]]), "Screen")
  expect_equal(format_compound_selector(prepared$last[[1L]]), "Button.primary")
})

test_that("single-compound selectors need no matching state", {
  prepared <- prepare_selectors(compile_text("A { width: 1; } .b, #c, :d, * { width: 2; }"))
  expect_equal(prepared$n_slots, 0L)
  expect_equal(prepared$parent_slot, rep(0L, 5L))
  expect_null(match_element(prepared, ink_element("A"), NULL)$state)
})

test_that("identical prefix compounds are shared", {
  prepared <- prepare_selectors(compile_text("A B { width: 1; } A > C { width: 2; } A.x D { width: 3; } B E { width: 4; }"))
  # Prefix compounds: A, A, A.x, B -> three distinct ones.
  expect_equal(prepared$n_slots, 4L)
  expect_length(prepared$prefix_compounds, 3L)
  # Equal compounds in a different order of classes are the same compound.
  prepared <- prepare_selectors(compile_text(".a.b X { width: 1; } .b.a Y { width: 2; }"))
  expect_length(prepared$prefix_compounds, 1L)
})

test_that("preparing an empty or variables-only stylesheet works", {
  for (css in c("", "$a: red;")) {
    prepared <- prepare_selectors(compile_text(css))
    expect_equal(prepared$n_selectors, 0L)
    expect_length(match_chain(prepared, list(ink_element("A"))), 0L)
  }
})

test_that("preparing does not modify the stylesheet", {
  sheet <- compile_text("A B { width: 1; }\n.x, #y > C { width: 2; }")
  before <- sheet
  prepare_selectors(sheet)
  expect_identical(sheet, before)
})

test_that("malformed stylesheets are rejected with the matcher's structured errors", {
  sheet <- compile_text("A B { width: 1; }")
  bad <- sheet
  bad$rules[[1L]] <- 42
  expect_match_error(prepare_selectors(bad), "invalid_stylesheet")
  bad <- sheet
  bad$rules[[1L]]$selectors$selectors[[1L]]$combinators <- "sibling"
  expect_match_error(prepare_selectors(bad), "invalid_selector")
  bad <- sheet
  bad$rules[[1L]]$selectors$selectors[[1L]]$compounds[[1L]]$classes <- 1
  expect_match_error(prepare_selectors(bad), "invalid_selector")
  expect_match_error(prepare_selectors("A {}"), "invalid_stylesheet")
})

test_that("matching with prepared selectors returns ink_matched_rule objects like ink_match", {
  sheet <- compile_text("A { width: 1; }\nA, .x { width: 2; }\nB { width: 3; }")
  element <- ink_element("A", classes = "x")
  matches <- match_chain(prepare_selectors(sheet), list(element))
  expect_identical(matches, ink_match(sheet, element)$matches)
  expect_s3_class(matches[[1L]], "ink_matched_rule")
  expect_equal(matches[[2L]]$selector_indices, 1:2)
  expect_equal(matches[[2L]]$winning_selector_index, 2L)
})
