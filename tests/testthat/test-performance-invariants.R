# Operation counts, not wall-clock time: they are deterministic, so they can
# prove that depth no longer multiplies the matching work without a fragile
# timing threshold. (Wall-clock numbers are in bench/benchmark-styles.R.)

# A chain of exactly `depth` elements: a Screen root and Fillers.
exact_chain <- function(depth) {
  deep_chain(depth - 1L, "Filler", root = ink_element("Screen"))
}

counters_after_compute <- function(css, depth) {
  sheet <- compile_text(css)
  prepared <- prepare_selectors(sheet)
  compute_style_path(sheet, element_chain(exact_chain(depth)), prepared = prepared)
  prepared_counters(prepared)
}

total_tests <- function(css, depth) {
  counters <- counters_after_compute(css, depth)
  counters$prefix_tests + counters$candidate_tests
}

descendant_css <- "Screen Filler { width: 1; }\nA > B C { width: 2; }\nA B > C { width: 3; }\nScreen > Filler { width: 4; }"

test_that("each element of a chain is matched exactly once", {
  for (depth in c(10L, 100L, 1000L)) {
    expect_equal(counters_after_compute(descendant_css, depth)$elements, depth)
  }
})

test_that("matching work grows linearly with depth: constant cost per extra element", {
  # The root and leaf may cost a little differently from the Fillers between
  # them, so compare the *marginal* cost of added elements. With linear scaling
  # it is the same wherever it is measured; with quadratic scaling it would grow.
  marginal <- function(from, to) (total_tests(descendant_css, to) - total_tests(descendant_css, from)) / (to - from)
  expect_equal(marginal(50L, 150L), marginal(150L, 600L))
  expect_equal(marginal(150L, 600L), marginal(600L, 1800L))
})

test_that("prefix compound tests are exactly proportional to the number of elements", {
  small <- counters_after_compute(descendant_css, 200L)
  large <- counters_after_compute(descendant_css, 800L)
  expect_equal(large$prefix_tests / large$elements, small$prefix_tests / small$elements)
})

test_that("the work per element is bounded by the stylesheet, not by the chain", {
  prepared <- prepare_selectors(compile_text(descendant_css))
  bound <- length(prepared$prefix_compounds) + prepared$n_selectors
  counters <- counters_after_compute(descendant_css, 600L)
  expect_lte(counters$prefix_tests + counters$candidate_tests, 600L * bound)
})

test_that("a longer stylesheet costs more per element, as it should", {
  few <- counters_after_compute("Screen Filler { width: 1; }", 100L)
  many <- counters_after_compute(
    paste(sprintf("Screen Filler.c%d { width: 1; }\nA%d B { width: 2; }", 1:20, 1:20), collapse = "\n"), 100L
  )
  expect_gt(many$prefix_tests, few$prefix_tests)
})

test_that("explaining a property matches each element once as well", {
  sheet <- compile_text(descendant_css)
  prepared <- prepare_selectors(sheet)
  explain_property(sheet, exact_chain(500L), "color", prepared = prepared)
  expect_equal(prepared_counters(prepared)$elements, 500L)
})

test_that("ink_match needs one element evaluation per chain element", {
  prepared <- prepare_selectors(compile_text(descendant_css))
  match_chain(prepared, element_chain(exact_chain(300L)))
  expect_equal(prepared_counters(prepared)$elements, 300L)
})

test_that("a long chain with a descendant selector computes correctly", {
  # Not a timing assertion: just a depth the original matcher needed seconds for.
  sheet <- compile_text("Screen { color: green; }\nScreen Filler { text-style: bold; }")
  style <- ink_compute(sheet, filler_chain(1500L))
  expect_equal(format(style$values$color), "green")
  expect_equal(format(style$values[["text-style"]]), "bold")
})
