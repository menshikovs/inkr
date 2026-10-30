# The optimized matcher against two independent references on random inputs:
#
# * reference_match_rules(): the original per-rule, per-selector matcher;
# * expected_matches(): an exhaustive oracle that tries every ancestor for every
#   combinator (helper-incremental.R).

test_that("the optimized matcher equals the original matcher on random stylesheets and trees", {
  set.seed(1234)
  different <- 0L
  nonempty <- 0L
  for (trial in seq_len(300L)) {
    sheet <- compile_text(random_sheet_css())
    chain <- element_chain(random_chain())
    optimized <- match_rules(sheet, chain)
    original <- reference_match_rules(sheet, chain)
    if (!identical(optimized, original)) different <- different + 1L
    nonempty <- nonempty + (length(original) > 0L)
  }
  expect_equal(different, 0L)
  # The comparison is only meaningful if the inputs often match something.
  expect_gt(nonempty, 120L)
  expect_lt(nonempty, 300L)
})

test_that("the optimized matcher equals the exhaustive oracle", {
  set.seed(77)
  different <- character()
  for (trial in seq_len(300L)) {
    css <- random_sheet_css(sample(2:10, 1L))
    sheet <- compile_text(css)
    leaf <- random_chain(sample(1:9, 1L))
    chain <- element_chain(leaf)
    got <- summarize_matches(match_rules(sheet, chain))
    want <- expected_matches(sheet, chain)
    if (!identical(got, want)) different <- c(different, css)
  }
  expect_equal(different, character())
})

test_that("every ancestor of a random tree is matched consistently", {
  # Matching an ancestor on its own must agree with its place in a longer chain.
  set.seed(5)
  problems <- 0L
  for (trial in seq_len(60L)) {
    sheet <- compile_text(random_sheet_css())
    leaf <- random_chain(sample(2:8, 1L))
    prepared <- prepare_selectors(sheet)
    chain <- element_chain(leaf)
    state <- NULL
    for (position in rev(seq_along(chain))) {
      step <- match_element(prepared, chain[[position]], state)
      state <- step$state
      alone <- match_rules(sheet, chain[position:length(chain)])
      if (!identical(step$matches, alone)) problems <- problems + 1L
    }
  }
  expect_equal(problems, 0L)
})

test_that("selector lists, ties and ids behave like the oracle", {
  set.seed(11)
  css <- paste(
    ".x, .y { width: 1; }", "A, #i, A.x { width: 2; }", "A B, B > C, * { width: 3; }",
    "C:s, .x .y, #i C { width: 4; }", sep = "\n"
  )
  sheet <- compile_text(css)
  different <- 0L
  for (trial in seq_len(150L)) {
    chain <- element_chain(random_chain(sample(1:7, 1L)))
    if (!identical(summarize_matches(match_rules(sheet, chain)), expected_matches(sheet, chain))) {
      different <- different + 1L
    }
  }
  expect_equal(different, 0L)
})

test_that("optimized and original matching agree on cascade results, provenance and explanations", {
  set.seed(8)
  for (trial in seq_len(25L)) {
    css <- paste(
      random_sheet_css(), "A { color: red; }", ".x { color: blue; padding: 1; }",
      "A B { color: inherit; }", "B > C { background: initial; }", sep = "\n"
    )
    sheet <- compile_text(css)
    leaf <- random_chain(sample(1:7, 1L))
    chain <- element_chain(leaf)
    style <- ink_compute(sheet, leaf)
    # Recompute by hand with the reference matcher feeding the same cascade.
    registry <- sheet$registry
    parent_values <- NULL
    resolved <- NULL
    for (position in rev(seq_along(chain))) {
      matches <- reference_match_rules(sheet, chain[position:length(chain)])
      resolved <- resolve_element_style(registry, ink_cascade(matches, registry), parent_values)
      parent_values <- resolved$values
    }
    expect_identical(style$values, resolved$values)
    expect_identical(style$provenance, resolved$provenance)
    for (property in c("color", "background", "padding")) {
      explanation <- ink_explain(sheet, leaf, property)
      expect_identical(explanation$value, resolved$values[[property]])
      expect_identical(explanation$origin, resolved$provenance[[property]]$origin)
    }
  }
})
