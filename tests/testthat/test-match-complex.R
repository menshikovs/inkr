test_that("a child combinator needs the direct parent", {
  expect_true(selector_matches("Container > Button", tree("Container", "Button")))
  expect_false(selector_matches("Container > Button", tree("Container", "Panel", "Button")))
  expect_false(selector_matches("Container > Button", tree("Panel", "Button")))
  expect_false(selector_matches("Container > Button", ink_element("Button")))
  expect_false(selector_matches("Container > Button", tree("Container", "Input")))
})

test_that("a descendant combinator accepts any depth", {
  expect_true(selector_matches("Container Button", tree("Container", "Button")))
  expect_true(selector_matches("Container Button", tree("Container", "Panel", "Button")))
  expect_true(selector_matches("Container Button", tree("Container", "P", "Q", "R", "Button")))
  expect_false(selector_matches("Container Button", tree("Panel", "Button")))
  expect_false(selector_matches("Container Button", ink_element("Button")))
})

test_that("the ancestor must be above the element, not below or beside it", {
  expect_false(selector_matches("Container Button", tree("Button", "Container")))
  expect_false(selector_matches("Container Button", ink_element("Container")))
})

test_that("only the target element has to match the last compound", {
  expect_false(selector_matches("A B", tree("A", "B", "C")))
  expect_true(selector_matches("A B", tree("A", "B")))
})

test_that("several ancestor levels are matched in order", {
  expect_true(selector_matches("A B C D", tree("A", "x", "B", "y", "C", "z", "D")))
  expect_false(selector_matches("A B C D", tree("B", "A", "C", "D")))
  expect_false(selector_matches("A B C D", tree("A", "B", "D")))
  expect_true(selector_matches("A > B > C", tree("A", "B", "C")))
  expect_false(selector_matches("A > B > C", tree("A", "x", "B", "C")))
  expect_false(selector_matches("A > B > C", tree("A", "B", "x", "C")))
})

test_that("mixed combinators follow the relationships in the selector", {
  button <- list(type = "Button", classes = "primary")
  expect_true(selector_matches("Screen > Container Button.primary", tree("Screen", "Container", "Panel", button)))
  expect_true(selector_matches("Screen > Container Button.primary", tree("Screen", "Container", button)))
  expect_false(selector_matches("Screen > Container Button.primary", tree("Screen", "Wrapper", "Container", button)))
  expect_false(selector_matches("Screen > Container Button.primary", tree("Screen", "Container", "Button")))

  focused <- list(type = "Button", states = "focus")
  expect_true(selector_matches("Screen Container > Button:focus", tree("Screen", "X", "Container", focused)))
  expect_true(selector_matches("Screen Container > Button:focus", tree("Screen", "Container", focused)))
  expect_false(selector_matches("Screen Container > Button:focus", tree("Screen", "Container", "X", focused)))
  expect_false(selector_matches("Screen Container > Button:focus", tree("Screen", "Container", "Button")))
})

test_that("ancestors are matched with the full compound selector", {
  focused_container <- list(type = "Container", states = "focus")
  expect_true(selector_matches("Container:focus Button", tree(focused_container, "Panel", "Button")))
  expect_false(selector_matches("Container:focus Button", tree("Container", "Panel", "Button")))
  expect_true(selector_matches("#main .row", tree(list(type = "Screen", id = "main"), list(type = "Row", classes = "row"))))
  expect_false(selector_matches("#main .row", tree(list(type = "Screen", id = "other"), list(type = "Row", classes = "row"))))
  expect_true(selector_matches("* > Button", tree("Anything", "Button")))
  expect_true(selector_matches("* Button", tree("Anything", "Button")))
  expect_false(selector_matches("* Button", ink_element("Button")))
})

test_that("a selector without combinators matches at the root", {
  expect_true(selector_matches("Screen", ink_element("Screen")))
  expect_true(selector_matches("Screen", tree("Screen")))
  expect_false(selector_matches("Screen > Button", ink_element("Screen")))
  expect_false(selector_matches("Screen Screen", ink_element("Screen")))
})

test_that("repeated type names are handled", {
  expect_true(selector_matches("B B", tree("B", "B")))
  expect_false(selector_matches("B B", tree("B")))
  expect_true(selector_matches("B B", tree("B", "x", "B")))
  expect_true(selector_matches("B > B > B", tree("B", "B", "B")))
  expect_false(selector_matches("B > B > B", tree("B", "B")))
  expect_false(selector_matches("B > B > B", tree("B", "B", "x", "B")))
})

test_that("the nearest candidate ancestor is not always the right one", {
  # The regression case: A > B C against A > B > B > C. The nearest B above C
  # has a B parent, but the farther B has the A parent.
  expect_true(selector_matches("A > B C", tree("A", "B", "B", "C")))
  expect_true(selector_matches("A > B C", tree("A", "B", "C")))
  expect_true(selector_matches("A > B C", tree("A", "B", "X", "C")))
  expect_true(selector_matches("A > B C", tree("A", "B", "B", "B", "C")))
  expect_true(selector_matches("A > B C", tree("A", "B", "X", "B", "C")))

  expect_false(selector_matches("A > B C", tree("B", "B", "C")))
  expect_false(selector_matches("A > B C", tree("A", "X", "B", "C")))
  expect_false(selector_matches("A > B C", tree("A", "X", "B", "B", "C")))
  expect_false(selector_matches("A > B C", tree("B", "A", "C")))
  expect_false(selector_matches("A > B C", tree("A", "B")))
})

test_that("other combinations that need alternative candidates match", {
  expect_true(selector_matches("A B > C", tree("A", "B", "B", "C")))
  expect_true(selector_matches("A B > C", tree("B", "A", "B", "C")))
  expect_false(selector_matches("A B > C", tree("A", "B", "X", "C")))
  expect_true(selector_matches("A > B > C D", tree("A", "B", "C", "C", "D")))
  expect_false(selector_matches("A > B > C D", tree("A", "X", "B", "C", "C", "D")))
  expect_true(selector_matches("A > B C > D", tree("A", "B", "B", "C", "D")))
  expect_false(selector_matches("A > B C > D", tree("A", "B", "B", "C", "X", "D")))
  expect_true(selector_matches("A B C > D", tree("A", "C", "B", "C", "D")))
})

test_that("deep parent chains are matched without recursion limits", {
  top <- ink_element("Top")
  leaf <- deep_chain(2500L, "Leaf", root = top)
  expect_true(selector_matches("Top Leaf", leaf))
  expect_false(selector_matches("Top > Leaf", leaf))
  expect_true(selector_matches("Filler > Leaf", leaf))
  expect_true(selector_matches("Top Filler Filler Leaf", leaf))
  expect_false(selector_matches("Missing Leaf", leaf))
  expect_true(selector_matches("Top Filler > Leaf", leaf))
  expect_false(selector_matches("Top > Filler > Leaf", leaf))
  # A long descendant chain of selectors against a deep tree.
  expect_true(selector_matches(paste(c(rep("Filler", 40L), "Leaf"), collapse = " "), leaf))
})

test_that("matching a complex selector does not modify the element", {
  leaf <- tree("A", "B", "B", "C")
  before <- leaf
  selector_matches("A > B C", leaf)
  expect_identical(leaf, before)
})

test_that("repeated matching gives the same answer", {
  leaf <- tree("A", "B", "B", "C")
  answers <- vapply(1:5, function(i) selector_matches("A > B C", leaf), NA)
  expect_true(all(answers))
})

test_that("the complex matcher rejects invalid input", {
  expect_match_error(ink_match_complex(NULL, ink_element("A")), "invalid_selector")
  expect_match_error(ink_match_complex(compound_of("A"), ink_element("A")), "invalid_selector")
  expect_match_error(ink_match_complex(selector_of("A"), "A"), "invalid_element")
  bad <- selector_of("A B")
  bad$combinators <- "sibling"
  expect_match_error(ink_match_complex(bad, ink_element("A")), "invalid_selector")
  bad <- selector_of("A B")
  bad$combinators <- character()
  expect_match_error(ink_match_complex(bad, ink_element("A")), "invalid_selector")
})

# Reference implementation: tries every ancestor for every combinator,
# recursively, with no cleverness. The matcher must agree with it.
reference_match <- function(selector, chain) {
  compounds <- selector$compounds
  combinators <- selector$combinators
  go <- function(k, i) {
    if (!compound_matches(compounds[[k]], chain[[i]])) {
      return(FALSE)
    }
    if (k == 1L) {
      return(TRUE)
    }
    parents <- if (combinators[[k - 1L]] == "child") i + 1L else if (i < length(chain)) seq.int(i + 1L, length(chain)) else integer()
    parents <- parents[parents <= length(chain)]
    any(vapply(parents, function(j) go(k - 1L, j), NA))
  }
  go(length(compounds), 1L)
}

test_that("the matcher agrees with an exhaustive reference on random trees", {
  set.seed(7)
  compound_pool <- c("A", "B", "C", "*", ".x", ".y", "A.x", "B.y", ":s", "C:s", "A:s.x")
  random_element <- function(depth) {
    parent <- NULL
    for (i in seq_len(depth)) {
      parent <- ink_element(
        sample(c("A", "B", "C"), 1L),
        classes = c("x", "y")[runif(2L) < 0.4],
        states = c("s")[runif(1L) < 0.4],
        parent = parent
      )
    }
    parent
  }
  disagreements <- 0L
  matched <- 0L
  for (trial in seq_len(500L)) {
    size <- sample.int(4L, 1L)
    parts <- sample(compound_pool, size, replace = TRUE)
    joins <- sample(c(" ", " > "), size - 1L, replace = TRUE)
    text <- paste0(parts, c(joins, ""), collapse = "")
    selector <- selector_of(text)
    leaf <- random_element(sample.int(7L, 1L))
    expected <- reference_match(selector, element_chain(leaf))
    actual <- ink_match_complex(selector, leaf)
    matched <- matched + expected
    disagreements <- disagreements + (expected != actual)
  }
  expect_equal(disagreements, 0L)
  # The generator must produce both outcomes for the comparison to mean much.
  expect_gt(matched, 20L)
  expect_lt(matched, 480L)
})
