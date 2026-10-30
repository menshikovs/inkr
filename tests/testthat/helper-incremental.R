# Helpers for the optimized-matching and session tests.

# --- An independent oracle for selector matching ------------------------------
#
# Deliberately naive: for each combinator it simply tries *every* candidate
# ancestor, recursively. It shares nothing with the engine except the
# single-compound test compound_matches() (itself tested separately) and the
# AST. `chain` is the target first, then its ancestors.

exhaustive_selector_match <- function(selector, chain) {
  compounds <- selector$compounds
  combinators <- selector$combinators
  go <- function(k, i) {
    if (!compound_matches(compounds[[k]], chain[[i]])) {
      return(FALSE)
    }
    if (k == 1L) {
      return(TRUE)
    }
    parents <- if (combinators[[k - 1L]] == "child") {
      i + 1L
    } else if (i < length(chain)) {
      seq.int(i + 1L, length(chain))
    } else {
      integer()
    }
    parents <- parents[parents <= length(chain)]
    any(vapply(parents, function(j) go(k - 1L, j), NA))
  }
  go(length(compounds), 1L)
}

# True if tuple `a` is lexicographically more specific than `b`.
more_specific <- function(a, b) {
  differing <- which(a != b)
  length(differing) > 0L && a[[differing[[1L]]]] > b[[differing[[1L]]]]
}

# What the matcher must return for `chain`, derived with the oracle: a list with
# one entry per matching rule (rule_index, selector_indices, winner, specificity).
expected_matches <- function(sheet, chain) {
  expected <- list()
  for (r in seq_along(sheet$rules)) {
    selectors <- sheet$rules[[r]]$selectors$selectors
    hits <- which(vapply(selectors, exhaustive_selector_match, NA, chain = chain))
    if (length(hits) == 0L) next
    winner <- hits[[1L]]
    for (j in hits[-1L]) {
      if (more_specific(ink_specificity(selectors[[j]]), ink_specificity(selectors[[winner]]))) winner <- j
    }
    expected[[length(expected) + 1L]] <- list(
      rule_index = r, selector_indices = as.integer(hits), winner = as.integer(winner),
      specificity = ink_specificity(selectors[[winner]])
    )
  }
  expected
}

summarize_matches <- function(matches) {
  lapply(matches, function(m) {
    list(
      rule_index = m$rule_index, selector_indices = m$selector_indices,
      winner = m$winning_selector_index, specificity = m$specificity
    )
  })
}

# --- Random inputs (use set.seed() in the test) --------------------------------

SELECTOR_POOL <- c(
  "A", "B", "C", "*", ".x", ".y", "#i", ":s", "A.x", "B.y", "C:s", "A:s.x", "#i.x", "*.y",
  "A B", "A > B", "A B C", "A > B C", "A B > C", "A > B > C", "B A", "A A", "A > A", "A A A",
  "A > A > A", ".x .y", ".x > .y", "A.x B.y", "A.x > B.y C", "* C", "* > C", "A * C", "A > * > C",
  "#i C", "#i > C", "B:s C", "A, B", "A > B, C", ".x, #i", "A B, B > C, *", "A:s B, A > B.y"
)

random_sheet_css <- function(rules = sample(2:8, 1L)) {
  paste(vapply(seq_len(rules), function(i) {
    sprintf("%s { width: %d; }", paste(sample(SELECTOR_POOL, sample(1:2, 1L)), collapse = ", "), i)
  }, ""), collapse = "\n")
}

random_chain <- function(depth = sample(1:8, 1L)) {
  parent <- NULL
  for (i in seq_len(depth)) {
    parent <- ink_element(
      sample(c("A", "B", "C"), 1L),
      id = if (runif(1L) < 0.15) "i" else NULL,
      classes = c("x", "y")[runif(2L) < 0.35],
      states = "s"[runif(1L) < 0.3],
      parent = parent
    )
  }
  parent
}
