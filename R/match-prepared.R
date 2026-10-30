# Prepared selectors and incremental matching.
#
# Why the original matcher was slow on deep trees
# -----------------------------------------------
# ink_compute() processes an element tree from the root to the target. The first
# matcher (selector-match.R) decided each element independently: for every rule
# and every selector it walked the element's whole ancestor chain again. For a
# chain of D elements that is D elements x up to D ancestors each: O(D^2) work,
# and the same ancestor prefixes were re-scanned for every descendant.
#
# The algorithm used now
# ----------------------
# Matching state is carried down the chain instead of recomputed. Number the
# compounds of a selector c1 b1 c2 b2 ... cn (b_k joins c_k and c_k+1). For an
# element e with parent p, define for each prefix position k < n:
#
#   self[k](e) = e matches c_k and
#                  k = 1, or
#                  b_(k-1) is "child":       self[k-1](p)
#                  b_(k-1) is "descendant":  any[k-1](p)
#   any[k](e)  = self[k](e) or any[k](p)       "some ancestor-or-self matched"
#
# and a selector matches e when e matches c_n and its last combinator is
# satisfied by p's state: self[n-1](p) for child, any[n-1](p) for descendant.
# Because the condition reads the *parent's* state, an element is never its own
# ancestor (`A A` needs a strict A ancestor), and because `any` records every
# earlier match rather than the nearest one, `A > B C` against A > B > B > C
# still finds the outer B whose parent is A. Each state is two logical vectors,
# one entry per prefix compound of the stylesheet, advanced by a handful of
# vectorized operations per element: no recursion, no repeated ancestor scans.
#
# Cost: with S prefix compounds (U distinct) in the stylesheet and a chain of D
# elements, the state work is O(D x (U compound tests + S vector steps)), i.e.
# linear in D. The final (rightmost) compound of each selector is tested only
# for the candidates the selector index returns. It still grows with the
# stylesheet size, as it should; it no longer grows with the depth beyond D.
#
# The selector index
# ------------------
# A selector can only match an element that matches its rightmost compound, so
# selectors are filed under that compound: by ID if it has one, else by its
# first class, else by type, else in a fallback list (universal selectors and
# pure state selectors). Filing under any one required attribute can only
# over-approximate the candidates (a selector filed under `.a` is a candidate
# for every element with class `a`), so there are no false negatives; every
# candidate is still checked in full with compound_matches(). Candidates are
# merged in ascending (rule, selector) order, which keeps results in stylesheet
# order.
#
# Prepared selectors keep the AST nodes (rule and selector indices, specificity
# tuples and spans come from the compiled stylesheet unchanged). The old matcher
# in selector-match.R is retained, unused by the engine, as the reference
# implementation the tests compare against.

# A normalized, index-ready view of a compiled stylesheet's selectors. Validates
# every rule and selector once, signalling the same ink_match_errors as the
# reference matcher does.
prepare_selectors <- function(sheet) {
  validate_compiled_stylesheet(sheet)
  rules <- sheet$rules
  selector_lists <- vector("list", length(rules))
  total <- 0L
  slots <- 0L
  for (r in seq_along(rules)) {
    validate_compiled_rule(rules[[r]], r)
    selectors <- rules[[r]]$selectors$selectors
    for (selector in selectors) {
      validate_complex_selector(selector)
      slots <- slots + length(selector$compounds) - 1L
    }
    selector_lists[[r]] <- selectors
    total <- total + length(selectors)
  }

  rule_index <- integer(total)
  selector_index <- integer(total)
  parent_slot <- integer(total)
  last_child <- logical(total)
  last <- vector("list", total)
  specificity <- vector("list", total)
  slot_comp <- integer(slots)
  slot_first <- logical(slots)
  slot_child <- logical(slots)
  slot_src <- integer(slots)

  prefix_keys <- new.env(parent = emptyenv())
  prefix_compounds <- list()
  by_id <- new.env(parent = emptyenv())
  by_class <- new.env(parent = emptyenv())
  by_type <- new.env(parent = emptyenv())
  fallback <- integer()

  s <- 0L
  slot <- 0L
  for (r in seq_along(rules)) {
    selectors <- selector_lists[[r]]
    for (j in seq_along(selectors)) {
      selector <- selectors[[j]]
      s <- s + 1L
      compounds <- selector$compounds
      n <- length(compounds)
      rule_index[[s]] <- r
      selector_index[[s]] <- j
      specificity[[s]] <- selector_specificity(selector)
      last[[s]] <- compounds[[n]]
      for (k in seq_len(n - 1L)) {
        slot <- slot + 1L
        key <- compound_key(compounds[[k]])
        id <- prefix_keys[[key]]
        if (is.null(id)) {
          prefix_compounds[[length(prefix_compounds) + 1L]] <- compounds[[k]]
          id <- length(prefix_compounds)
          prefix_keys[[key]] <- id
        }
        slot_comp[[slot]] <- id
        slot_first[[slot]] <- k == 1L
        slot_child[[slot]] <- k > 1L && identical(selector$combinators[[k - 1L]], "child")
        slot_src[[slot]] <- if (k > 1L) slot - 1L else 1L
      }
      if (n > 1L) {
        parent_slot[[s]] <- slot
        last_child[[s]] <- identical(selector$combinators[[n - 1L]], "child")
      }
      final <- compounds[[n]]
      if (!is.null(final$id)) {
        index_add(by_id, final$id, s)
      } else if (length(final$classes) > 0L) {
        index_add(by_class, final$classes[[1L]], s)
      } else if (!is.null(final$type) && final$type != "*") {
        index_add(by_type, final$type, s)
      } else {
        fallback <- c(fallback, s)
      }
    }
  }

  structure(
    list(
      rules = rules,
      n_rules = length(rules),
      n_selectors = total,
      rule_index = rule_index,
      selector_index = selector_index,
      specificity = specificity,
      last = last,
      parent_slot = parent_slot,
      last_child = last_child,
      n_slots = slots,
      prefix_compounds = prefix_compounds,
      slot_comp = slot_comp,
      slot_first = slot_first,
      slot_child = slot_child,
      slot_src = slot_src,
      by_id = by_id,
      by_class = by_class,
      by_type = by_type,
      fallback = fallback,
      empty_state = list(self = logical(slots), any = logical(slots)),
      counters = new_match_counters()
    ),
    class = "ink_prepared_selectors"
  )
}

index_add <- function(index, key, selector) {
  index[[key]] <- c(index[[key]], selector)
}

# Two compounds with the same key match exactly the same elements. Only used
# while preparing; identifiers cannot contain "|" or ",".
compound_key <- function(compound) {
  paste(
    compound$type %||% "", compound$id %||% "",
    paste(sort(unique(compound$classes)), collapse = ","),
    paste(sort(unique(compound$pseudo_classes)), collapse = ","),
    sep = "|"
  )
}

# Operation counters, owned by the prepared object. They count work, not time,
# so tests can verify that depth no longer multiplies the matching effort.
new_match_counters <- function() {
  counters <- new.env(parent = emptyenv())
  counters$elements <- 0L
  counters$prefix_tests <- 0L
  counters$candidate_tests <- 0L
  counters
}

prepared_counters <- function(prepared) {
  as.list(prepared$counters)
}

# Selectors whose rightmost compound might match `element`, ascending.
index_candidates <- function(prepared, element) {
  candidates <- prepared$fallback
  if (!is.null(element$id)) {
    candidates <- c(candidates, prepared$by_id[[element$id]])
  }
  for (class in element$classes) {
    candidates <- c(candidates, prepared$by_class[[class]])
  }
  candidates <- c(candidates, prepared$by_type[[element$type]])
  if (length(candidates) > 1L) sort(unique(candidates)) else candidates
}

# Advances the matching state from the parent to `element`. `parent_state` is
# NULL for a root. Returns `list(matches, state)`: the matching rules of
# `element` (as match_rules() returns them) and the state to hand to its
# children.
match_element <- function(prepared, element, parent_state = NULL) {
  parent <- if (is.null(parent_state)) prepared$empty_state else parent_state
  state <- NULL
  if (prepared$n_slots > 0L) {
    matched <- vapply(prepared$prefix_compounds, compound_matches, logical(1), element = element)
    from_parent <- ifelse(
      prepared$slot_child,
      parent$self[prepared$slot_src],
      parent$any[prepared$slot_src]
    )
    self <- matched[prepared$slot_comp] & (prepared$slot_first | from_parent)
    state <- list(self = self, any = self | parent$any)
  }

  candidates <- index_candidates(prepared, element)
  hits <- integer()
  for (s in candidates) {
    if (!compound_matches(prepared$last[[s]], element)) {
      next
    }
    slot <- prepared$parent_slot[[s]]
    if (slot == 0L || (if (prepared$last_child[[s]]) parent$self[[slot]] else parent$any[[slot]])) {
      hits <- c(hits, s)
    }
  }

  counters <- prepared$counters
  counters$elements <- counters$elements + 1L
  counters$prefix_tests <- counters$prefix_tests + length(prepared$prefix_compounds)
  counters$candidate_tests <- counters$candidate_tests + length(candidates)
  list(matches = matched_rules(prepared, hits), state = state)
}

# Groups matching selectors (ascending flat indices) into one ink_matched_rule
# per rule, exactly as the reference matcher builds them.
matched_rules <- function(prepared, hits) {
  matches <- list()
  position <- 1L
  while (position <= length(hits)) {
    rule <- prepared$rule_index[[hits[[position]]]]
    selectors <- integer()
    winner <- NULL
    while (position <= length(hits) && prepared$rule_index[[hits[[position]]]] == rule) {
      s <- hits[[position]]
      selectors <- c(selectors, prepared$selector_index[[s]])
      # Strictly greater, so on a tie the earlier selector stays the winner.
      if (is.null(winner) || compare_specificity(prepared$specificity[[s]], prepared$specificity[[winner]]) > 0L) {
        winner <- s
      }
      position <- position + 1L
    }
    matches[[length(matches) + 1L]] <- structure(
      list(
        rule_index = as.integer(rule),
        rule = prepared$rules[[rule]],
        selector_indices = selectors,
        winning_selector_index = as.integer(prepared$selector_index[[winner]]),
        specificity = prepared$specificity[[winner]],
        span = prepared$rules[[rule]]$span
      ),
      class = "ink_matched_rule"
    )
  }
  matches
}

# The matching rules for the element at the head of `chain` (see
# element_chain(): the target first), carrying the matching state from the root.
match_chain <- function(prepared, chain) {
  state <- NULL
  result <- NULL
  for (position in rev(seq_along(chain))) {
    result <- match_element(prepared, chain[[position]], state)
    state <- result$state
  }
  result$matches
}
