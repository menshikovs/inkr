# Selector matching.
#
# Selectors are matched directly on the parsed selector AST, against an
# element and its parent chain. Nothing here converts selectors to text.
#
# A complex selector `c1 b1 c2 b2 c3` is matched right to left. `c3` must match
# the target element. Each combinator then relates the compound on its left to
# the compounds already matched on its right:
#
# * child (`>`): the left compound must match the *parent* of an element that
#   matched the right compound;
# * descendant: it may match *any ancestor* of such an element.
#
# Mixing the two means that the nearest ancestor that matches a compound is not
# always the right one. For `A > B C` and the tree A > B > B > C, the nearest B
# above C has a B parent, not an A, but the farther B has the A parent.
# Committing to the nearest candidate (greedy matching) would wrongly fail.
#
# So the matcher keeps the *set* of all chain positions at which the current
# compound can match, and advances that set one compound at a time. This is a
# dynamic program: each (compound, ancestor) pair is tested at most once, the
# cost is O(compounds x depth), and no backtracking or recursion is needed.

# Validation -----------------------------------------------------------------

abort_selector <- function(detail) {
  abort_match("invalid_selector", detail, "selector")
}

# Checks the shape of a complex selector AST node. It does not look at spans.
validate_complex_selector <- function(selector) {
  if (!inherits(selector, "ink_complex_selector")) {
    abort_selector(sprintf(
      "`selector` must be an ink_complex_selector, not %s.", describe_input(selector)
    ))
  }
  compounds <- selector$compounds
  combinators <- selector$combinators
  if (!is.list(compounds) || length(compounds) == 0L) {
    abort_selector("The selector has no compound selectors.")
  }
  if (!is.character(combinators) || length(combinators) != length(compounds) - 1L ||
    !all(combinators %in% c("descendant", "child"))) {
    abort_selector("The selector's combinators are malformed.")
  }
  for (compound in compounds) {
    validate_compound_selector(compound)
  }
  invisible(selector)
}

validate_compound_selector <- function(compound) {
  valid <- inherits(compound, "ink_compound_selector") &&
    (is.null(compound$type) || is_plain_string(compound$type)) &&
    (is.null(compound$id) || is_plain_string(compound$id)) &&
    is.character(compound$classes) && !anyNA(compound$classes) &&
    is.character(compound$pseudo_classes) && !anyNA(compound$pseudo_classes)
  if (!valid) {
    abort_selector("The selector contains a malformed compound selector.")
  }
  invisible(compound)
}

# Simple and compound selectors ---------------------------------------------

# Does `element` satisfy every condition of `compound`? All conditions must
# hold for the same element. Comparisons are exact and case-sensitive.
ink_match_compound <- function(compound, element) {
  validate_compound_selector(compound)
  if (!is_well_formed_element(element)) {
    abort_match("invalid_element", "`element` is not a valid ink_element.", "element")
  }
  compound_matches(compound, element)
}

# The unchecked core of ink_match_compound(), for use after validation.
compound_matches <- function(compound, element) {
  type <- compound$type
  id <- compound$id
  (is.null(type) || type == "*" || type == element$type) &&
    (is.null(id) || identical(id, element$id)) &&
    all(compound$classes %in% element$classes) &&
    all(compound$pseudo_classes %in% element$states)
}

# Complex selectors ----------------------------------------------------------

# Does the selector match `element`? The element's ancestors are inspected as
# needed; the element tree is never modified.
ink_match_complex <- function(selector, element) {
  validate_complex_selector(selector)
  complex_matches(selector, element_chain(element))
}

# `chain` is element_chain(): the target first, then its ancestors, nearest
# first. The selector must already be validated.
complex_matches <- function(selector, chain) {
  compounds <- selector$compounds
  last <- length(compounds)
  if (!compound_matches(compounds[[last]], chain[[1L]])) {
    return(FALSE)
  }
  # Chain positions where the compound just processed matches, in increasing
  # order (position 1 is the target).
  positions <- 1L
  depth <- length(chain)
  for (k in rev(seq_len(last - 1L))) {
    candidates <- candidate_positions(selector$combinators[[k]], positions, depth)
    positions <- candidates[vapply(
      candidates,
      function(j) compound_matches(compounds[[k]], chain[[j]]),
      logical(1)
    )]
    if (length(positions) == 0L) {
      return(FALSE)
    }
  }
  TRUE
}

# Chain positions that may hold the compound to the left of `combinator`, given
# the positions matched for the compound to its right.
candidate_positions <- function(combinator, positions, depth) {
  if (combinator == "child") {
    candidates <- positions + 1L
    return(candidates[candidates <= depth])
  }
  # Descendant: any position above the nearest matched one.
  first <- positions[[1L]] + 1L
  if (first > depth) integer() else first:depth
}
