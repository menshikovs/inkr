# Candidate collection for ink_explain().
#
# The declarations that competed for one property on the target element are the
# cascade's own candidates (collect_declarations(), the function the cascade
# consumes), filtered to that property and enriched with what the match result
# knows about their rule. The winner is not chosen here: it is the winner the
# cascade already picked while the style was computed. This file only
# classifies the other candidates relative to that winner.
#
# A candidate (class `ink_explanation_candidate`) has:
#
#   property, rule_index, declaration_index,
#   selector_index              the matching selector with the highest
#                               specificity (the one the cascade used),
#   matching_selector_indices   every selector of the rule that matched,
#   specificity                 c(id, class, type) of `selector_index`,
#   selector                    that selector's AST node,
#   value                       the typed declaration value (may be a keyword),
#   raw                         the declaration's value text as written,
#   span                        the declaration's source span,
#   status                      "winner" or "overridden",
#   reason                      NULL for the winner, else "lower_specificity",
#                               "earlier_rule" or "earlier_declaration".
#
# Candidates are returned in stylesheet source order (rule, then declaration),
# never sorted by priority.

explanation_candidates <- function(matches, registry, property, winner) {
  collected <- collect_declarations(matches, registry)
  is_property <- vapply(collected, function(candidate) identical(candidate$property, property), NA)
  candidates <- collected[is_property]
  if (length(candidates) == 0L) {
    if (!is.null(winner)) {
      abort_explain(
        "invalid_explanation_state",
        sprintf("The cascade chose a winner for '%s' but no declaration for it matched.", property)
      )
    }
    return(list())
  }
  if (is.null(winner)) {
    abort_explain(
      "invalid_explanation_state",
      sprintf("Declarations for '%s' matched but the cascade chose no winner.", property)
    )
  }
  rule_positions <- vapply(matches, function(matched) matched$rule_index, 0L)
  found_winner <- FALSE
  explained <- lapply(candidates, function(candidate) {
    matched <- matches[[match(candidate$rule_index, rule_positions)]]
    is_winner <- same_declaration(candidate, winner)
    found_winner <<- found_winner || is_winner
    new_explanation_candidate(candidate, matched, if (is_winner) NULL else override_reason(candidate, winner))
  })
  if (!found_winner) {
    abort_explain(
      "invalid_explanation_state",
      sprintf("The winning declaration for '%s' is not among the matched declarations.", property)
    )
  }
  explained
}

same_declaration <- function(a, b) {
  identical(a$rule_index, b$rule_index) && identical(a$declaration_index, b$declaration_index)
}

new_explanation_candidate <- function(candidate, matched, reason) {
  declaration <- matched$rule$declarations[[candidate$declaration_index]]
  structure(
    list(
      property = candidate$property,
      rule_index = candidate$rule_index,
      declaration_index = candidate$declaration_index,
      selector_index = candidate$selector_index,
      matching_selector_indices = matched$selector_indices,
      specificity = candidate$specificity,
      selector = matched$rule$selectors$selectors[[candidate$selector_index]],
      value = candidate$value,
      raw = declaration[["raw"]],
      span = candidate$span,
      status = if (is.null(reason)) "winner" else "overridden",
      reason = reason
    ),
    class = "ink_explanation_candidate"
  )
}

# Why `candidate` lost to `winner`, using the same three comparisons, in the
# same order, as the cascade (compare_priority()). Anything that does not lose
# by priority is a contradiction, not a case to explain away.
override_reason <- function(candidate, winner) {
  if (compare_priority(candidate, winner) != -1L) {
    abort_explain(
      "invalid_explanation_state",
      sprintf(
        "Declaration %d of rule %d for '%s' does not rank below the recorded winner.",
        candidate$declaration_index, candidate$rule_index, candidate$property
      )
    )
  }
  if (compare_specificity(candidate$specificity, winner$specificity) < 0L) {
    return("lower_specificity")
  }
  if (candidate$rule_index < winner$rule_index) {
    return("earlier_rule")
  }
  "earlier_declaration"
}
