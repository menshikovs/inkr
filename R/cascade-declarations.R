# Declaration collection.
#
# Turns the rules matched for one element into a flat list of cascade
# candidates, one per declaration, in stylesheet source order. A candidate is
#
#   list(property, value, specificity, rule_index, declaration_index,
#        selector_index, span)
#
# `specificity` and `selector_index` come from the match result: they are those
# of the most specific selector of the rule that actually matched the element.
# Every rule is represented once, so a selector list never duplicates
# declarations.

collect_declarations <- function(matches, registry) {
  candidates <- list()
  for (matched in matches) {
    check_matched_rule(matched)
    declarations <- matched$rule$declarations
    for (index in seq_along(declarations)) {
      declaration <- declarations[[index]]
      check_declaration(declaration, registry, matched$rule_index, index)
      candidates[[length(candidates) + 1L]] <- list(
        property = declaration[["name"]],
        value = declaration[["value"]],
        specificity = matched$specificity,
        rule_index = matched$rule_index,
        declaration_index = as.integer(index),
        selector_index = matched$winning_selector_index,
        span = declaration$span
      )
    }
  }
  candidates
}

check_matched_rule <- function(matched) {
  valid <- inherits(matched, "ink_matched_rule") &&
    is.integer(matched$rule_index) && length(matched$rule_index) == 1L &&
    is.integer(matched$specificity) && length(matched$specificity) == 3L &&
    is.integer(matched$winning_selector_index) &&
    is.list(matched$rule) && is.list(matched$rule$declarations)
  if (!isTRUE(valid)) {
    abort_compute("invalid_cascade_result", "A matched rule has an unexpected structure.")
  }
}

check_declaration <- function(declaration, registry, rule_index, index) {
  where <- sprintf("declaration %d of rule %d", index, rule_index)
  if (!is.list(declaration) || !is_plain_string(declaration[["name"]]) || is.null(declaration[["value"]])) {
    abort_compute("invalid_cascade_result", sprintf("The %s is malformed.", where))
  }
  if (is.null(registry$properties[[declaration[["name"]]]])) {
    abort_compute(
      "invalid_cascade_result",
      sprintf(
        "The %s sets '%s', which is not in the stylesheet's registry.",
        where, declaration[["name"]]
      )
    )
  }
}
