#' Match a compiled stylesheet against an element
#'
#' `ink_match()` finds the rules of a compiled stylesheet whose selectors match
#' an element. It selects *applicable rules* only. It does not decide which
#' declaration wins when rules conflict, compute styles, inherit values or
#' resolve `inherit` / `initial`: for `Button { color: red; }` and
#' `.primary { color: blue; }`, a primary button matches both rules and both are
#' returned. That work is left to a later cascade stage.
#'
#' @section Selector semantics:
#' * `*` matches every element.
#' * `Button` matches elements whose `type` is `"Button"`.
#' * `#save` matches elements whose `id` is `"save"`.
#' * `.primary` matches elements whose `classes` include `"primary"`.
#' * `:focus` matches elements whose `states` include `"focus"`. Any valid
#'   identifier is a state (`DataTable:loading` works); which states are
#'   active is up to the application.
#' * `Button.primary:focus` is a compound selector: *every* condition must hold
#'   for the *same* element.
#' * `Container Button` (descendant): the element is a `Button` and some
#'   ancestor, at any depth, is a `Container`.
#' * `Container > Button` (child): the element is a `Button` and its direct
#'   parent is a `Container`.
#' * `Button, Input` (selector list): the rule matches if any selector matches.
#'
#' All comparisons are exact and case-sensitive. A selector that does not match
#' is not an error.
#'
#' Selectors are matched from right to left against the parent chain. Mixed
#' combinators are handled exactly: for `A > B C` and the chain
#' `A > B > B > C`, `C` matches, even though the nearest `B` ancestor has no `A`
#' parent. The matcher tracks every ancestor that can satisfy each part of the
#' selector rather than committing to the nearest one.
#'
#' @section Specificity of a match:
#' Each matched rule records the [specificity][ink_specificity] of the most
#' specific selector in its list *that matched this element*. Selectors that
#' did not match never contribute. Ties go to the earliest selector.
#'
#' @section Result:
#' An `ink_match_result` is a list with:
#' * `element`: the element that was matched.
#' * `matches`: a list with one `ink_matched_rule` per matching rule, in
#'   stylesheet source order (not sorted by specificity). Each rule appears at
#'   most once. An `ink_matched_rule` has:
#'   * `rule_index`: the rule's position in `sheet$rules`.
#'   * `rule`: the `ink_compiled_rule`, unchanged (selectors, declarations in
#'     source order including duplicates, and span).
#'   * `selector_indices`: positions, within the rule's selector list, of every
#'     selector that matched.
#'   * `winning_selector_index`: the matching selector with the highest
#'     specificity (the earliest one if tied). Diagnostic only; it is not a
#'     cascade decision.
#'   * `specificity`: the integer vector `c(id, class, type)` of that selector.
#'   * `span`: the rule's source span.
#'
#' @section Limitations:
#' * No cascade: conflicting declarations are all kept.
#' * No caching or indexing; every rule is tested on each call.
#' * Only the selector syntax the parser supports is matched.
#'
#' @param sheet An [ink_compiled_stylesheet][ink_compile].
#' @param element An [ink_element].
#' @param x An `ink_match_result`.
#' @param n Maximum number of matching rules to print: a single non-negative
#'   number (`Inf` prints all).
#' @param ... Ignored.
#' @return `ink_match()` returns an `ink_match_result`. Invalid inputs signal an
#'   `ink_match_error` ([ink_match_error]). `print()` returns `x` invisibly.
#' @seealso [ink_element()], [ink_specificity()], [ink_compile()]
#' @name ink_match
#' @examples
#' sheet <- ink_compile(ink_parse("
#'   Button { color: white; }
#'   .primary { background: blue; }
#'   Button.primary:focus { border: round red; }
#'   #save { color: green; }
#'   Input { color: red; }
#' "))
#'
#' screen <- ink_element("Screen", id = "main")
#' button <- ink_element(
#'   "Button",
#'   id = "save", classes = "primary", states = "focus",
#'   parent = screen
#' )
#'
#' matches <- ink_match(sheet, button)
#' matches
#' vapply(matches$matches, function(m) m$rule_index, integer(1))
#' @export
ink_match <- function(sheet, element) {
  validate_compiled_stylesheet(sheet)
  chain <- element_chain(element)
  structure(
    list(element = element, matches = match_rules(sheet, chain)),
    class = "ink_match_result"
  )
}

# The matching rules for the element at the head of `chain` (see
# element_chain()), in stylesheet source order. The sheet must already have
# passed validate_compiled_stylesheet(). The selectors are prepared and matched
# incrementally from the root (match-prepared.R); callers that match many
# elements should prepare once and carry state themselves, as ink_compute()
# does.
match_rules <- function(sheet, chain) {
  match_chain(prepare_selectors(sheet), chain)
}

# The original matcher: every rule, every selector, a fresh walk up the
# ancestors each time. Slower but simple, and independent of the prepared
# machinery, so it is kept as the reference the optimized matcher is tested
# against. The engine does not use it.
reference_match_rules <- function(sheet, chain) {
  matches <- list()
  for (index in seq_along(sheet$rules)) {
    matched <- match_rule(sheet$rules[[index]], index, chain)
    if (!is.null(matched)) {
      matches[[length(matches) + 1L]] <- matched
    }
  }
  matches
}

validate_compiled_stylesheet <- function(sheet) {
  if (!inherits(sheet, "ink_compiled_stylesheet")) {
    abort_match(
      "invalid_stylesheet",
      sprintf(
        "`sheet` must be an ink_compiled_stylesheet from ink_compile(), not %s.",
        describe_input(sheet)
      ),
      "sheet"
    )
  }
  if (!is.list(sheet$rules)) {
    abort_match("invalid_stylesheet", "`sheet` has no list of rules.", "sheet")
  }
  invisible(sheet)
}

validate_compiled_rule <- function(rule, index) {
  valid <- inherits(rule, "ink_compiled_rule") &&
    inherits(rule$selectors, "ink_selector_list") &&
    is.list(rule$selectors$selectors) && length(rule$selectors$selectors) > 0L
  if (!valid) {
    abort_match(
      "invalid_stylesheet",
      sprintf("Rule %d of `sheet` is not a valid compiled rule.", index),
      "sheet"
    )
  }
  invisible(rule)
}

# Returns an `ink_matched_rule`, or NULL if no selector of the rule matches.
match_rule <- function(rule, index, chain) {
  validate_compiled_rule(rule, index)
  selectors <- rule$selectors$selectors
  hits <- integer()
  winner <- NULL
  winner_specificity <- NULL
  for (j in seq_along(selectors)) {
    selector <- selectors[[j]]
    validate_complex_selector(selector)
    if (!complex_matches(selector, chain)) {
      next
    }
    hits <- c(hits, j)
    specificity <- selector_specificity(selector)
    # Strictly greater, so on a tie the earlier selector stays the winner.
    if (is.null(winner) || compare_specificity(specificity, winner_specificity) > 0L) {
      winner <- j
      winner_specificity <- specificity
    }
  }
  if (is.null(winner)) {
    return(NULL)
  }
  structure(
    list(
      rule_index = as.integer(index),
      rule = rule,
      selector_indices = hits,
      winning_selector_index = as.integer(winner),
      specificity = winner_specificity,
      span = rule$span
    ),
    class = "ink_matched_rule"
  )
}

#' @rdname ink_match
#' @export
print.ink_match_result <- function(x, n = 20L, ...) {
  n <- check_print_limit(n)
  count <- length(x$matches)
  cat(sprintf(
    "<ink_match_result> %d matching rule%s for %s\n",
    count, if (count == 1L) "" else "s", format(x$element)
  ))
  for (i in seq_len(min(count, n))) {
    cat("\n")
    print_matched_rule(i, x$matches[[i]])
  }
  if (count > n) {
    cat(sprintf("\n... and %d more\n", count - n))
  }
  invisible(x)
}

print_matched_rule <- function(position, matched) {
  selector <- matched$rule$selectors$selectors[[matched$winning_selector_index]]
  specificity <- matched$specificity
  cat(sprintf("[%d] %s\n", position, format_complex_selector(selector)))
  cat(sprintf(
    "    specificity: (%d, %d, %d)\n",
    specificity[["id"]], specificity[["class"]], specificity[["type"]]
  ))
  cat(sprintf("    rule: %d\n", matched$rule_index))
  if (length(matched$selector_indices) > 1L) {
    cat(sprintf(
      "    selectors matched: %s\n", paste(matched$selector_indices, collapse = ", ")
    ))
  }
}
