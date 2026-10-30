#' Explain a computed property value
#'
#' `ink_explain()` answers *why* an element has the computed value it has for
#' one property. The three inspection functions divide the work:
#'
#' * [ink_match()] finds the rules that apply to an element.
#' * [ink_compute()] produces the final typed value of every property.
#' * `ink_explain()` describes how one of those values came about: which
#'   declarations competed, which won and why the others lost, and, for values
#'   that were not declared, where they were inherited or defaulted from.
#'
#' The explanation is not a second implementation of the cascade. It runs the
#' same loop as [ink_compute()] (matching, cascade, keyword resolution and
#' inheritance) and observes its intermediate results, so it cannot disagree
#' with the computed style: `ink_explain(sheet, element, p)$value` is identical
#' to `ink_compute(sheet, element)$values[[p]]`, and its `origin` and winner
#' equal the computed provenance. [ink_compute()] itself builds no explanation
#' and pays nothing for this function.
#'
#' @section Structure of an `ink_style_explanation`:
#' A list with:
#' * `element`, `property`: the inputs.
#' * `type`: the property's value type (`"color"`, ...).
#' * `value`: the final computed typed value, never a keyword and never a string.
#' * `origin`: how the value was obtained: `"declaration"`, `"inherited"`,
#'   `"initial"`, `"explicit_inherit"` or `"explicit_initial"` (the same
#'   vocabulary as the computed-style provenance, see [ink_computed_style]).
#' * `winner`: the winning declaration (an `ink_explanation_candidate`) when a
#'   declaration won, including one that said `inherit` or `initial`; otherwise
#'   `NULL`.
#' * `candidates`: every declaration for this property from the rules that match
#'   the element, in stylesheet source order (rule, then declaration; *not*
#'   sorted by priority). Each rule contributes its declarations once, however
#'   many of its selectors matched. An `ink_explanation_candidate` has
#'   `property`, `rule_index`, `declaration_index`, `selector_index` (the
#'   matching selector with the highest specificity), `matching_selector_indices`
#'   (all matching selectors of the rule), `specificity` (of `selector_index`,
#'   as `c(id, class, type)`), `selector` (that selector's syntax-tree node),
#'   `value` (typed, possibly an `ink_keyword`), `raw` (the value as written),
#'   `span` (the declaration's source span), `status` (`"winner"` or
#'   `"overridden"`) and `reason`.
#' * `resolution`: how the final value was reached; see below.
#' * `source`: the stylesheet's source name, to read `span`s with.
#'
#' @section Why a declaration lost:
#' `reason` is `NULL` for the winner and otherwise one of
#' * `"lower_specificity"`: its selector is less specific than the winner's.
#'   Specificity is compared lexicographically (IDs, then classes and
#'   pseudo-classes, then types), so source order is irrelevant here.
#' * `"earlier_rule"`: equal specificity, but the winner's rule comes later.
#' * `"earlier_declaration"`: equal specificity in the same rule, but the winner
#'   is declared later.
#'
#' These describe priority, not values: a declaration with the same value as the
#' winner is still overridden.
#'
#' For a selector list, only selectors that matched count; the specificity of
#' a rule is that of its most specific matching selector (the earliest one on a
#' tie).
#'
#' @section Resolution:
#' `resolution` is a list with:
#' * `origin`, `source` (`"declaration"`, `"parent"` or `"registry"`) and
#'   `keyword` (`"inherit"` / `"initial"` or `NULL`): copied from the computed
#'   provenance of the element.
#' * `initial_value`: the registry's initial value for the property.
#' * `path`: one `ink_resolution_step` per element, from the root (`position`
#'   1) to `element`, each with its own `origin`, `value` and `provenance`
#'   (the record [ink_compute()] kept for that element), `supplied_by` (the
#'   `position` of the step that originally supplied its value) and
#'   `in_effective_chain`.
#' * `effective_source`: the step that originally supplied the final value. It
#'   is the target itself for a declaration or a value defaulted on the spot;
#'   for an inherited value it is the nearest ancestor that declared the
#'   property (or took the registry's initial value). Ancestors above it appear
#'   in `path` with `in_effective_chain = FALSE`: they are context, not
#'   sources. An explicit `initial` ends the search: the registry value is the
#'   source and no ancestor is consulted.
#'
#' @section Limitations:
#' * One property per call.
#' * The explanation recomputes the style path from the root, so its cost
#'   matches [ink_compute()]; nothing is cached.
#' * `print()` abbreviates long lists, but the object is always complete.
#'
#' @param sheet An [ink_compiled_stylesheet][ink_compile].
#' @param element An [ink_element].
#' @param property One property name registered in the stylesheet's registry.
#' @param x An `ink_style_explanation`.
#' @param n Maximum number of entries shown in each section of the printed
#'   output (`Inf` for all).
#' @param ... Ignored.
#' @return An `ink_style_explanation`. Invalid input signals an
#'   `ink_explain_error` ([ink_explain_error]). `print()` returns `x`
#'   invisibly.
#' @seealso [ink_match()], [ink_compute()], [ink_computed_style]
#' @name ink_explain
#' @examples
#' sheet <- ink_compile(ink_parse("
#'   Button { color: white; }
#'   .primary { color: blue; }
#'   #save { color: green; }
#' "))
#' button <- ink_element("Button", id = "save", classes = "primary")
#'
#' explanation <- ink_explain(sheet, button, "color")
#' explanation
#'
#' explanation$value
#' vapply(explanation$candidates, function(c) c$status, character(1))
#'
#' # Inherited values name the ancestor that supplied them.
#' sheet <- ink_compile(ink_parse("Screen { color: green; }"))
#' tree <- ink_element(
#'   "Button",
#'   parent = ink_element("Panel", parent = ink_element("Screen"))
#' )
#' ink_explain(sheet, tree, "color")
#' @export
ink_explain <- function(sheet, element, property) {
  if (missing(sheet)) abort_explain("invalid_stylesheet", "`sheet` is required.", "sheet")
  if (missing(element)) abort_explain("invalid_element", "`element` is required.", "element")
  if (missing(property)) abort_explain("invalid_property", "`property` is required.", "property")
  tryCatch(
    explain_property(sheet, element, property),
    ink_compute_error = translate_compute_error
  )
}

# `prepared` lets a caller that already holds the prepared selectors (a style
# session) skip preparing them again.
explain_property <- function(sheet, element, property, prepared = NULL) {
  validate_compute_sheet(sheet)
  chain <- compute_chain(element)
  definition <- find_explain_property(sheet$registry, property)
  depth <- length(chain)

  recorder <- new_resolution_recorder(property, depth)
  resolved <- compute_style_path(sheet, chain, recorder$visit, prepared)

  steps <- finish_resolution_steps(recorder$steps, chain)
  candidates <- explanation_candidates(
    recorder$target$matches, sheet$registry, property, recorder$target$winner
  )
  winner_position <- which(vapply(candidates, function(c) identical(c$status, "winner"), NA))
  winner <- if (length(winner_position)) candidates[[winner_position]] else NULL
  resolution <- new_resolution(steps, definition)
  check_explanation_consistency(resolved, property, resolution, winner)

  structure(
    list(
      element = element,
      property = property,
      type = definition$type,
      value = resolved$values[[property]],
      origin = resolution$origin,
      winner = winner,
      candidates = candidates,
      resolution = resolution,
      source = sheet$source
    ),
    class = "ink_style_explanation"
  )
}

# `property` must be one string naming a registered property.
find_explain_property <- function(registry, property) {
  if (!is_plain_string(property) || !nzchar(property)) {
    abort_explain(
      "invalid_property",
      sprintf(
        "`property` must be one non-empty string naming a registered property, not %s.",
        describe_input(property)
      ),
      "property"
    )
  }
  definition <- registry$properties[[property]]
  if (is.null(definition)) {
    abort_explain(
      "unknown_property",
      sprintf(
        "Unknown property '%s'. Registered properties: %s.",
        property, paste(names(registry$properties), collapse = ", ")
      ),
      "property"
    )
  }
  definition
}

# The explanation is assembled from separate observations of one computation;
# these checks make a contradiction between them a loud error instead of a
# misleading explanation.
check_explanation_consistency <- function(resolved, property, resolution, winner) {
  provenance <- resolved$provenance[[property]]
  declared <- provenance$origin %in% c("declaration", "explicit_inherit", "explicit_initial")
  consistent <- identical(resolution$origin, provenance$origin) &&
    identical(declared, !is.null(winner)) &&
    (is.null(winner) || (
      identical(winner$rule_index, provenance$rule_index) &&
        identical(winner$declaration_index, provenance$declaration_index) &&
        identical(winner$selector_index, provenance$selector_index) &&
        identical(winner$specificity, provenance$specificity)
    ))
  if (!isTRUE(consistent)) {
    abort_explain(
      "invalid_explanation_state",
      sprintf("The explanation of '%s' disagrees with the computed style.", property)
    )
  }
}
