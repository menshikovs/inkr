#' Compute the style of an element
#'
#' `ink_compute()` produces the final style of an element: a typed value for
#' every property in the stylesheet's registry. It is the last step of the
#' pipeline:
#'
#' 1. **Parsing** ([ink_parse()]) turns text into a syntax tree.
#' 2. **Compiling** ([ink_compile()]) resolves variables and turns values into
#'    typed objects.
#' 3. **Matching** ([ink_match()]) finds the rules whose selectors apply to an
#'    element.
#' 4. **Cascading** picks, for each property, the one declaration that wins.
#' 5. **Computing** (this function) resolves `inherit` / `initial`, applies
#'    inheritance and fills in initial values.
#'
#' Nothing is rendered or laid out: the result is data for a renderer.
#'
#' @section Cascade:
#' Declarations for the same property compete in this order:
#' 1. Higher selector [specificity][ink_specificity] wins. Specificity is
#'    compared lexicographically (IDs, then classes and pseudo-classes, then
#'    types), so an earlier `#save` rule beats a later `Button` rule.
#' 2. With equal specificity, the declaration in the later rule wins.
#' 3. Within one rule, the later declaration wins.
#'
#' For a rule with a selector list, the specificity used is that of the most
#' specific selector that actually matched the element. Each property is
#' decided on its own, so one style can combine values from many rules. Only the
#' stylesheet itself is considered: there are no other origins, inline styles or
#' `!important`.
#'
#' @section Inheritance and defaults:
#' For every registered property, in this order:
#' 1. If a declaration wins, its value is used (see keywords below).
#' 2. Otherwise, if the property inherits (its registry `inherits` flag; `color`
#'    and `text-style` among the built-ins), the **parent's computed value** is
#'    used. Because it is the computed value, a value set on a grandparent
#'    passes through parents that declare nothing.
#' 3. Otherwise, or if the element has no parent, the registry's initial value
#'    is used.
#'
#' Custom properties follow the same rules through their registry entry; no
#' other code is involved.
#'
#' @section Keywords:
#' `inherit` and `initial` are ordinary declarations that compete in the
#' cascade; if one wins it is resolved:
#' * `inherit` takes the parent's computed value, even for properties that do
#'   not inherit by default. On an element without a parent it gives the
#'   registry's initial value.
#' * `initial` takes the registry's initial value, even for properties that
#'   inherit by default. It is not "transparent" or "terminal default" unless
#'   the registry says so.
#'
#' @section How the parent chain is processed:
#' The element's ancestors are collected into a list and processed from the root
#' down to the element, one loop iteration per element, each ancestor exactly
#' once. Each iteration matches that element, cascades, and resolves its
#' properties from the computed values of the iteration before. There is no
#' recursion, so deep trees do not hit R's recursion limit.
#'
#' @section Limitations:
#' * No caching: every call matches and cascades every element on the path.
#' * A single stylesheet; no `unset`, `revert`, `!important` or inline styles.
#' * Matching cost grows with tree depth for selectors that use descendant
#'   combinators.
#'
#' @param sheet An [ink_compiled_stylesheet][ink_compile].
#' @param element An [ink_element].
#' @return An [ink_computed_style]. Invalid input signals an `ink_compute_error`
#'   ([ink_compute_error]).
#' @seealso [ink_match()], [ink_computed_style]
#' @name ink_compute
#' @examples
#' sheet <- ink_compile(ink_parse("
#'   Button { color: white; background: blue; }
#'   Button.primary { color: yellow; }
#'   #save { color: green; }
#' "))
#' element <- ink_element("Button", id = "save", classes = "primary")
#' style <- ink_compute(sheet, element)
#' style
#' style$provenance$color$rule_index
#' @export
ink_compute <- function(sheet, element) {
  validate_compute_sheet(sheet)
  chain <- compute_chain(element)
  resolved <- compute_style_path(sheet, chain)
  new_ink_computed_style(resolved$values, resolved$provenance, element)
}

# Computes the styles of a validated chain (see compute_chain()) from the root
# down and returns the target's `list(values, provenance)`. This is the single
# implementation of the match / cascade / resolve loop: ink_compute() uses it
# as is, and ink_explain() passes a `visit` function to observe each element's
# intermediate results without a second implementation.
#
# `visit(position, matches, winners, resolved)` is called once per element,
# root first, after it has been resolved. `position` indexes `chain` (the
# target is 1), `matches` are the rules matched for that element, `winners` the
# cascade winners and `resolved` its computed values and provenance.
compute_style_path <- function(sheet, chain, visit = NULL, prepared = NULL) {
  depth <- length(chain)
  # Root first: `chain[[depth]]` is the root and `chain[[1L]]` the target.
  # `step` always holds the result for the element processed last, which is the
  # parent of the next one: its matching state and its computed style.
  step <- NULL
  with_compute_errors({
    if (is.null(prepared)) {
      prepared <- prepare_selectors(sheet)
    }
    for (position in depth:1L) {
      step <- compute_element_step(prepared, sheet$registry, chain[[position]], step)
      if (!is.null(visit)) {
        visit(position, step$matches, step$winners, step$resolved)
      }
    }
  })
  step$resolved
}

# One element of the root-to-target computation: match it (reusing the parent's
# matching state), run the cascade, resolve keywords and inheritance from the
# parent's computed values. `parent` is NULL for a root, otherwise a previous
# result (its `state` and `resolved` are used). Shared by compute_style_path()
# and by style sessions, which keep these results between calls.
compute_element_step <- function(prepared, registry, element, parent = NULL) {
  matched <- match_element(prepared, element, parent$state)
  winners <- ink_cascade(matched$matches, registry)
  resolved <- resolve_element_style(registry, winners, parent$resolved$values)
  list(matches = matched$matches, state = matched$state, winners = winners, resolved = resolved)
}

# Matching errors found while preparing or matching a stylesheet are reported
# as damaged-stylesheet compute errors.
with_compute_errors <- function(expr) {
  tryCatch(
    expr,
    ink_match_error = function(e) {
      abort_compute("invalid_compiled_stylesheet", e$detail, "sheet")
    }
  )
}

validate_compute_sheet <- function(sheet) {
  tryCatch(
    {
      validate_compiled_stylesheet(sheet)
      validate_registry(sheet$registry)
    },
    ink_match_error = function(e) {
      abort_compute("invalid_compiled_stylesheet", e$detail, "sheet")
    },
    ink_compile_error = function(e) {
      abort_compute(
        "invalid_compiled_stylesheet",
        paste("The stylesheet's registry is invalid:", e$detail),
        "sheet"
      )
    }
  )
  invisible(sheet)
}

# The element followed by its ancestors, validated, with errors reported as
# compute errors.
compute_chain <- function(element) {
  walked <- walk_element_chain(element)
  if (!is.null(walked$problem)) {
    code <- if (identical(walked$problem$kind, "element")) "invalid_element" else "invalid_parent_chain"
    abort_compute(code, walked$problem$detail, "element")
  }
  walked$chain
}
