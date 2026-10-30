#' Selector specificity
#'
#' `ink_specificity()` counts the parts of a selector that decide how specific
#' it is, following CSS:
#'
#' * `id`: ID selectors (`#save`).
#' * `class`: class selectors (`.primary`) and pseudo-classes (`:focus`).
#' * `type`: type selectors (`Button`).
#'
#' The universal selector `*` and combinators count for nothing. The result
#' depends only on the selector's structure.
#'
#' | Selector               | `id` | `class` | `type` |
#' |------------------------|------|---------|--------|
#' | `*`                    | 0    | 0       | 0      |
#' | `Button`               | 0    | 0       | 1      |
#' | `.primary`             | 0    | 1       | 0      |
#' | `#save`                | 1    | 0       | 0      |
#' | `Button.primary:focus` | 0    | 2       | 1      |
#' | `#dialog > Button`     | 1    | 0       | 1      |
#'
#' Specificities are compared *lexicographically*: more IDs always wins, then
#' more classes, then more types. They are deliberately not collapsed into one
#' number such as `100 * id + 10 * class + type`, which gives wrong answers
#' once a count passes 9 (eleven classes would beat one ID). An internal helper
#' does the comparison.
#'
#' A selector *list* (`Button, #save`) has no specificity of its own: each
#' selector in it has one, and which applies depends on which selectors match
#' the element. [ink_match()] reports the highest specificity among the
#' selectors that actually matched.
#'
#' @param selector An `ink_complex_selector` from a parsed stylesheet, for
#'   example `ast$statements[[1]]$selectors$selectors[[1]]`.
#' @return An integer vector with names `id`, `class` and `type`. Input that is
#'   not a well-formed `ink_complex_selector` signals an `ink_match_error`
#'   (code `"invalid_selector"`); see [ink_match_error].
#' @seealso [ink_match()]
#' @examples
#' ast <- ink_parse("#dialog > Button.primary:focus { color: red; }")
#' selector <- ast$statements[[1]]$selectors$selectors[[1]]
#' ink_specificity(selector)
#' @export
ink_specificity <- function(selector) {
  validate_complex_selector(selector)
  selector_specificity(selector)
}

# The unchecked core of ink_specificity(), for selectors already validated.
selector_specificity <- function(selector) {
  compounds <- selector$compounds
  ids <- sum(vapply(compounds, function(compound) !is.null(compound$id), logical(1)))
  classes <- sum(vapply(
    compounds,
    function(compound) length(compound$classes) + length(compound$pseudo_classes),
    integer(1)
  ))
  types <- sum(vapply(
    compounds,
    function(compound) !is.null(compound$type) && compound$type != "*",
    logical(1)
  ))
  c(id = as.integer(ids), class = as.integer(classes), type = as.integer(types))
}

# Lexicographic comparison of two specificities: -1 if `a` is less specific
# than `b`, 0 if equal, 1 if more specific. The first component that differs
# decides, whatever the size of the later ones.
compare_specificity <- function(a, b) {
  for (i in seq_along(a)) {
    if (a[[i]] != b[[i]]) {
      return(if (a[[i]] < b[[i]]) -1L else 1L)
    }
  }
  0L
}
