#' Computation errors
#'
#' [ink_compute()] signals an error of class `ink_compute_error` when its
#' inputs are not valid or an internal invariant of the cascade is violated. A
#' stylesheet that matches nothing, an element without a parent, and a
#' stylesheet without rules are all valid and never an error.
#'
#' The condition has these fields, in addition to the usual `message`:
#'
#' * `code`: a stable machine-readable string, see below.
#' * `detail`: the human-readable explanation (the same text as `message`).
#' * `argument`: the offending argument (`"sheet"` or `"element"`), or `NULL`.
#'
#' @section Error codes:
#' * `invalid_compiled_stylesheet`: `sheet` is not an `ink_compiled_stylesheet`,
#'   or its rules, selectors or registry are damaged.
#' * `invalid_element`: `element` is not a valid `ink_element`.
#' * `invalid_parent_chain`: an ancestor of `element` is malformed, or the chain
#'   is implausibly deep.
#' * `invalid_cascade_result`: the matched declarations are inconsistent, for
#'   example a declaration for a property that is not in the registry.
#' * `invalid_computed_value`: a value does not have the type its property
#'   requires, or is a keyword other than `inherit` / `initial`.
#'
#' @name ink_compute_error
#' @seealso [ink_compute()]
#' @examples
#' err <- tryCatch(ink_compute(list(), ink_element("Button")), ink_compute_error = identity)
#' err$code
NULL

new_ink_compute_error <- function(code, detail, argument = NULL) {
  structure(
    class = c("ink_compute_error", "error", "condition"),
    list(message = detail, call = NULL, code = code, detail = detail, argument = argument)
  )
}

abort_compute <- function(code, detail, argument = NULL) {
  stop(new_ink_compute_error(code, detail, argument))
}
