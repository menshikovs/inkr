#' Element and matching errors
#'
#' [ink_element()], [ink_match()] and [ink_specificity()] signal an error of
#' class `ink_match_error` when they are given invalid input: a malformed
#' element, something that is not an element or compiled stylesheet, or a
#' damaged selector. This is different from a selector that is valid but does
#' not match, which is a normal result and never an error.
#'
#' The condition has these fields, in addition to the usual `message`:
#'
#' * `code`: a stable machine-readable string, see below.
#' * `detail`: the human-readable explanation (the same text as `message`).
#' * `argument`: the name of the offending argument (`"type"`, `"parent"`,
#'   `"element"`, ...), or `NULL`.
#'
#' @section Error codes:
#' * `invalid_element_type`: `type` is missing, not one non-missing string, or
#'   not a valid identifier (this includes `"*"`).
#' * `invalid_element_id`: `id` is not `NULL` or one valid identifier.
#' * `invalid_element_classes`: `classes` is not a character vector of valid
#'   identifiers.
#' * `invalid_element_states`: `states` is not a character vector of valid
#'   identifiers.
#' * `invalid_element_parent`: `parent` is not `NULL` or an `ink_element`.
#' * `invalid_element`: the element given to [ink_match()] is not an
#'   `ink_element`, or it (or an ancestor) has been modified into an invalid
#'   shape.
#' * `invalid_stylesheet`: the stylesheet given to [ink_match()] is not an
#'   `ink_compiled_stylesheet`, or its rules are damaged.
#' * `invalid_selector`: the selector given to [ink_specificity()], or found
#'   in a rule, is not a well-formed `ink_complex_selector`.
#'
#' Identifiers use ASCII letters, digits, `_` and `-` and start with a letter or
#' `_`, exactly as in stylesheets.
#'
#' @name ink_match_error
#' @seealso [ink_element()], [ink_match()]
#' @examples
#' err <- tryCatch(ink_element(type = "Button", id = "#save"), ink_match_error = identity)
#' err$code
#' err$argument
NULL

new_ink_match_error <- function(code, detail, argument = NULL) {
  structure(
    class = c("ink_match_error", "error", "condition"),
    list(message = detail, call = NULL, code = code, detail = detail, argument = argument)
  )
}

abort_match <- function(code, detail, argument = NULL) {
  stop(new_ink_match_error(code, detail, argument))
}
