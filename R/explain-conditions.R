#' Explanation errors
#'
#' [ink_explain()] signals an error of class `ink_explain_error` when its inputs
#' are not valid or when the facts it gathers contradict each other. A valid
#' property that no declaration sets is not an error: the explanation simply
#' reports zero declarations.
#'
#' The condition has these fields, in addition to the usual `message`:
#'
#' * `code`: a stable machine-readable string, see below.
#' * `detail`: the human-readable explanation (the same text as `message`).
#' * `argument`: the offending argument (`"sheet"`, `"element"` or
#'   `"property"`), or `NULL`.
#'
#' @section Error codes:
#' * `invalid_stylesheet`: `sheet` is not an `ink_compiled_stylesheet`, or its
#'   rules, selectors or registry are damaged.
#' * `invalid_element`: `element`, or one of its ancestors, is not a valid
#'   `ink_element`.
#' * `invalid_property`: `property` is not a single non-missing, non-empty
#'   string.
#' * `unknown_property`: `property` is not in the stylesheet's registry.
#' * `invalid_explanation_state`: the cascade data is inconsistent, for example
#'   a declaration that is not for a registered property or a candidate that
#'   outranks the recorded winner. This indicates damaged input or a bug, never
#'   a normal stylesheet.
#'
#' @name ink_explain_error
#' @seealso [ink_explain()]
#' @examples
#' sheet <- ink_compile(ink_parse("Button { color: red; }"))
#' err <- tryCatch(
#'   ink_explain(sheet, ink_element("Button"), "colour"),
#'   ink_explain_error = identity
#' )
#' err$code
NULL

new_ink_explain_error <- function(code, detail, argument = NULL) {
  structure(
    class = c("ink_explain_error", "error", "condition"),
    list(message = detail, call = NULL, code = code, detail = detail, argument = argument)
  )
}

abort_explain <- function(code, detail, argument = NULL) {
  stop(new_ink_explain_error(code, detail, argument))
}

# The inputs are validated by the compute layer; its errors are re-signalled
# as explain errors so callers deal with one condition class.
translate_compute_error <- function(error) {
  code <- switch(error$code,
    invalid_compiled_stylesheet = "invalid_stylesheet",
    invalid_element = ,
    invalid_parent_chain = "invalid_element",
    "invalid_explanation_state"
  )
  abort_explain(code, error$detail, error$argument)
}
