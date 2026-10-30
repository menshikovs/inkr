#' Session errors
#'
#' The style session functions ([ink_session()] and friends) signal an error of
#' class `ink_session_error` when a request is invalid. Errors never leave a
#' session half-modified: every change is validated before anything is
#' committed.
#'
#' The condition has these fields, in addition to the usual `message`:
#'
#' * `code`: a stable machine-readable string, see below.
#' * `detail`: the human-readable explanation (the same text as `message`).
#' * `argument`: the offending argument (`"session"`, `"key"`, `"parent_key"`,
#'   ...), or `NULL`.
#'
#' @section Error codes:
#' * `invalid_session`: the first argument is not a session.
#' * `invalid_node_key`: a key is not one non-missing, non-empty string.
#' * `unknown_node`: the key (of [ink_session_compute()],
#'   [ink_session_remove()] or [ink_session_explain()]) is not in the session.
#' * `unknown_parent`: `parent_key` names a node that is not in the session.
#' * `cyclic_parent`: the parent would be the node itself or one of its
#'   descendants.
#' * `invalid_update`: a node description is invalid: `type` is missing for a new
#'   node, or `type`, `id`, `classes` or `states` is not valid (same rules as
#'   [ink_element()]).
#' * `invalid_session_state`: the session's internal data has been damaged (for
#'   example by editing the session object directly).
#'
#' Errors raised while computing a style (a damaged stylesheet, say) keep their
#' own classes: `ink_compute_error` and `ink_explain_error`.
#'
#' @name ink_session_error
#' @seealso [ink_session()]
#' @examples
#' session <- ink_session(ink_compile(ink_parse("Button { color: red; }")))
#' err <- tryCatch(
#'   ink_session_set(session, "button", type = "Button", parent_key = "missing"),
#'   ink_session_error = identity
#' )
#' err$code
NULL

new_ink_session_error <- function(code, detail, argument = NULL) {
  structure(
    class = c("ink_session_error", "error", "condition"),
    list(message = detail, call = NULL, code = code, detail = detail, argument = argument)
  )
}

abort_session <- function(code, detail, argument = NULL) {
  stop(new_ink_session_error(code, detail, argument))
}
