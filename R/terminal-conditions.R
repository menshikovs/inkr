#' Terminal adapter errors
#'
#' [ink_terminal_capabilities()], [ink_terminal_style()] and [ink_ansi_wrap()]
#' signal an error of class `ink_terminal_error` when their input is invalid.
#' Problems that belong to an earlier stage keep that stage's condition class: a
#' stylesheet that does not compile is an `ink_compile_error`, whatever
#' happens to it afterwards.
#'
#' The condition has these fields, in addition to the usual `message`:
#'
#' * `code`: a stable machine-readable string, see below.
#' * `detail`: the human-readable explanation (the same text as `message`).
#' * `argument`: the offending argument (`"style"`, `"matte"`, `"text"`, ...),
#'   or `NULL`.
#'
#' @section Error codes:
#' * `invalid_computed_style`: `style` is not an [ink_computed_style], or lacks
#'   a built-in property, or one has the wrong type or an unresolved keyword.
#' * `invalid_capabilities`: a capabilities argument is invalid (`unicode`,
#'   `text_styles`), or an object that should be an `ink_terminal_capabilities`
#'   is not.
#' * `invalid_color_depth`: `color_depth` is not one of `"truecolor"`,
#'   `"ansi256"`, `"ansi16"`, `"none"`.
#' * `invalid_color_value`: a color value is malformed (for example channels
#'   outside 0-255).
#' * `unknown_alpha_background`: a partially transparent color has to be
#'   composited but no concrete RGB background is known. Supply `matte`.
#' * `invalid_matte`: `matte` is not an opaque RGB color.
#' * `invalid_terminal_style`: the `style` given to [ink_ansi_wrap()] is not a
#'   well-formed `ink_terminal_style`.
#' * `invalid_text`: `text` is not a single, non-missing, valid UTF-8 string.
#' * `invalid_reset`: `reset` is not `TRUE` or `FALSE`.
#'
#' @name ink_terminal_error
#' @seealso [ink_terminal_style()]
#' @examples
#' err <- tryCatch(
#'   ink_terminal_capabilities(color_depth = "rainbow"),
#'   ink_terminal_error = identity
#' )
#' err$code
NULL

new_ink_terminal_error <- function(code, detail, argument = NULL) {
  structure(
    class = c("ink_terminal_error", "error", "condition"),
    list(message = detail, call = NULL, code = code, detail = detail, argument = argument)
  )
}

abort_terminal <- function(code, detail, argument = NULL) {
  stop(new_ink_terminal_error(code, detail, argument))
}
