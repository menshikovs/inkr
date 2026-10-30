# Text styles. See ?ink_values for the representation.

# Canonical order of the flags; also the set of supported flags.
TEXT_STYLE_FLAGS <- c("bold", "dim", "italic", "underline", "reverse", "strike")

TEXT_STYLE_EXPECTED <- paste0(
  "Expected 'none' or one or more of: ",
  paste(TEXT_STYLE_FLAGS, collapse = ", "), "."
)

# `flags` may be in any order and contain duplicates; the result is canonical.
new_ink_text_style <- function(flags = character()) {
  stopifnot(is.character(flags), all(flags %in% TEXT_STYLE_FLAGS))
  structure(
    list(flags = TEXT_STYLE_FLAGS[TEXT_STYLE_FLAGS %in% flags]),
    class = "ink_text_style"
  )
}

parse_text_style_tokens <- function(tokens) {
  if (length(tokens) == 0L) {
    abort_value_problem("invalid_text_style", TEXT_STYLE_EXPECTED)
  }
  is_word <- vapply(tokens, function(token) token$type == "ident", logical(1))
  if (!all(is_word)) {
    abort_value_problem("invalid_text_style", TEXT_STYLE_EXPECTED)
  }
  words <- vapply(tokens, function(token) token$value, "")
  if ("none" %in% words) {
    if (length(words) > 1L) {
      abort_value_problem(
        "invalid_text_style",
        "'none' cannot be combined with other text styles."
      )
    }
    return(new_ink_text_style())
  }
  unknown <- setdiff(words, TEXT_STYLE_FLAGS)
  if (length(unknown) > 0L) {
    abort_value_problem(
      "invalid_text_style",
      sprintf("Unknown text style '%s'. %s", unknown[[1L]], TEXT_STYLE_EXPECTED)
    )
  }
  new_ink_text_style(words)
}

#' @export
#' @noRd
format.ink_text_style <- function(x, ...) {
  if (length(x$flags) == 0L) "none" else paste(x$flags, collapse = " ")
}

#' @export
#' @noRd
print.ink_text_style <- function(x, ...) print_value(x)
