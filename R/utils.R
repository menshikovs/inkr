# Internal helpers shared across the package.

`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

# Validates `text` and returns it as a single UTF-8 string. A character vector
# with several elements is treated as lines of one stylesheet.
normalize_text <- function(text, arg = "text") {
  if (!is.character(text) || anyNA(text)) {
    stop(
      sprintf("`%s` must be a character vector without missing values.", arg),
      call. = FALSE
    )
  }
  text <- enc2utf8(paste(text, collapse = "\n"))
  if (!validUTF8(text)) {
    stop(sprintf("`%s` must be valid UTF-8.", arg), call. = FALSE)
  }
  text
}

check_source <- function(source) {
  if (!is.character(source) || length(source) != 1L || is.na(source)) {
    stop("`source` must be a single string.", call. = FALSE)
  }
  source
}
