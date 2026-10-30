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

# Validates the `n` argument of the print methods: how many entries to show.
check_print_limit <- function(n) {
  if (!is.numeric(n) || length(n) != 1L || is.na(n) || n < 0) {
    stop("`n` must be a single non-negative number (use Inf to print everything).", call. = FALSE)
  }
  floor(n)
}
