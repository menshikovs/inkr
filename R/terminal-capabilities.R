#' Terminal capability profiles
#'
#' `ink_terminal_capabilities()` describes what a particular terminal can
#' display. The terminal style adapter ([ink_terminal_style()]) uses it to choose
#' how to express a computed style. A capabilities object is plain data:
#' creating one writes nothing, and nothing is detected. In particular
#' `TERM`, `COLORTERM`, `NO_COLOR` and the console are never inspected, so the
#' same call gives the same object on every machine. A host application that
#' wants detection can do it itself and pass the result in.
#'
#' @section Color depth:
#' `color_depth` is exactly one of
#' * `"truecolor"`: 24-bit RGB (SGR `38;2;r;g;b`).
#' * `"ansi256"`: the xterm 256-color palette (SGR `38;5;n`).
#' * `"ansi16"`: the 16 standard ANSI colors (SGR 30-37 and 90-97).
#' * `"none"`: no color instructions at all.
#'
#' @section Text styles:
#' `text_styles` lists the attributes the terminal supports, a subset of
#' `"bold"`, `"dim"`, `"italic"`, `"underline"`, `"reverse"` and `"strike"`
#' (all of them by default; `character()` for none). Attributes a style asks for
#' that are not listed are left out of the terminal instructions, but the style
#' still records what was requested. Duplicates are removed and the fixed order
#' above is used.
#'
#' @section Unicode:
#' `unicode = TRUE` allows Unicode border characters; `FALSE` selects an
#' ASCII-only fallback.
#'
#' @param color_depth One of the color depths above.
#' @param unicode `TRUE` or `FALSE`.
#' @param text_styles Character vector of supported attributes.
#' @param x An `ink_terminal_capabilities`.
#' @param ... Ignored.
#' @return An `ink_terminal_capabilities`: a list with `color_depth`, `unicode`
#'   and `text_styles`. Invalid arguments signal an `ink_terminal_error`
#'   ([ink_terminal_error]). `print()` returns `x` invisibly.
#' @seealso [ink_terminal_style()], [ink_ansi_wrap()]
#' @name ink_terminal_capabilities
#' @examples
#' ink_terminal_capabilities()
#' ink_terminal_capabilities("ansi16", unicode = FALSE, text_styles = c("bold", "underline"))
#' @export
ink_terminal_capabilities <- function(color_depth = "truecolor", unicode = TRUE,
                                      text_styles = TEXT_STYLE_FLAGS) {
  structure(
    list(
      color_depth = check_color_depth(color_depth),
      unicode = check_unicode_flag(unicode),
      text_styles = check_supported_text_styles(text_styles)
    ),
    class = "ink_terminal_capabilities"
  )
}

COLOR_DEPTHS <- c("truecolor", "ansi256", "ansi16", "none")

check_color_depth <- function(color_depth) {
  if (!is_plain_string(color_depth) || !(color_depth %in% COLOR_DEPTHS)) {
    shown <- if (is_plain_string(color_depth)) show_text(color_depth) else describe_input(color_depth)
    abort_terminal(
      "invalid_color_depth",
      sprintf(
        "`color_depth` must be one of %s, not %s.",
        paste(shQuote(COLOR_DEPTHS, type = "cmd"), collapse = ", "), shown
      ),
      "color_depth"
    )
  }
  as.vector(color_depth, mode = "character")
}

check_unicode_flag <- function(unicode) {
  if (!is.logical(unicode) || length(unicode) != 1L || is.na(unicode)) {
    abort_terminal(
      "invalid_capabilities",
      sprintf("`unicode` must be TRUE or FALSE, not %s.", describe_input(unicode)),
      "unicode"
    )
  }
  as.vector(unicode, mode = "logical")
}

check_supported_text_styles <- function(text_styles) {
  if (!is.character(text_styles) || anyNA(text_styles)) {
    abort_terminal(
      "invalid_capabilities",
      sprintf("`text_styles` must be a character vector without missing values, not %s.", describe_input(text_styles)),
      "text_styles"
    )
  }
  unknown <- setdiff(text_styles, TEXT_STYLE_FLAGS)
  if (length(unknown) > 0L) {
    abort_terminal(
      "invalid_capabilities",
      sprintf(
        "Unknown text style %s in `text_styles`. Supported: %s.",
        show_text(unknown[[1L]]), paste(TEXT_STYLE_FLAGS, collapse = ", ")
      ),
      "text_styles"
    )
  }
  TEXT_STYLE_FLAGS[TEXT_STYLE_FLAGS %in% text_styles]
}

# True capabilities objects only; used wherever one is accepted.
validate_capabilities <- function(capabilities, argument = "capabilities") {
  if (!inherits(capabilities, "ink_terminal_capabilities") || !is.list(capabilities)) {
    abort_terminal(
      "invalid_capabilities",
      sprintf(
        "`%s` must be created by ink_terminal_capabilities(), not %s.",
        argument, describe_input(capabilities)
      ),
      argument
    )
  }
  check_color_depth(capabilities$color_depth)
  check_unicode_flag(capabilities$unicode)
  check_supported_text_styles(capabilities$text_styles)
  invisible(capabilities)
}

#' @rdname ink_terminal_capabilities
#' @export
print.ink_terminal_capabilities <- function(x, ...) {
  cat(sprintf(
    "<ink_terminal_capabilities> color: %s | unicode: %s | text styles: %s\n",
    x$color_depth, if (x$unicode) "yes" else "no (ASCII)",
    if (length(x$text_styles)) paste(x$text_styles, collapse = " ") else "none"
  ))
  invisible(x)
}
