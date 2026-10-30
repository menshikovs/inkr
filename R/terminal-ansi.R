#' Wrap text in ANSI SGR escape sequences
#'
#' `ink_ansi_wrap()` returns `text` surrounded by the "Select Graphic Rendition"
#' escape sequences for the text attributes, foreground color and background color
#' of an [ink_terminal_style()]. It is a small serialization helper, not a
#' renderer: it does not draw borders, apply padding, wrap or measure lines,
#' move the cursor, clear or resize the terminal, and it never writes to the
#' terminal itself. Displaying the result (for example with [cat()]) is up to
#' you.
#'
#' @section Output:
#' The result is `ESC[` *parameters* `m`, then the text, then (if `reset`) the
#' reset sequence `ESC[0m`. Parameters are joined with `;` in a fixed order:
#' enabled attributes (bold 1, dim 2, italic 3, underline 4, reverse 7, strike 9,
#' in that order), then the foreground, then the background:
#'
#' | Color kind   | foreground       | background       |
#' |--------------|------------------|------------------|
#' | `rgb`        | `38;2;r;g;b`     | `48;2;r;g;b`     |
#' | `palette256` | `38;5;n`         | `48;5;n`         |
#' | `palette16`  | 30-37, 90-97     | 40-47, 100-107   |
#' | `default`    | `39`             | `49`             |
#'
#' For example bold red text under truecolor is `ESC[1;38;2;255;0;0m`.
#' Colors with `kind = "none"` (transparent, or any color under
#' `color_depth = "none"`) and attributes the terminal does not support add
#' nothing.
#'
#' The terminal's default colors are SGR `39` (foreground) and `49`
#' (background). They are emitted when the stylesheet asked for them (a declared
#' or inherited `default`, or `inherit` / `initial` resolving to it) and not when
#' a color was simply never set, even though both compute to `default`; otherwise
#' every style would carry a pointless `39`. A style with nothing to apply returns
#' the text unchanged, without escape sequences, and so does an empty `text`.
#'
#' @section Reset:
#' With `reset = TRUE` (the default) the styling is ended by `ESC[0m`, so it
#' cannot leak into whatever is printed next. With `reset = FALSE` no reset is
#' appended and the attributes and colors stay active in the terminal until
#' something else resets them; use it only when you control what follows.
#'
#' @section Safety of `text`:
#' `text` may come from an untrusted source, and terminals interpret control
#' characters. Before the text is wrapped, these characters are **removed**:
#' ESC and every other C0 control character except tab and line feed
#' (`U+0001`-`U+0008`, `U+000B`-`U+001F`, which includes carriage return,
#' backspace, bell and form feed), DEL (`U+007F`), and the C1 controls
#' (`U+0080`-`U+009F`, which include the single-character CSI `U+009B`). An
#' injected `"\033[2J"` therefore shows up as the harmless text `[2J`. Tab and
#' newline are kept, and a multi-line text is wrapped as one span (the attributes
#' continue across line breaks). Everything else, including non-ASCII text, is
#' unchanged. The only escape sequences in the result are those this function
#' adds.
#'
#' @param text A single, non-missing character string.
#' @param style An [ink_terminal_style()].
#' @param reset `TRUE` or `FALSE`: append the reset sequence.
#' @return A character string. Invalid input signals an `ink_terminal_error`
#'   ([ink_terminal_error]).
#' @seealso [ink_terminal_style()], [ink_terminal_capabilities()]
#' @name ink_ansi_wrap
#' @examples
#' sheet <- ink_compile(ink_parse("Button { color: #ff0000; text-style: bold; }"))
#' terminal <- ink_terminal_style(ink_compute(sheet, ink_element("Button")))
#'
#' wrapped <- ink_ansi_wrap("Save", terminal)
#' # Show the escape sequences literally:
#' encodeString(wrapped)
#' # cat(wrapped, "\n") would display red bold text on an ANSI terminal.
#' @export
ink_ansi_wrap <- function(text, style, reset = TRUE) {
  text <- check_wrap_text(text)
  validate_terminal_style(style)
  if (!is.logical(reset) || length(reset) != 1L || is.na(reset)) {
    abort_terminal("invalid_reset", sprintf("`reset` must be TRUE or FALSE, not %s.", describe_input(reset)), "reset")
  }
  text <- sanitize_terminal_text(text)
  parameters <- sgr_parameters(style)
  if (length(parameters) == 0L || !nzchar(text)) {
    return(text)
  }
  paste0("\033[", paste(parameters, collapse = ";"), "m", text, if (reset) "\033[0m" else "")
}

check_wrap_text <- function(text) {
  if (!is_plain_string(text)) {
    abort_terminal(
      "invalid_text",
      sprintf("`text` must be one non-missing character string, not %s.", describe_input(text)),
      "text"
    )
  }
  text <- enc2utf8(as.vector(text, mode = "character"))
  if (!validUTF8(text)) {
    abort_terminal("invalid_text", "`text` is not valid UTF-8.", "text")
  }
  text
}

# Removes the control characters described in the documentation above.
sanitize_terminal_text <- function(text) {
  gsub("[\\x01-\\x08\\x0B-\\x1F\\x7F\\x{80}-\\x{9F}]", "", text, perl = TRUE)
}

# The SGR parameters for a style, in output order; character(0) if there is no
# styling to apply.
sgr_parameters <- function(style) {
  attributes <- as.character(SGR_ATTRIBUTES[style$text_styles$enabled])
  foreground <- sgr_color(style$foreground, background = FALSE)
  background <- sgr_color(style$background, background = TRUE)
  c(attributes, foreground, background)
}

sgr_color <- function(color, background) {
  switch(color$kind,
    rgb = c(if (background) "48" else "38", "2", as.character(color$rgb)),
    palette256 = c(if (background) "48" else "38", "5", as.character(color$index)),
    palette16 = as.character(
      if (color$index < 8L) (if (background) 40L else 30L) + color$index
      else (if (background) 100L else 90L) + color$index - 8L
    ),
    default = if (!isTRUE(color$explicit)) character() else if (background) "49" else "39",
    none = character()
  )
}

# Structure check for the style given to ink_ansi_wrap(): enough to serialize it
# safely, without trusting its contents.
validate_terminal_style <- function(style) {
  fail <- function(what) {
    abort_terminal("invalid_terminal_style", sprintf("`style` is not a valid ink_terminal_style: %s.", what), "style")
  }
  if (!inherits(style, "ink_terminal_style") || !is.list(style)) {
    abort_terminal(
      "invalid_terminal_style",
      sprintf("`style` must be created by ink_terminal_style(), not %s.", describe_input(style)),
      "style"
    )
  }
  for (name in c("foreground", "background")) {
    color <- style[[name]]
    if (!inherits(color, "ink_terminal_color") || !(color$kind %in% c("rgb", "palette256", "palette16", "default", "none"))) {
      fail(sprintf("`%s` is not a terminal color", name))
    }
    ok <- switch(color$kind,
      rgb = is.numeric(color$rgb) && length(color$rgb) == 3L && !anyNA(color$rgb) && all(color$rgb >= 0 & color$rgb <= 255),
      palette256 = is.numeric(color$index) && length(color$index) == 1L && color$index %in% 0:255,
      palette16 = is.numeric(color$index) && length(color$index) == 1L && color$index %in% 0:15,
      TRUE
    )
    if (!isTRUE(ok)) fail(sprintf("`%s` has invalid values", name))
  }
  enabled <- style$text_styles$enabled
  if (!is.character(enabled) || !all(enabled %in% TEXT_STYLE_FLAGS)) {
    fail("`text_styles$enabled` has unknown attributes")
  }
  invisible(style)
}
