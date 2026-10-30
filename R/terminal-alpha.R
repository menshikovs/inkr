# Alpha compositing.
#
# Terminals have no alpha channel, so a colour with partial alpha is composited
# onto a concrete RGB colour before it is quantized or sent:
#
#   result = alpha * colour + (1 - alpha) * underlying      (per channel)
#
# with alpha = a / 255 and the result rounded half up to a whole 0-255 value.
# The underlying colour must be known: the element's own opaque background, or
# an explicit `matte`. The terminal's real background is unknowable, so it is
# never guessed.

composite_rgb <- function(rgb, alpha255, underlying) {
  alpha <- alpha255 / 255
  as.integer(floor(alpha * rgb + (1 - alpha) * underlying + 0.5))
}

# `matte` -> NULL or an integer c(r, g, b). It must be an opaque RGB colour: a
# hex string such as "#181825" or an `ink_color` of kind "rgb" with alpha 255.
# ANSI names, `default` and `transparent` are not concrete colours.
check_matte <- function(matte) {
  if (is.null(matte)) {
    return(NULL)
  }
  color <- matte
  if (is_plain_string(matte)) {
    color <- tryCatch(
      value_from_text("color", matte),
      ink_value_problem = function(problem) {
        abort_terminal("invalid_matte", sprintf("`matte` %s is not a color. %s", show_text(matte), problem$expected), "matte")
      }
    )
  }
  if (!inherits(color, "ink_color")) {
    abort_terminal(
      "invalid_matte",
      sprintf("`matte` must be NULL, a color string such as \"#181825\" or an ink color, not %s.", describe_input(matte)),
      "matte"
    )
  }
  if (!identical(color$kind, "rgb") || !isTRUE(color$a == 255L)) {
    abort_terminal(
      "invalid_matte",
      "`matte` must be an opaque RGB color (for example \"#181825\"): ANSI names, `default`, `transparent` and colors with alpha are not concrete.",
      "matte"
    )
  }
  check_ink_color(color, "matte")
  c(color$r, color$g, color$b)
}

abort_unknown_background <- function(what) {
  abort_terminal(
    "unknown_alpha_background",
    sprintf(
      "The %s has partial transparency but no concrete background to composite it onto. Supply `matte`, an opaque RGB color such as \"#181825\".",
      what
    ),
    "matte"
  )
}
