# Colour descriptors: what a terminal is asked to display for one colour.
#
# An `ink_terminal_color` has
#   kind    "rgb" (24-bit), "palette256" (xterm index 16-255), "palette16"
#           (ANSI index 0-15), "default" (the terminal's own colour) or
#           "none" (no instruction);
#   source  what the Ink colour was: "rgb", "ansi", "default" or "transparent";
#   rgb     the opaque RGB the instruction stands for (after alpha compositing),
#           or NULL when not applicable (named ANSI colours, default, none);
#   index   the palette index for the two palette kinds, else NULL;
#   reason  for kind "none": "transparent" (no fill / nothing to draw) or
#           "color_depth_none" (the terminal shows no colour);
#   explicit  FALSE only for a `default` colour that was never set (the computed
#           style's provenance says "initial"): it is the terminal's colour anyway,
#           so serialization sends nothing for it. TRUE for everything else,
#           including a `default` that was declared, inherited or asked for with
#           `inherit` / `initial`.
#
# `default` and `transparent` stay different: `default` is an instruction (the
# terminal's own colour), `transparent` is the absence of one.

new_terminal_color <- function(kind, source, rgb = NULL, index = NULL, reason = NULL, explicit = TRUE) {
  structure(
    list(kind = kind, source = source, rgb = rgb, index = index, reason = reason, explicit = explicit),
    class = "ink_terminal_color"
  )
}

terminal_none <- function(source, reason) {
  new_terminal_color("none", source, reason = reason)
}

# Validation of an Ink colour value (an `ink_color`), without repairing it.
check_ink_color <- function(color, what) {
  fail <- function() {
    abort_terminal("invalid_color_value", sprintf("The %s is not a valid ink_color.", what), "style")
  }
  if (!inherits(color, "ink_color") || !is.list(color) || !is_plain_string(color$kind)) fail()
  valid <- switch(color$kind,
    rgb = {
      channels <- c(color$r, color$g, color$b, color$a)
      is.numeric(channels) && length(channels) == 4L && !anyNA(channels) &&
        all(channels == round(channels)) && all(channels >= 0 & channels <= 255)
    },
    ansi = is.numeric(color$index) && length(color$index) == 1L && !is.na(color$index) &&
      color$index %in% 0:15,
    default = ,
    transparent = TRUE,
    FALSE
  )
  if (!isTRUE(valid)) fail()
  invisible(color)
}

terminal_color_from_rgb <- function(rgb, depth, source) {
  rgb <- as.integer(rgb)
  switch(depth,
    truecolor = new_terminal_color("rgb", source, rgb = rgb),
    ansi256 = new_terminal_color("palette256", source, rgb = rgb, index = nearest_ansi256(rgb)),
    ansi16 = new_terminal_color("palette16", source, rgb = rgb, index = nearest_ansi16(rgb))
  )
}

# A foreground-like colour (foreground, border): `underlying` is the concrete RGB
# a partially transparent colour is composited onto, or NULL if unknown.
resolve_foreground_color <- function(color, depth, underlying, what) {
  switch(color$kind,
    transparent = terminal_none("transparent", "transparent"),
    default = new_terminal_color("default", "default"),
    ansi = new_terminal_color("palette16", "ansi", index = as.integer(color$index)),
    rgb = {
      rgb <- c(color$r, color$g, color$b)
      if (color$a == 255L) {
        terminal_color_from_rgb(rgb, depth, "rgb")
      } else {
        if (is.null(underlying)) abort_unknown_background(what)
        terminal_color_from_rgb(composite_rgb(rgb, color$a, underlying), depth, "rgb")
      }
    }
  )
}

# The background: returns the descriptor and the concrete RGB it stands for (if
# any), which later foreground compositing uses. Alpha 0 draws nothing.
resolve_background_color <- function(color, depth, matte) {
  switch(color$kind,
    transparent = list(color = terminal_none("transparent", "transparent"), rgb = NULL),
    default = list(color = new_terminal_color("default", "default"), rgb = NULL),
    ansi = list(color = new_terminal_color("palette16", "ansi", index = as.integer(color$index)), rgb = NULL),
    rgb = {
      rgb <- c(color$r, color$g, color$b)
      if (color$a == 0L) {
        list(color = terminal_none("rgb", "transparent"), rgb = NULL)
      } else if (color$a == 255L) {
        list(color = terminal_color_from_rgb(rgb, depth, "rgb"), rgb = as.integer(rgb))
      } else {
        if (is.null(matte)) abort_unknown_background("background")
        flat <- composite_rgb(rgb, color$a, matte)
        list(color = terminal_color_from_rgb(flat, depth, "rgb"), rgb = flat)
      }
    }
  )
}

# Resolves foreground, background and (optionally) border colours together,
# because a partially transparent foreground or border sits on the background.
# With colour depth "none" nothing is displayed, so nothing is composited either.
resolve_terminal_colors <- function(foreground, background, border, depth, matte) {
  if (identical(depth, "none")) {
    off <- function(color) terminal_none(color$kind, "color_depth_none")
    return(list(
      foreground = off(foreground), background = off(background),
      border = if (is.null(border)) NULL else off(border)
    ))
  }
  back <- resolve_background_color(background, depth, matte)
  underlying <- back$rgb %||% matte
  list(
    foreground = resolve_foreground_color(foreground, depth, underlying, "foreground color"),
    background = back$color,
    border = if (is.null(border)) NULL else resolve_foreground_color(border, depth, underlying, "border color")
  )
}

#' @export
#' @noRd
format.ink_terminal_color <- function(x, ...) {
  switch(x$kind,
    rgb = sprintf("rgb(%d, %d, %d)", x$rgb[[1L]], x$rgb[[2L]], x$rgb[[3L]]),
    palette256 = sprintf("palette256(%d)", x$index),
    palette16 = sprintf("palette16(%d)", x$index),
    default = "default",
    none = sprintf("none (%s)", x$reason)
  )
}

#' @export
#' @noRd
print.ink_terminal_color <- function(x, ...) {
  cat("<ink_terminal_color> ", format(x), "\n", sep = "")
  invisible(x)
}
