#' Adapt a computed style to a terminal
#'
#' `ink_terminal_style()` turns an [ink_computed_style] into a description of how
#' it can be shown on a terminal with the given [capabilities][ink_terminal_capabilities]:
#' resolved colors, text attributes, border characters, spacing and size
#' metadata. It is a *description*, not a rendering: nothing is drawn, written to
#' the terminal, laid out or wrapped. A host TUI framework decides where things
#' go; the adapter supplies the ingredients. [ink_ansi_wrap()] turns the colors
#' and attributes into escape sequences for a piece of text.
#'
#' The computed style is not modified and does not know about terminals: one
#' computed style can be adapted for any number of capability profiles. Styles
#' from [ink_compute()] and from [ink_session_compute()] are accepted alike.
#'
#' @section Structure of an `ink_terminal_style`:
#' A list with
#' * `foreground`, `background`: `ink_terminal_color` descriptors for `color` and
#'   `background`.
#' * `text_styles`: `list(requested, enabled)`: the attributes the style asked
#'   for and those the terminal supports (a subset, in the fixed order `bold`,
#'   `dim`, `italic`, `underline`, `reverse`, `strike`).
#' * `border`: `list(style, glyphs, color, unicode)`. `glyphs` is a named
#'   character vector (`top_left`, `top_right`, `bottom_left`, `bottom_right`,
#'   `horizontal`, `vertical`) or `NULL` for `none`; `color` is a descriptor
#'   (separate from the glyphs) or `NULL`.
#' * `padding`, `margin`: integer vectors `c(top, right, bottom, left)` in
#'   terminal cells, exactly as computed.
#' * `width`, `height`: the computed `ink_length` values (cells or `auto`),
#'   untouched. Nothing is resolved; `auto` stays `auto`.
#' * `extra_properties`: every other registered property (custom ones such as
#'   `row-stripe`), as the original typed values. The adapter gives them no
#'   meaning of its own; they are kept for the host.
#' * `capabilities`: the profile used. `matte`: the matte as an integer
#'   `c(r, g, b)`, or `NULL`.
#'
#' A color descriptor has `kind` (`"rgb"`, `"palette256"`, `"palette16"`,
#' `"default"` or `"none"`), `source` (what the Ink color was: `"rgb"`, `"ansi"`,
#' `"default"` or `"transparent"`), `rgb` (the opaque RGB it stands for, if any),
#' `index` (palette index), `reason` (for `"none"`: `"transparent"` or
#' `"color_depth_none"`) and `explicit` (`FALSE` only for a `default` that was not asked
#' for by the element's own declarations, which needs no instruction).
#'
#' @section Colors:
#' * **Truecolor**: opaque RGB colors are kept exactly (`#7c3aed` is
#'   124, 58, 237).
#' * **ansi256**: an RGB color is mapped to the xterm palette. Each channel of
#'   the 6 x 6 x 6 cube (levels 0, 95, 135, 175, 215, 255; indices 16-231) snaps
#'   to its nearest level (the lower on a tie); the nearest grey of the 24-step
#'   ramp (8, 18, ..., 238; indices 232-255) is the alternative. Whichever is
#'   nearer in squared RGB distance wins; the cube wins a tie. Indices 0-15 are
#'   theme-dependent and are not used for RGB colors. The mapping is not
#'   lossless.
#' * **ansi16**: an RGB color goes to the nearest of the 16 colors of a fixed
#'   reference palette (the xterm defaults) by squared RGB distance; the lowest
#'   index wins a tie.
#' * **none**: no color instructions at all. Colors are not replaced by black.
#' * **Named ANSI colors** (`red`, `bright-blue`, ...) stay ANSI colors at every
#'   depth except `"none"`: their appearance is the terminal theme's.
#' * **`default`** is the terminal's own color, an instruction in its own right.
#'   **`transparent`** is the absence of an instruction (no background fill; for a
#'   foreground, nothing to set). They are different values and stay different.
#'
#' @section Alpha:
#' Terminals have no alpha. A color with alpha 255 is used as is; a background
#' with alpha 0 is no fill. Any other alpha is composited onto an opaque RGB color
#' before quantization: `result = alpha * color + (1 - alpha) * underlying` per
#' channel (alpha = a / 255, rounded half up). A partially transparent foreground
#' or border color sits on the style's resolved background when that is a concrete
#' RGB, otherwise on `matte`; a partially transparent background sits on `matte`.
#' If a color needs compositing and no concrete underlying color is known, an
#' `ink_terminal_error` with code `unknown_alpha_background` is signalled: the
#' terminal's real background is never guessed. With `color_depth = "none"` no
#' color is shown, so nothing is composited.
#'
#' @section Borders:
#' | Style  | corners (tl tr bl br)       | horizontal | vertical |
#' |--------|-----------------------------|------------|----------|
#' | single | U+250C U+2510 U+2514 U+2518 | U+2500     | U+2502   |
#' | double | U+2554 U+2557 U+255A U+255D | U+2550     | U+2551   |
#' | round  | U+256D U+256E U+2570 U+256F | U+2500     | U+2502   |
#' | heavy  | U+250F U+2513 U+2517 U+251B | U+2501     | U+2503   |
#'
#' With `unicode = FALSE` every style uses `+` for the corners, `-` for the
#' horizontal and `|` for the vertical edges. `none` has no glyphs.
#'
#' @param style An [ink_computed_style].
#' @param capabilities An [ink_terminal_capabilities] profile. The default is
#'   truecolor, Unicode, all text attributes.
#' @param matte `NULL`, or an opaque RGB color (a string such as `"#181825"`) used
#'   to composite partially transparent colors onto when no concrete background
#'   is otherwise known.
#' @param x An `ink_terminal_style`.
#' @param ... Ignored.
#' @return An `ink_terminal_style`. Invalid input signals an
#'   `ink_terminal_error` ([ink_terminal_error]). `print()` returns `x`
#'   invisibly.
#' @seealso [ink_terminal_capabilities()], [ink_ansi_wrap()], [ink_compute()]
#' @name ink_terminal_style
#' @examples
#' sheet <- ink_compile(ink_parse("
#'   $accent: #7c3aed;
#'   Button.primary {
#'     color: white;
#'     background: $accent;
#'     text-style: bold;
#'     padding: 1 2;
#'     border: round $accent;
#'   }
#' "))
#' computed <- ink_compute(sheet, ink_element("Button", classes = "primary"))
#'
#' ink_terminal_style(computed)
#' # The same style for a 16-color, ASCII-only terminal:
#' ink_terminal_style(computed, ink_terminal_capabilities("ansi16", unicode = FALSE))
#' @export
ink_terminal_style <- function(style, capabilities = ink_terminal_capabilities(), matte = NULL) {
  validate_capabilities(capabilities)
  matte_rgb <- check_matte(matte)
  values <- check_computed_style(style)

  depth <- capabilities$color_depth
  border <- values[["border"]]
  colors <- resolve_terminal_colors(
    values[["color"]], values[["background"]],
    if (identical(border$style, "none")) NULL else border$color,
    depth, matte_rgb
  )

  colors$foreground <- mark_implicit_default(colors$foreground, style, "color")
  colors$background <- mark_implicit_default(colors$background, style, "background")

  structure(
    list(
      foreground = colors$foreground,
      background = colors$background,
      text_styles = terminal_text_styles(values[["text-style"]], capabilities),
      border = terminal_border(border, capabilities, colors$border),
      padding = values[["padding"]]$values,
      margin = values[["margin"]]$values,
      width = values[["width"]],
      height = values[["height"]],
      extra_properties = values[setdiff(names(values), names(BUILTIN_TERMINAL_PROPERTIES))],
      capabilities = capabilities,
      matte = matte_rgb
    ),
    class = "ink_terminal_style"
  )
}

# A `default` colour that the element's own declarations did not ask for -- the
# property is unset (provenance "initial") or its value was merely inherited
# ("inherited"; the default may have come all the way from the registry) -- is
# marked implicit: serialization sends no instruction for it. `default` from a
# declaration, or from `inherit` / `initial`, stays explicit. A style without
# provenance is treated as explicit.
mark_implicit_default <- function(color, style, property) {
  origin <- style$provenance[[property]]$origin
  if (identical(color$kind, "default") && (identical(origin, "initial") || identical(origin, "inherited"))) {
    color$explicit <- FALSE
  }
  color
}

# The built-in properties the adapter understands, with the class each must have.
BUILTIN_TERMINAL_PROPERTIES <- c(
  color = "ink_color", background = "ink_color", `text-style` = "ink_text_style",
  padding = "ink_spacing", margin = "ink_spacing", border = "ink_border",
  width = "ink_length", height = "ink_length"
)

# Checks a computed style and returns its values. Problems are reported, never
# repaired.
check_computed_style <- function(style) {
  if (!inherits(style, "ink_computed_style") || !is.list(style) || !is.list(style$values)) {
    abort_terminal(
      "invalid_computed_style",
      sprintf("`style` must be an ink_computed_style from ink_compute(), not %s.", describe_input(style)),
      "style"
    )
  }
  values <- style$values
  for (name in names(BUILTIN_TERMINAL_PROPERTIES)) {
    if (!inherits(values[[name]], BUILTIN_TERMINAL_PROPERTIES[[name]])) {
      abort_terminal(
        "invalid_computed_style",
        sprintf(
          "The computed style has no valid '%s' (expected an %s, found %s).",
          name, BUILTIN_TERMINAL_PROPERTIES[[name]], describe_input(values[[name]])
        ),
        "style"
      )
    }
  }
  if (any(vapply(values, inherits, NA, what = "ink_keyword"))) {
    abort_terminal("invalid_computed_style", "The computed style contains an unresolved inherit/initial keyword.", "style")
  }
  check_ink_color(values[["color"]], "color value")
  check_ink_color(values[["background"]], "background value")
  border <- values[["border"]]
  if (!is_plain_string(border$style) || !(border$style %in% BORDER_STYLES)) {
    abort_terminal("invalid_computed_style", "The border value has an unknown style.", "style")
  }
  if (!identical(border$style, "none")) {
    check_ink_color(border$color, "border color")
  }
  for (name in c("padding", "margin")) {
    sides <- values[[name]]$values
    if (!is.numeric(sides) || length(sides) != 4L || anyNA(sides) || any(sides < 0)) {
      abort_terminal("invalid_computed_style", sprintf("The %s value is not four non-negative cell counts.", name), "style")
    }
  }
  flags <- values[["text-style"]]$flags
  if (!is.character(flags) || !all(flags %in% TEXT_STYLE_FLAGS)) {
    abort_terminal("invalid_computed_style", "The text-style value has unknown flags.", "style")
  }
  values
}

#' @rdname ink_terminal_style
#' @export
print.ink_terminal_style <- function(x, ...) {
  caps <- x$capabilities
  cat(sprintf(
    "<ink_terminal_style> %s, %s\n",
    caps$color_depth, if (caps$unicode) "unicode" else "ASCII"
  ))
  show_side <- function(sides) paste0(names(sides), "=", sides, collapse = " ")
  show_length <- function(length) format(length)
  cat("  foreground  ", format(x$foreground), "\n", sep = "")
  cat("  background  ", format(x$background), "\n", sep = "")
  attrs <- x$text_styles
  cat(
    "  text styles ", if (length(attrs$enabled)) paste(attrs$enabled, collapse = " ") else "none",
    if (length(setdiff(attrs$requested, attrs$enabled))) {
      paste0("  (unsupported, omitted: ", paste(setdiff(attrs$requested, attrs$enabled), collapse = " "), ")")
    } else {
      ""
    },
    "\n", sep = ""
  )
  border <- x$border
  if (identical(border$style, "none")) {
    cat("  border      none\n")
  } else {
    g <- border$glyphs
    cat(
      "  border      ", border$style, "  ", g[["top_left"]], g[["horizontal"]], g[["top_right"]],
      " / ", g[["vertical"]], " / ", g[["bottom_left"]], g[["horizontal"]], g[["bottom_right"]],
      "  color ", format(border$color), "\n", sep = ""
    )
  }
  cat("  padding     ", show_side(x$padding), "\n", sep = "")
  cat("  margin      ", show_side(x$margin), "\n", sep = "")
  cat("  width       ", show_length(x$width), "\n", sep = "")
  cat("  height      ", show_length(x$height), "\n", sep = "")
  if (length(x$extra_properties)) {
    cat("  other       ", paste(names(x$extra_properties), collapse = ", "), "\n", sep = "")
  }
  invisible(x)
}
