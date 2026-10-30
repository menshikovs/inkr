# Border glyph definitions.
#
# The adapter supplies the characters a renderer needs to draw a border; it
# never draws one. Glyph code points are written as \u escapes so the source
# stays ASCII.

UNICODE_BORDER_GLYPHS <- list(
  single = c(
    top_left = "\u250c", top_right = "\u2510", bottom_left = "\u2514", bottom_right = "\u2518",
    horizontal = "\u2500", vertical = "\u2502"
  ),
  double = c(
    top_left = "\u2554", top_right = "\u2557", bottom_left = "\u255a", bottom_right = "\u255d",
    horizontal = "\u2550", vertical = "\u2551"
  ),
  round = c(
    top_left = "\u256d", top_right = "\u256e", bottom_left = "\u2570", bottom_right = "\u256f",
    horizontal = "\u2500", vertical = "\u2502"
  ),
  heavy = c(
    top_left = "\u250f", top_right = "\u2513", bottom_left = "\u2517", bottom_right = "\u251b",
    horizontal = "\u2501", vertical = "\u2503"
  )
)

# Every style falls back to the same plain ASCII box.
ASCII_BORDER_GLYPHS <- c(
  top_left = "+", top_right = "+", bottom_left = "+", bottom_right = "+",
  horizontal = "-", vertical = "|"
)

# `color` is the already resolved terminal colour of the border (or NULL for
# `none`). Colour and glyphs are kept apart.
terminal_border <- function(border, capabilities, color) {
  if (identical(border$style, "none")) {
    return(list(style = "none", glyphs = NULL, color = NULL, unicode = capabilities$unicode))
  }
  glyphs <- if (capabilities$unicode) UNICODE_BORDER_GLYPHS[[border$style]] else ASCII_BORDER_GLYPHS
  list(style = border$style, glyphs = glyphs, color = color, unicode = capabilities$unicode)
}
