# Colors. See ?ink_values for the representation.

ANSI_BASE_COLORS <- c(
  "black", "red", "green", "yellow", "blue", "magenta", "cyan", "white"
)

# All sixteen ANSI names in palette order; the position is the index + 1.
ANSI_COLOR_NAMES <- c(ANSI_BASE_COLORS, paste0("bright-", ANSI_BASE_COLORS))

COLOR_EXPECTED <- paste(
  "Expected a hex color (#rgb, #rgba, #rrggbb or #rrggbbaa),",
  "an ANSI color name (black, red, green, yellow, blue, magenta, cyan, white,",
  "or a bright- variant such as bright-red), 'default' or 'transparent'."
)

# Constructors ---------------------------------------------------------------

new_ink_color_rgb <- function(r, g, b, a = 255L) {
  channels <- c(r, g, b, a)
  stopifnot(
    is.numeric(channels), length(channels) == 4L, !anyNA(channels),
    all(channels == round(channels)), all(channels >= 0L & channels <= 255L)
  )
  structure(
    list(
      kind = "rgb",
      r = as.integer(r), g = as.integer(g), b = as.integer(b), a = as.integer(a)
    ),
    class = "ink_color"
  )
}

new_ink_color_ansi <- function(name) {
  index <- match(name, ANSI_COLOR_NAMES)
  stopifnot(length(name) == 1L, !is.na(index))
  structure(
    list(kind = "ansi", name = name, index = index - 1L),
    class = "ink_color"
  )
}

new_ink_color_default <- function() {
  structure(list(kind = "default"), class = "ink_color")
}

new_ink_color_transparent <- function() {
  structure(list(kind = "transparent"), class = "ink_color")
}

# Parsing --------------------------------------------------------------------

parse_color_tokens <- function(tokens) {
  if (length(tokens) != 1L) {
    abort_value_problem("invalid_color", COLOR_EXPECTED)
  }
  token <- tokens[[1L]]
  if (token$type == "hex_color") {
    return(color_from_hex(token$value))
  }
  if (token$type == "ident") {
    return(color_from_keyword(token$value))
  }
  abort_value_problem(
    "invalid_color",
    sprintf("'%s' is not a color. %s", token$value, COLOR_EXPECTED)
  )
}

color_from_hex <- function(text) {
  digits <- substring(text, 2L)
  if (!(nchar(digits) %in% c(3L, 4L, 6L, 8L)) || !grepl("^[0-9A-Fa-f]+$", digits)) {
    abort_value_problem(
      "invalid_color",
      sprintf("'%s' is not a valid hex color. %s", text, COLOR_EXPECTED)
    )
  }
  if (nchar(digits) <= 4L) {
    pairs <- strsplit(digits, "")[[1L]]
    digits <- paste0(pairs, pairs, collapse = "")
  }
  starts <- seq(1L, nchar(digits), by = 2L)
  channels <- strtoi(substring(digits, starts, starts + 1L), 16L)
  new_ink_color_rgb(channels[[1L]], channels[[2L]], channels[[3L]], if (length(channels) == 4L) channels[[4L]] else 255L)
}

color_from_keyword <- function(name) {
  if (name == "default") {
    return(new_ink_color_default())
  }
  if (name == "transparent") {
    return(new_ink_color_transparent())
  }
  if (name %in% ANSI_COLOR_NAMES) {
    return(new_ink_color_ansi(name))
  }
  abort_value_problem(
    "invalid_color",
    sprintf("Unknown color '%s'. %s", name, COLOR_EXPECTED)
  )
}

# Formatting -----------------------------------------------------------------

#' @export
#' @noRd
format.ink_color <- function(x, ...) {
  switch(x$kind,
    rgb = if (x$a == 255L) {
      sprintf("#%02x%02x%02x", x$r, x$g, x$b)
    } else {
      sprintf("#%02x%02x%02x%02x", x$r, x$g, x$b, x$a)
    },
    ansi = x$name,
    default = "default",
    transparent = "transparent"
  )
}

#' @export
#' @noRd
print.ink_color <- function(x, ...) print_value(x)
