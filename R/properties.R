# Property definitions.
#
# A property definition is `list(name, type, initial, inherits)` of class
# `ink_property`: `type` is one of VALUE_TYPES, `initial` is a typed value of
# that type (already normalized), and `inherits` is a single TRUE/FALSE.

new_ink_property <- function(name, type, initial, inherits) {
  structure(
    list(name = name, type = type, initial = initial, inherits = inherits),
    class = "ink_property"
  )
}

# The built-in properties, in registry order. Initial values are given as
# source text and parsed when the registry is built, so they are checked by
# the same code as user input.
builtin_property_specs <- function() {
  spec <- function(name, type, initial, inherits) {
    list(name = name, type = type, initial = initial, inherits = inherits)
  }
  list(
    spec("color", "color", "default", TRUE),
    spec("background", "color", "transparent", FALSE),
    spec("text-style", "text_style", "none", TRUE),
    spec("padding", "spacing", "0", FALSE),
    spec("margin", "spacing", "0", FALSE),
    spec("border", "border", "none", FALSE),
    spec("width", "length", "auto", FALSE),
    spec("height", "length", "auto", FALSE)
  )
}
