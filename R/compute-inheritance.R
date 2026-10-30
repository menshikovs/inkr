# Keyword resolution and inheritance for one element.
#
# Input: the registry, the winning declarations from the cascade (named by
# property) and the *computed values of the parent* (a list named by property,
# or NULL for a root element). Output: a computed value and provenance for every
# registered property, in registry order.
#
# Inheritance always reads the parent's computed values, never declarations
# attached to the parent. A grandparent's `color` therefore reaches the child
# through the parent even if the parent declares nothing: the parent's computed
# `color` already is the grandparent's.

resolve_element_style <- function(registry, winners, parent_values) {
  properties <- registry$properties
  values <- vector("list", length(properties))
  origins <- vector("list", length(properties))
  for (k in seq_along(properties)) {
    resolved <- resolve_property(properties[[k]], winners[[properties[[k]]$name]], parent_values)
    values[[k]] <- resolved$value
    origins[[k]] <- resolved$origin
  }
  names(values) <- names(properties)
  names(origins) <- names(properties)
  list(values = values, provenance = origins)
}

# `winner` is the winning cascade candidate for the property, or NULL.
resolve_property <- function(property, winner, parent_values) {
  name <- property$name
  if (is.null(winner)) {
    return(resolve_undeclared(property, parent_values))
  }
  value <- winner$value
  if (inherits(value, "ink_keyword")) {
    return(resolve_keyword(property, winner, parent_values))
  }
  check_computed_value(property, value)
  list(value = value, origin = new_style_origin(name, "declaration", "declaration", winner))
}

# No declaration won: inherit if the property says so, otherwise (and at the
# root) use the registry's initial value.
resolve_undeclared <- function(property, parent_values) {
  name <- property$name
  if (isTRUE(property$inherits) && !is.null(parent_values)) {
    return(list(
      value = parent_value(parent_values, property),
      origin = new_style_origin(name, "inherited", "parent")
    ))
  }
  list(value = property$initial, origin = new_style_origin(name, "initial", "registry"))
}

# `inherit` takes the parent's computed value, even for properties that do not
# inherit by default; at the root there is no parent, so it becomes the initial
# value. `initial` always takes the registry's initial value, even for
# properties that inherit by default.
resolve_keyword <- function(property, winner, parent_values) {
  name <- property$name
  keyword <- winner$value$keyword
  if (identical(keyword, "inherit")) {
    if (!is.null(parent_values)) {
      return(list(
        value = parent_value(parent_values, property),
        origin = new_style_origin(name, "explicit_inherit", "parent", winner, keyword)
      ))
    }
    return(list(
      value = property$initial,
      origin = new_style_origin(name, "explicit_inherit", "registry", winner, keyword)
    ))
  }
  if (identical(keyword, "initial")) {
    return(list(
      value = property$initial,
      origin = new_style_origin(name, "explicit_initial", "registry", winner, keyword)
    ))
  }
  abort_compute(
    "invalid_computed_value",
    sprintf("Property '%s' has an unsupported keyword.", name)
  )
}

parent_value <- function(parent_values, property) {
  value <- parent_values[[property$name]]
  if (is.null(value)) {
    abort_compute(
      "invalid_computed_value",
      sprintf("The parent's computed style has no value for '%s'.", property$name)
    )
  }
  value
}

# A computed value must be a typed value of the property's type.
check_computed_value <- function(property, value) {
  if (!inherits(value, VALUE_CLASSES[[property$type]])) {
    abort_compute(
      "invalid_computed_value",
      sprintf(
        "The value of '%s' must be an %s, not %s.",
        property$name, VALUE_CLASSES[[property$type]], describe_input(value)
      )
    )
  }
}
