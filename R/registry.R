#' Property registry
#'
#' The registry says which properties a stylesheet may use and what type of
#' value each takes. [ink_compile()] rejects every property that is not in
#' the registry it is given.
#'
#' `ink_registry()` returns a registry holding the built-in properties.
#' `ink_register_property()` returns a copy with one more property; the
#' registry you pass in is never modified, so the built-in set cannot be
#' changed by accident.
#'
#' @section Built-in properties:
#' | Property     | Type         | Inherits | Initial value |
#' |--------------|--------------|----------|---------------|
#' | `color`      | `color`      | yes      | `default`     |
#' | `background` | `color`      | no       | `transparent` |
#' | `text-style` | `text_style` | yes      | `none`        |
#' | `padding`    | `spacing`    | no       | `0`           |
#' | `margin`     | `spacing`    | no       | `0`           |
#' | `border`     | `border`     | no       | `none`        |
#' | `width`      | `length`     | no       | `auto`        |
#' | `height`     | `length`     | no       | `auto`        |
#'
#' The `inherits` flag and initial value are metadata for the later cascade
#' stage; compiling does not use them.
#'
#' @section Value types:
#' `type` is one of `"color"`, `"spacing"`, `"border"`, `"length"` or
#' `"text_style"`. Their syntax and representation are described in
#' [ink_values]. Custom value parsers are not supported.
#'
#' @section Structure:
#' A registry is a list with one field, `properties`: a list of properties
#' named by property name, in registration order. Each property is a list with
#' `name`, `type`, `initial` (a typed value) and `inherits`. Treat it as
#' read-only: [ink_compile()] checks the registry and signals
#' `ink_compile_error` with code `"invalid_registry_definition"` if it has
#' been tampered with.
#'
#' @param registry An `ink_registry`.
#' @param name Property name: ASCII letters, digits, `_` and `-`, starting with
#'   a letter or `_`. Names are case-sensitive and must not already be
#'   registered.
#' @param type One of the value types above.
#' @param initial The property's initial value: either a string in the
#'   property's value syntax (`"transparent"`, `"1 2"`), which is parsed and
#'   normalized, or an already typed value of the right class. Variables and
#'   `inherit` / `initial` are not allowed.
#' @param inherits Whether the property is inherited from the parent element.
#' @param x An `ink_registry`.
#' @param ... Ignored.
#' @return `ink_registry()` and `ink_register_property()` return an
#'   `ink_registry`. Invalid definitions signal an `ink_compile_error` with
#'   code `"invalid_registry_definition"`; see [ink_compile_error].
#' @name ink_registry
#' @examples
#' registry <- ink_registry()
#' registry <- ink_register_property(
#'   registry,
#'   name = "row-stripe",
#'   type = "color",
#'   initial = "transparent",
#'   inherits = FALSE
#' )
#' registry
#'
#' sheet <- ink_compile(ink_parse("DataTable { row-stripe: #242424; }"), registry)
#' sheet$rules[[1]]$declarations[[1]]$value
NULL

#' @rdname ink_registry
#' @export
ink_registry <- function() {
  specs <- builtin_property_specs()
  properties <- lapply(specs, function(spec) {
    new_ink_property(
      spec$name, spec$type,
      normalize_initial(spec$name, spec$type, spec$initial),
      spec$inherits
    )
  })
  names(properties) <- vapply(specs, function(spec) spec$name, "")
  new_ink_registry(properties)
}

#' @rdname ink_registry
#' @export
ink_register_property <- function(registry, name, type, initial, inherits = FALSE) {
  validate_registry(registry)
  check_property_name(name)
  check_property_type(name, type)
  if (!is.null(registry$properties[[name]])) {
    abort_registry(sprintf("Property '%s' is already registered.", name), name)
  }
  if (!is.logical(inherits) || length(inherits) != 1L || is.na(inherits)) {
    abort_registry(
      sprintf("`inherits` for property '%s' must be TRUE or FALSE.", name), name
    )
  }
  property <- new_ink_property(name, type, normalize_initial(name, type, initial), inherits)
  registry$properties[[name]] <- property
  registry
}

new_ink_registry <- function(properties) {
  structure(list(properties = properties), class = "ink_registry")
}

check_property_name <- function(name) {
  if (!is.character(name) || length(name) != 1L || is.na(name)) {
    abort_registry("A property name must be a single string.")
  }
  if (!grepl("^[A-Za-z_][A-Za-z0-9_-]*$", name)) {
    abort_registry(
      sprintf(
        "Invalid property name '%s'. Names use ASCII letters, digits, '_' and '-', and start with a letter or '_'.",
        name
      ),
      name
    )
  }
}

check_property_type <- function(name, type) {
  if (!is.character(type) || length(type) != 1L || is.na(type) || !(type %in% VALUE_TYPES)) {
    shown <- if (is.character(type) && length(type) == 1L && !is.na(type)) type else "<invalid>"
    abort_registry(
      sprintf(
        "Unknown type '%s' for property '%s'. Supported types: %s.",
        shown, name, paste(VALUE_TYPES, collapse = ", ")
      ),
      name
    )
  }
}

# `initial` may be source text or an already typed value; returns a typed value.
normalize_initial <- function(name, type, initial) {
  if (is.character(initial) && length(initial) == 1L && !is.na(initial)) {
    return(tryCatch(
      value_from_text(type, initial),
      ink_value_problem = function(problem) {
        abort_registry(
          sprintf(
            "Invalid initial value '%s' for %s property '%s'. %s",
            initial, type, name, problem$expected
          ),
          name
        )
      }
    ))
  }
  if (inherits(initial, VALUE_CLASSES[[type]])) {
    return(initial)
  }
  abort_registry(
    sprintf(
      "The initial value of %s property '%s' must be a string or an %s value.",
      type, name, VALUE_CLASSES[[type]]
    ),
    name
  )
}

# Registries are plain lists, so anything could have been put in one. Checked
# before a registry is extended or used for compilation.
validate_registry <- function(registry) {
  if (!inherits(registry, "ink_registry") || !is.list(registry$properties)) {
    abort_registry("`registry` must be created by ink_registry().")
  }
  properties <- registry$properties
  keys <- names(properties)
  if (length(properties) > 0L && (is.null(keys) || anyNA(keys) || any(keys == "") || anyDuplicated(keys))) {
    abort_registry("The registry has missing or duplicated property names.")
  }
  for (key in keys) {
    validate_registered_property(key, properties[[key]])
  }
  invisible(registry)
}

validate_registered_property <- function(key, property) {
  valid <- inherits(property, "ink_property") &&
    identical(property$name, key) &&
    is.character(property$type) && length(property$type) == 1L &&
    property$type %in% VALUE_TYPES &&
    inherits(property$initial, VALUE_CLASSES[[property$type]]) &&
    is.logical(property$inherits) && length(property$inherits) == 1L &&
    !is.na(property$inherits)
  if (!isTRUE(valid)) {
    abort_registry(sprintf("The registry entry for '%s' is not a valid property definition.", key), key)
  }
}

#' @rdname ink_registry
#' @export
print.ink_registry <- function(x, ...) {
  properties <- x$properties
  cat(sprintf(
    "<ink_registry> %d propert%s\n",
    length(properties), if (length(properties) == 1L) "y" else "ies"
  ))
  for (property in properties) {
    cat(sprintf(
      "  %-14s %-11s %-9s initial: %s\n",
      property$name, property$type,
      if (property$inherits) "inherits" else "own",
      format(property$initial)
    ))
  }
  invisible(x)
}
