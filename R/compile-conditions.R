#' Compilation errors
#'
#' [ink_compile()] and the property registry signal an error of class
#' `ink_compile_error` when the input is well-formed syntax but not a valid
#' stylesheet. Syntax errors remain `ink_parse_error` (see [ink_parse()]);
#' arguments of the wrong type (for example passing text to [ink_compile()])
#' are ordinary errors.
#'
#' The condition has these fields, in addition to the usual `message`:
#'
#' * `code`: a stable machine-readable string, see below.
#' * `detail`: the human-readable explanation without the location prefix that
#'   `message` carries (`"<source>:<line>:<column>: <detail>"`).
#' * `source`, `line`, `column`, `offset`: where the problem was found. These
#'   are `NULL` for registry errors, which have no source location.
#' * `span`: a list with `start` and `end` locations (see [ink_stylesheet_ast]
#'   for the conventions), or `NULL`. Value errors point at the declaration's
#'   value, unknown properties at the property name, circular references at
#'   the declaration of the first variable in the cycle.
#' * `property`: the property name, when the error concerns a declaration.
#' * `variable`: the variable name (without `$`), when the error concerns one.
#' * `chain`: for `"circular_variable"`, the variable names along the cycle,
#'   first and last element equal.
#'
#' @section Error codes:
#' * `undefined_variable`: a `$name` is used but never declared.
#' * `circular_variable`: variables refer to each other in a cycle. The message
#'   shows the chain, for example `$a -> $b -> $c -> $a`.
#' * `variable_too_large`: variable substitution would expand a value to more
#'   than 1000 tokens (a sign of exponential growth through nested variables).
#' * `unknown_property`: the property is not in the registry.
#' * `invalid_property_value`: the value cannot be tokenized, or is otherwise
#'   unusable regardless of the property's type.
#' * `invalid_color`, `invalid_spacing`, `invalid_border`, `invalid_length`,
#'   `invalid_text_style`: the value does not match the property's type.
#' * `invalid_registry_definition`: a property definition or registry is
#'   invalid (bad name, unknown type, invalid initial value, duplicate
#'   registration, or a registry object that was tampered with).
#'
#' Only the first problem found is reported. Variables are checked before
#' rules, so an error in a variable can be reported even if a rule earlier in
#' the file is also invalid.
#'
#' @name ink_compile_error
#' @seealso [ink_compile()]
#' @examples
#' err <- tryCatch(
#'   ink_compile(ink_parse("Button { padding: 1 two; }")),
#'   ink_compile_error = identity
#' )
#' err$code
#' conditionMessage(err)
NULL

new_ink_compile_error <- function(code, detail, source = NULL, span = NULL,
                                  property = NULL, variable = NULL, chain = NULL) {
  located <- !is.null(source) && !is.null(span)
  start <- span$start
  message <- if (located) {
    sprintf("%s:%s: %s", source, format_location(start), detail)
  } else {
    detail
  }
  structure(
    class = c("ink_compile_error", "error", "condition"),
    list(
      message = message,
      call = NULL,
      code = code,
      detail = detail,
      source = source,
      line = start$line,
      column = start$column,
      offset = start$offset,
      span = span,
      property = property,
      variable = variable,
      chain = chain
    )
  )
}

ink_abort_compile <- function(code, detail, source = NULL, span = NULL,
                              property = NULL, variable = NULL, chain = NULL) {
  stop(new_ink_compile_error(code, detail, source, span, property, variable, chain))
}

# Where an error is reported: the source name, the span to point at, and what
# is being compiled (for messages and the `property` / `variable` fields).
new_compile_context <- function(source, span, owner, property = NULL, variable = NULL) {
  list(source = source, span = span, owner = owner, property = property, variable = variable)
}

abort_in_context <- function(ctx, code, detail, chain = NULL) {
  ink_abort_compile(
    code, detail,
    source = ctx$source, span = ctx$span,
    property = ctx$property, variable = ctx$variable, chain = chain
  )
}

# Errors for invalid registries and property definitions.
abort_registry <- function(detail, property = NULL) {
  ink_abort_compile("invalid_registry_definition", detail, property = property)
}
