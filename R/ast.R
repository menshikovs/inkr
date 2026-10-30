#' The inkr abstract syntax tree
#'
#' [ink_parse()] and [ink_read()] return an object of class
#' `ink_stylesheet_ast`: a plain list describing the stylesheet exactly as
#' written. Nothing is resolved or validated at this stage; variables are not
#' substituted and property values are kept as raw text, ready for a later
#' compilation step.
#'
#' @section Structure:
#' An `ink_stylesheet_ast` has two fields:
#'
#' * `statements`: a list of statement nodes in source order.
#' * `source`: the source name (`"<text>"` or the file path).
#'
#' Every node is a list with an S3 class (`ink_variable_declaration`,
#' `ink_rule`, ...) that also inherits from `ink_node`, and a `span` field
#' holding its source location. The node types are:
#'
#' * `ink_variable_declaration` (`$name: value;`): `name` (without the `$`),
#'   `value`, `value_span`, `span`.
#' * `ink_rule`: `selectors` (an `ink_selector_list`), `declarations` (a list
#'   of `ink_property_declaration`), `span` (from the first selector to the
#'   closing brace).
#' * `ink_selector_list` (`A, B`): `selectors`, a list of
#'   `ink_complex_selector`; `span`.
#' * `ink_complex_selector` (`A > B C`): `compounds`, a list of
#'   `ink_compound_selector`, and `combinators`, a character vector of length
#'   `length(compounds) - 1` holding `"descendant"` or `"child"`.
#'   `combinators[[i]]` joins `compounds[[i]]` and `compounds[[i + 1]]`.
#'   `span`.
#' * `ink_compound_selector` (`Button#ok.primary:focus`): `type` (a type name,
#'   `"*"` for the universal selector, or `NULL`), `id` (or `NULL`),
#'   `classes` and `pseudo_classes` (character vectors, in source order, without
#'   their `.` / `:` prefix), `span`.
#' * `ink_property_declaration` (`property: value;`): `name`, `value`,
#'   `value_span`, `span`.
#'
#' @section Raw values:
#' A declaration's `value` is its tokens' source text. Runs of whitespace and
#' comments between tokens are collapsed into one space, so `1   2` and
#' `1 /* gap */ 2` both give `"1 2"`.
#'
#' @section Source locations:
#' A location is a list with `line` and `column` (both 1-based) and `offset`
#' (0-based number of preceding characters). Columns and offsets count
#' characters, not bytes; a tab is one character. Lines are separated by
#' `"\n"`; `"\r"` is ordinary whitespace.
#'
#' A `span` is a list with `start` and `end` locations. `end` is exclusive:
#' it is the location just after the last character of the node. For example
#' the span of `Button` at the start of a file is `1:1` to `1:7`, offsets 0
#' to 6.
#'
#' @param x An `ink_stylesheet_ast`.
#' @param n Maximum number of statements to print: a single non-negative number
#'   (`Inf` prints all).
#' @param ... Ignored.
#' @return `print()` returns `x` invisibly.
#' @name ink_stylesheet_ast
#' @examples
#' ast <- ink_parse("Button.primary:focus { color: $primary; padding: 1 2; }")
#' ast
#'
#' rule <- ast$statements[[1]]
#' rule$selectors$selectors[[1]]$compounds[[1]]$classes
#' rule$declarations[[2]]$value
NULL

# Constructors ---------------------------------------------------------------
#
# The only place AST nodes are created. They check their inputs so that a
# malformed node fails here, not somewhere downstream.

new_ink_stylesheet_ast <- function(statements, source) {
  stopifnot(
    is.list(statements),
    all(vapply(statements, inherits, logical(1), what = "ink_node")),
    is.character(source), length(source) == 1L
  )
  structure(
    list(statements = statements, source = source),
    class = "ink_stylesheet_ast"
  )
}

new_ink_node <- function(class, fields) {
  structure(fields, class = c(class, "ink_node"))
}

new_ink_variable_declaration <- function(name, value, value_span, span) {
  stopifnot(is.character(name), length(name) == 1L, is.character(value), length(value) == 1L)
  new_ink_node(
    "ink_variable_declaration",
    list(name = name, value = value, value_span = value_span, span = span)
  )
}

new_ink_property_declaration <- function(name, value, value_span, span) {
  stopifnot(is.character(name), length(name) == 1L, is.character(value), length(value) == 1L)
  new_ink_node(
    "ink_property_declaration",
    list(name = name, value = value, value_span = value_span, span = span)
  )
}

new_ink_rule <- function(selectors, declarations, span) {
  stopifnot(
    inherits(selectors, "ink_selector_list"),
    is.list(declarations),
    all(vapply(declarations, inherits, logical(1), what = "ink_property_declaration"))
  )
  new_ink_node(
    "ink_rule",
    list(selectors = selectors, declarations = declarations, span = span)
  )
}

new_ink_selector_list <- function(selectors, span) {
  stopifnot(
    length(selectors) >= 1L,
    all(vapply(selectors, inherits, logical(1), what = "ink_complex_selector"))
  )
  new_ink_node("ink_selector_list", list(selectors = selectors, span = span))
}

new_ink_complex_selector <- function(compounds, combinators, span) {
  stopifnot(
    length(compounds) >= 1L,
    all(vapply(compounds, inherits, logical(1), what = "ink_compound_selector")),
    is.character(combinators),
    length(combinators) == length(compounds) - 1L,
    all(combinators %in% c("descendant", "child"))
  )
  new_ink_node(
    "ink_complex_selector",
    list(compounds = compounds, combinators = combinators, span = span)
  )
}

new_ink_compound_selector <- function(type, id, classes, pseudo_classes, span) {
  stopifnot(
    is.null(type) || (is.character(type) && length(type) == 1L),
    is.null(id) || (is.character(id) && length(id) == 1L),
    is.character(classes),
    is.character(pseudo_classes)
  )
  # `list(type = NULL)` keeps the element, so the structure is always the same.
  new_ink_node(
    "ink_compound_selector",
    list(
      type = type, id = id, classes = classes,
      pseudo_classes = pseudo_classes, span = span
    )
  )
}

# Printing -------------------------------------------------------------------

#' @rdname ink_stylesheet_ast
#' @export
print.ink_stylesheet_ast <- function(x, n = 20L, ...) {
  n <- check_print_limit(n)
  count <- length(x$statements)
  cat(
    sprintf(
      "<ink_stylesheet_ast> %s: %d statement%s\n",
      x$source, count, if (count == 1L) "" else "s"
    )
  )
  shown <- seq_len(min(count, n))
  for (i in shown) {
    node <- x$statements[[i]]
    cat(sprintf(
      "  [%d] %s  (%s)\n",
      i, format_statement(node), format_location(node$span$start)
    ))
  }
  if (count > length(shown)) {
    cat(sprintf("  ... and %d more\n", count - length(shown)))
  }
  invisible(x)
}

format_statement <- function(node) {
  if (inherits(node, "ink_variable_declaration")) {
    return(sprintf("$%s: %s;", node$name, node$value))
  }
  count <- length(node$declarations)
  sprintf(
    "%s { %d declaration%s }",
    format_selector_list(node$selectors), count, if (count == 1L) "" else "s"
  )
}

format_selector_list <- function(x) {
  paste(vapply(x$selectors, format_complex_selector, ""), collapse = ", ")
}

format_complex_selector <- function(x) {
  compounds <- vapply(x$compounds, format_compound_selector, "")
  separators <- c("", ifelse(x$combinators == "child", " > ", " "))
  paste0(separators, compounds, collapse = "")
}

format_compound_selector <- function(x) {
  # paste0() would turn `paste0(".", character(0))` into ".", hence the helper.
  prefixed <- function(prefix, values) {
    if (length(values)) paste0(prefix, values, collapse = "") else ""
  }
  paste0(
    x$type %||% "",
    prefixed("#", x$id),
    prefixed(".", x$classes),
    prefixed(":", x$pseudo_classes)
  )
}
