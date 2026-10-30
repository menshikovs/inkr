#' Runtime elements
#'
#' `ink_element()` describes one logical component of a terminal UI for
#' selector matching: its type, ID, classes, active states and parent. It is
#' *not* a stylesheet node, and it does not render, handle events or know
#' anything about the terminal. (The AST classes inherit from `ink_node`;
#' elements are a separate concept and never inherit from it.)
#'
#' An element is an ordinary list; treat it as immutable. To "change" an
#' element, create a new one. The parent chain defines the logical tree, for
#' example `Screen#main` contains `Container#content.panel`, which contains
#' `Button#save.primary.rounded:focus`.
#'
#' Only parents are stored. There are no children and no back-references, so
#' an element can be matched without building the whole application tree.
#'
#' @section Fields:
#' * `type`: the component type, a single identifier such as `"Button"`.
#'   Matched by type selectors (`Button`). Case-sensitive. `"*"` is not a type.
#' * `id`: `NULL`, or a single identifier without the `#`. Matched by ID
#'   selectors (`#save`). Case-sensitive.
#' * `classes`: a character vector of identifiers without the `.`, duplicates
#'   removed, in order of first occurrence. Matched by class selectors.
#' * `states`: a character vector of identifiers without the `:`, duplicates
#'   removed. Matched by pseudo-class selectors (`:focus`). Any valid
#'   identifier is a state; `focus`, `selected` and `disabled` are conventional
#'   but not special. Ink does not decide whether a state is active: the
#'   application supplies the states.
#' * `parent`: `NULL` or another `ink_element`.
#'
#' Identifiers use ASCII letters, digits, `_` and `-` and start with a letter
#' or `_`, as in stylesheets.
#'
#' @section Identity:
#' Matching depends only on an element's attributes and its parent chain. ID
#' uniqueness across a tree is not checked, because no complete tree is
#' available; keeping IDs unique is the responsibility of the application.
#'
#' @param type The element type (required).
#' @param id `NULL` or the element ID, without `#`.
#' @param classes Class names, without `.`. Use `character()` for none.
#' @param states Active state names, without `:`. Use `character()` for none.
#' @param parent `NULL` or the parent `ink_element`.
#' @param x An `ink_element`.
#' @param ... Ignored.
#' @return `ink_element()` returns an `ink_element`. Invalid arguments signal an
#'   `ink_match_error` ([ink_match_error]). `format()` returns a string like
#'   `Button#save.primary:focus`; `print()` returns `x` invisibly.
#' @seealso [ink_match()], [ink_specificity()]
#' @name ink_element
#' @examples
#' screen <- ink_element(type = "Screen", id = "main")
#' container <- ink_element("Container", id = "content", classes = "panel", parent = screen)
#' button <- ink_element(
#'   "Button",
#'   id = "save",
#'   classes = c("primary", "rounded"),
#'   states = "focus",
#'   parent = container
#' )
#' button
#' button$parent$id
NULL

#' @rdname ink_element
#' @export
ink_element <- function(type, id = NULL, classes = character(), states = character(),
                        parent = NULL) {
  if (missing(type)) {
    abort_match("invalid_element_type", "`type` is required.", "type")
  }
  structure(
    list(
      type = check_element_type(type),
      id = check_element_id(id),
      classes = check_identifier_set(classes, "classes", "invalid_element_classes", "."),
      states = check_identifier_set(states, "states", "invalid_element_states", ":"),
      parent = check_element_parent(parent)
    ),
    class = "ink_element"
  )
}

#' @rdname ink_element
#' @export
format.ink_element <- function(x, ...) {
  # Same notation as selectors, so an element reads like the selector that
  # would describe it.
  format_compound_selector(list(
    type = x$type, id = x$id, classes = x$classes, pseudo_classes = x$states
  ))
}

#' @rdname ink_element
#' @export
print.ink_element <- function(x, ...) {
  cat("<ink_element> ", format(x), "\n", sep = "")
  if (!is.null(x$parent)) {
    chain <- element_chain(x)
    labels <- vapply(rev(chain), format, "")
    cat("  path: ", paste(labels, collapse = " > "), "\n", sep = "")
  }
  invisible(x)
}
