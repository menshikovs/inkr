# Validation and normalization of runtime element attributes.
#
# Identifiers are checked with the same rule the lexer uses: ASCII letters,
# digits, `_` and `-`, starting with a letter or `_`. `\\z` (not `$`) is used
# so a trailing newline is not accepted.

is_identifier <- function(x) {
  grepl("^[A-Za-z_][A-Za-z0-9_-]*\\z", x, perl = TRUE)
}

is_plain_string <- function(x) {
  is.character(x) && length(x) == 1L && !is.na(x)
}

describe_input <- function(x) {
  if (is.null(x)) "NULL" else sprintf("an object of class '%s'", class(x)[[1L]])
}

show_text <- function(x) {
  sprintf("'%s'", encodeString(x))
}

# Strips names and other attributes, so stored values are plain vectors.
plain_character <- function(x) {
  as.vector(x, mode = "character")
}

check_element_type <- function(type) {
  if (!is_plain_string(type)) {
    abort_match(
      "invalid_element_type",
      sprintf("`type` must be one non-missing string, not %s.", describe_input(type)),
      "type"
    )
  }
  if (!is_identifier(type)) {
    hint <- if (type == "*") " The universal selector '*' is not an element type." else ""
    abort_match(
      "invalid_element_type",
      sprintf("`type` %s is not a valid identifier.%s", show_text(type), hint),
      "type"
    )
  }
  plain_character(type)
}

check_element_id <- function(id) {
  if (is.null(id)) {
    return(NULL)
  }
  if (!is_plain_string(id)) {
    abort_match(
      "invalid_element_id",
      sprintf("`id` must be NULL or one non-missing string, not %s.", describe_input(id)),
      "id"
    )
  }
  if (!is_identifier(id)) {
    abort_match(
      "invalid_element_id",
      sprintf("`id` %s is not a valid identifier (write it without the '#').", show_text(id)),
      "id"
    )
  }
  plain_character(id)
}

# Shared by `classes` and `states`. Returns the entries without duplicates, in
# order of first occurrence.
check_identifier_set <- function(x, argument, code, prefix) {
  if (!is.character(x)) {
    abort_match(
      code,
      sprintf("`%s` must be a character vector, not %s.", argument, describe_input(x)),
      argument
    )
  }
  if (anyNA(x)) {
    abort_match(code, sprintf("`%s` must not contain missing values.", argument), argument)
  }
  invalid <- x[!is_identifier(x)]
  if (length(invalid) > 0L) {
    abort_match(
      code,
      sprintf(
        "`%s` entry %s is not a valid identifier (write it without the '%s').",
        argument, show_text(invalid[[1L]]), prefix
      ),
      argument
    )
  }
  unique(plain_character(x))
}

check_element_parent <- function(parent) {
  if (is.null(parent)) {
    return(NULL)
  }
  if (inherits(parent, "ink_node")) {
    abort_match(
      "invalid_element_parent",
      "`parent` must be an ink_element or NULL, not a stylesheet AST node.",
      "parent"
    )
  }
  if (!inherits(parent, "ink_element")) {
    abort_match(
      "invalid_element_parent",
      sprintf("`parent` must be an ink_element or NULL, not %s.", describe_input(parent)),
      "parent"
    )
  }
  if (!is_well_formed_element(parent)) {
    abort_match("invalid_element_parent", "`parent` is not a valid ink_element.", "parent")
  }
  parent
}

# A cheap structural check (no regular expressions) used on the matching path.
# Elements built by ink_element() always pass; this guards against objects
# that were edited by hand.
is_well_formed_element <- function(x) {
  inherits(x, "ink_element") &&
    is.list(x) &&
    is_plain_string(x$type) &&
    (is.null(x$id) || is_plain_string(x$id)) &&
    is.character(x$classes) && !anyNA(x$classes) &&
    is.character(x$states) && !anyNA(x$states) &&
    (is.null(x$parent) || inherits(x$parent, "ink_element"))
}

# Longest parent chain accepted. Elements are plain lists, so a chain cannot
# loop back on itself; the bound is a safety net for hand-built objects.
MAX_ELEMENT_DEPTH <- 100000L

# Walks from `element` up through its parents, checking each one. Never
# signals: returns `list(chain, problem)` where `chain` is the element followed
# by its ancestors (nearest first) and `problem` is NULL or
# `list(kind, detail)` with kind "element", "ancestor" or "depth". Iterative,
# so deep trees cannot overflow the stack.
walk_element_chain <- function(element) {
  problem <- function(kind, detail, chain) {
    list(chain = chain, problem = list(kind = kind, detail = detail))
  }
  if (!inherits(element, "ink_element")) {
    return(problem(
      "element",
      sprintf("`element` must be an ink_element, not %s.", describe_input(element)),
      list()
    ))
  }
  chain <- list()
  current <- element
  while (!is.null(current)) {
    if (length(chain) >= MAX_ELEMENT_DEPTH) {
      return(problem(
        "depth",
        sprintf("The parent chain is deeper than %d elements.", MAX_ELEMENT_DEPTH),
        chain
      ))
    }
    if (!is_well_formed_element(current)) {
      if (length(chain) == 0L) {
        return(problem("element", "`element` is not a valid ink_element.", chain))
      }
      return(problem(
        "ancestor",
        sprintf("Ancestor %d of `element` is not a valid ink_element.", length(chain)),
        chain
      ))
    }
    chain[[length(chain) + 1L]] <- current
    current <- current$parent
  }
  list(chain = chain, problem = NULL)
}

# Returns the element followed by its ancestors, nearest first, after checking
# each of them.
element_chain <- function(element) {
  walked <- walk_element_chain(element)
  if (!is.null(walked$problem)) {
    abort_match("invalid_element", walked$problem$detail, "element")
  }
  walked$chain
}
