# Session state: the keyed tree, its dependency index and its cache.
#
# A session is an environment (class "ink_session") because its lifetime and
# owner are explicit: it is created by ink_session() and changed only through
# the session functions. Nothing is global.
#
# Fields of the state environment
#   sheet, prepared, registry   the compiled stylesheet it was created from and
#                               its prepared selectors
#   nodes      key -> node record, a list(key, type, id, classes, states,
#              parent_key, revision). `revision` is taken from a session-wide
#              clock whenever the node, or anything above it, changes.
#   children   key -> character vector of child keys (the dependency index
#              used to find everything below a changed node)
#   cache      key -> cache entry, a list(revision, parent_revision, element,
#              state, resolved): the node's computed style, the matching state
#              its children resume from, and the element (with ancestors)
#   clock      the last revision handed out
#   node_count number of nodes (kept so loops never need to count them)
#   counters   hits, misses, computed, invalidated
#
# Invalidation is eager and conservative: changing a node drops the cache of the
# node and of its whole subtree, because descendant selectors and inheritance
# can make any descendant depend on it. Cache entries also carry the revision of
# their node and of its parent, and are only used when both still match, so an
# entry that somehow survived could not be returned.

new_session_state <- function(sheet) {
  state <- new.env(parent = emptyenv())
  state$sheet <- sheet
  state$registry <- sheet$registry
  state$prepared <- with_compute_errors(prepare_selectors(sheet))
  state$nodes <- new.env(parent = emptyenv())
  state$children <- new.env(parent = emptyenv())
  state$cache <- new.env(parent = emptyenv())
  state$clock <- 0L
  state$node_count <- 0L
  state$counters <- new.env(parent = emptyenv())
  for (name in c("hits", "misses", "computed", "invalidated")) {
    state$counters[[name]] <- 0L
  }
  state
}

SESSION_FIELDS <- c(
  "sheet", "registry", "prepared", "nodes", "children", "cache", "clock", "node_count", "counters"
)

# The state environment of a session, after checking it is one.
session_state <- function(session) {
  if (!is.environment(session) || !inherits(session, "ink_session")) {
    abort_session(
      "invalid_session",
      sprintf("`session` must be created by ink_session(), not %s.", describe_input(session)),
      "session"
    )
  }
  if (!all(vapply(SESSION_FIELDS, exists, NA, envir = session, inherits = FALSE))) {
    abort_session("invalid_session_state", "The session's internal data is incomplete.", "session")
  }
  session
}

tick <- function(state) {
  state$clock <- state$clock + 1L
  state$clock
}

count <- function(state, name, by = 1L) {
  state$counters[[name]] <- state$counters[[name]] + by
}

# Keys ------------------------------------------------------------------------

check_node_key <- function(key, argument = "key") {
  if (!is_plain_string(key) || !nzchar(key)) {
    abort_session(
      "invalid_node_key",
      sprintf("`%s` must be one non-empty string, not %s.", argument, describe_input(key)),
      argument
    )
  }
  as.vector(key, mode = "character")
}

check_known_node <- function(state, key) {
  key <- check_node_key(key)
  if (is.null(state$nodes[[key]])) {
    abort_session("unknown_node", sprintf("There is no node with key '%s' in the session.", key), "key")
  }
  key
}

# Node attributes -------------------------------------------------------------

# Runs one of the element-attribute checks, reporting failures as invalid
# updates (the same rules as ink_element()).
checked_attribute <- function(check) {
  tryCatch(
    check,
    ink_match_error = function(e) abort_session("invalid_update", e$detail, e$argument)
  )
}

# The attributes a set() call would give the node, as a list, validating
# everything and changing nothing. Arguments that were not supplied keep the
# node's current value; ones supplied (even as NULL or character()) replace it.
resolve_node_update <- function(state, key, existing, supplied, values) {
  take <- function(name, check, default) {
    if (supplied[[name]]) {
      checked_attribute(check(values[[name]]))
    } else if (!is.null(existing)) {
      existing[[name]]
    } else {
      default
    }
  }
  if (!supplied$type && is.null(existing)) {
    abort_session("invalid_update", "`type` is required when a node is created.", "type")
  }
  list(
    key = key,
    type = take("type", check_element_type, NULL),
    id = take("id", check_element_id, NULL),
    classes = take(
      "classes",
      function(x) check_identifier_set(x, "classes", "invalid_element_classes", "."), character()
    ),
    states = take(
      "states",
      function(x) check_identifier_set(x, "states", "invalid_element_states", ":"), character()
    ),
    parent_key = if (supplied$parent_key) {
      if (is.null(values$parent_key)) NULL else check_node_key(values$parent_key, "parent_key")
    } else if (!is.null(existing)) {
      existing$parent_key
    } else {
      NULL
    }
  )
}

# A parent must exist and must not be the node or one of its descendants.
check_parent <- function(state, key, parent_key, existing = NULL) {
  if (is.null(parent_key)) {
    return(invisible())
  }
  if (identical(parent_key, key)) {
    abort_session("cyclic_parent", sprintf("Node '%s' cannot be its own parent.", key), "parent_key")
  }
  if (is.null(state$nodes[[parent_key]])) {
    abort_session(
      "unknown_parent",
      sprintf("The parent '%s' is not in the session; add it first.", parent_key),
      "parent_key"
    )
  }
  # A new node has no descendants, and an unchanged parent cannot create a loop,
  # so only a *changed* parent of an existing node needs the walk. This keeps
  # building a deep tree linear.
  if (is.null(existing) || identical(existing$parent_key, parent_key)) {
    return(invisible())
  }
  # Walk up from the proposed parent; meeting the node means a loop. The walk
  # is bounded by the number of nodes, so even a damaged tree cannot hang it.
  current <- parent_key
  for (step in seq_len(state$node_count + 1L)) {
    if (is.null(current)) {
      return(invisible())
    }
    if (identical(current, key)) {
      abort_session(
        "cyclic_parent",
        sprintf("Making '%s' the parent of '%s' would create a cycle.", parent_key, key),
        "parent_key"
      )
    }
    node <- state$nodes[[current]]
    if (is.null(node)) {
      abort_session("invalid_session_state", sprintf("Node '%s' has a missing ancestor.", current))
    }
    current <- node$parent_key
  }
  abort_session("invalid_session_state", "The session tree contains a cycle.")
}

same_node <- function(a, b) {
  identical(a$type, b$type) && identical(a$id, b$id) && identical(a$classes, b$classes) &&
    identical(a$states, b$states) && identical(a$parent_key, b$parent_key)
}

# Tree edits -------------------------------------------------------------------

link_child <- function(state, parent_key, key) {
  if (!is.null(parent_key)) {
    state$children[[parent_key]] <- c(state$children[[parent_key]], key)
  }
}

unlink_child <- function(state, parent_key, key) {
  if (!is.null(parent_key)) {
    remaining <- setdiff(state$children[[parent_key]], key)
    if (length(remaining) > 0L) state$children[[parent_key]] <- remaining else rm(list = parent_key, envir = state$children)
  }
}

# `key` and every node below it, parents before children. Iterative.
subtree_keys <- function(state, key) {
  found <- list()
  stack <- key
  while (length(stack) > 0L) {
    current <- stack[[length(stack)]]
    stack <- stack[-length(stack)]
    found[[length(found) + 1L]] <- current
    stack <- c(stack, state$children[[current]])
  }
  unlist(found, use.names = FALSE)
}

# Drops the cached style of `key` and of everything below it and gives each of
# those nodes a new revision.
invalidate_subtree <- function(state, key) {
  dropped <- 0L
  for (current in subtree_keys(state, key)) {
    if (!is.null(state$cache[[current]])) {
      rm(list = current, envir = state$cache)
      dropped <- dropped + 1L
    }
    state$nodes[[current]]$revision <- tick(state)
  }
  count(state, "invalidated", dropped)
  invisible(dropped)
}

# Cache -------------------------------------------------------------------------

# The cache entry for `key` if it is provably current, else NULL (a stale entry
# is discarded).
valid_cache_entry <- function(state, key) {
  entry <- state$cache[[key]]
  if (is.null(entry)) {
    return(NULL)
  }
  node <- state$nodes[[key]]
  parent_revision <- if (is.null(node$parent_key)) NA_integer_ else state$nodes[[node$parent_key]]$revision
  if (identical(entry$revision, node$revision) && identical(entry$parent_revision, parent_revision)) {
    return(entry)
  }
  rm(list = key, envir = state$cache)
  count(state, "invalidated")
  NULL
}

compute_node_entry <- function(state, key, parent_entry) {
  node <- state$nodes[[key]]
  # Attributes were validated when the node was set, so the element is built
  # directly, in the same shape ink_element() produces.
  element <- structure(
    list(
      type = node$type, id = node$id, classes = node$classes, states = node$states,
      parent = parent_entry$element
    ),
    class = "ink_element"
  )
  step <- with_compute_errors(compute_element_step(state$prepared, state$registry, element, parent_entry))
  count(state, "computed")
  entry <- list(
    revision = node$revision,
    parent_revision = if (is.null(node$parent_key)) NA_integer_ else state$nodes[[node$parent_key]]$revision,
    element = element,
    state = step$state,
    resolved = step$resolved
  )
  state$cache[[key]] <- entry
  entry
}

# The nearest valid cached ancestors let a computation resume part-way down:
# only the nodes between there and `key` are computed, root first.
ensure_cached <- function(state, key) {
  pending <- list()
  current <- key
  parent_entry <- NULL
  while (!is.null(current)) {
    node <- state$nodes[[current]]
    if (is.null(node)) {
      abort_session("invalid_session_state", sprintf("Node '%s' has a missing ancestor.", current))
    }
    if (!identical(current, key)) {
      cached <- valid_cache_entry(state, current)
      if (!is.null(cached)) {
        parent_entry <- cached
        break
      }
    }
    pending[[length(pending) + 1L]] <- current
    if (length(pending) > state$node_count) {
      abort_session("invalid_session_state", "The session tree contains a cycle.")
    }
    current <- node$parent_key
  }
  entry <- parent_entry
  for (k in rev(unlist(pending))) {
    entry <- compute_node_entry(state, k, entry)
  }
  entry
}

# The element for `key` with its ancestors, built from the node records (not
# from the cache), root first.
session_element <- function(state, key) {
  path <- list()
  current <- key
  while (!is.null(current)) {
    path[[length(path) + 1L]] <- current
    current <- state$nodes[[current]]$parent_key
    if (length(path) > state$node_count) {
      abort_session("invalid_session_state", "The session tree contains a cycle.")
    }
  }
  element <- NULL
  for (k in rev(unlist(path))) {
    node <- state$nodes[[k]]
    element <- structure(
      list(type = node$type, id = node$id, classes = node$classes, states = node$states, parent = element),
      class = "ink_element"
    )
  }
  element
}
