#' Style sessions: cached, incremental style computation
#'
#' A style session owns a tree of explicitly keyed elements for one compiled
#' stylesheet and remembers the computed styles it has produced. It is an
#' optional, faster way to compute styles for a tree that is queried many times
#' and changed a little at a time (an interactive application). The stateless
#' functions [ink_match()], [ink_compute()] and [ink_explain()] keep working
#' without sessions and give the same results.
#'
#' @section Identity: Nodes are identified by a **key**, a string you choose
#'   (`"save-button"`). The key is a session-local runtime identity and is
#'   unrelated to the CSS `id` that selectors like `#save` match: several nodes
#'   may share a CSS `id`, and the key of a node never matters to selectors. A
#'   session never guesses identity from an element's contents or position.
#'
#' @section Lifecycle:
#' 1. `ink_session(sheet)` creates an empty session. The stylesheet's selectors
#'    are prepared once.
#' 2. `ink_session_set()` adds a node or updates one.
#' 3. `ink_session_compute()` returns its computed style, computing only what is
#'    not already cached.
#' 4. `ink_session_remove()` removes a node and everything below it.
#'
#' A session is a mutable object (an environment): copying the variable does not
#' copy the session. Two sessions made from the same stylesheet are completely
#' independent, and the stylesheet itself is never modified.
#'
#' @section Adding and updating nodes:
#' `ink_session_set()` is an *upsert*. For a new key, `type` is required and the
#' other arguments default to "none" (`id = NULL`, no classes or states, no
#' parent, i.e. a root). For an existing key, only the arguments you supply
#' change; omitted arguments keep their value, while `id = NULL`,
#' `classes = character()`, `states = character()` and `parent_key = NULL`
#' reset a field explicitly. Setting a node to what it already is does nothing.
#' Attributes follow the rules of [ink_element()]. A parent must already be in
#' the session, and cannot be the node or one of its descendants.
#'
#' Every change is checked before anything is modified: a call that signals an
#' `ink_session_error` leaves the session exactly as it was.
#'
#' @section Cache and invalidation:
#' Computed styles are cached per node, together with the selector-matching state
#' that the node's children continue from. Because descendant selectors
#' (`.active Button`) and inheritance (`color`) make a node's style depend on its
#' ancestors, a change invalidates conservatively:
#'
#' * Changing a node (type, id, classes, states or parent) drops the cached
#'   styles of that node **and all its descendants**.
#' * Moving a node to a new parent (reparenting) is such a change: the moved
#'   subtree is recomputed under its new ancestors.
#' * Removing a node drops its whole subtree.
#' * Ancestors, siblings and unrelated nodes keep their caches.
#' * Computing a node reuses the nearest valid cached ancestor and computes only
#'   the nodes below it.
#'
#' No finer dependency tracking (per property, per selector) is attempted.
#'
#' @section Statistics:
#' `ink_session_stats()` returns counters, deterministic for a given sequence of
#' calls: `hits` and `misses` (of `ink_session_compute()` calls), `computed`
#' (node styles computed, including ancestors), `invalidated` (cache entries
#' dropped), `cached` (entries now held) and `nodes`.
#'
#' @section Explanations:
#' `ink_session_explain()` is [ink_explain()] for a node. It is not cached: it
#' recomputes the style path with the same machinery, so it can never be stale.
#'
#' @section Limitations: Sessions do not watch files, detect application changes
#'   or notify anyone; they only know what `ink_session_set()` and
#'   `ink_session_remove()` tell them. Caches are not shared between sessions,
#'   saved to disk or sized.
#'
#' @param sheet An [ink_compiled_stylesheet][ink_compile].
#' @param session A session from `ink_session()`.
#' @param key A string identifying a node in the session.
#' @param type,id,classes,states Element attributes, as in [ink_element()].
#' @param parent_key `NULL` (a root) or the key of the parent node.
#' @param property A registered property name, as in [ink_explain()].
#' @param x A session or session statistics.
#' @param ... Ignored.
#' @return `ink_session()` returns a session. `ink_session_set()` returns the
#'   session invisibly. `ink_session_compute()` returns an
#'   [ink_computed_style], identical to
#'   `ink_compute(sheet, <the element for key>)`. `ink_session_remove()`
#'   returns the removed keys invisibly. `ink_session_stats()` returns a list
#'   of class `ink_session_stats`. `ink_session_explain()` returns an
#'   [ink_style_explanation][ink_explain]. Invalid requests signal an
#'   `ink_session_error` ([ink_session_error]).
#' @seealso [ink_compute()], [ink_explain()]
#' @name ink_session
#' @examples
#' sheet <- ink_compile(ink_parse("
#'   Screen { color: white; }
#'   .active Button { color: yellow; }
#' "))
#' session <- ink_session(sheet)
#' ink_session_set(session, "root", type = "Screen")
#' ink_session_set(session, "panel", type = "Container", parent_key = "root")
#' ink_session_set(session, "button", type = "Button", classes = "primary", parent_key = "panel")
#'
#' ink_session_compute(session, "button")$values$color   # white
#' ink_session_compute(session, "button")                # cached
#'
#' # An ancestor changes: the button is recomputed.
#' ink_session_set(session, "panel", classes = "active")
#' ink_session_compute(session, "button")$values$color   # yellow
#' ink_session_stats(session)
NULL

#' @rdname ink_session
#' @export
ink_session <- function(sheet) {
  if (missing(sheet)) {
    abort_compute("invalid_compiled_stylesheet", "`sheet` is required.", "sheet")
  }
  validate_compute_sheet(sheet)
  state <- new_session_state(sheet)
  class(state) <- "ink_session"
  state
}

#' @rdname ink_session
#' @export
ink_session_set <- function(session, key, type, id = NULL, classes = character(),
                            states = character(), parent_key = NULL) {
  state <- session_state(session)
  key <- check_node_key(key)
  supplied <- list(
    type = !missing(type), id = !missing(id), classes = !missing(classes),
    states = !missing(states), parent_key = !missing(parent_key)
  )
  values <- list()
  if (supplied$type) values["type"] <- list(type)
  if (supplied$id) values["id"] <- list(id)
  if (supplied$classes) values["classes"] <- list(classes)
  if (supplied$states) values["states"] <- list(states)
  if (supplied$parent_key) values["parent_key"] <- list(parent_key)

  # Validate everything first; nothing below this block can fail.
  existing <- state$nodes[[key]]
  node <- resolve_node_update(state, key, existing, supplied, values)
  check_parent(state, key, node$parent_key, existing)

  if (is.null(existing)) {
    node$revision <- tick(state)
    state$nodes[[key]] <- node
    state$node_count <- state$node_count + 1L
    link_child(state, node$parent_key, key)
  } else if (!same_node(existing, node)) {
    if (!identical(existing$parent_key, node$parent_key)) {
      unlink_child(state, existing$parent_key, key)
      link_child(state, node$parent_key, key)
    }
    node$revision <- existing$revision
    state$nodes[[key]] <- node
    invalidate_subtree(state, key)
  }
  invisible(session)
}

#' @rdname ink_session
#' @export
ink_session_compute <- function(session, key) {
  state <- session_state(session)
  key <- check_known_node(state, key)
  entry <- valid_cache_entry(state, key)
  if (is.null(entry)) {
    count(state, "misses")
    entry <- ensure_cached(state, key)
  } else {
    count(state, "hits")
  }
  new_ink_computed_style(entry$resolved$values, entry$resolved$provenance, entry$element)
}

#' @rdname ink_session
#' @export
ink_session_remove <- function(session, key) {
  state <- session_state(session)
  key <- check_known_node(state, key)
  removed <- subtree_keys(state, key)
  unlink_child(state, state$nodes[[key]]$parent_key, key)
  dropped <- 0L
  for (current in removed) {
    rm(list = current, envir = state$nodes)
    if (exists(current, envir = state$children, inherits = FALSE)) {
      rm(list = current, envir = state$children)
    }
    if (exists(current, envir = state$cache, inherits = FALSE)) {
      rm(list = current, envir = state$cache)
      dropped <- dropped + 1L
    }
  }
  state$node_count <- state$node_count - length(removed)
  count(state, "invalidated", dropped)
  invisible(removed)
}

#' @rdname ink_session
#' @export
ink_session_stats <- function(session) {
  state <- session_state(session)
  structure(
    list(
      hits = state$counters$hits,
      misses = state$counters$misses,
      computed = state$counters$computed,
      invalidated = state$counters$invalidated,
      cached = length(ls(state$cache, all.names = TRUE)),
      nodes = state$node_count
    ),
    class = "ink_session_stats"
  )
}

#' @rdname ink_session
#' @export
ink_session_explain <- function(session, key, property) {
  state <- session_state(session)
  key <- check_known_node(state, key)
  if (missing(property)) {
    abort_explain("invalid_property", "`property` is required.", "property")
  }
  tryCatch(
    explain_property(state$sheet, session_element(state, key), property, prepared = state$prepared),
    ink_compute_error = translate_compute_error
  )
}

#' @rdname ink_session
#' @export
print.ink_session <- function(x, ...) {
  state <- session_state(x)
  cat(sprintf(
    "<ink_session> %d node%s, %d cached\n",
    state$node_count, if (state$node_count == 1L) "" else "s",
    length(ls(state$cache, all.names = TRUE))
  ))
  invisible(x)
}

#' @rdname ink_session
#' @export
print.ink_session_stats <- function(x, ...) {
  cat(sprintf(
    "<ink_session_stats> %d node%s, %d cached | hits %d, misses %d | computed %d, invalidated %d\n",
    x$nodes, if (x$nodes == 1L) "" else "s", x$cached, x$hits, x$misses, x$computed, x$invalidated
  ))
  invisible(x)
}
