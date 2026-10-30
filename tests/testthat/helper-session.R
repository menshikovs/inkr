# Helpers for session tests.

# Adds nodes to a session. Each node is a list(key, type, id, classes, states,
# parent_key); missing fields are left out of the call.
add_nodes <- function(session, nodes) {
  for (node in nodes) {
    do.call(ink_session_set, c(list(session = session), node))
  }
  invisible(session)
}

node <- function(key, type, ..., parent_key = NULL) {
  c(list(key = key, type = type), list(...), if (!is.null(parent_key)) list(parent_key = parent_key))
}

session_error <- function(expr) {
  tryCatch(
    {
      expr
      NULL
    },
    ink_session_error = function(e) e
  )
}

expect_session_error <- function(expr, code) {
  err <- session_error(expr)
  testthat::expect_s3_class(err, "ink_session_error")
  if (!is.null(err)) {
    testthat::expect_equal(err$code, code)
    testthat::expect_true(nzchar(err$message))
  }
  invisible(err)
}

# --- An independent model of the tree ------------------------------------------
# A model is a named list of node descriptions kept by the test itself, never
# derived from the session. model_element() builds the stateless element for a
# key, so session results can be compared with a fresh ink_compute().

model_node <- function(type, id = NULL, classes = character(), states = character(), parent_key = NULL) {
  list(type = type, id = id, classes = classes, states = states, parent_key = parent_key)
}

model_element <- function(model, key) {
  path <- character()
  current <- key
  while (!is.null(current)) {
    path <- c(current, path)
    current <- model[[current]]$parent_key
  }
  parent <- NULL
  for (k in path) {
    n <- model[[k]]
    parent <- ink_element(n$type, id = n$id, classes = n$classes, states = n$states, parent = parent)
  }
  parent
}

model_descendants <- function(model, key) {
  found <- character()
  for (k in names(model)) {
    current <- model[[k]]$parent_key
    while (!is.null(current)) {
      if (identical(current, key)) {
        found <- c(found, k)
        break
      }
      current <- model[[current]]$parent_key
    }
  }
  found
}

# Builds a session from a model (parents before children).
session_from_model <- function(sheet, model) {
  session <- ink_session(sheet)
  remaining <- names(model)
  while (length(remaining) > 0L) {
    for (k in remaining) {
      n <- model[[k]]
      if (is.null(n$parent_key) || !(n$parent_key %in% remaining)) {
        ink_session_set(
          session, k, type = n$type, id = n$id, classes = n$classes,
          states = n$states, parent_key = n$parent_key
        )
        remaining <- setdiff(remaining, k)
      }
    }
  }
  session
}

# Every node of the model must compute to what a fresh stateless computation of
# the model's element gives.
expect_session_matches_model <- function(session, sheet, model) {
  for (key in names(model)) {
    testthat::expect_identical(
      ink_session_compute(session, key),
      ink_compute(sheet, model_element(model, key)),
      info = key
    )
  }
}
