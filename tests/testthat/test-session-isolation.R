sheet <- compile_text("
  Screen { color: white; }
  .hot Button { color: red; }
  Button { background: blue; }
")

build <- function() {
  session <- ink_session(sheet)
  add_nodes(session, list(
    node("root", "Screen"), node("panel", "Panel", parent_key = "root"),
    node("button", "Button", parent_key = "panel")
  ))
  session
}

test_that("two sessions from one stylesheet are independent", {
  a <- build()
  b <- build()
  expect_false(identical(a, b))
  ink_session_compute(a, "button")
  expect_equal(ink_session_stats(b)$cached, 0L)
  expect_equal(ink_session_stats(a)$cached, 3L)

  ink_session_set(a, "panel", classes = "hot")
  expect_equal(format(ink_session_compute(a, "button")$values$color), "red")
  expect_equal(format(ink_session_compute(b, "button")$values$color), "white")

  ink_session_remove(b, "panel")
  expect_equal(ink_session_stats(a)$nodes, 3L)
  expect_equal(format(ink_session_compute(a, "button")$values$color), "red")
  expect_session_error(ink_session_compute(b, "button"), "unknown_node")
})

test_that("sessions do not share nodes, caches or counters", {
  a <- build()
  b <- build()
  ink_session_compute(a, "button")
  ink_session_compute(a, "button")
  expect_equal(ink_session_stats(a)$hits, 1L)
  expect_equal(ink_session_stats(b)$hits, 0L)
  expect_equal(ink_session_stats(b)$misses, 0L)
})

test_that("the compiled stylesheet is never modified", {
  before <- sheet
  session <- build()
  ink_session_compute(session, "button")
  ink_session_set(session, "panel", classes = "hot")
  ink_session_compute(session, "button")
  ink_session_remove(session, "panel")
  ink_session_explain(session, "root", "color")
  expect_identical(sheet, before)
})

test_that("a session keeps its own copy of the stylesheet", {
  local_sheet <- compile_text("Button { color: red; }")
  session <- ink_session(local_sheet)
  ink_session_set(session, "b", type = "Button")
  local_sheet$rules <- list()
  expect_equal(format(ink_session_compute(session, "b")$values$color), "red")
})

test_that("two sessions from different stylesheets give each their own results", {
  other <- compile_text("Button { color: green; }")
  a <- ink_session(sheet)
  b <- ink_session(other)
  ink_session_set(a, "x", type = "Button")
  ink_session_set(b, "x", type = "Button")
  expect_equal(format(ink_session_compute(a, "x")$values$background), "blue")
  expect_equal(format(ink_session_compute(b, "x")$values$color), "green")
  expect_equal(format(ink_session_compute(b, "x")$values$background), "transparent")
})

test_that("stateless functions are unaffected by sessions and give the same results", {
  session <- build()
  element <- ink_element("Button", parent = ink_element("Panel", parent = ink_element("Screen")))
  stateless <- ink_compute(sheet, element)
  expect_identical(ink_session_compute(session, "button"), stateless)
  expect_identical(ink_compute(sheet, element), stateless)
  expect_identical(ink_match(sheet, element)$matches, reference_match_rules(sheet, element_chain(element)))
  expect_identical(
    ink_session_explain(session, "button", "color"),
    ink_explain(sheet, element, "color")
  )
})

test_that("removing a node removes its whole subtree", {
  session <- build()
  ink_session_compute(session, "button")
  removed <- ink_session_remove(session, "panel")
  expect_setequal(removed, c("panel", "button"))
  expect_equal(ink_session_stats(session)$nodes, 1L)
  expect_equal(ink_session_stats(session)$cached, 1L)
  expect_equal(ink_session_stats(session)$invalidated, 2L)
  expect_session_error(ink_session_compute(session, "button"), "unknown_node")
  expect_session_error(ink_session_set(session, "x", type = "A", parent_key = "panel"), "unknown_parent")
  expect_equal(format(ink_session_compute(session, "root")$values$color), "white")
})

test_that("a removed key can be added again with a fresh style", {
  session <- build()
  ink_session_set(session, "panel", classes = "hot")
  expect_equal(format(ink_session_compute(session, "button")$values$color), "red")
  ink_session_remove(session, "panel")
  ink_session_set(session, "panel", type = "Panel", parent_key = "root")
  ink_session_set(session, "button", type = "Button", parent_key = "panel")
  expect_equal(format(ink_session_compute(session, "button")$values$color), "white")
})

test_that("removing a root removes everything", {
  session <- build()
  ink_session_compute(session, "button")
  ink_session_remove(session, "root")
  expect_equal(unlist(unclass(ink_session_stats(session)))[c("nodes", "cached")], c(nodes = 0, cached = 0))
})

test_that("removing leaves siblings' caches in place", {
  session <- build()
  ink_session_set(session, "other", type = "Button", parent_key = "root")
  ink_session_compute(session, "button")
  ink_session_compute(session, "other")
  before <- ink_session_stats(session)
  ink_session_remove(session, "button")
  ink_session_compute(session, "other")
  after <- ink_session_stats(session)
  expect_equal(after$hits, before$hits + 1L)
  expect_equal(after$computed, before$computed)
})

# --- Atomic updates -----------------------------------------------------------

session_snapshot <- function(session) {
  list(
    stats = ink_session_stats(session),
    nodes = mget(sort(ls(session$nodes)), envir = session$nodes),
    children = mget(sort(ls(session$children)), envir = session$children),
    cache = mget(sort(ls(session$cache)), envir = session$cache),
    clock = session$clock,
    node_count = session$node_count
  )
}

test_that("a failed update on an existing node changes nothing", {
  session <- build()
  ink_session_compute(session, "button")
  before <- session_snapshot(session)
  bad_updates <- list(
    list(type = ""), list(id = "#bad"), list(classes = NA_character_), list(states = ":focus"),
    list(parent_key = "missing"), list(parent_key = "button"), list(parent_key = 5),
    # A valid change combined with an invalid one must not be half-applied.
    list(classes = "hot", id = "#bad"), list(classes = "hot", parent_key = "missing"),
    list(type = "Other", states = ":bad")
  )
  for (update in bad_updates) {
    err <- session_error(do.call(ink_session_set, c(list(session = session, key = "panel"), update)))
    expect_s3_class(err, "ink_session_error")
    expect_identical(session_snapshot(session), before)
  }
  expect_equal(format(ink_session_compute(session, "button")$values$color), "white")
})

test_that("a failed insert leaves no node behind", {
  session <- build()
  before <- session_snapshot(session)
  expect_session_error(ink_session_set(session, "new", type = "A", parent_key = "missing"), "unknown_parent")
  expect_session_error(ink_session_set(session, "new", type = "A", id = "#x"), "invalid_update")
  expect_session_error(ink_session_set(session, "new"), "invalid_update")
  expect_session_error(ink_session_set(session, "new", type = "A", parent_key = "new"), "cyclic_parent")
  expect_identical(session_snapshot(session), before)
  expect_session_error(ink_session_compute(session, "new"), "unknown_node")
})

test_that("a failed removal changes nothing", {
  session <- build()
  ink_session_compute(session, "button")
  before <- session_snapshot(session)
  expect_session_error(ink_session_remove(session, "missing"), "unknown_node")
  expect_session_error(ink_session_remove(session, ""), "invalid_node_key")
  expect_identical(session_snapshot(session), before)
})

test_that("a failed compute leaves the session usable", {
  broken <- sheet
  broken$rules[[3L]]$declarations[[1L]]$name <- "bogus"
  session <- ink_session(broken)
  add_nodes(session, list(node("root", "Screen"), node("b", "Button", parent_key = "root")))
  err <- tryCatch(ink_session_compute(session, "b"), error = identity)
  expect_s3_class(err, "ink_compute_error")
  expect_equal(ink_session_stats(session)$nodes, 2L)
  # The session is still usable for nodes that do not hit the damaged rule.
  ink_session_set(session, "x", type = "Unrelated")
  expect_s3_class(ink_session_compute(session, "x"), "ink_computed_style")
})
