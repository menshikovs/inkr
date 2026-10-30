sheet <- compile_text("
  Screen { color: white; }
  Container { padding: 1; }
  Button { background: blue; }
  .primary { color: yellow; }
  #save { border: round green; }
")

test_that("ink_session creates an empty session", {
  session <- ink_session(sheet)
  expect_s3_class(session, "ink_session")
  stats <- ink_session_stats(session)
  expect_s3_class(stats, "ink_session_stats")
  expect_equal(unlist(unclass(stats)), c(hits = 0, misses = 0, computed = 0, invalidated = 0, cached = 0, nodes = 0))
  expect_output(print(session), "<ink_session> 0 nodes, 0 cached", fixed = TRUE)
})

test_that("a single root can be added and computed", {
  session <- ink_session(sheet)
  expect_identical(ink_session_set(session, "root", type = "Screen"), session)
  style <- ink_session_compute(session, "root")
  expect_s3_class(style, "ink_computed_style")
  expect_equal(format(style$values$color), "white")
  expect_identical(style, ink_compute(sheet, ink_element("Screen")))
})

test_that("a parent and child compute like the stateless equivalent", {
  session <- ink_session(sheet)
  ink_session_set(session, "root", type = "Screen", id = "main")
  ink_session_set(session, "button", type = "Button", id = "save", classes = "primary", parent_key = "root")
  style <- ink_session_compute(session, "button")
  element <- ink_element(
    "Button", id = "save", classes = "primary",
    parent = ink_element("Screen", id = "main")
  )
  expect_identical(style, ink_compute(sheet, element))
  expect_identical(style$element, element)
  expect_equal(format(style$values$color), "yellow")
  expect_equal(format(style$values$border), "round green")
})

test_that("several siblings under one parent", {
  session <- ink_session(sheet)
  add_nodes(session, list(
    node("root", "Screen"),
    node("a", "Button", classes = "primary", parent_key = "root"),
    node("b", "Button", parent_key = "root"),
    node("c", "Container", parent_key = "root")
  ))
  expect_equal(format(ink_session_compute(session, "a")$values$color), "yellow")
  expect_equal(format(ink_session_compute(session, "b")$values$color), "white")
  expect_equal(format(ink_session_compute(session, "c")$values$padding), "top=1 right=1 bottom=1 left=1")
  expect_equal(ink_session_stats(session)$nodes, 4L)
})

test_that("sessions are used by key, and CSS ids are separate", {
  session <- ink_session(sheet)
  add_nodes(session, list(
    node("one", "Button", id = "save"),
    node("two", "Button", id = "save")
  ))
  expect_identical(ink_session_compute(session, "one")$values, ink_session_compute(session, "two")$values)
  expect_equal(format(ink_session_compute(session, "two")$values$border), "round green")
  # A node whose key looks like a selector is still just a key.
  ink_session_set(session, "#save", type = "Input")
  expect_equal(format(ink_session_compute(session, "#save")$values$border), "none")
})

test_that("set is an upsert: omitted arguments keep their value", {
  session <- ink_session(sheet)
  add_nodes(session, list(
    node("root", "Screen"),
    node("b", "Button", id = "save", classes = c("primary", "x"), states = "focus", parent_key = "root")
  ))
  ink_session_set(session, "b", classes = "other")
  style <- ink_session_compute(session, "b")
  expect_identical(style$element$id, "save")
  expect_identical(style$element$type, "Button")
  expect_identical(style$element$classes, "other")
  expect_identical(style$element$states, "focus")
  expect_identical(style$element$parent$type, "Screen")
})

test_that("explicit NULL and empty values reset fields", {
  session <- ink_session(sheet)
  add_nodes(session, list(
    node("root", "Screen"),
    node("b", "Button", id = "save", classes = "primary", states = "focus", parent_key = "root")
  ))
  ink_session_set(session, "b", id = NULL)
  expect_null(ink_session_compute(session, "b")$element$id)
  ink_session_set(session, "b", classes = character())
  expect_identical(ink_session_compute(session, "b")$element$classes, character())
  ink_session_set(session, "b", states = character())
  expect_identical(ink_session_compute(session, "b")$element$states, character())
  ink_session_set(session, "b", parent_key = NULL)
  expect_null(ink_session_compute(session, "b")$element$parent)
  expect_identical(ink_session_compute(session, "b")$element$type, "Button")
})

test_that("classes and states are normalized like ink_element does", {
  session <- ink_session(sheet)
  ink_session_set(session, "n", type = "Button", classes = c("b", "a", "b"), states = c("s", "s"))
  element <- ink_session_compute(session, "n")$element
  expect_identical(element, ink_element("Button", classes = c("b", "a", "b"), states = c("s", "s")))
  expect_identical(element$classes, c("b", "a"))
})

test_that("setting a node to what it already is changes nothing", {
  session <- ink_session(sheet)
  add_nodes(session, list(node("root", "Screen"), node("b", "Button", classes = "x", parent_key = "root")))
  ink_session_compute(session, "b")
  before <- ink_session_stats(session)
  ink_session_set(session, "b", type = "Button", classes = "x", parent_key = "root")
  ink_session_set(session, "root", type = "Screen")
  expect_identical(ink_session_stats(session), before)
  ink_session_compute(session, "b")
  expect_equal(ink_session_stats(session)$hits, before$hits + 1L)
})

test_that("invalid sessions and keys are rejected", {
  for (bad in list(NULL, list(), "session", 1, new.env(), sheet)) {
    expect_session_error(ink_session_stats(bad), "invalid_session")
    expect_session_error(ink_session_set(bad, "a", type = "A"), "invalid_session")
    expect_session_error(ink_session_compute(bad, "a"), "invalid_session")
    expect_session_error(ink_session_remove(bad, "a"), "invalid_session")
  }
  session <- ink_session(sheet)
  for (key in list(NA_character_, "", 1, NULL, c("a", "b"), character(), TRUE, list("a"))) {
    expect_session_error(ink_session_set(session, key, type = "A"), "invalid_node_key")
    expect_session_error(ink_session_compute(session, key), "invalid_node_key")
    expect_session_error(ink_session_remove(session, key), "invalid_node_key")
  }
})

test_that("unknown nodes are reported", {
  session <- ink_session(sheet)
  expect_session_error(ink_session_compute(session, "ghost"), "unknown_node")
  expect_session_error(ink_session_remove(session, "ghost"), "unknown_node")
  expect_session_error(ink_session_explain(session, "ghost", "color"), "unknown_node")
})

test_that("a stylesheet that is not compiled cannot start a session", {
  for (bad in list(NULL, list(), "x", ink_parse("A {}"), ink_registry())) {
    expect_error(ink_session(bad), class = "ink_compute_error")
  }
  expect_error(ink_session(), class = "ink_compute_error")
})

test_that("invalid node descriptions are invalid updates", {
  session <- ink_session(sheet)
  expect_session_error(ink_session_set(session, "n"), "invalid_update")
  expect_session_error(ink_session_set(session, "n", type = ""), "invalid_update")
  expect_session_error(ink_session_set(session, "n", type = "*"), "invalid_update")
  expect_session_error(ink_session_set(session, "n", type = "A", id = "#x"), "invalid_update")
  expect_session_error(ink_session_set(session, "n", type = "A", classes = NA_character_), "invalid_update")
  expect_session_error(ink_session_set(session, "n", type = "A", classes = ".x"), "invalid_update")
  expect_session_error(ink_session_set(session, "n", type = "A", states = ":focus"), "invalid_update")
  expect_session_error(ink_session_set(session, "n", type = "A", classes = NULL), "invalid_update")
  err <- session_error(ink_session_set(session, "n", type = "A", id = "#x"))
  expect_equal(err$argument, "id")
})

test_that("unknown and cyclic parents are rejected", {
  session <- ink_session(sheet)
  add_nodes(session, list(node("a", "Screen"), node("b", "Container", parent_key = "a"), node("c", "Button", parent_key = "b")))
  expect_session_error(ink_session_set(session, "x", type = "A", parent_key = "nope"), "unknown_parent")
  expect_session_error(ink_session_set(session, "b", parent_key = "nope"), "unknown_parent")
  expect_session_error(ink_session_set(session, "x", type = "A", parent_key = "x"), "cyclic_parent")
  expect_session_error(ink_session_set(session, "a", parent_key = "a"), "cyclic_parent")
  expect_session_error(ink_session_set(session, "a", parent_key = "b"), "cyclic_parent")
  expect_session_error(ink_session_set(session, "a", parent_key = "c"), "cyclic_parent")
  expect_session_error(ink_session_set(session, "b", parent_key = "c"), "cyclic_parent")
  expect_session_error(ink_session_set(session, "n", type = "A", parent_key = 1), "invalid_node_key")
  expect_session_error(ink_session_set(session, "n", type = "A", parent_key = ""), "invalid_node_key")
})

test_that("a damaged session is reported, not misused", {
  session <- ink_session(sheet)
  ink_session_set(session, "a", type = "Screen")
  broken <- session
  rm("nodes", envir = broken)
  expect_session_error(ink_session_compute(broken, "a"), "invalid_session_state")
  session2 <- ink_session(sheet)
  ink_session_set(session2, "a", type = "Screen")
  ink_session_set(session2, "b", type = "Button", parent_key = "a")
  rm("a", envir = session2$nodes)
  expect_session_error(ink_session_compute(session2, "b"), "invalid_session_state")
})

test_that("session errors are ink_session_error and nothing else", {
  err <- session_error(ink_session_compute(ink_session(sheet), "ghost"))
  expect_s3_class(err, "ink_session_error")
  expect_s3_class(err, "error")
  for (other in c("ink_match_error", "ink_compute_error", "ink_explain_error")) {
    expect_false(inherits(err, other))
  }
  expect_named(unclass(err), c("message", "call", "code", "detail", "argument"))
})

test_that("custom registered properties work in sessions", {
  registry <- registry_with("gap", "length", "1", inherits = TRUE)
  custom <- compile_text("Screen { gap: 9; }\n.wide { gap: 20; }", registry = registry)
  session <- ink_session(custom)
  add_nodes(session, list(node("r", "Screen"), node("c", "Button", parent_key = "r")))
  expect_equal(format(ink_session_compute(session, "c")$values$gap), "9")
  ink_session_set(session, "r", classes = "wide")
  expect_equal(format(ink_session_compute(session, "c")$values$gap), "20")
})

test_that("session explanations agree with stateless explanations", {
  session <- ink_session(sheet)
  add_nodes(session, list(
    node("root", "Screen"),
    node("b", "Button", id = "save", classes = "primary", parent_key = "root")
  ))
  element <- model_element(
    list(
      root = model_node("Screen"),
      b = model_node("Button", id = "save", classes = "primary", parent_key = "root")
    ),
    "b"
  )
  expect_identical(
    ink_session_explain(session, "b", "color"),
    ink_explain(sheet, element, "color")
  )
  expect_session_error_free <- ink_session_explain(session, "b", "border")
  expect_s3_class(expect_session_error_free, "ink_style_explanation")
  expect_error(ink_session_explain(session, "b", "nope"), class = "ink_explain_error")
  expect_error(ink_session_explain(session, "b"), class = "ink_explain_error")
  ink_session_set(session, "b", classes = character())
  expect_equal(format(ink_session_explain(session, "b", "color")$value), "white")
  expect_equal(ink_session_explain(session, "b", "color")$origin, "inherited")
})
