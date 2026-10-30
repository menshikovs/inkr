sheet <- compile_text("
  Panel.active Button { color: yellow; border: round green; }
  Panel { color: white; }
  Panel.active { color: red; }
  Screen > Panel Button { width: 7; }
")

# Screen
# +-- Panel.active
# |   +-- Button
# +-- Panel
two_panels <- function() {
  session <- ink_session(sheet)
  add_nodes(session, list(
    node("screen", "Screen"),
    node("active", "Panel", classes = "active", parent_key = "screen"),
    node("plain", "Panel", parent_key = "screen"),
    node("button", "Button", parent_key = "active")
  ))
  session
}

test_that("moving a node to another parent changes its style", {
  session <- two_panels()
  before <- ink_session_compute(session, "button")
  expect_equal(format(before$values$color), "yellow")
  expect_equal(format(before$values$border), "round green")

  ink_session_set(session, "button", parent_key = "plain")
  after <- ink_session_compute(session, "button")
  expect_equal(format(after$values$color), "white")
  expect_equal(format(after$values$border), "none")
  expect_identical(after$element$parent$classes, character())
  expect_identical(
    after,
    ink_compute(sheet, ink_element("Button", parent = ink_element("Panel", parent = ink_element("Screen"))))
  )
})

test_that("moving back restores the original style", {
  session <- two_panels()
  original <- ink_session_compute(session, "button")
  ink_session_set(session, "button", parent_key = "plain")
  ink_session_compute(session, "button")
  ink_session_set(session, "button", parent_key = "active")
  expect_identical(ink_session_compute(session, "button"), original)
})

test_that("reparenting recomputes the moved subtree", {
  session <- ink_session(sheet)
  add_nodes(session, list(
    node("screen", "Screen"),
    node("active", "Panel", classes = "active", parent_key = "screen"),
    node("plain", "Panel", parent_key = "screen"),
    node("box", "Box", parent_key = "active"),
    node("button", "Button", parent_key = "box"),
    node("label", "Label", parent_key = "button")
  ))
  for (key in c("button", "label")) ink_session_compute(session, key)
  expect_equal(format(ink_session_compute(session, "label")$values$color), "yellow")
  ink_session_set(session, "box", parent_key = "plain")
  # box, button and label are invalidated; screen, active and plain are not.
  expect_equal(ink_session_stats(session)$invalidated, 3L)
  expect_equal(format(ink_session_compute(session, "label")$values$color), "white")
  expect_equal(format(ink_session_compute(session, "button")$values$border), "none")
})

test_that("the old parent and siblings are unaffected by a move", {
  session <- two_panels()
  ink_session_compute(session, "button")
  ink_session_compute(session, "plain")
  before <- ink_session_stats(session)
  ink_session_set(session, "button", parent_key = "plain")
  ink_session_compute(session, "active")
  ink_session_compute(session, "plain")
  after <- ink_session_stats(session)
  expect_equal(after$hits, before$hits + 2L)
  expect_equal(after$computed, before$computed)
})

test_that("a node can become a root and a root can gain a parent", {
  session <- two_panels()
  ink_session_set(session, "button", parent_key = NULL)
  expect_null(ink_session_compute(session, "button")$element$parent)
  expect_equal(format(ink_session_compute(session, "button")$values$color), "default")
  ink_session_set(session, "button", parent_key = "active")
  expect_equal(format(ink_session_compute(session, "button")$values$color), "yellow")
  # The screen is an ancestor of the button, so it cannot move below it.
  expect_session_error(ink_session_set(session, "screen", parent_key = "button"), "cyclic_parent")
  expect_session_error(ink_session_set(session, "screen", parent_key = "plain"), "cyclic_parent")
})

test_that("an ancestor can be reparented", {
  session <- two_panels()
  ink_session_set(session, "extra", type = "Wrapper")
  ink_session_set(session, "active", parent_key = "extra")
  style <- ink_session_compute(session, "button")
  expect_identical(style$element$parent$parent$type, "Wrapper")
  expect_equal(format(style$values$width), "auto")
  ink_session_set(session, "active", parent_key = "screen")
  expect_equal(format(ink_session_compute(session, "button")$values$width), "7")
})

test_that("a rejected move leaves everything as it was", {
  session <- two_panels()
  before <- ink_session_compute(session, "button")
  stats <- ink_session_stats(session)
  expect_session_error(ink_session_set(session, "active", parent_key = "button"), "cyclic_parent")
  expect_session_error(ink_session_set(session, "button", parent_key = "missing"), "unknown_parent")
  expect_identical(ink_session_stats(session), stats)
  expect_identical(ink_session_compute(session, "button"), before)
})

test_that("child bookkeeping stays correct through many moves", {
  session <- ink_session(sheet)
  add_nodes(session, list(
    node("screen", "Screen"), node("p1", "Panel", parent_key = "screen"),
    node("p2", "Panel", parent_key = "screen"), node("b", "Button", parent_key = "p1")
  ))
  for (i in 1:6) {
    ink_session_set(session, "b", parent_key = if (i %% 2 == 0) "p1" else "p2")
  }
  # After an even number of moves the button is back under p1; removing p2 must
  # not touch it, and removing p1 must remove it.
  ink_session_remove(session, "p2")
  expect_equal(ink_session_compute(session, "b")$element$parent$type, "Panel")
  expect_equal(ink_session_stats(session)$nodes, 3L)
  ink_session_remove(session, "p1")
  expect_session_error(ink_session_compute(session, "b"), "unknown_node")
  expect_equal(ink_session_stats(session)$nodes, 1L)
})
