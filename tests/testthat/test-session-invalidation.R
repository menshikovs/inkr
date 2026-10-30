style_of <- function(session, key) ink_session_compute(session, key)

test_that("an ancestor class change reaches a descendant selector", {
  sheet <- compile_text(".active Button { color: red; }")
  session <- ink_session(sheet)
  add_nodes(session, list(node("c", "Container"), node("b", "Button", parent_key = "c")))
  expect_equal(format(style_of(session, "b")$values$color), "default")
  ink_session_set(session, "c", classes = "active")
  expect_equal(format(style_of(session, "b")$values$color), "red")
  ink_session_set(session, "c", classes = character())
  expect_equal(format(style_of(session, "b")$values$color), "default")
})

test_that("an ancestor state change reaches a descendant selector", {
  sheet <- compile_text("Panel:focus Button { border: round green; }")
  session <- ink_session(sheet)
  add_nodes(session, list(node("p", "Panel"), node("b", "Button", parent_key = "p")))
  expect_equal(format(style_of(session, "b")$values$border), "none")
  ink_session_set(session, "p", states = "focus")
  expect_equal(format(style_of(session, "b")$values$border), "round green")
  ink_session_set(session, "p", states = character())
  expect_equal(format(style_of(session, "b")$values$border), "none")
})

test_that("an ancestor change reaches inherited values", {
  sheet <- compile_text("Panel { color: white; }\nPanel.active { color: yellow; }")
  session <- ink_session(sheet)
  add_nodes(session, list(node("p", "Panel"), node("m", "Mid", parent_key = "p"), node("b", "Button", parent_key = "m")))
  expect_equal(format(style_of(session, "b")$values$color), "white")
  ink_session_set(session, "p", classes = "active")
  expect_equal(format(style_of(session, "b")$values$color), "yellow")
  expect_equal(style_of(session, "b")$provenance$color$origin, "inherited")
})

test_that("leaf class and state updates are applied", {
  sheet <- compile_text("Button { color: white; }\nButton.primary { color: yellow; }\nButton:focus { color: green; }")
  session <- ink_session(sheet)
  add_nodes(session, list(node("r", "Screen"), node("b", "Button", parent_key = "r")))
  expect_equal(format(style_of(session, "b")$values$color), "white")
  ink_session_set(session, "b", classes = "primary")
  expect_equal(format(style_of(session, "b")$values$color), "yellow")
  ink_session_set(session, "b", states = "focus")
  expect_equal(format(style_of(session, "b")$values$color), "green")
})

test_that("ancestor id and type updates are applied", {
  sheet <- compile_text("#main Button { width: 5; }\nPanel Button { height: 6; }")
  session <- ink_session(sheet)
  add_nodes(session, list(node("p", "Box"), node("b", "Button", parent_key = "p")))
  expect_equal(format(style_of(session, "b")$values$width), "auto")
  ink_session_set(session, "p", id = "main")
  expect_equal(format(style_of(session, "b")$values$width), "5")
  expect_equal(format(style_of(session, "b")$values$height), "auto")
  ink_session_set(session, "p", type = "Panel")
  expect_equal(format(style_of(session, "b")$values$height), "6")
  ink_session_set(session, "p", id = NULL)
  expect_equal(format(style_of(session, "b")$values$width), "auto")
})

test_that("a change invalidates the node and its whole subtree", {
  sheet <- compile_text("Screen { color: white; }")
  session <- ink_session(sheet)
  add_nodes(session, list(
    node("root", "Screen"), node("a", "A", parent_key = "root"),
    node("a1", "A1", parent_key = "a"), node("a2", "A2", parent_key = "a"),
    node("a11", "A11", parent_key = "a1")
  ))
  for (key in c("a11", "a2")) ink_session_compute(session, key)
  expect_equal(ink_session_stats(session)$cached, 5L)
  ink_session_set(session, "a", classes = "changed")
  stats <- ink_session_stats(session)
  # a, a1, a11, a2 were cached; root was not touched.
  expect_equal(stats$invalidated, 4L)
  expect_equal(stats$cached, 1L)
  expect_false(is.null(session$cache[["root"]]))
})

test_that("nodes outside the changed subtree keep their cache", {
  sheet <- compile_text("Screen { color: white; }\n.hot Button { color: red; }")
  session <- ink_session(sheet)
  add_nodes(session, list(
    node("root", "Screen"),
    node("left", "Panel", parent_key = "root"), node("lb", "Button", parent_key = "left"),
    node("right", "Panel", parent_key = "root"), node("rb", "Button", parent_key = "right")
  ))
  ink_session_compute(session, "lb")
  ink_session_compute(session, "rb")
  before <- ink_session_stats(session)
  ink_session_set(session, "left", classes = "hot")
  # The right branch and the root are untouched.
  ink_session_compute(session, "rb")
  mid <- ink_session_stats(session)
  expect_equal(mid$hits, before$hits + 1L)
  expect_equal(mid$computed, before$computed)
  ink_session_compute(session, "lb")
  after <- ink_session_stats(session)
  # Only the changed subtree (left, lb) was recomputed, from the cached root.
  expect_equal(after$computed, before$computed + 2L)
  expect_equal(format(style_of(session, "lb")$values$color), "red")
  expect_equal(format(style_of(session, "rb")$values$color), "white")
})

test_that("a leaf change recomputes only the leaf", {
  sheet <- compile_text("Screen { color: white; }\nButton.hot { color: red; }")
  session <- ink_session(sheet)
  add_nodes(session, list(node("a", "Screen"), node("b", "Panel", parent_key = "a"), node("c", "Panel", parent_key = "b"),
    node("d", "Button", parent_key = "c")))
  ink_session_compute(session, "d")
  before <- ink_session_stats(session)$computed
  ink_session_set(session, "d", classes = "hot")
  expect_equal(format(style_of(session, "d")$values$color), "red")
  expect_equal(ink_session_stats(session)$computed, before + 1L)
})

test_that("explicit inherit and initial follow ancestor changes", {
  sheet <- compile_text("
    Panel { background: blue; color: red; }
    Panel.alt { background: green; color: yellow; }
    Mid { background: inherit; color: initial; }
    Button { background: inherit; }
  ")
  session <- ink_session(sheet)
  add_nodes(session, list(node("p", "Panel"), node("m", "Mid", parent_key = "p"), node("b", "Button", parent_key = "m")))
  expect_equal(format(style_of(session, "b")$values$background), "blue")
  expect_equal(format(style_of(session, "b")$values$color), "default")
  ink_session_set(session, "p", classes = "alt")
  expect_equal(format(style_of(session, "b")$values$background), "green")
  expect_equal(format(style_of(session, "b")$values$color), "default")
  expect_equal(style_of(session, "m")$provenance$background$origin, "explicit_inherit")
})

test_that("custom inherited and non-inherited properties follow ancestor changes", {
  registry <- registry_with("accent", "color", "white", inherits = TRUE)
  registry <- ink_register_property(registry, "stripe", "color", "transparent", inherits = FALSE)
  sheet <- compile_text("Panel { accent: red; stripe: red; }\nPanel.alt { accent: blue; }\nButton { stripe: inherit; }", registry = registry)
  session <- ink_session(sheet)
  add_nodes(session, list(node("p", "Panel"), node("b", "Button", parent_key = "p")))
  expect_equal(format(style_of(session, "b")$values$accent), "red")
  expect_equal(format(style_of(session, "b")$values$stripe), "red")
  ink_session_set(session, "p", classes = "alt")
  expect_equal(format(style_of(session, "b")$values$accent), "blue")
  expect_equal(format(style_of(session, "b")$values$stripe), "red")
})

test_that("a stale explanation is impossible: explanations follow mutations", {
  sheet <- compile_text("Panel { color: white; }\nPanel.active { color: yellow; }")
  session <- ink_session(sheet)
  add_nodes(session, list(node("p", "Panel"), node("b", "Button", parent_key = "p")))
  expect_equal(format(ink_session_explain(session, "b", "color")$value), "white")
  ink_session_set(session, "p", classes = "active")
  explanation <- ink_session_explain(session, "b", "color")
  expect_equal(format(explanation$value), "yellow")
  expect_identical(explanation$value, ink_session_compute(session, "b")$values$color)
  expect_equal(format(explanation$resolution$effective_source$element), "Panel.active")
})

test_that("random mutation sequences always agree with fresh computations", {
  set.seed(2025)
  sheet <- compile_text("
    Screen { color: white; background: blue; }
    .x { color: red; } .y Button { background: green; } #i { padding: 1; }
    Panel:s > Button { color: yellow; } Screen Panel Button.x { width: 5; }
    Panel { text-style: bold; } Button { color: inherit; border: single red; }
    .y { margin: 2; } Panel.x Button, Box > Button:s { height: 4; background: initial; }
  ")
  types <- c("Screen", "Panel", "Button", "Box")
  model <- list(root = model_node("Screen"))
  session <- session_from_model(sheet, model)
  counter <- 0L
  problems <- character()
  for (step in seq_len(160L)) {
    keys <- names(model)
    action <- sample(c("add", "attr", "reparent", "remove", "compute"), 1L, prob = c(0.3, 0.25, 0.15, 0.1, 0.2))
    if (action == "add" || length(keys) < 3L) {
      counter <- counter + 1L
      key <- paste0("k", counter)
      parent <- sample(keys, 1L)
      model[[key]] <- model_node(sample(types, 1L), parent_key = parent)
      ink_session_set(session, key, type = model[[key]]$type, parent_key = parent)
    } else if (action == "attr") {
      key <- sample(keys, 1L)
      what <- sample(c("classes", "states", "id", "type"), 1L)
      if (what == "classes") {
        value <- c("x", "y")[runif(2L) < 0.5]
        model[[key]]$classes <- value
        ink_session_set(session, key, classes = value)
      } else if (what == "states") {
        value <- c("s")[runif(1L) < 0.5]
        model[[key]]$states <- value
        ink_session_set(session, key, states = value)
      } else if (what == "id") {
        value <- if (runif(1L) < 0.5) "i" else NULL
        model[[key]]["id"] <- list(value)
        ink_session_set(session, key, id = value)
      } else {
        value <- sample(types, 1L)
        model[[key]]$type <- value
        ink_session_set(session, key, type = value)
      }
    } else if (action == "reparent") {
      key <- sample(setdiff(keys, "root"), 1L)
      blocked <- c(key, model_descendants(model, key))
      options <- setdiff(keys, blocked)
      parent <- if (length(options)) sample(options, 1L) else NULL
      if (!is.null(parent)) {
        model[[key]]["parent_key"] <- list(parent)
        ink_session_set(session, key, parent_key = parent)
      }
    } else if (action == "remove") {
      key <- sample(setdiff(keys, "root"), 1L)
      gone <- c(key, model_descendants(model, key))
      model[gone] <- NULL
      removed <- ink_session_remove(session, key)
      if (!setequal(removed, gone)) problems <- c(problems, sprintf("step %d: removed keys differ", step))
    }
    probe <- sample(names(model), min(3L, length(model)))
    for (key in probe) {
      got <- ink_session_compute(session, key)
      want <- ink_compute(sheet, model_element(model, key))
      if (!identical(got, want)) problems <- c(problems, sprintf("step %d (%s): %s differs", step, action, key))
    }
  }
  expect_equal(problems, character())
  # Everything left is also right.
  expect_session_matches_model(session, sheet, model)
  expect_gt(ink_session_stats(session)$hits, 20L)
  expect_gt(ink_session_stats(session)$invalidated, 20L)
})
