sheet <- compile_text("
  Screen { color: white; }
  Container { padding: 1; }
  Button { background: blue; }
  .primary { color: yellow; }
")

chain_session <- function(depth = 4L) {
  session <- ink_session(sheet)
  ink_session_set(session, "n1", type = "Screen")
  for (i in 2:depth) {
    ink_session_set(session, paste0("n", i), type = if (i == depth) "Button" else "Container", parent_key = paste0("n", i - 1L))
  }
  session
}

test_that("the first computation is a miss that computes the whole path", {
  session <- chain_session(4L)
  ink_session_compute(session, "n4")
  stats <- ink_session_stats(session)
  expect_equal(c(stats$hits, stats$misses, stats$computed, stats$cached), c(0L, 1L, 4L, 4L))
})

test_that("repeating a computation is a cache hit and computes nothing", {
  session <- chain_session(4L)
  first <- ink_session_compute(session, "n4")
  second <- ink_session_compute(session, "n4")
  third <- ink_session_compute(session, "n4")
  expect_identical(first, second)
  expect_identical(second, third)
  stats <- ink_session_stats(session)
  expect_equal(c(stats$hits, stats$misses, stats$computed), c(2L, 1L, 4L))
})

test_that("computing a leaf caches its ancestors too", {
  session <- chain_session(4L)
  ink_session_compute(session, "n4")
  ink_session_compute(session, "n2")
  ink_session_compute(session, "n1")
  stats <- ink_session_stats(session)
  # One miss (the leaf); the two ancestor lookups are hits; nothing recomputed.
  expect_equal(c(stats$hits, stats$misses, stats$computed), c(2L, 1L, 4L))
})

test_that("computing a parent after its child is a hit", {
  session <- chain_session(3L)
  ink_session_compute(session, "n3")
  before <- ink_session_stats(session)
  ink_session_compute(session, "n2")
  after <- ink_session_stats(session)
  expect_equal(after$hits, before$hits + 1L)
  expect_equal(after$computed, before$computed)
})

test_that("siblings reuse the cached parent", {
  session <- ink_session(sheet)
  ink_session_set(session, "root", type = "Screen")
  ink_session_set(session, "panel", type = "Container", parent_key = "root")
  for (i in 1:5) ink_session_set(session, paste0("b", i), type = "Button", parent_key = "panel")
  for (i in 1:5) ink_session_compute(session, paste0("b", i))
  stats <- ink_session_stats(session)
  # root + panel once, then one node per button.
  expect_equal(stats$computed, 2L + 5L)
  expect_equal(stats$misses, 5L)
  expect_equal(stats$hits, 0L)
  expect_equal(stats$cached, 7L)
})

test_that("a deep new leaf only computes itself when its ancestors are cached", {
  session <- chain_session(6L)
  ink_session_compute(session, "n6")
  before <- ink_session_stats(session)$computed
  ink_session_set(session, "leaf", type = "Button", parent_key = "n6")
  ink_session_compute(session, "leaf")
  expect_equal(ink_session_stats(session)$computed, before + 1L)
})

test_that("cached results equal uncached ones", {
  session <- chain_session(5L)
  warm <- ink_session_compute(session, "n5")
  element <- ink_element("Button", parent = ink_element("Container", parent = ink_element("Container",
    parent = ink_element("Container", parent = ink_element("Screen")))))
  expect_identical(warm, ink_compute(sheet, element))
  expect_identical(ink_session_compute(session, "n5"), warm)
})

test_that("statistics are deterministic for a fixed sequence of calls", {
  run <- function() {
    session <- chain_session(5L)
    ink_session_compute(session, "n5")
    ink_session_compute(session, "n3")
    ink_session_set(session, "n2", classes = "x")
    ink_session_compute(session, "n5")
    ink_session_compute(session, "n1")
    unclass(ink_session_stats(session))
  }
  expect_identical(run(), run())
})

test_that("reading statistics does not change results or counters", {
  session <- chain_session(3L)
  a <- ink_session_compute(session, "n3")
  s1 <- ink_session_stats(session)
  s2 <- ink_session_stats(session)
  expect_identical(s1, s2)
  expect_identical(ink_session_compute(session, "n3"), a)
  expect_output(print(s1), "<ink_session_stats> 3 nodes, 3 cached | hits 0, misses 1", fixed = TRUE)
})

test_that("a cache entry is only used when its revisions match", {
  session <- chain_session(3L)
  ink_session_compute(session, "n3")
  entry <- session$cache[["n3"]]
  expect_false(is.null(entry))
  # A stale entry (as if it had survived an invalidation) is discarded.
  stale <- entry
  stale$revision <- entry$revision - 1L
  session$cache[["n3"]] <- stale
  expect_null(valid_cache_entry(session, "n3"))
  expect_null(session$cache[["n3"]])
  # And one whose parent has changed since is discarded too.
  ink_session_compute(session, "n3")
  entry <- session$cache[["n3"]]
  entry$parent_revision <- entry$parent_revision + 100L
  session$cache[["n3"]] <- entry
  expect_null(valid_cache_entry(session, "n3"))
  expect_identical(ink_session_compute(session, "n3"), ink_compute(sheet, model_element(
    list(n1 = model_node("Screen"), n2 = model_node("Container", parent_key = "n1"), n3 = model_node("Button", parent_key = "n2")), "n3")))
})

test_that("cache entries record the node's revision", {
  session <- chain_session(2L)
  ink_session_compute(session, "n2")
  expect_identical(session$cache[["n2"]]$revision, session$nodes[["n2"]]$revision)
  expect_identical(session$cache[["n2"]]$parent_revision, session$nodes[["n1"]]$revision)
  expect_true(is.na(session$cache[["n1"]]$parent_revision))
})
