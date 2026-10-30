# Reproducible benchmarks for style computation.
#
# Usage (from the package root, or pass the package directory):
#
#   Rscript bench/benchmark-styles.R [package-dir] [--quick]
#
# The script loads the package source with pkgload (or an installed inkr if
# pkgload is unavailable), so it can be pointed at any checkout and the results
# of two versions compared on the same machine. Timing uses system.time() only.
# Wall-clock numbers depend on the machine and on what else is running: compare
# runs made together, not numbers from different machines.
#
# Scenarios
#   A  deep chain, no descendant selectors
#   B  deep chain, a descendant selector (Screen Filler)
#   C  deep chain, mixed combinators (A > B C, A B > C, A > B > C)
#   D  sessions: first / repeated / after leaf change / after ancestor change
#   E  sessions: many siblings under one parent
# D and E need the session API and are skipped on versions without it.

args <- commandArgs(trailingOnly = TRUE)
quick <- "--quick" %in% args
dirs <- args[!startsWith(args, "--")]
package_dir <- if (length(dirs)) dirs[[1L]] else "."

if (requireNamespace("pkgload", quietly = TRUE) && file.exists(file.path(package_dir, "DESCRIPTION"))) {
  suppressMessages(pkgload::load_all(package_dir, quiet = TRUE))
} else {
  library(inkr)
}
has_sessions <- exists("ink_session", mode = "function")

elapsed <- function(expr) system.time(expr)[["elapsed"]]

# Median of `reps` timings of `fn()`, in seconds.
timed <- function(fn, reps) {
  stats::median(vapply(seq_len(reps), function(i) elapsed(fn()), 0))
}

# More repetitions for cheap runs, fewer for slow ones (see the report).
reps_for <- function(depth) if (quick) 1L else if (depth <= 1000L) 3L else if (depth <= 2000L) 2L else 1L

chain_of <- function(depth, leaf = "Filler", root = "Screen") {
  parent <- ink_element(root)
  for (i in seq_len(depth - 1L)) parent <- ink_element("Filler", parent = parent)
  if (depth == 1L) parent else ink_element(leaf, parent = parent)
}

mixed_chain <- function(depth) {
  parent <- ink_element("B", parent = ink_element("A"))
  for (i in seq_len(depth - 3L)) parent <- ink_element(if (i %% 2L == 0L) "B" else "X", parent = parent)
  ink_element("C", parent = parent)
}

compile_css <- function(css) ink_compile(ink_parse(css))

scaling <- function(depths, seconds) {
  ratio <- c(NA, seconds[-1L] / seconds[-length(seconds)])
  growth <- c(NA, depths[-1L] / depths[-length(depths)])
  data.frame(depth = depths, seconds = round(seconds, 3), time_ratio = round(ratio, 2), depth_ratio = growth)
}

report <- function(title, depths, fn_for_depth) {
  cat("\n", title, "\n", sep = "")
  seconds <- vapply(depths, function(d) {
    input <- fn_for_depth(d)
    timed(input, reps_for(d))
  }, 0)
  print(scaling(depths, seconds), row.names = FALSE)
  invisible(seconds)
}

cat("inkr style benchmarks\n")
cat("R:", R.version.string, "| platform:", R.version$platform, "| date:", format(Sys.Date()), "\n")
cat("sessions available:", has_sessions, "| mode:", if (quick) "quick (1 run)" else "median of 3/2/1 runs", "\n")

depths <- if (quick) c(100L, 500L, 1000L) else c(100L, 500L, 1000L, 2000L, 4000L)

# A ----------------------------------------------------------------------------
sheet_a <- compile_css("Screen { color: green; } Filler { padding: 1; }")
report(
  "A. chain, no descendant selectors (2 rules): ink_compute()", depths,
  function(d) { leaf <- chain_of(d); function() ink_compute(sheet_a, leaf) }
)

# B ----------------------------------------------------------------------------
sheet_b <- compile_css("Screen { color: green; } Screen Filler { color: white; }")
report(
  "B. chain, descendant selector `Screen Filler` (2 rules): ink_compute()", depths,
  function(d) { leaf <- chain_of(d); function() ink_compute(sheet_b, leaf) }
)

# C ----------------------------------------------------------------------------
sheet_c <- compile_css("A > B C { color: red; } A B > C { background: blue; } A > B > C { padding: 1; }")
report(
  "C. mixed combinators (3 rules, alternating B/X chain): ink_compute()",
  depths[depths <= 2000L],
  function(d) { leaf <- mixed_chain(d); function() ink_compute(sheet_c, leaf) }
)

# D ----------------------------------------------------------------------------
if (has_sessions) {
  cat("\nD. sessions, chain with a descendant selector (sheet B)\n")
  rows <- list()
  for (d in if (quick) c(500L, 1000L) else c(500L, 1000L, 2000L)) {
    session <- ink_session(sheet_b)
    build <- elapsed({
      ink_session_set(session, "n1", type = "Screen")
      for (i in 2:(d - 1L)) ink_session_set(session, paste0("n", i), type = "Filler", parent_key = paste0("n", i - 1L))
      ink_session_set(session, "leaf", type = "Filler", parent_key = paste0("n", d - 1L))
    })
    first <- elapsed(ink_session_compute(session, "leaf"))
    after_first <- ink_session_stats(session)
    second <- elapsed(ink_session_compute(session, "leaf"))
    ink_session_set(session, "leaf", classes = "hot")
    leaf_change <- elapsed(ink_session_compute(session, "leaf"))
    ink_session_set(session, paste0("n", d %/% 2L), classes = "hot")
    middle_change <- elapsed(ink_session_compute(session, "leaf"))
    ink_session_set(session, "n1", classes = "hot")
    root_change <- elapsed(ink_session_compute(session, "leaf"))
    stats <- ink_session_stats(session)
    rows[[length(rows) + 1L]] <- data.frame(
      depth = d, build = round(build, 3), first = round(first, 3), second = round(second, 4),
      after_leaf_change = round(leaf_change, 4), after_middle_change = round(middle_change, 3),
      after_root_change = round(root_change, 3),
      computed = stats$computed, hits = stats$hits, misses = stats$misses,
      invalidated = stats$invalidated
    )
    stopifnot(after_first$computed == d)
  }
  print(do.call(rbind, rows), row.names = FALSE)
  cat("(single runs; `computed` counts node styles computed over the whole sequence)\n")
}

# E ----------------------------------------------------------------------------
if (has_sessions) {
  cat("\nE. sessions, siblings under one deep parent chain (depth 200, sheet B)\n")
  rows <- list()
  for (n in if (quick) c(100L, 400L) else c(100L, 400L, 1600L)) {
    session <- ink_session(sheet_b)
    ink_session_set(session, "n1", type = "Screen")
    for (i in 2:200) ink_session_set(session, paste0("n", i), type = "Filler", parent_key = paste0("n", i - 1L))
    for (i in seq_len(n)) ink_session_set(session, paste0("s", i), type = "Filler", parent_key = "n200")
    session_time <- elapsed(for (i in seq_len(n)) ink_session_compute(session, paste0("s", i)))
    stats <- ink_session_stats(session)
    parent <- chain_of(200L)
    siblings <- lapply(seq_len(n), function(i) ink_element("Filler", parent = parent))
    stateless_time <- elapsed(for (element in siblings) ink_compute(sheet_b, element))
    rows[[length(rows) + 1L]] <- data.frame(
      siblings = n, session_seconds = round(session_time, 3), stateless_seconds = round(stateless_time, 3),
      speedup = round(stateless_time / session_time, 1), computed = stats$computed,
      hits = stats$hits, misses = stats$misses
    )
  }
  print(do.call(rbind, rows), row.names = FALSE)
}
cat("\n")
