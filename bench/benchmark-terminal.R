# Lightweight timing of the terminal style adapter.
#
#   Rscript bench/benchmark-terminal.R [package-dir]
#
# The adapter does no parsing, matching, cascading or I/O, so each call should
# cost a small fraction of computing the style it adapts. Timings depend on the
# machine; compare runs made together.

args <- commandArgs(trailingOnly = TRUE)
package_dir <- if (length(args)) args[[1L]] else "."
if (requireNamespace("pkgload", quietly = TRUE) && file.exists(file.path(package_dir, "DESCRIPTION"))) {
  suppressMessages(pkgload::load_all(package_dir, quiet = TRUE))
} else {
  library(inkr)
}

sheet <- ink_compile(ink_parse("
  $accent: #7c3aed;
  Screen { color: white; background: #181825; }
  Button.primary { background: $accent; text-style: bold; padding: 1 2; border: round $accent; }
"))
element <- ink_element("Button", classes = "primary", parent = ink_element("Screen"))
computed <- ink_compute(sheet, element)

per_call_ms <- function(expr_fn, n) {
  elapsed <- system.time(for (i in seq_len(n)) expr_fn())[["elapsed"]]
  1000 * elapsed / n
}

cat("R:", R.version.string, "| platform:", R.version$platform, "\n")
profiles <- list(
  truecolor = ink_terminal_capabilities("truecolor"),
  ansi256 = ink_terminal_capabilities("ansi256"),
  ansi16 = ink_terminal_capabilities("ansi16", unicode = FALSE),
  none = ink_terminal_capabilities("none")
)
results <- data.frame(
  operation = c("ink_compute (for comparison)", paste0("ink_terminal_style, ", names(profiles)), "ink_ansi_wrap"),
  ms_per_call = round(c(
    per_call_ms(function() ink_compute(sheet, element), 200L),
    vapply(profiles, function(p) per_call_ms(function() ink_terminal_style(computed, p), 2000L), 0),
    {
      terminal <- ink_terminal_style(computed)
      per_call_ms(function() ink_ansi_wrap("Save", terminal), 2000L)
    }
  ), 3)
)
print(results, row.names = FALSE)
