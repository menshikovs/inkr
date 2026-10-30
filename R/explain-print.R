# Printing an ink_style_explanation.
#
# The printed text is a view of the structured object, never its source: every
# fact shown here is read from fields documented in ?ink_explain. Output is
# plain text (no ANSI codes, no dependence on terminal width). Long lists are
# abbreviated with an explicit note; the object itself is always complete.

#' @rdname ink_explain
#' @export
print.ink_style_explanation <- function(x, n = 10L, ...) {
  n <- check_print_limit(n)
  cat("<ink_style_explanation>\n\n")
  cat("Element: ", format(x$element), "\n", sep = "")
  cat("Property: ", x$property, "\n", sep = "")
  cat("Computed: ", format(x$value), "\n", sep = "")
  cat("Origin: ", describe_origin(x$origin), "\n", sep = "")

  if (!is.null(x$winner)) {
    cat("\nWinner:\n")
    print_candidate(x$winner, x$source, "  ", show_reason = FALSE)
  } else {
    cat("\nNo declaration for '", x$property, "' applies to this element.\n", sep = "")
  }

  overridden <- Filter(function(candidate) identical(candidate$status, "overridden"), x$candidates)
  if (length(overridden) > 0L) {
    cat("\nOverridden:\n")
    shown <- seq_len(min(length(overridden), n))
    for (i in shown) {
      if (i > 1L) cat("\n")
      print_candidate(overridden[[i]], x$source, "  ", show_reason = TRUE)
    }
    if (length(overridden) > length(shown)) {
      cat(sprintf(
        "\n  ... %d more overridden declaration%s not shown (all are in $candidates)\n",
        length(overridden) - length(shown),
        if (length(overridden) - length(shown) == 1L) "" else "s"
      ))
    }
  }

  if (!identical(x$origin, "declaration")) {
    print_resolution(x, n)
  }
  invisible(x)
}

describe_origin <- function(origin) {
  switch(origin,
    declaration = "declaration",
    inherited = "inherited",
    initial = "initial (registry initial value)",
    explicit_inherit = "explicit_inherit (declared `inherit`)",
    explicit_initial = "explicit_initial (declared `initial`)",
    origin
  )
}

format_specificity <- function(specificity) {
  sprintf("(%d, %d, %d)", specificity[["id"]], specificity[["class"]], specificity[["type"]])
}

format_source <- function(source, span) {
  sprintf("%s:%d:%d", source, span$start$line, span$start$column)
}

print_candidate <- function(candidate, source, indent, show_reason) {
  detail <- paste0(indent, "  ")
  value <- format(candidate$value)
  note <- if (identical(value, candidate$raw)) "" else sprintf("  [= %s]", value)
  cat(indent, format_complex_selector(candidate$selector), "\n", sep = "")
  cat(detail, candidate$property, ": ", candidate$raw, note, "\n", sep = "")
  cat(detail, "specificity: ", format_specificity(candidate$specificity), "\n", sep = "")
  if (show_reason) {
    cat(detail, "reason: ", gsub("_", " ", candidate$reason, fixed = TRUE), "\n", sep = "")
  }
  cat(detail, "rule ", candidate$rule_index, ", declaration ", candidate$declaration_index, "\n", sep = "")
  cat(detail, "source: ", format_source(source, candidate$span), "\n", sep = "")
}

# Indices of `total` items to show when at most `n` are allowed: the first and
# the last n - 1 (the target end of a path matters most). A path is always shown
# with at least its two ends, and a single item is shown once.
visible_indices <- function(total, n) {
  if (total <= max(n, 1)) {
    return(seq_len(total))
  }
  c(1L, seq.int(total - max(n, 2) + 2L, total))
}

print_resolution <- function(x, n) {
  steps <- Filter(function(step) isTRUE(step$in_effective_chain), x$resolution$path)
  cat("\nResolution:\n")
  keep <- visible_indices(length(steps), n)
  previous <- NULL
  for (i in keep) {
    if (!is.null(previous) && i > previous + 1L) {
      cat(sprintf("  ... %d element%s omitted ...\n\n", i - previous - 1L, if (i - previous - 1L == 1L) "" else "s"))
    }
    step <- steps[[i]]
    parent <- if (i > 1L) steps[[i - 1L]] else NULL
    cat("  ", format(step$element), "\n", sep = "")
    cat("    ", describe_step(step, parent, x$source), "\n", sep = "")
    if (i < length(steps)) cat("\n")
    previous <- i
  }
}

describe_step <- function(step, parent, source) {
  provenance <- step$provenance
  value <- format(step$value)
  parent_label <- if (is.null(parent)) "its parent" else format(parent$element)
  switch(step$origin,
    declaration = sprintf(
      "declaration: %s (specificity %s, %s)",
      value, format_specificity(provenance$specificity), format_source(source, provenance$span)
    ),
    inherited = sprintf("inherited from %s: %s", parent_label, value),
    initial = sprintf("initial value (registry): %s", value),
    explicit_inherit = if (identical(provenance$source, "parent")) {
      sprintf(
        "explicit inherit (%s): took the value of %s: %s",
        format_source(source, provenance$span), parent_label, value
      )
    } else {
      sprintf(
        "explicit inherit (%s) with no parent: initial value (registry): %s",
        format_source(source, provenance$span), value
      )
    },
    explicit_initial = sprintf(
      "explicit initial (%s): initial value (registry): %s",
      format_source(source, provenance$span), value
    ),
    value
  )
}
