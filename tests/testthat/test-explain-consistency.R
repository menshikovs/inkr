# Differential tests: ink_explain() is compared with ink_compute() and with the
# match results, on randomly generated stylesheets and trees. There is no
# second cascade here: values and winners are compared with what ink_compute()
# recorded, and the only extra oracle is `outranks()`, a plain restatement of
# "higher specificity, then later rule, then later declaration".

random_stylesheet <- function() {
  selectors <- c(
    "Button", ".a", ".b", "#x", ":s", "Button.a", "Screen Button", "Screen > Button",
    "Button, #x", ".a, .b:s", "*", "Panel Button.b", "Button.a.b:s", "Panel > Button", "Panel"
  )
  declarations <- c(
    "color: red", "color: blue", "color: #fff", "color: inherit", "color: initial",
    "background: green", "background: inherit", "background: initial", "background: transparent",
    "padding: 1", "padding: 2 3", "padding: inherit", "padding: initial",
    "width: 3", "width: auto", "width: inherit",
    "text-style: bold", "text-style: italic", "text-style: inherit", "text-style: initial"
  )
  rules <- vapply(seq_len(sample(3:9, 1L)), function(i) {
    body <- paste0(sample(declarations, sample(1:4, 1L)), ";", collapse = " ")
    sprintf("%s { %s }", sample(selectors, 1L), body)
  }, "")
  paste(rules, collapse = "\n")
}

random_element <- function() {
  parent <- NULL
  for (i in seq_len(sample(1:5, 1L))) {
    parent <- ink_element(
      sample(c("Screen", "Panel", "Button"), 1L),
      id = if (runif(1L) < 0.25) "x" else NULL,
      classes = c("a", "b")[runif(2L) < 0.4],
      states = "s"[runif(1L) < 0.4],
      parent = parent
    )
  }
  parent
}

# Returns a character vector describing every disagreement found.
differences <- function(sheet, element) {
  problems <- character()
  note <- function(...) problems <<- c(problems, paste0(...))
  style <- ink_compute(sheet, element)
  matches <- ink_match(sheet, element)$matches
  for (property in names(style$values)) {
    explanation <- ink_explain(sheet, element, property)
    origin <- style$provenance[[property]]
    if (!identical(explanation$value, style$values[[property]])) note(property, ": value differs")
    if (!identical(explanation$origin, origin$origin)) note(property, ": origin differs")
    if (inherits(explanation$value, "ink_keyword")) note(property, ": unresolved keyword")

    expected_count <- sum(vapply(matches, function(m) {
      sum(vapply(m$rule$declarations, function(d) identical(d$name, property), NA))
    }, 0L))
    if (length(explanation$candidates) != expected_count) note(property, ": candidate count differs")

    sequence <- vapply(explanation$candidates, function(c) c$rule_index * 1000L + c$declaration_index, 0L)
    if (is.unsorted(sequence, strictly = TRUE)) note(property, ": candidates not in source order")

    winner <- explanation$winner
    if (is.null(winner)) {
      if (length(explanation$candidates) > 0L) note(property, ": candidates but no winner")
      if (!is.null(origin$rule_index)) note(property, ": provenance has a declaration")
      next
    }
    winners <- Filter(function(c) identical(c$status, "winner"), explanation$candidates)
    if (length(winners) != 1L) note(property, ": not exactly one winner")
    for (field in c("rule_index", "declaration_index", "selector_index", "specificity", "span")) {
      if (!identical(winner[[field]], origin[[field]])) note(property, ": winner ", field, " differs")
    }
    for (candidate in explanation$candidates) {
      if (identical(candidate$status, "winner")) next
      if (outranks(candidate, winner)) note(property, ": a loser outranks the winner")
      if (!outranks(winner, candidate)) note(property, ": winner does not outrank a loser")
      expected_reason <- if (any(candidate$specificity != winner$specificity)) {
        "lower_specificity"
      } else if (candidate$rule_index != winner$rule_index) {
        "earlier_rule"
      } else {
        "earlier_declaration"
      }
      if (!identical(candidate$reason, expected_reason)) note(property, ": wrong reason")
    }
  }
  problems
}

test_that("explanations agree with computed styles on random stylesheets and trees", {
  set.seed(2024)
  all_problems <- character()
  keyword_cases <- 0L
  for (trial in seq_len(120L)) {
    css <- random_stylesheet()
    element <- random_element()
    sheet <- compile_text(css)
    found <- differences(sheet, element)
    if (length(found) > 0L) {
      all_problems <- c(all_problems, paste0(css, " | ", format(element), ": ", found))
    }
    origins <- vapply(ink_compute(sheet, element)$provenance, function(o) o$origin, "")
    keyword_cases <- keyword_cases + any(grepl("^explicit", origins))
  }
  expect_equal(all_problems, character())
  # The generator has to exercise keywords, or the comparison proves little.
  expect_gt(keyword_cases, 20L)
})

test_that("random trees with custom properties are explained consistently", {
  set.seed(99)
  registry <- registry_with("gap", "length", "1", inherits = TRUE)
  registry <- ink_register_property(registry, "tone", "color", "white", inherits = FALSE)
  problems <- character()
  for (trial in seq_len(40L)) {
    css <- paste(
      random_stylesheet(),
      sprintf("%s { gap: %d; tone: %s; }", sample(c("Screen", "Panel", ".a", "Button"), 1L), sample(1:9, 1L),
              sample(c("red", "inherit", "initial"), 1L)),
      sep = "\n"
    )
    sheet <- compile_text(css, registry = registry)
    found <- differences(sheet, random_element())
    if (length(found) > 0L) problems <- c(problems, paste0(css, ": ", found))
  }
  expect_equal(problems, character())
})

test_that("explaining every property of a tree gives the same values as one compute call", {
  set.seed(5)
  for (trial in seq_len(10L)) {
    sheet <- compile_text(random_stylesheet())
    element <- random_element()
    style <- ink_compute(sheet, element)
    explained <- lapply(names(style$values), function(p) ink_explain(sheet, element, p)$value)
    names(explained) <- names(style$values)
    expect_identical(explained, style$values)
  }
})
