# Golden cases: stylesheets and element trees whose results were captured with
# the engine as it was before the selector optimization (Task 05), stored in
# golden/golden-task05.rds. golden-results.R in this directory documents how it
# is produced. The optimized engine must reproduce every captured result
# exactly (structurally, field by field).
#
# Cases are generated without R's random number generator (a small Lehmer
# generator instead), so the inputs are identical on every R version.

golden_generator <- function(seed) {
  state <- seed
  function(n) {
    state <<- (state * 16807) %% 2147483647
    as.integer(state %% n) + 1L
  }
}

golden_tree <- function(specs) {
  parent <- NULL
  for (spec in specs) {
    parent <- do.call(ink_element, c(spec, list(parent = parent)))
  }
  parent
}

# Every element along the chain, leaf first.
golden_chain <- function(leaf) {
  elements <- list()
  while (!is.null(leaf)) {
    elements[[length(elements) + 1L]] <- leaf
    leaf <- leaf$parent
  }
  elements
}

golden_handwritten <- function() {
  spec_tree <- golden_tree(list(
    list(type = "Screen", id = "main"),
    list(type = "Container", classes = "panel"),
    list(type = "Button", id = "save", classes = c("primary", "rounded"), states = "focus")
  ))
  mixed <- function(...) golden_tree(lapply(list(...), function(type) list(type = type)))
  list(
    list(
      name = "spec-example",
      css = "Button { color: white; background: blue; padding: 1; }
             Button.primary { color: yellow; }
             #save { color: green; }
             .primary { background: green; }
             Button.primary:focus { border: round red; }",
      leaves = list(spec_tree, spec_tree$parent, ink_element("Button", id = "save", classes = "primary"))
    ),
    list(
      name = "inheritance",
      css = "$primary: #7c3aed;
             Screen { color: white; background: #181825; }
             Container { padding: 1; }
             Button { color: inherit; background: blue; border: single white; }
             Button.primary { background: $primary; }
             Button:focus { text-style: bold; }
             #save { border: round green; }",
      leaves = list(spec_tree, ink_element("Button"), spec_tree$parent$parent)
    ),
    list(
      name = "mixed-combinators",
      css = "A > B C { color: red; }
             A B > C { background: blue; }
             A > B > C { padding: 1; }
             A C { margin: 2; }
             * { width: 3; }
             B C, A > B { height: 4; }",
      leaves = list(
        mixed("A", "B", "B", "C"), mixed("A", "B", "C"), mixed("B", "B", "C"),
        mixed("A", "X", "B", "C"), mixed("A", "B", "X", "C"), mixed("B", "A", "B", "C"),
        mixed("C"), mixed("A", "C"), mixed("A", "A")
      )
    ),
    list(
      name = "strict-ancestors",
      css = "A A { color: red; } A > A { background: blue; } A A A { padding: 1; }",
      leaves = list(mixed("A"), mixed("A", "A"), mixed("A", "A", "A"), mixed("A", "X", "A"))
    ),
    list(
      name = "keywords",
      css = "Screen { color: red; background: blue; padding: 2; }
             Container { color: initial; background: inherit; }
             Button { color: inherit; padding: inherit; width: initial; }",
      leaves = list(mixed("Screen", "Container", "Button"), mixed("Button"), mixed("Screen", "Button"))
    ),
    list(
      name = "selector-lists",
      css = "Button, #save, .primary { color: red; }
             .a, .b { background: blue; }
             Screen Button, Container > Button { padding: 1; }
             *, Button { margin: 1; }",
      leaves = list(
        spec_tree,
        golden_tree(list(list(type = "Screen"), list(type = "Button", classes = c("a", "b")))),
        golden_tree(list(list(type = "Container"), list(type = "Button", classes = "b")))
      )
    ),
    list(
      name = "states-and-ids",
      css = "Panel:focus Button { border: round green; }
             #p Button:disabled { color: red; }
             :selected { text-style: bold; }
             Panel.active { color: yellow; } Panel { color: white; }",
      leaves = list(
        golden_tree(list(list(type = "Panel", states = "focus"), list(type = "Button"))),
        golden_tree(list(list(type = "Panel", id = "p", classes = "active"), list(type = "Button", states = c("disabled", "selected")))),
        golden_tree(list(list(type = "Panel"), list(type = "Button", states = "selected")))
      )
    )
  )
}

golden_generated <- function(count = 36L) {
  selectors <- c(
    "Button", ".a", ".b", "#x", ":s", "Button.a", "Screen Button", "Screen > Button",
    "Button, #x", ".a, .b:s", "*", "Panel Button.b", "Button.a.b:s", "Panel > Button",
    "Panel", "Screen Panel Button", "Screen > Panel Button", "Screen Panel > Button",
    "Panel Panel", "Panel > Panel > Button", ".a .a", "Panel.a > Button"
  )
  declarations <- c(
    "color: red", "color: blue", "color: #fff", "color: inherit", "color: initial",
    "background: green", "background: inherit", "background: initial",
    "padding: 1", "padding: 2 3", "padding: inherit", "width: 3", "width: auto",
    "text-style: bold", "text-style: italic", "text-style: inherit"
  )
  cases <- vector("list", count)
  for (i in seq_len(count)) {
    pick <- golden_generator(1000L + i)
    rules <- vapply(seq_len(2L + pick(7L)), function(r) {
      body <- paste0(declarations[vapply(seq_len(pick(4L)), function(d) pick(length(declarations)), 0L)], ";", collapse = " ")
      sprintf("%s { %s }", selectors[[pick(length(selectors))]], body)
    }, "")
    leaves <- lapply(1:3, function(t) {
      depth <- 1L + pick(6L)
      specs <- lapply(seq_len(depth), function(level) {
        list(
          type = c("Screen", "Panel", "Button")[[pick(3L)]],
          id = if (pick(5L) == 1L) "x" else NULL,
          classes = c("a", "b")[vapply(1:2, function(k) pick(3L) == 1L, NA)],
          states = if (pick(3L) == 1L) "s" else character()
        )
      })
      golden_tree(specs)
    })
    cases[[i]] <- list(name = sprintf("generated-%02d", i), css = paste(rules, collapse = "\n"), leaves = leaves)
  }
  cases
}

golden_cases <- function() {
  c(golden_handwritten(), golden_generated())
}

# Everything observable for one case, through the public API only.
golden_snapshot <- function(case) {
  sheet <- ink_compile(ink_parse(case$css))
  elements <- unlist(lapply(case$leaves, golden_chain), recursive = FALSE)
  lapply(elements, function(element) {
    explained <- lapply(c("color", "background", "padding", "text-style"), function(property) {
      explanation <- ink_explain(sheet, element, property)
      list(
        value = explanation$value, origin = explanation$origin,
        winner = explanation$winner, candidates = explanation$candidates,
        effective_position = explanation$resolution$effective_source$position,
        path_origins = vapply(explanation$resolution$path, function(step) step$origin, ""),
        path_supplied_by = vapply(explanation$resolution$path, function(step) step$supplied_by, 0L)
      )
    })
    list(
      match = ink_match(sheet, element)$matches,
      style = ink_compute(sheet, element),
      explanations = explained
    )
  })
}

golden_all <- function() {
  cases <- golden_cases()
  results <- lapply(cases, golden_snapshot)
  names(results) <- vapply(cases, function(case) case$name, "")
  results
}
