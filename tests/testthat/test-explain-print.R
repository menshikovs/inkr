printed <- function(explanation, ...) {
  capture.output(print(explanation, ...))
}

with_width <- function(width, code) {
  old <- options(width = width)
  on.exit(options(old))
  force(code)
}

example_css <- "
Button {
    color: white;
}

.primary {
    color: blue;
}

#save {
    color: green;
}
"

test_that("printing a declared value shows the winner and the overridden declarations", {
  explanation <- explain_css(
    example_css, ink_element("Button", id = "save", classes = "primary", states = "focus"),
    source = "app.inkr"
  )
  output <- printed(explanation)
  expect_equal(output[[1L]], "<ink_style_explanation>")
  expect_true("Element: Button#save.primary:focus" %in% output)
  expect_true("Property: color" %in% output)
  expect_true("Computed: green" %in% output)
  expect_true("Origin: declaration" %in% output)
  expect_true("Winner:" %in% output)
  expect_true("Overridden:" %in% output)
  expect_true(any(grepl("specificity: (1, 0, 0)", output, fixed = TRUE)))
  expect_true(any(grepl("specificity: (0, 0, 1)", output, fixed = TRUE)))
  expect_true(any(grepl("specificity: (0, 1, 0)", output, fixed = TRUE)))
  expect_true(any(grepl("reason: lower specificity", output, fixed = TRUE)))
  expect_true(any(grepl("source: app.inkr:11:5", output, fixed = TRUE)))
  expect_true(any(grepl("^  #save$", output)))
  expect_true(any(grepl("^  Button$", output)))
  expect_true(any(grepl("^  \\.primary$", output)))
})

test_that("the winner is listed before the overridden declarations, in source order", {
  output <- printed(explain_css(example_css, ink_element("Button", id = "save", classes = "primary")))
  winner_line <- grep("^Winner:", output)
  overridden_line <- grep("^Overridden:", output)
  expect_lt(winner_line, overridden_line)
  names_in_order <- grep("^  (Button|\\.primary)$", output)
  expect_equal(output[names_in_order], c("  Button", "  .primary"))
})

test_that("printing returns its argument invisibly and does not change it", {
  explanation <- explain_css(example_css, ink_element("Button"))
  before <- explanation
  output <- capture.output(result <- print(explanation))
  expect_identical(result, explanation)
  expect_identical(explanation, before)
  expect_gt(length(output), 5L)
})

test_that("a single winner prints without an Overridden section", {
  output <- printed(explain_css("Button { color: red; }", ink_element("Button")))
  expect_true("Winner:" %in% output)
  expect_false("Overridden:" %in% output)
})

test_that("an initial value says no declaration applies and names the registry", {
  output <- printed(explain_css("Button { padding: 1; }", ink_element("Button")))
  expect_true("Origin: initial (registry initial value)" %in% output)
  expect_true(any(grepl("No declaration for 'color' applies", output, fixed = TRUE)))
  expect_false("Winner:" %in% output)
  expect_true(any(grepl("initial value (registry): default", output, fixed = TRUE)))
})

test_that("an inherited value prints the resolution path from the declaring ancestor", {
  css <- "Screen { color: white; }"
  leaf <- tree(list(type = "Screen", id = "main"), list(type = "Container", id = "content"), list(type = "Button", id = "save"))
  output <- printed(explain_css(css, leaf))
  expect_true("Origin: inherited" %in% output)
  expect_true("Resolution:" %in% output)
  expect_true("  Screen#main" %in% output)
  expect_true(any(grepl("declaration: white", output, fixed = TRUE)))
  expect_true(any(grepl("inherited from Screen#main", output, fixed = TRUE)))
  expect_true(any(grepl("inherited from Container#content", output, fixed = TRUE)))
  positions <- vapply(c("  Screen#main", "  Container#content", "  Button#save"), function(l) which(output == l), 0L)
  expect_equal(unname(positions), sort(unname(positions)))
})

test_that("context-only ancestors are not part of the printed resolution", {
  css <- "Screen { color: red; }\nContainer { color: blue; }"
  output <- printed(explain_css(css, tree("Screen", "Container", "Button")))
  expect_false("  Screen" %in% output)
  expect_true("  Container" %in% output)
  expect_true("  Button" %in% output)
})

test_that("explicit keywords print the winning declaration and the resolution", {
  css <- "Screen { color: red; }\nButton { color: inherit; }"
  output <- printed(explain_css(css, tree("Screen", "Button"), source = "app.inkr"))
  expect_true(any(grepl("^Origin: explicit_inherit", output)))
  expect_true("Winner:" %in% output)
  expect_true(any(grepl("color: inherit", output, fixed = TRUE)))
  expect_true(any(grepl("explicit inherit (app.inkr:2:10)", output, fixed = TRUE)))

  initial <- printed(explain_css("Button { color: initial; }", ink_element("Button")))
  expect_true(any(grepl("^Origin: explicit_initial", initial)))
  expect_true(any(grepl("explicit initial", initial, fixed = TRUE)))
})

test_that("a value taken from a variable shows both the text and the result", {
  output <- printed(explain_css("$c: #7c3aed;\nButton { color: $c; }", ink_element("Button")))
  expect_true(any(grepl("color: $c  [= #7c3aed]", output, fixed = TRUE)))
  expect_true("Computed: #7c3aed" %in% output)
})

test_that("many overridden declarations are abbreviated explicitly", {
  css <- paste(sprintf("Button { color: red; width: %d; }", 1:40), collapse = "\n")
  explanation <- explain_css(css, ink_element("Button"))
  output <- printed(explanation, n = 5L)
  expect_equal(sum(grepl("^    reason: ", output)), 5L)
  expect_true(any(grepl("34 more overridden declarations not shown", output, fixed = TRUE)))
  # Nothing is lost from the object.
  expect_length(explanation$candidates, 40L)
  full <- printed(explanation, n = Inf)
  expect_equal(sum(grepl("^    reason: ", full)), 39L)
  expect_false(any(grepl("not shown", full, fixed = TRUE)))
})

test_that("long inheritance chains are abbreviated, keeping the ends", {
  explanation <- explain_css("Screen { color: green; }", filler_chain(100L))
  output <- printed(explanation, n = 4L)
  expect_true(any(grepl("elements omitted", output, fixed = TRUE)))
  expect_true("  Screen" %in% output)
  expect_equal(sum(grepl("^  Filler$", output)), 3L)
  expect_length(explanation$resolution$path, 101L)
  full <- printed(explanation, n = Inf)
  expect_equal(sum(grepl("^  Filler$", full)), 100L)
  expect_false(any(grepl("omitted", full, fixed = TRUE)))
})

test_that("a deep chain prints in reasonable time and size", {
  explanation <- explain_css("Screen { color: green; }", filler_chain(1500L))
  output <- printed(explanation)
  expect_lt(length(output), 100L)
})

test_that("long selectors and values are printed whole", {
  selector <- paste(rep("Container", 8L), collapse = " > ")
  css <- sprintf("%s Button.very-long-class-name-for-testing { border: round bright-magenta; }", selector)
  leaf <- do.call(tree, c(as.list(rep("Container", 8L)), list(list(type = "Button", classes = "very-long-class-name-for-testing"))))
  output <- printed(explain_css(css, leaf, "border"))
  expect_true(any(grepl("Container Container", output, fixed = TRUE)) || any(grepl("Container > Container", output, fixed = TRUE)))
  expect_true(any(grepl(".very-long-class-name-for-testing", output, fixed = TRUE)))
  expect_true("Computed: round bright-magenta" %in% output)
})

test_that("custom properties print like any other", {
  registry <- registry_with("row-stripe", "color", "transparent")
  output <- printed(explain_css("DataTable { row-stripe: #242424; }", ink_element("DataTable"), "row-stripe", registry))
  expect_true("Property: row-stripe" %in% output)
  expect_true("Computed: #242424" %in% output)
})

test_that("printed output has no escape sequences and no trailing blank lines", {
  explanations <- list(
    explain_css(example_css, ink_element("Button", id = "save", classes = "primary")),
    explain_css("Screen { color: red; }", tree("Screen", "Button")),
    explain_css("", ink_element("Button")),
    explain_css("Button { color: initial; }", ink_element("Button"))
  )
  for (explanation in explanations) {
    output <- printed(explanation)
    expect_false(any(grepl("\033", output, fixed = TRUE)))
    expect_false(any(grepl("\\t", output)))
    expect_false(identical(output[[length(output)]], ""))
  }
})

test_that("printing does not depend on the console width", {
  explanation <- explain_css(example_css, ink_element("Button", id = "save", classes = "primary"))
  narrow <- with_width(20L, printed(explanation))
  wide <- with_width(200L, printed(explanation))
  expect_identical(narrow, wide)
})
