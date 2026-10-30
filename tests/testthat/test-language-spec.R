# The language specification (docs/language-spec-v0.1.md) is checked against
# the package: its examples must behave as it says, and its tables must match
# the property registry, the specificity calculation and the error codes.
#
# docs/ is part of the source repository, not of the installed package, so these
# tests are skipped when the specification is not reachable (for example under
# R CMD check on a built tarball).

spec_lines <- function() {
  path <- test_path("..", "..", "docs", "language-spec-v0.1.md")
  skip_if(!file.exists(path), "the language specification is not part of the installed package")
  readLines(path, encoding = "UTF-8")
}

# Fenced code blocks: a list of list(info, text).
spec_blocks <- function(lines) {
  blocks <- list()
  open <- NA_integer_
  info <- ""
  for (i in seq_along(lines)) {
    if (startsWith(lines[[i]], "```")) {
      if (is.na(open)) {
        open <- i
        info <- trimws(substring(lines[[i]], 4L))
      } else {
        blocks[[length(blocks) + 1L]] <- list(info = info, text = paste(lines[(open + 1L):(i - 1L)], collapse = "\n"))
        open <- NA_integer_
      }
    }
  }
  blocks
}

# Rows of the Markdown table between <!-- marker --> and <!-- /marker -->, as a
# list of character vectors of cells (the header and separator are dropped).
spec_table <- function(lines, marker) {
  start <- which(lines == sprintf("<!-- %s -->", marker))
  end <- which(lines == sprintf("<!-- /%s -->", marker))
  stopifnot(length(start) == 1L, length(end) == 1L, end > start)
  rows <- lines[(start + 3L):(end - 1L)]
  lapply(rows, function(row) {
    cells <- strsplit(sub("^[|] ", "", sub(" [|]$", "", row)), " [|] ")[[1L]]
    trimws(cells)
  })
}

unquote <- function(cell) sub("^`(.*)`$", "\\1", cell)

# The condition raised by a stylesheet text, as c(class, code), or NULL if it
# parses and compiles.
inkr_outcome <- function(text) {
  tryCatch(
    {
      ink_compile(ink_parse(text))
      NULL
    },
    ink_parse_error = function(e) c("parse", e$code),
    ink_compile_error = function(e) c("compile", e$code)
  )
}

test_that("every `inkr` example parses and compiles", {
  blocks <- Filter(function(b) identical(b$info, "inkr"), spec_blocks(spec_lines()))
  expect_gte(length(blocks), 9L)
  for (block in blocks) {
    expect_no_error(ink_compile(ink_parse(block$text)))
  }
})

test_that("every `inkr-invalid` example fails with the stated error", {
  blocks <- Filter(function(b) startsWith(b$info, "inkr-invalid"), spec_blocks(spec_lines()))
  expect_gte(length(blocks), 2L)
  for (block in blocks) {
    expected <- strsplit(trimws(sub("^inkr-invalid", "", block$info)), ":", fixed = TRUE)[[1L]]
    expect_identical(inkr_outcome(block$text), expected, info = block$text)
  }
})

test_that("the property table is the property registry", {
  rows <- spec_table(spec_lines(), "builtin-properties")
  registry <- ink_registry()$properties
  expect_identical(vapply(rows, function(r) unquote(r[[1L]]), ""), names(registry))
  for (row in rows) {
    name <- unquote(row[[1L]])
    property <- registry[[name]]
    expect_identical(unquote(row[[2L]]), property$type, info = name)
    expect_identical(row[[3L]] == "yes", property$inherits, info = name)
    expect_identical(value_from_text(property$type, unquote(row[[4L]])), property$initial, info = name)
  }
})

test_that("the specificity table is what ink_specificity computes", {
  rows <- spec_table(spec_lines(), "specificity")
  expect_gte(length(rows), 10L)
  for (row in rows) {
    selector <- unquote(row[[1L]])
    expected <- as.integer(strsplit(row[[2L]], ", ", fixed = TRUE)[[1L]])
    expect_identical(unname(ink_specificity(selector_of(selector))), expected, info = selector)
  }
})

test_that("each parse error code in the table is raised by its example", {
  rows <- spec_table(spec_lines(), "diagnostics:parse")
  codes <- vapply(rows, function(r) unquote(r[[1L]]), "")
  expect_setequal(codes, c(
    "unterminated_comment", "unexpected_character", "invalid_selector", "invalid_variable",
    "invalid_hex_color", "unsupported_at_rule", "unsupported_string", "unsupported_function",
    "unsupported_important", "unsupported_nesting", "unexpected_token", "unexpected_end_of_input",
    "unexpected_closing_brace", "missing_opening_brace", "missing_closing_brace", "missing_colon",
    "missing_semicolon", "empty_value"
  ))
  for (row in rows) {
    expect_identical(inkr_outcome(unquote(row[[3L]])), c("parse", unquote(row[[1L]])), info = row[[1L]])
  }
})

test_that("each compile error code in the table is raised by its example", {
  rows <- spec_table(spec_lines(), "diagnostics:compile")
  codes <- vapply(rows, function(r) unquote(r[[1L]]), "")
  expect_setequal(codes, c(
    "undefined_variable", "circular_variable", "variable_too_large", "unknown_property",
    "invalid_color", "invalid_spacing", "invalid_border", "invalid_length", "invalid_text_style",
    "invalid_property_value", "invalid_registry_definition"
  ))
  checked <- 0L
  for (row in rows) {
    if (identical(row[[3L]], "none")) next
    checked <- checked + 1L
    expect_identical(inkr_outcome(unquote(row[[3L]])), c("compile", unquote(row[[1L]])), info = row[[1L]])
  }
  expect_equal(checked, 9L)
})

test_that("every unsupported feature in the table is an error, as stated", {
  rows <- spec_table(spec_lines(), "unsupported")
  expect_gte(length(rows), 12L)
  for (row in rows) {
    outcome <- inkr_outcome(unquote(row[[2L]]))
    expect_false(is.null(outcome), info = row[[1L]])
    expect_identical(outcome[[2L]], unquote(row[[3L]]), info = row[[1L]])
  }
})

test_that("statements the specification makes about details hold", {
  # Comments and whitespace around a combinator.
  expect_null(inkr_outcome("A /**/ B { }"))
  expect_identical(inkr_outcome("A/**/B { }"), c("parse", "invalid_selector"))
  # Non-ASCII text is allowed in comments only.
  expect_null(inkr_outcome("/* café */ A { }"))
  expect_identical(inkr_outcome("é { }"), c("parse", "unexpected_character"))
  # CRLF line endings.
  expect_null(inkr_outcome("A {\r\n  width: 3;\r\n}\r\n"))
  # The last declaration needs its semicolon; empty rules and sheets are fine.
  expect_identical(inkr_outcome("A { width: 3 }"), c("parse", "missing_semicolon"))
  expect_null(inkr_outcome(""))
  expect_null(inkr_outcome("A { }"))
  # Variables hold keywords; they can be used before they are declared.
  expect_null(inkr_outcome("A { color: $k; } $k: inherit;"))
  # Identifiers are case-sensitive.
  expect_identical(inkr_outcome("A { Color: red; }"), c("compile", "unknown_property"))
  expect_identical(inkr_outcome("A { color: Red; }"), c("compile", "invalid_color"))
})
