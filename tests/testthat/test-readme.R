# The R examples in README.md are run, in order, using only the package's
# exported functions, and the output shown in their `#>` comments is compared
# with what they actually print. README.md is part of the source repository, not
# of the installed package, so the test is skipped when it is not reachable.

readme_path <- function() {
  path <- test_path("..", "..", "README.md")
  skip_if(!file.exists(path), "README.md is not part of the installed package")
  path
}

# The ```r blocks of the README, as character vectors of lines.
readme_blocks <- function(lines) {
  blocks <- list()
  open <- NA_integer_
  for (i in seq_along(lines)) {
    if (startsWith(lines[[i]], "```")) {
      if (is.na(open)) {
        if (identical(trimws(lines[[i]]), "```r")) open <- i else open <- 0L
      } else {
        if (open > 0L) blocks[[length(blocks) + 1L]] <- lines[(open + 1L):(i - 1L)]
        open <- NA_integer_
      }
    }
  }
  blocks
}

# An environment that sees the exported functions of inkr and nothing else of it.
readme_environment <- function() {
  env <- new.env(parent = globalenv())
  for (name in getNamespaceExports("inkr")) {
    assign(name, getExportedValue("inkr", name), envir = env)
  }
  # Stands in for the installed package's files: inst/ of the source tree.
  inst <- test_path("..", "..", "inst")
  env$system.file <- function(..., package = "base") {
    if (identical(package, "inkr")) file.path(inst, ...) else base::system.file(..., package = package)
  }
  env
}

test_that("every README example runs and prints what its comments show", {
  blocks <- readme_blocks(readLines(readme_path(), encoding = "UTF-8"))
  expect_gt(length(blocks), 10L)
  env <- readme_environment()
  checked <- 0L
  for (block in blocks) {
    text <- paste(block, collapse = "\n")
    if (grepl("install_github|roxygenise|devtools::test", text)) next
    code <- block[!grepl("^library\\(inkr\\)\\s*$", block)]
    expected <- trimws(sub("^.*#>\\s?", "", code[grepl("#>", code, fixed = TRUE)]))
    expected <- expected[nzchar(expected)]

    printed <- character()
    for (expression in parse(text = paste(code, collapse = "\n"), keep.source = FALSE)) {
      printed <- c(printed, capture.output({
        result <- withVisible(eval(expression, env))
        if (result$visible) print(result$value)
      }))
    }
    printed <- trimws(printed)
    for (line in expected) {
      expect_true(any(grepl(line, printed, fixed = TRUE)), info = paste("README output not produced:", line))
      checked <- checked + 1L
    }
  }
  expect_gt(checked, 40L)
})

test_that("the README documents the public functions by name", {
  readme <- paste(readLines(readme_path(), encoding = "UTF-8"), collapse = "\n")
  for (name in setdiff(getNamespaceExports("inkr"), "ink_session_remove")) {
    expect_true(grepl(name, readme, fixed = TRUE), info = name)
  }
})
