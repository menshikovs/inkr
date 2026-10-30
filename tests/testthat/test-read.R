write_bytes <- function(bytes, ext = ".inkr") {
  path <- tempfile(fileext = ext)
  writeBin(bytes, path)
  path
}

test_that("ink_read parses a stylesheet file", {
  path <- write_bytes(charToRaw("$a: 1;\nA { b: $a; }\n"))
  on.exit(unlink(path))
  ast <- ink_read(path)
  expect_s3_class(ast, "ink_stylesheet_ast")
  expect_length(ast$statements, 2L)
  expect_equal(ast$source, path)
})

test_that("ink_read parses the packaged example", {
  path <- system.file("examples", "basic.inkr", package = "inkr")
  skip_if(path == "", "installed examples are not available")
  ast <- ink_read(path)
  expect_s3_class(ast, "ink_stylesheet_ast")
  sources <- unique(ast$source)
  expect_equal(sources, path)
})

test_that("ink_read handles empty files, CRLF and a byte order mark", {
  empty <- write_bytes(raw())
  on.exit(unlink(empty), add = TRUE)
  expect_length(ink_read(empty)$statements, 0L)

  bom <- write_bytes(c(as.raw(c(0xEF, 0xBB, 0xBF)), charToRaw("A { b: c; }")))
  on.exit(unlink(bom), add = TRUE)
  ast <- ink_read(bom)
  expect_equal(ast$statements[[1L]]$span$start$column, 1L)

  crlf <- write_bytes(charToRaw("A {\r\n  b: c;\r\n}\r\n"))
  on.exit(unlink(crlf), add = TRUE)
  expect_equal(ink_read(crlf)$statements[[1L]]$span$end$line, 3L)
})

test_that("ink_read decodes UTF-8 content", {
  path <- write_bytes(charToRaw(enc2utf8("/* é中 */ A { b: c; }")))
  on.exit(unlink(path))
  ast <- ink_read(path)
  expect_equal(ast$statements[[1L]]$span$start$column, 10L)
})

test_that("syntax errors from ink_read name the file", {
  path <- write_bytes(charToRaw("A {\n  b: c\n}\n"))
  on.exit(unlink(path))
  err <- tryCatch(ink_read(path), ink_parse_error = identity)
  expect_s3_class(err, "ink_parse_error")
  expect_equal(err$source, path)
  expect_equal(c(err$line, err$column), c(2L, 7L))
  expect_match(conditionMessage(err), path, fixed = TRUE)
})

test_that("ink_read reports a missing file informatively", {
  path <- file.path(tempdir(), "does-not-exist.inkr")
  err <- tryCatch(ink_read(path), error = identity)
  expect_s3_class(err, "ink_read_error")
  expect_false(inherits(err, "ink_parse_error"))
  expect_match(conditionMessage(err), "does not exist")
  expect_match(conditionMessage(err), "does-not-exist.inkr", fixed = TRUE)
  expect_equal(err$path, path)
})

test_that("ink_read reports directories, binary and non-UTF-8 files", {
  dir <- tempfile()
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE))
  expect_error(ink_read(dir), "directory", class = "ink_read_error")

  binary <- write_bytes(as.raw(c(0x41, 0x00, 0x42)))
  on.exit(unlink(binary), add = TRUE)
  expect_error(ink_read(binary), "NUL", class = "ink_read_error")

  latin1 <- write_bytes(as.raw(c(0x41, 0xe9, 0x42)))
  on.exit(unlink(latin1), add = TRUE)
  expect_error(ink_read(latin1), "UTF-8", class = "ink_read_error")
})

test_that("ink_read validates its argument", {
  expect_error(ink_read(1), "single string")
  expect_error(ink_read(NA_character_), "single string")
  expect_error(ink_read(c("a", "b")), "single string")
})
