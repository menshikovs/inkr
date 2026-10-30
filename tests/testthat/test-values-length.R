test_that("cell counts are accepted", {
  length30 <- value("length", "30")
  expect_s3_class(length30, "ink_length")
  expect_equal(length30$kind, "cells")
  expect_equal(length30$cells, 30L)
  expect_type(length30$cells, "integer")
  expect_equal(value("length", "5")$cells, 5L)
})

test_that("auto is accepted", {
  auto <- value("length", "auto")
  expect_equal(auto$kind, "auto")
  expect_true(is.na(auto$cells))
})

test_that("boundary values are accepted", {
  expect_equal(value("length", "0")$cells, 0L)
  expect_equal(value("length", "2147483647")$cells, 2147483647L)
  expect_identical(value("length", "007"), value("length", "7"))
})

test_that("whitespace is ignored", {
  expect_identical(value("length", " 30 "), value("length", "30"))
  expect_identical(value("length", "\nauto\n"), value("length", "auto"))
})

test_that("unsupported lengths are rejected", {
  invalid <- c(
    "", "-1", "+1", "1.5", "50%", "1fr", "10vh", "10px", "calc", "AUTO",
    "1 2", "auto auto", "2147483648", "red", "#fff", "none", "1,2"
  )
  for (text in invalid) {
    expect_value_problem("length", text, "invalid_length")
  }
})

test_that("lengths format as the number or auto", {
  expect_equal(format(value("length", "30")), "30")
  expect_equal(format(value("length", "auto")), "auto")
  expect_output(print(value("length", "auto")), "<ink_length> auto", fixed = TRUE)
})

test_that("the constructor rejects invalid cells", {
  expect_error(new_ink_length_cells(-1))
  expect_error(new_ink_length_cells(1.5))
  expect_error(new_ink_length_cells(NA_integer_))
})

test_that("width and height compile", {
  sheet <- compile_text("A { width: 30; height: auto; }")
  declarations <- sheet$rules[[1L]]$declarations
  expect_equal(declarations[[1L]]$value$cells, 30L)
  expect_equal(declarations[[2L]]$value$kind, "auto")
  expect_compile_error("A { width: 50%; }", "invalid_length", 1, 12)
})
