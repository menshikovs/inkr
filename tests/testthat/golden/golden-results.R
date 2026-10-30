# How golden-task05.rds was produced.
#
# It holds golden_all() (see ../helper-golden.R) evaluated with the engine as of
# Task 05, before the prepared-selector optimization of Task 06. It must not be
# regenerated to make a failing test pass: a difference means the engine's
# observable behaviour changed.
#
#   pkgload::load_all(".")
#   source("tests/testthat/helper-golden.R")
#   saveRDS(golden_all(), "tests/testthat/golden/golden-task05.rds", compress = "xz")
