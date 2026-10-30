indexed <- function(css) prepare_selectors(compile_text(css))

test_that("selectors are filed by ID, class, type or fallback", {
  prepared <- indexed("#save { width: 1; }\n.primary { width: 2; }\nButton { width: 3; }\n* { width: 4; }\n:focus { width: 5; }")
  expect_equal(prepared$by_id[["save"]], 1L)
  expect_equal(prepared$by_class[["primary"]], 2L)
  expect_equal(prepared$by_type[["Button"]], 3L)
  expect_equal(prepared$fallback, c(4L, 5L))
})

test_that("a compound is filed under its strongest key: id, then class, then type", {
  prepared <- indexed("Button#save.primary:focus { width: 1; }\nButton.primary.big { width: 2; }\nButton:focus { width: 3; }")
  expect_equal(prepared$by_id[["save"]], 1L)
  # Selector 1 is filed under its ID only, so the class entry holds just selector 2.
  expect_equal(prepared$by_class[["primary"]], 2L)
  expect_equal(prepared$by_type[["Button"]], 3L)
})

test_that("only the rightmost compound is indexed", {
  prepared <- indexed("Screen > Container Button.primary { width: 1; }")
  expect_equal(prepared$by_class[["primary"]], 1L)
  expect_null(prepared$by_type[["Screen"]])
  expect_null(prepared$by_type[["Container"]])
  expect_null(prepared$by_type[["Button"]])
  expect_length(prepared$fallback, 0L)

  prepared <- indexed("Button .a * { width: 1; }")
  expect_equal(prepared$fallback, 1L)
  expect_null(prepared$by_type[["Button"]])
})

test_that("selector lists are indexed per selector with their own flat index", {
  prepared <- indexed("Button, #save, .primary { width: 1; }\nInput { width: 2; }")
  expect_equal(prepared$by_type[["Button"]], 1L)
  expect_equal(prepared$by_id[["save"]], 2L)
  expect_equal(prepared$by_class[["primary"]], 3L)
  expect_equal(prepared$by_type[["Input"]], 4L)
})

test_that("candidates are the filed selectors that could match, ascending and unique", {
  prepared <- indexed("Button { width: 1; }\n.primary { width: 2; }\n#save { width: 3; }\nScreen Button { width: 4; }\n* { width: 5; }\nInput { width: 6; }")
  button <- ink_element("Button", id = "save", classes = c("primary", "other"))
  expect_equal(index_candidates(prepared, button), c(1L, 2L, 3L, 4L, 5L))
  expect_equal(index_candidates(prepared, ink_element("Input")), c(5L, 6L))
  expect_equal(index_candidates(prepared, ink_element("Other")), 5L)
  expect_equal(index_candidates(prepared, ink_element("X", classes = "primary")), c(2L, 5L))
})

test_that("a selector with several classes is a candidate for each of its elements' classes only once", {
  prepared <- indexed(".a.b.c { width: 1; }")
  expect_equal(index_candidates(prepared, ink_element("X", classes = c("a", "b", "c"))), 1L)
  expect_equal(index_candidates(prepared, ink_element("X", classes = c("b", "c"))), integer())
  expect_equal(index_candidates(prepared, ink_element("X", classes = "a")), 1L)
})

test_that("elements are tested only against their candidates", {
  prepared <- indexed(paste(sprintf(".c%d { width: %d; }", 1:200, 1:200), collapse = "\n"))
  element <- ink_element("X", classes = c("c7", "c150"))
  before <- prepared_counters(prepared)$candidate_tests
  matched <- match_element(prepared, element, NULL)
  tested <- prepared_counters(prepared)$candidate_tests - before
  expect_equal(tested, 2L)
  expect_equal(vapply(matched$matches, function(m) m$rule_index, 0L), c(7L, 150L))
})

test_that("the index never loses a selector that matches (no false negatives)", {
  set.seed(31)
  misses <- 0L
  checked <- 0L
  for (trial in seq_len(150L)) {
    sheet <- compile_text(random_sheet_css(sample(3:12, 1L)))
    prepared <- prepare_selectors(sheet)
    leaf <- random_chain()
    chain <- element_chain(leaf)
    candidates <- index_candidates(prepared, leaf)
    flat <- 0L
    for (r in seq_along(sheet$rules)) {
      for (j in seq_along(sheet$rules[[r]]$selectors$selectors)) {
        flat <- flat + 1L
        if (exhaustive_selector_match(sheet$rules[[r]]$selectors$selectors[[j]], chain)) {
          checked <- checked + 1L
          if (!(flat %in% candidates)) misses <- misses + 1L
        }
      }
    }
  }
  expect_equal(misses, 0L)
  expect_gt(checked, 100L)
})

test_that("the index returns far fewer candidates than there are selectors", {
  css <- paste(c(sprintf("Type%d { width: 1; }", 1:100), sprintf(".class%d { width: 1; }", 1:100), "Button { width: 1; }"), collapse = "\n")
  prepared <- indexed(css)
  expect_equal(prepared$n_selectors, 201L)
  expect_equal(length(index_candidates(prepared, ink_element("Button", classes = "class5"))), 2L)
})
