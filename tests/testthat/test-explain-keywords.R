test_that("explicit inherit on an inherited property is distinct from automatic inheritance", {
  css <- "Screen { color: white; }\nButton { color: inherit; }"
  explicit <- explain_css(css, tree("Screen", "Button"))
  expect_equal(explicit$origin, "explicit_inherit")
  expect_equal(format(explicit$value), "white")
  expect_s3_class(explicit$value, "ink_color")

  automatic <- explain_css("Screen { color: white; }", tree("Screen", "Button"))
  expect_equal(automatic$origin, "inherited")
  expect_identical(explicit$value, automatic$value)
  expect_false(identical(explicit$origin, automatic$origin))
})

test_that("explicit inherit on a non-inherited property keeps its winning declaration", {
  css <- "Screen { background: blue; }\nButton { background: inherit; }"
  explanation <- explain_css(css, tree("Screen", "Button"), "background")
  expect_equal(explanation$origin, "explicit_inherit")
  expect_equal(format(explanation$value), "blue")
  winner <- explanation$winner
  expect_s3_class(winner$value, "ink_keyword")
  expect_equal(winner$value$keyword, "inherit")
  expect_equal(winner$raw, "inherit")
  expect_equal(winner$rule_index, 2L)
  expect_equal(winner$span$start$line, 2L)
  expect_equal(explanation$resolution$keyword, "inherit")
})

test_that("explicit inherit shows the parent-value resolution path", {
  css <- "Screen { background: blue; }\nContainer { background: inherit; }\nButton { background: inherit; }"
  explanation <- explain_css(css, tree("Screen", "Container", "Button"), "background")
  path <- explanation$resolution$path
  expect_equal(
    vapply(path, function(step) step$origin, ""),
    c("declaration", "explicit_inherit", "explicit_inherit")
  )
  expect_equal(vapply(path, function(step) step$supplied_by, 0L), c(1L, 1L, 1L))
  expect_equal(format(explanation$resolution$effective_source$element), "Screen")
  expect_equal(format(explanation$value), "blue")
})

test_that("explicit inherit at the root resolves from the registry", {
  explanation <- explain_css("Screen { color: inherit; }", ink_element("Screen"))
  expect_equal(explanation$origin, "explicit_inherit")
  expect_equal(explanation$resolution$source, "registry")
  expect_equal(format(explanation$value), "default")
  expect_false(is.null(explanation$winner))
  expect_equal(explanation$resolution$effective_source$position, 1L)
})

test_that("explicit initial is distinct from an implicit initial value", {
  explicit <- explain_css("Button { color: initial; }", ink_element("Button"))
  implicit <- explain_css("Button { padding: 1; }", ink_element("Button"))
  expect_equal(explicit$origin, "explicit_initial")
  expect_equal(implicit$origin, "initial")
  expect_identical(explicit$value, implicit$value)
  expect_false(is.null(explicit$winner))
  expect_null(implicit$winner)
  expect_equal(explicit$winner$raw, "initial")
})

test_that("explicit initial ends the inheritance search", {
  css <- "Screen { color: red; }\nContainer { color: blue; }\nButton { color: initial; }"
  explanation <- explain_css(css, tree("Screen", "Container", "Button"))
  expect_equal(explanation$origin, "explicit_initial")
  expect_equal(format(explanation$value), "default")
  expect_identical(explanation$value, explanation$resolution$initial_value)
  expect_equal(explanation$resolution$effective_source$position, 3L)
  expect_equal(explanation$resolution$source, "registry")
  chain <- Filter(function(step) step$in_effective_chain, explanation$resolution$path)
  expect_length(chain, 1L)
})

test_that("explicit initial applies to non-inherited properties too", {
  explanation <- explain_css("Button { background: initial; }", ink_element("Button"), "background")
  expect_equal(explanation$origin, "explicit_initial")
  expect_equal(format(explanation$value), "transparent")
})

test_that("inheritance below an explicit initial continues from the reset value", {
  css <- "Screen { color: red; }\nContainer { color: initial; }"
  explanation <- explain_css(css, tree("Screen", "Container", "Button"))
  expect_equal(explanation$origin, "inherited")
  expect_equal(format(explanation$value), "default")
  expect_equal(explanation$resolution$effective_source$origin, "explicit_initial")
  expect_equal(format(explanation$resolution$effective_source$element), "Container")
})

test_that("a stronger keyword declaration wins and the weaker value loses", {
  css <- "Screen { color: green; }\nButton { color: red; }\nButton.primary { color: inherit; }"
  explanation <- explain_css(css, tree("Screen", list(type = "Button", classes = "primary")))
  expect_equal(explanation$origin, "explicit_inherit")
  expect_equal(format(explanation$value), "green")
  expect_equal(reasons(explanation), c("lower_specificity", "winner"))
})

test_that("a keyword held in a variable is explained as that keyword", {
  css <- "$k: initial;\nScreen { color: green; }\nButton { color: $k; }"
  explanation <- explain_css(css, tree("Screen", "Button"))
  expect_equal(explanation$origin, "explicit_initial")
  expect_equal(explanation$winner$raw, "$k")
  expect_equal(explanation$winner$value$keyword, "initial")
})

test_that("the final value is never an unresolved keyword", {
  properties <- names(ink_registry()$properties)
  for (keyword in c("inherit", "initial")) {
    css <- paste(sprintf("Button { %s: %s; }", properties, keyword), collapse = "\n")
    sheet <- compile_text(css)
    for (element in list(ink_element("Button"), tree("Screen", "Button"))) {
      for (property in properties) {
        explanation <- ink_explain(sheet, element, property)
        expect_false(inherits(explanation$value, "ink_keyword"), info = property)
        expect_true(inherits(explanation$winner$value, "ink_keyword"), info = property)
        expect_true(
          explanation$origin %in% c("explicit_inherit", "explicit_initial"),
          info = property
        )
      }
    }
  }
})

test_that("keyword explanations agree with computed styles", {
  css <- "Screen { color: red; background: blue; padding: 1; }
          Container { color: initial; background: inherit; padding: inherit; }
          Button { color: inherit; background: initial; }"
  sheet <- compile_text(css)
  for (element in list(tree("Screen", "Container", "Button"), tree("Screen", "Container"), ink_element("Button"))) {
    expect_explanations_consistent(sheet, element)
  }
})
