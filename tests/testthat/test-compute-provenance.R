css <- "
Screen {
  color: white;
  background: blue;
}

Button {
  width: 4;
  color: inherit;
  padding: initial;
}

Button, #save {
  border: round red;
}

Button.primary {
  background: green;
  background: yellow;
}
"

provenance_fixture <- function() {
  sheet <- compile_text(css)
  element <- tree("Screen", list(type = "Button", id = "save", classes = "primary"))
  list(sheet = sheet, element = element, style = ink_compute(sheet, element))
}

test_that("every property has a provenance record with the same fields", {
  style <- provenance_fixture()$style
  expect_equal(names(style$provenance), names(style$values))
  for (origin in style$provenance) {
    expect_s3_class(origin, "ink_style_origin")
    expect_named(
      origin,
      c(
        "property", "origin", "source", "keyword", "rule_index",
        "declaration_index", "selector_index", "specificity", "span"
      )
    )
  }
  expect_equal(
    vapply(style$provenance, function(o) o$property, ""),
    setNames(names(style$values), names(style$values))
  )
})

test_that("a winning declaration records its position, selector, specificity and span", {
  fixture <- provenance_fixture()
  origin <- fixture$style$provenance$width
  expect_equal(origin$origin, "declaration")
  expect_equal(origin$source, "declaration")
  expect_null(origin$keyword)
  expect_equal(origin$rule_index, 2L)
  expect_equal(origin$declaration_index, 1L)
  expect_equal(origin$selector_index, 1L)
  expect_identical(origin$specificity, c(id = 0L, class = 0L, type = 1L))
  declaration <- fixture$sheet$rules[[2L]]$declarations[[1L]]
  expect_identical(origin$span, declaration$span)
  expect_equal(origin$span$start$line, 8L)
})

test_that("the winning declaration index counts duplicates", {
  origin <- provenance_fixture()$style$provenance$background
  expect_equal(origin$origin, "declaration")
  expect_equal(origin$rule_index, 4L)
  expect_equal(origin$declaration_index, 2L)
  expect_equal(origin$span$start$line, 19L)
})

test_that("a selector list records the winning selector and its specificity", {
  fixture <- provenance_fixture()
  origin <- fixture$style$provenance$border
  expect_equal(origin$rule_index, 3L)
  expect_equal(origin$selector_index, 2L)
  expect_identical(origin$specificity, c(id = 1L, class = 0L, type = 0L))

  plain <- compute_css(css, tree("Screen", "Button"))$provenance$border
  expect_equal(plain$selector_index, 1L)
  expect_identical(plain$specificity, c(id = 0L, class = 0L, type = 1L))
})

test_that("an inherited value records that it came from the parent", {
  fixture <- provenance_fixture()
  child_color <- compute_css("Screen { color: white; }", tree("Screen", "Button"))$provenance$color
  expect_equal(child_color$origin, "inherited")
  expect_equal(child_color$source, "parent")
  expect_null(child_color$keyword)
  expect_null(child_color$rule_index)
  expect_null(child_color$declaration_index)
  expect_null(child_color$selector_index)
  expect_null(child_color$specificity)
  expect_null(child_color$span)
})

test_that("an initial value records the registry as its source", {
  origin <- compute_css("Button { color: red; }", ink_element("Button"))$provenance$margin
  expect_equal(origin$origin, "initial")
  expect_equal(origin$source, "registry")
  expect_null(origin$rule_index)
  expect_null(origin$span)
})

test_that("explicit inherit keeps the declaration and notes the resolution", {
  fixture <- provenance_fixture()
  origin <- fixture$style$provenance$color
  expect_equal(origin$origin, "explicit_inherit")
  expect_equal(origin$source, "parent")
  expect_equal(origin$keyword, "inherit")
  expect_equal(origin$rule_index, 2L)
  expect_equal(origin$declaration_index, 2L)
  expect_identical(origin$span, fixture$sheet$rules[[2L]]$declarations[[2L]]$span)
  expect_equal(shown(fixture$style, "color"), "white")
})

test_that("explicit inherit on a root resolves from the registry", {
  fixture <- provenance_fixture()
  root <- ink_compute(fixture$sheet, ink_element("Button"))
  origin <- root$provenance$color
  expect_equal(origin$origin, "explicit_inherit")
  expect_equal(origin$source, "registry")
  expect_equal(origin$keyword, "inherit")
  expect_equal(origin$rule_index, 2L)
  expect_equal(shown(root, "color"), "default")
})

test_that("explicit initial keeps the declaration and notes the resolution", {
  fixture <- provenance_fixture()
  origin <- fixture$style$provenance$padding
  expect_equal(origin$origin, "explicit_initial")
  expect_equal(origin$source, "registry")
  expect_equal(origin$keyword, "initial")
  expect_equal(origin$rule_index, 2L)
  expect_equal(origin$declaration_index, 3L)
  expect_equal(shown(fixture$style, "padding"), "top=0 right=0 bottom=0 left=0")
})

test_that("provenance origins are drawn from the documented set", {
  style <- provenance_fixture()$style
  origins <- vapply(style$provenance, function(o) o$origin, "")
  documented <- c("declaration", "inherited", "initial", "explicit_inherit", "explicit_initial")
  expect_true(all(origins %in% documented))
  expect_equal(unname(origins[["color"]]), "explicit_inherit")
  expect_equal(unname(origins[["height"]]), "initial")
})

test_that("provenance of an ancestor's declaration is not copied onto the child", {
  style <- compute_css("Screen { text-style: bold; }", tree("Screen", "Mid", "Button"))
  origin <- style$provenance[["text-style"]]
  expect_equal(origin$origin, "inherited")
  expect_null(origin$rule_index)
})

test_that("losing declarations are not retained", {
  style <- compute_css("Button { color: red; }\nButton { color: blue; }", ink_element("Button"))
  origin <- style$provenance$color
  expect_equal(origin$rule_index, 2L)
  expect_equal(length(origin), 9L)
  expect_false("candidates" %in% names(origin))
})

test_that("provenance is deterministic", {
  fixture <- provenance_fixture()
  again <- ink_compute(fixture$sheet, fixture$element)
  expect_identical(fixture$style$provenance, again$provenance)
})
