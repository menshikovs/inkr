button <- ink_element("Button")

test_that("the later of two equal rules wins and the earlier is an earlier_rule", {
  explanation <- explain_css("Button { color: red; }\nButton { color: blue; }", button)
  expect_equal(format(explanation$value), "blue")
  expect_equal(reasons(explanation), c("earlier_rule", "winner"))
  expect_equal(explanation$winner$rule_index, 2L)
})

test_that("the later of two declarations in one rule wins", {
  explanation <- explain_css("Button { color: red; color: blue; }", button)
  expect_equal(format(explanation$value), "blue")
  expect_equal(reasons(explanation), c("earlier_declaration", "winner"))
  expect_equal(explanation$winner$declaration_index, 2L)
})

test_that("duplicate declarations among several rules are ordered by rule then declaration", {
  css <- "Button { color: red; color: green; }\nButton { color: blue; color: yellow; }"
  explanation <- explain_css(css, button)
  expect_equal(format(explanation$value), "yellow")
  expect_equal(
    reasons(explanation),
    c("earlier_rule", "earlier_rule", "earlier_declaration", "winner")
  )
})

test_that("rule position outranks declaration position", {
  css <- "Button { width: 1; width: 2; color: red; }\nButton { color: blue; }"
  explanation <- explain_css(css, button)
  expect_equal(format(explanation$value), "blue")
  # The loser sits at declaration 3, the winner at declaration 1: the rule decides.
  expect_equal(explanation$candidates[[1L]]$declaration_index, 3L)
  expect_equal(explanation$winner$declaration_index, 1L)
  expect_equal(reasons(explanation), c("earlier_rule", "winner"))
})

test_that("an earlier higher-specificity declaration wins over later ones", {
  css <- "#save { color: red; }\nButton { color: blue; color: green; }"
  explanation <- explain_css(css, ink_element("Button", id = "save"))
  expect_equal(format(explanation$value), "red")
  expect_equal(reasons(explanation), c("winner", "lower_specificity", "lower_specificity"))
})

test_that("equivalent values still lose by priority", {
  css <- "Button { color: red; }\nButton { color: red; }\nButton { color: #ff0000; }"
  explanation <- explain_css(css, button)
  expect_equal(reasons(explanation), c("earlier_rule", "earlier_rule", "winner"))
  # Equal-looking values do not turn the contest into a tie without a winner.
  expect_false(is.null(explanation$winner))
  expect_equal(explanation$winner$rule_index, 3L)
})

test_that("identical values at different specificities are still ranked", {
  css <- ".x { color: red; }\nButton { color: red; }"
  explanation <- explain_css(css, ink_element("Button", classes = "x"))
  expect_identical(explanation$candidates[[1L]]$value, explanation$candidates[[2L]]$value)
  expect_equal(reasons(explanation), c("winner", "lower_specificity"))
})

test_that("a reason is lower_specificity even when it also comes earlier", {
  css <- "Button { color: red; }\n.x { color: blue; }"
  explanation <- explain_css(css, ink_element("Button", classes = "x"))
  expect_equal(explanation$candidates[[1L]]$reason, "lower_specificity")
})

test_that("every reason comes from the documented set", {
  css <- paste(
    "Button { color: red; color: blue; }", "Button { color: green; }", ".x { color: white; }",
    "#a { color: black; }", "Button { color: yellow; }", sep = "\n"
  )
  explanation <- explain_css(css, ink_element("Button", id = "a", classes = "x"))
  documented <- c("lower_specificity", "earlier_rule", "earlier_declaration")
  losers <- Filter(function(c) c$status == "overridden", explanation$candidates)
  expect_true(all(vapply(losers, function(c) c$reason %in% documented, NA)))
  expect_true(all(vapply(losers, function(c) is.character(c$reason) && length(c$reason) == 1L, NA)))
})

test_that("a winner outranks every other candidate", {
  css <- paste(
    "Button { color: red; color: blue; }", "Button { color: green; }", ".x { color: white; }",
    "Button.x { color: black; }", "Button { color: yellow; }", sep = "\n"
  )
  explanation <- explain_css(css, ink_element("Button", classes = "x"))
  losers <- Filter(function(c) c$status == "overridden", explanation$candidates)
  for (loser in losers) {
    expect_true(outranks(explanation$winner, loser))
    expect_false(outranks(loser, explanation$winner))
  }
})

test_that("source locations identify the winning declaration", {
  css <- "Button {\n    color: red;\n}\n\n#save {\n    color: blue;\n}"
  explanation <- explain_css(css, ink_element("Button", id = "save"), source = "app.inkr")
  expect_equal(explanation$source, "app.inkr")
  span <- explanation$winner$span
  expect_equal(c(span$start$line, span$start$column), c(6L, 5L))
  loser <- explanation$candidates[[1L]]
  expect_equal(c(loser$span$start$line, loser$span$start$column), c(2L, 5L))
})
