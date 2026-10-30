test_that("empty input yields only an eof token", {
  tokens <- ink_tokenize("")
  expect_length(tokens, 1L)
  expect_equal(tokens[[1L]]$type, "eof")
  expect_equal(loc(tokens[[1L]]$start), c(line = 1, column = 1, offset = 0))
  expect_equal(tokens[[1L]]$start, tokens[[1L]]$end)
})

test_that("a simple rule is split into the expected tokens", {
  tokens <- ink_tokenize("Button { color: red; }")
  expect_equal(
    token_types(tokens),
    c(
      "ident", "whitespace", "lbrace", "whitespace", "ident", "colon",
      "whitespace", "ident", "semicolon", "whitespace", "rbrace", "eof"
    )
  )
  expect_equal(
    token_values(significant(tokens)),
    c("Button", "{", "color", ":", "red", ";", "}")
  )
})

test_that("token values reproduce the input exactly", {
  inputs <- c(
    "",
    "Button { color: red; }",
    "/* c */\n$a: #fff;\n\nA > B.c:d, #e {\r\n\tx-y: 1 -2.5 50%;\n}\n",
    "* { a: b }"
  )
  for (input in inputs) {
    tokens <- ink_tokenize(input)
    expect_equal(paste(token_values(tokens), collapse = ""), input)
  }
})

test_that("a multiline stylesheet is tokenized", {
  text <- "A {\n  b: c;\n}\n\nD {}\n"
  tokens <- significant(ink_tokenize(text))
  expect_equal(
    token_values(tokens),
    c("A", "{", "b", ":", "c", ";", "}", "D", "{", "}")
  )
})

test_that("whitespace is preserved as tokens", {
  tokens <- ink_tokenize("A  \t\n B")
  expect_equal(token_types(tokens), c("ident", "whitespace", "ident", "eof"))
  expect_equal(tokens[[2L]]$value, "  \t\n ")
})

test_that("comments are tokens", {
  tokens <- ink_tokenize("/* one */ A /* two\nlines */ { }")
  comments <- tokens[token_types(tokens) == "comment"]
  expect_equal(token_values(comments), c("/* one */", "/* two\nlines */"))
})

test_that("comment contents are not lexed", {
  tokens <- ink_tokenize("/* { } ; * / #zz $ ' */ A")
  expect_equal(token_types(tokens), c("comment", "whitespace", "ident", "eof"))
  expect_equal(ink_tokenize("/**/")[[1L]]$value, "/**/")
  expect_equal(ink_tokenize("/***/")[[1L]]$value, "/***/")
})

test_that("`#` is an id in selectors and a colour in values", {
  tokens <- significant(ink_tokenize("#submit { color: #ff0000; }"))
  expect_equal(
    token_types(tokens),
    c("id", "lbrace", "ident", "colon", "hex_color", "semicolon", "rbrace")
  )
  expect_equal(tokens[[1L]]$value, "#submit")
  expect_equal(tokens[[5L]]$value, "#ff0000")
})

test_that("an id that looks like a hex colour is still an id in a selector", {
  tokens <- significant(ink_tokenize("#fff { }"))
  expect_equal(tokens[[1L]]$type, "id")
})

test_that("hexadecimal colours with 3, 4, 6 and 8 digits are accepted", {
  for (hex in c("#fff", "#FFFF", "#7c3aed", "#7C3AED80", "#123")) {
    tokens <- significant(ink_tokenize(sprintf("A { c: %s; }", hex)))
    expect_equal(tokens[[5L]]$type, "hex_color")
    expect_equal(tokens[[5L]]$value, hex)
  }
})

test_that("malformed hexadecimal colours are rejected", {
  for (hex in c("#", "#ff", "#fffff", "#fffffff", "#ggg", "#ab-cd")) {
    err <- tryCatch(
      ink_tokenize(sprintf("A { c: %s; }", hex)),
      ink_parse_error = identity
    )
    expect_s3_class(err, "ink_parse_error")
    expect_equal(err$code, "invalid_hex_color")
    expect_equal(c(err$line, err$column), c(1L, 8L))
  }
})

test_that("variable references are tokens in values and declarations", {
  tokens <- significant(ink_tokenize("$primary: #fff; A { color: $primary; }"))
  expect_equal(
    token_types(tokens),
    c(
      "variable", "colon", "hex_color", "semicolon", "ident", "lbrace",
      "ident", "colon", "variable", "semicolon", "rbrace"
    )
  )
  expect_equal(tokens[[1L]]$value, "$primary")
  expect_equal(tokens[[9L]]$value, "$primary")
})

test_that("the colon after a variable name is not a pseudo-class", {
  tokens <- significant(ink_tokenize("$a:red;"))
  expect_equal(token_types(tokens), c("variable", "colon", "ident", "semicolon"))
})

test_that("selector tokens are recognised", {
  tokens <- significant(ink_tokenize("* A.b#c:d > E, F { }"))
  expect_equal(
    token_types(tokens),
    c(
      "star", "ident", "class", "id", "pseudo", "gt", "ident", "comma",
      "ident", "lbrace", "rbrace"
    )
  )
  expect_equal(token_values(tokens)[3:5], c(".b", "#c", ":d"))
})

test_that("a colon followed by a name is a pseudo-class only in selectors", {
  tokens <- significant(ink_tokenize("A:b { c:d; }"))
  expect_equal(tokens[[2L]]$type, "pseudo")
  expect_equal(tokens[[5L]]$type, "colon")
  expect_equal(tokens[[6L]]$type, "ident")
})

test_that("numbers keep their sign, fraction and unit", {
  tokens <- significant(ink_tokenize("A { x: 1 -2 +3 .5 1.5 50% 10vh -0.25fr; }"))
  values <- tokens[5:12]
  expect_true(all(token_types(values) == "number"))
  expect_equal(
    token_values(values),
    c("1", "-2", "+3", ".5", "1.5", "50%", "10vh", "-0.25fr")
  )
})

test_that("identifiers allow letters, digits, underscores and hyphens", {
  tokens <- significant(ink_tokenize("_a-b_1 { text-style: bold-italic; }"))
  expect_equal(token_values(tokens)[c(1, 3, 5)], c("_a-b_1", "text-style", "bold-italic"))
})

test_that("commas are tokens in values", {
  tokens <- significant(ink_tokenize("A { x: 1, 2; }"))
  expect_equal(token_types(tokens)[5:7], c("number", "comma", "number"))
})

test_that("locations use 1-based lines and columns and 0-based offsets", {
  tokens <- ink_tokenize("Button {\n  color: red;\n}")
  button <- tokens[[1L]]
  expect_equal(loc(button$start), c(line = 1, column = 1, offset = 0))
  expect_equal(loc(button$end), c(line = 1, column = 7, offset = 6))

  color <- tokens[token_values(tokens) == "color"][[1L]]
  expect_equal(loc(color$start), c(line = 2, column = 3, offset = 11))
  expect_equal(loc(color$end), c(line = 2, column = 8, offset = 16))

  close <- tokens[token_types(tokens) == "rbrace"][[1L]]
  expect_equal(loc(close$start), c(line = 3, column = 1, offset = 23))
  expect_equal(loc(close$end), c(line = 3, column = 2, offset = 24))
})

test_that("a token that ends in a newline ends at column 1 of the next line", {
  tokens <- ink_tokenize("A\n\nB")
  whitespace <- tokens[[2L]]
  expect_equal(whitespace$value, "\n\n")
  expect_equal(loc(whitespace$start), c(line = 1, column = 2, offset = 1))
  expect_equal(loc(whitespace$end), c(line = 3, column = 1, offset = 3))
})

test_that("the eof token sits just after the last character", {
  tokens <- ink_tokenize("A\nBC")
  eof <- tokens[[length(tokens)]]
  expect_equal(eof$type, "eof")
  expect_equal(loc(eof$start), c(line = 2, column = 3, offset = 4))

  tokens <- ink_tokenize("A\n")
  eof <- tokens[[length(tokens)]]
  expect_equal(loc(eof$start), c(line = 2, column = 1, offset = 2))
})

test_that("a multiline comment ends on the line where it closes", {
  comment <- ink_tokenize("/* a\nb */")[[1L]]
  expect_equal(loc(comment$start), c(line = 1, column = 1, offset = 0))
  expect_equal(loc(comment$end), c(line = 2, column = 5, offset = 9))
})

test_that("carriage returns are whitespace and do not add lines", {
  tokens <- ink_tokenize("A {\r\n}\r\n")
  close <- tokens[token_types(tokens) == "rbrace"][[1L]]
  expect_equal(loc(close$start), c(line = 2, column = 1, offset = 5))
})

test_that("columns and offsets count characters, not bytes", {
  tokens <- ink_tokenize("/* é中 */ A")
  ident <- tokens[token_types(tokens) == "ident"][[1L]]
  expect_equal(loc(ident$start), c(line = 1, column = 10, offset = 9))
})

test_that("an unterminated comment is a lexical error", {
  err <- tryCatch(ink_tokenize("A { }\n  /* never closed"), ink_parse_error = identity)
  expect_s3_class(err, "ink_parse_error")
  expect_equal(err$code, "unterminated_comment")
  expect_equal(c(err$line, err$column), c(2L, 3L))
  expect_equal(loc(err$span$end), c(line = 2, column = 18, offset = 23))
})

test_that("`/*/` does not close its own comment", {
  err <- tryCatch(ink_tokenize("/*/"), ink_parse_error = identity)
  expect_equal(err$code, "unterminated_comment")
})

test_that("comments do not nest", {
  err <- tryCatch(ink_tokenize("/* a /* b */ c */"), ink_parse_error = identity)
  expect_s3_class(err, "ink_parse_error")
  expect_equal(err$code, "unexpected_character")
})

test_that("unexpected characters are reported with their location", {
  err <- tryCatch(ink_tokenize("A {\n  b: c%;\n}"), ink_parse_error = identity)
  expect_equal(err$code, "unexpected_character")
  expect_equal(c(err$line, err$column), c(2L, 7L))
  expect_match(err$detail, "'%'", fixed = TRUE)

  err <- tryCatch(ink_tokenize("é { }"), ink_parse_error = identity)
  expect_equal(err$code, "unexpected_character")
  expect_match(err$detail, "U+00E9", fixed = TRUE)
})

test_that("unsupported syntax is reported explicitly", {
  codes <- c(
    "@import 'x';" = "unsupported_at_rule",
    "A { c: 'x'; }" = "unsupported_string",
    "A { c: \"x\"; }" = "unsupported_string",
    "A { c: rgb(1); }" = "unsupported_function",
    "A { c: red !important; }" = "unsupported_important",
    "A::before { }" = "invalid_selector",
    "A { .b { } }" = "unsupported_nesting",
    "A { &:hover { } }" = "unsupported_nesting"
  )
  for (text in names(codes)) {
    err <- tryCatch(ink_tokenize(text), ink_parse_error = identity)
    expect_s3_class(err, "ink_parse_error")
    expect_equal(err$code, codes[[text]], info = text)
  }
})

test_that("a sigil without a name is an error", {
  for (text in c(". { }", "# { }", "A { c: $; }", "$1: x;")) {
    err <- tryCatch(ink_tokenize(text), ink_parse_error = identity)
    expect_s3_class(err, "ink_parse_error")
    expect_true(err$code %in% c("invalid_selector", "invalid_variable"), info = text)
  }
})

test_that("ink_lex reports a lexical error as a final error token", {
  tokens <- ink_lex("A { c: % ; }")
  last <- tokens[[length(tokens)]]
  expect_equal(last$type, "error")
  expect_equal(last$code, "unexpected_character")
  expect_equal(loc(last$start), c(line = 1, column = 8, offset = 7))
})

test_that("R code in the input is treated as data", {
  err <- tryCatch(ink_tokenize("`1+1` { }"), ink_parse_error = identity)
  expect_s3_class(err, "ink_parse_error")
  expect_equal(err$code, "unexpected_character")
})

test_that("tokenizer arguments are validated", {
  expect_error(ink_tokenize(1), "character vector")
  expect_error(ink_tokenize(NA_character_), "missing values")
  expect_error(ink_tokenize("A", source = NA), "single string")
  expect_length(ink_tokenize(character()), 1L)
  expect_equal(
    token_values(significant(ink_tokenize(c("A", "B")))),
    c("A", "B")
  )
})
