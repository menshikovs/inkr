# Selector parsing.
#
# selector_list    := complex ("," complex)*
# complex          := compound ((">" | <whitespace>) compound)*
# compound         := (ident | "*")? (class | id | pseudo)*   (non-empty)
#
# Whitespace between two compounds is the descendant combinator; it is only
# meaningful there, which is why tokens remember `space_before`.

is_compound_start <- function(token) {
  token$type %in% c("ident", "star", "class", "id", "pseudo")
}

parse_selector_list <- function(p) {
  selectors <- list(parse_complex_selector(p, after = NULL))
  while (parser_peek(p)$type == "comma") {
    parser_next(p)
    selectors[[length(selectors) + 1L]] <- parse_complex_selector(p, after = ",")
  }
  new_ink_selector_list(
    selectors,
    ink_span(selectors[[1L]]$span$start, selectors[[length(selectors)]]$span$end)
  )
}

# `after` names the token that must be followed by a selector (for messages),
# or NULL at the start of a rule.
parse_complex_selector <- function(p, after) {
  compounds <- list(parse_compound_selector(p, after))
  combinators <- character()
  repeat {
    token <- parser_peek(p)
    if (token$type == "gt") {
      parser_next(p)
      combinator <- "child"
      next_after <- ">"
    } else if (is_compound_start(token) && token$space_before) {
      combinator <- "descendant"
      next_after <- NULL
    } else {
      break
    }
    combinators <- c(combinators, combinator)
    compounds[[length(compounds) + 1L]] <- parse_compound_selector(p, next_after)
  }
  new_ink_complex_selector(
    compounds, combinators,
    ink_span(compounds[[1L]]$span$start, compounds[[length(compounds)]]$span$end)
  )
}

parse_compound_selector <- function(p, after) {
  first <- parser_peek(p)
  if (!is_compound_start(first)) {
    abort_expected_selector(p, first, after)
  }
  parts <- list(type = NULL, id = NULL, classes = character(), pseudo_classes = character())
  start <- first$start
  end <- first$end
  token <- parser_next(p)
  repeat {
    parts <- add_selector_part(p, parts, token)
    end <- token$end
    token <- parser_peek(p)
    if (!is_compound_start(token) || token$space_before) {
      break
    }
    parser_next(p)
  }
  new_ink_compound_selector(
    type = parts$type, id = parts$id,
    classes = parts$classes, pseudo_classes = parts$pseudo_classes,
    span = ink_span(start, end)
  )
}

# Records `token` in the compound being built. A type or `*` is only valid as
# the first part; an ID may appear once.
add_selector_part <- function(p, parts, token) {
  name <- substring(token$value, 2L)
  switch(token$type,
    ident = ,
    star = {
      if (!is.null(parts$type) || length(parts$classes) || !is.null(parts$id) ||
        length(parts$pseudo_classes)) {
        parser_abort_at(
          p, "invalid_selector",
          sprintf(
            "A type or '*' selector must come first in a compound selector, found '%s'",
            token$value
          ),
          token
        )
      }
      parts$type <- token$value
    },
    id = {
      if (!is.null(parts$id)) {
        parser_abort_at(
          p, "invalid_selector",
          "A compound selector can have only one ID", token
        )
      }
      parts$id <- name
    },
    class = parts$classes <- c(parts$classes, name),
    pseudo = parts$pseudo_classes <- c(parts$pseudo_classes, name)
  )
  parts
}

# `token` is where a selector was required but not found.
abort_expected_selector <- function(p, token, after) {
  if (token$type == "eof") {
    parser_abort_at(
      p, "unexpected_end_of_input",
      "Unexpected end of input; expected a selector", token
    )
  }
  if (is.null(after) && token$type == "rbrace") {
    parser_abort_at(p, "unexpected_closing_brace", "Unexpected '}'", token)
  }
  if (is.null(after) && !(token$type %in% c("lbrace", "gt", "comma"))) {
    parser_abort_at(
      p, "unexpected_token",
      sprintf("Expected a selector or variable, found %s", describe_token(token)),
      token
    )
  }
  after_text <- if (is.null(after)) "" else sprintf(" after '%s'", after)
  parser_abort_at(
    p, "invalid_selector",
    sprintf("Expected a selector%s, found %s", after_text, describe_token(token)),
    token
  )
}
