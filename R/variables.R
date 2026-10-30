# Variable resolution.
#
# Variables are global to a stylesheet. Resolution works on value tokens (see
# values.R), never on text:
#
# 1. collect: the last declaration of each name is the effective one.
# 2. lex: every effective value is tokenized and its references listed.
# 3. check: every reference must name an effective variable.
# 4. expand: a depth-first search over the reference graph, using an explicit
#    stack. A variable is expanded (its references replaced by the already
#    expanded tokens of the variables it uses) when the search finishes with
#    it, so dependencies always come first. Reaching a variable that is still
#    on the stack is a cycle, and the stack holds the chain.
#
# All effective variables are processed, whether or not a rule uses them.

# Largest number of tokens a value may expand to. Real values have a handful;
# the limit stops `$a: $b $b; $b: $c $c; ...` from growing exponentially.
MAX_EXPANDED_TOKENS <- 1000L

# Returns `list(variables, tokens)`: `variables` is a named list of
# `ink_resolved_variable` in source order of the effective declarations;
# `tokens` is an environment mapping each name to its fully expanded tokens.
resolve_variables <- function(ast) {
  source <- ast$source
  effective <- collect_effective_variables(ast)
  var_names <- vapply(effective, function(decl) decl$name, "")

  contexts <- new.env(parent = emptyenv())
  for (decl in effective) {
    ctx <- new_compile_context(
      source, decl$value_span, sprintf("variable '$%s'", decl$name),
      variable = decl$name
    )
    ctx$declaration_span <- decl$span
    contexts[[decl$name]] <- ctx
  }

  lexed <- lex_variable_values(effective, contexts)
  check_references(var_names, lexed$refs, contexts)
  resolved <- expand_variables(var_names, lexed$tokens, lexed$refs, contexts)

  variables <- lapply(effective, function(decl) {
    structure(
      list(
        name = decl$name,
        value = tokens_text(resolved[[decl$name]]),
        raw = decl$value,
        span = decl$span,
        value_span = decl$value_span
      ),
      class = "ink_resolved_variable"
    )
  })
  names(variables) <- var_names
  list(variables = variables, tokens = resolved)
}

# The last declaration of each name, in the source order of those declarations.
collect_effective_variables <- function(ast) {
  is_variable <- vapply(ast$statements, inherits, logical(1), what = "ink_variable_declaration")
  declarations <- ast$statements[is_variable]
  names <- vapply(declarations, function(decl) decl$name, "")
  declarations[!duplicated(names, fromLast = TRUE)]
}

# Tokenizes a declaration's raw value; `ctx` says where to report problems.
lex_declared_value <- function(text, ctx) {
  tryCatch(
    value_tokens(text),
    ink_value_problem = function(problem) {
      abort_in_context(
        ctx, problem$code,
        sprintf("Invalid value in %s. %s", ctx$owner, problem$expected)
      )
    }
  )
}

variable_name <- function(token) {
  substring(token$value, 2L)
}

lex_variable_values <- function(effective, contexts) {
  tokens <- new.env(parent = emptyenv())
  refs <- new.env(parent = emptyenv())
  for (decl in effective) {
    value <- lex_declared_value(decl$value, contexts[[decl$name]])
    tokens[[decl$name]] <- value
    is_ref <- vapply(value, function(token) token$type == "variable", logical(1))
    refs[[decl$name]] <- unique(vapply(value[is_ref], variable_name, ""))
  }
  list(tokens = tokens, refs = refs)
}

check_references <- function(var_names, refs, contexts) {
  for (name in var_names) {
    for (ref in refs[[name]]) {
      if (is.null(contexts[[ref]])) {
        abort_undefined_variable(ref, contexts[[name]])
      }
    }
  }
}

abort_undefined_variable <- function(name, ctx) {
  abort_in_context(
    ctx, "undefined_variable",
    sprintf("Undefined variable '$%s' used in %s.", name, ctx$owner)
  )
}

expand_variables <- function(var_names, tokens, refs, contexts) {
  state <- new.env(parent = emptyenv()) # absent: unseen, 1: on stack, 2: done
  cursor <- new.env(parent = emptyenv()) # next reference to visit, per variable
  resolved <- new.env(parent = emptyenv())
  stack <- character(length(var_names))
  for (start in var_names) {
    if (!is.null(state[[start]])) {
      next
    }
    depth <- 1L
    stack[[1L]] <- start
    state[[start]] <- 1L
    cursor[[start]] <- 1L
    while (depth > 0L) {
      current <- stack[[depth]]
      index <- cursor[[current]]
      deps <- refs[[current]]
      if (index > length(deps)) {
        resolved[[current]] <- expand_tokens(tokens[[current]], resolved, contexts[[current]])
        state[[current]] <- 2L
        depth <- depth - 1L
        next
      }
      cursor[[current]] <- index + 1L
      dep <- deps[[index]]
      if (is.null(state[[dep]])) {
        depth <- depth + 1L
        stack[[depth]] <- dep
        state[[dep]] <- 1L
        cursor[[dep]] <- 1L
      } else if (state[[dep]] == 1L) {
        first <- match(dep, stack[seq_len(depth)])
        abort_circular_variable(c(stack[first:depth], dep), contexts)
      }
    }
  }
  resolved
}

# `chain` runs along the cycle and ends with the variable it started from.
abort_circular_variable <- function(chain, contexts) {
  ctx <- contexts[[chain[[1L]]]]
  ctx$span <- ctx$declaration_span
  abort_in_context(
    ctx, "circular_variable",
    paste0("Circular variable reference: ", paste0("$", chain, collapse = " -> ")),
    chain = chain
  )
}

# Replaces each variable token by the expanded tokens of that variable.
# `resolved` must already contain every variable the tokens refer to, except
# for names that are genuinely undefined (reported as such).
expand_tokens <- function(tokens, resolved, ctx) {
  pieces <- lapply(tokens, function(token) {
    if (token$type != "variable") {
      return(list(token))
    }
    value <- resolved[[variable_name(token)]]
    if (is.null(value)) {
      abort_undefined_variable(variable_name(token), ctx)
    }
    value
  })
  if (sum(lengths(pieces)) > MAX_EXPANDED_TOKENS) {
    abort_in_context(
      ctx, "variable_too_large",
      sprintf(
        "Variable substitution makes %s longer than %d tokens.",
        ctx$owner, MAX_EXPANDED_TOKENS
      )
    )
  }
  if (length(pieces) == 0L) {
    return(list())
  }
  unlist(pieces, recursive = FALSE)
}
