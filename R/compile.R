#' Compile a parsed stylesheet
#'
#' `ink_compile()` turns an [ink_stylesheet_ast] into an
#' `ink_compiled_stylesheet`: variables are resolved, every declaration is
#' checked against the property registry, and its value becomes a typed
#' object (see [ink_values]). The AST is not modified.
#'
#' Compiling does not match selectors, compute specificity or apply the
#' cascade. Selectors, declaration order and duplicate declarations are kept
#' exactly as written so that a later stage can do that work.
#'
#' @section Variables:
#' * Variables are global to the stylesheet and may be used before they are
#'   declared.
#' * A variable can be used anywhere inside a value, including in multi-part
#'   values (`border: round $primary;`), and in the value of another variable.
#' * If a name is declared more than once, the last declaration wins. Earlier
#'   declarations stay in the AST but take no part in resolution.
#' * Names are case-sensitive: `$Primary` and `$primary` are different.
#' * Substitution works on whole tokens, so `$a` never matches the start of
#'   `$accent`.
#' * Every effective variable is checked, even if no rule uses it: it must not
#'   refer to an undefined variable or take part in a cycle. Cycles are
#'   reported with their chain, for example `$a -> $b -> $c -> $a`.
#' * A value that would expand to more than 1000 tokens is rejected.
#'
#' Variables have no type of their own. Their value is checked where it is
#' used, against the type of the property.
#'
#' @section CSS-wide keywords:
#' `inherit` and `initial` are accepted for every registered property and
#' compile to an `ink_keyword` object. They are not resolved.
#'
#' @section Compiled stylesheet:
#' An `ink_compiled_stylesheet` is a list with:
#' * `rules`: a list of `ink_compiled_rule`, in source order. Each has
#'   `selectors` (the original `ink_selector_list` node from the AST), `span`,
#'   and `declarations`: a list of `ink_compiled_declaration` in source order,
#'   duplicates included. A declaration has `name`, `type` (the property's
#'   value type), `value` (a typed value or `ink_keyword`), `raw` (the value
#'   as written, before substitution), `span` and `value_span`.
#' * `variables`: the effective variables as a list named by variable name,
#'   in source order. Each is an `ink_resolved_variable` with `name`, `value`
#'   (the fully substituted text), `raw`, `span` and `value_span`.
#' * `registry`: the registry used.
#' * `source`: the source name, copied from the AST.
#'
#' Spans have the form described in [ink_stylesheet_ast].
#'
#' @section Limitations:
#' * Only the first error is reported; there is no recovery.
#' * Errors in values that came through a variable point at the declaration
#'   that uses the value, not at the variable.
#' * Properties that are not registered are rejected, never ignored.
#' * `margin` accepts only non-negative integers, like `padding`.
#'
#' @param ast An [ink_stylesheet_ast] from [ink_parse()] or [ink_read()].
#' @param registry An [ink_registry].
#' @param x An `ink_compiled_stylesheet`.
#' @param n Maximum number of rules (and of variables) to print: a single
#'   non-negative number (`Inf` prints all).
#' @param ... Ignored.
#' @return `ink_compile()` returns an `ink_compiled_stylesheet`. Invalid
#'   stylesheets signal an `ink_compile_error` ([ink_compile_error]).
#'   `print()` returns `x` invisibly.
#' @name ink_compile
#' @examples
#' ast <- ink_parse("
#'   $primary: #7c3aed;
#'
#'   Button.primary {
#'     background: $primary;
#'     padding: 1 2;
#'     border: round $primary;
#'     text-style: bold;
#'   }
#' ")
#' sheet <- ink_compile(ast)
#' sheet
#'
#' sheet$rules[[1]]$declarations[[1]]$value$r
#'
#' # Declarations are kept as written, duplicates included.
#' two <- ink_compile(ink_parse("Button { color: red; color: blue; }"))
#' length(two$rules[[1]]$declarations)
NULL

#' @rdname ink_compile
#' @export
ink_compile <- function(ast, registry = ink_registry()) {
  if (!inherits(ast, "ink_stylesheet_ast")) {
    stop("`ast` must be an ink_stylesheet_ast from ink_parse() or ink_read().", call. = FALSE)
  }
  validate_registry(registry)
  variables <- resolve_variables(ast)
  is_rule <- vapply(ast$statements, inherits, logical(1), what = "ink_rule")
  rules <- lapply(ast$statements[is_rule], compile_rule, registry, variables$tokens, ast$source)
  structure(
    list(
      rules = rules,
      variables = variables$variables,
      registry = registry,
      source = ast$source
    ),
    class = "ink_compiled_stylesheet"
  )
}

compile_rule <- function(rule, registry, resolved, source) {
  declarations <- lapply(rule$declarations, compile_declaration, registry, resolved, source)
  structure(
    list(selectors = rule$selectors, declarations = declarations, span = rule$span),
    class = "ink_compiled_rule"
  )
}

compile_declaration <- function(decl, registry, resolved, source) {
  property <- registry$properties[[decl$name]]
  if (is.null(property)) {
    abort_unknown_property(decl, registry, source)
  }
  ctx <- new_compile_context(
    source, decl$value_span, sprintf("property '%s'", decl$name),
    property = decl$name
  )
  tokens <- expand_tokens(lex_declared_value(decl$value, ctx), resolved, ctx)
  value <- as_css_wide_keyword(tokens) %||% parse_declared_value(property$type, tokens, decl, ctx)
  structure(
    list(
      name = decl$name,
      type = property$type,
      value = value,
      raw = decl$value,
      span = decl$span,
      value_span = decl$value_span
    ),
    class = "ink_compiled_declaration"
  )
}

# Parses tokens as `type`, turning a mismatch into a located compile error.
parse_declared_value <- function(type, tokens, decl, ctx) {
  tryCatch(
    parse_typed_value(type, tokens),
    ink_value_problem = function(problem) {
      expanded <- tokens_text(tokens)
      shown <- sprintf("'%s'", decl$value)
      if (!identical(expanded, decl$value)) {
        shown <- sprintf("%s (expands to '%s')", shown, expanded)
      }
      abort_in_context(
        ctx, problem$code,
        sprintf("Invalid %s value %s. %s", decl$name, shown, problem$expected)
      )
    }
  )
}

abort_unknown_property <- function(decl, registry, source) {
  start <- decl$span$start
  width <- nchar(decl$name)
  end <- ink_loc(start$line, start$column + width, start$offset + width)
  known <- names(registry$properties)
  ink_abort_compile(
    "unknown_property",
    sprintf(
      "Unknown property '%s'. Known properties: %s.",
      decl$name, paste(known, collapse = ", ")
    ),
    source = source, span = ink_span(start, end), property = decl$name
  )
}

# Printing -------------------------------------------------------------------

#' @rdname ink_compile
#' @export
print.ink_compiled_stylesheet <- function(x, n = 20L, ...) {
  n <- check_print_limit(n)
  rules <- length(x$rules)
  variables <- length(x$variables)
  cat(sprintf(
    "<ink_compiled_stylesheet> %s: %d rule%s, %d variable%s\n",
    x$source, rules, if (rules == 1L) "" else "s",
    variables, if (variables == 1L) "" else "s"
  ))
  if (variables > 0L) {
    cat("variables:\n")
    for (variable in x$variables[seq_len(min(variables, n))]) {
      cat(sprintf("  $%s = %s\n", variable$name, variable$value))
    }
    if (variables > n) {
      cat(sprintf("  ... and %d more\n", variables - n))
    }
  }
  for (i in seq_len(min(rules, n))) {
    print_compiled_rule(i, x$rules[[i]])
  }
  if (rules > n) {
    cat(sprintf("... and %d more rules\n", rules - n))
  }
  invisible(x)
}

print_compiled_rule <- function(index, rule) {
  cat(sprintf(
    "[%d] %s  (%s)\n",
    index, format_selector_list(rule$selectors), format_location(rule$span$start)
  ))
  for (decl in rule$declarations) {
    cat(sprintf(
      "    %s: %s  <%s>\n",
      decl$name, format(decl$value), sub("^ink_", "", class(decl$value)[[1L]])
    ))
  }
}
