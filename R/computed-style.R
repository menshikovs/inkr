#' Computed styles
#'
#' An `ink_computed_style` holds the final, typed value of every property for
#' one element, as returned by [ink_compute()]. It is a plain list; treat it as
#' immutable.
#'
#' @section Fields:
#' * `values`: a list with one entry per property of the stylesheet's registry,
#'   named by property and in registry order (built-ins first, then custom
#'   properties in registration order). Each entry is a typed value (see
#'   [ink_values]), never a string. No `inherit` / `initial` markers and no
#'   variable references remain. Access hyphenated names with
#'   `style$values[["text-style"]]`.
#' * `provenance`: a list named like `values`. Each entry is an
#'   `ink_style_origin` describing how that value was chosen:
#'   * `origin`: `"declaration"` (a declaration won), `"inherited"` (taken from
#'     the parent's computed value), `"initial"` (the registry's initial value),
#'     `"explicit_inherit"` or `"explicit_initial"` (a winning declaration used
#'     that keyword; it has been resolved).
#'   * `source`: where the final value came from: `"declaration"`, `"parent"`
#'     or `"registry"`.
#'   * `keyword`: `"inherit"` / `"initial"` for the explicit origins, else
#'     `NULL`.
#'   * `rule_index`, `declaration_index`, `selector_index`, `specificity`,
#'     `span`: the winning declaration (rule position in the stylesheet,
#'     declaration position in its rule, the matching selector with the highest
#'     specificity, that specificity, and the declaration's source span), for
#'     origins that involve a declaration; `NULL` otherwise.
#'
#'   Losing declarations are not recorded.
#' * `element`: the element the style was computed for.
#'
#' Computed styles contain no ANSI codes and no layout information.
#'
#' @param x An `ink_computed_style`.
#' @param ... Ignored.
#' @return `print()` returns `x` invisibly.
#' @seealso [ink_compute()]
#' @name ink_computed_style
#' @examples
#' sheet <- ink_compile(ink_parse("Screen { color: green; } Button { padding: 1; }"))
#' button <- ink_element("Button", parent = ink_element("Screen"))
#' style <- ink_compute(sheet, button)
#' style$values$color
#' style$provenance$color$origin
#' style$provenance$padding$rule_index
NULL

new_ink_computed_style <- function(values, provenance, element) {
  structure(
    list(values = values, provenance = provenance, element = element),
    class = "ink_computed_style"
  )
}

#' @rdname ink_computed_style
#' @export
print.ink_computed_style <- function(x, ...) {
  cat("<ink_computed_style> ", format(x$element), "\n\n", sep = "")
  names <- names(x$values)
  width <- if (length(names)) max(nchar(names)) else 0L
  for (name in names) {
    cat(sprintf("  %-*s  %s\n", width, name, format(x$values[[name]])))
  }
  invisible(x)
}
