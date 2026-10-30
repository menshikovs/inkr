#' inkr: CSS-like styles for terminal user interfaces
#'
#' inkr parses a CSS-like stylesheet language (`.inkr` files), works out which
#' style applies to a logical user-interface element, and describes that style
#' in terms a terminal can show. It is independent of any terminal user
#' interface framework.
#'
#' @section The pipeline:
#' Each step is a separate function, and each consumes the output of the one
#' before:
#'
#' 1. **Parsing**: [ink_parse()], [ink_read()] turn text into a syntax tree.
#' 2. **Compilation**: [ink_compile()] resolves variables and turns values into
#'    typed objects, checked against a property registry ([ink_registry()],
#'    [ink_register_property()]).
#' 3. **Selector matching**: [ink_match()] finds the rules that apply to an
#'    [ink_element()]; [ink_specificity()] gives a selector's specificity.
#' 4. **Cascade and computed styles**: [ink_compute()] applies the cascade,
#'    `inherit` / `initial` and inheritance, giving a typed value for every
#'    property. [ink_explain()] says how one value was obtained.
#' 5. **Terminal style adaptation**: [ink_terminal_style()] turns a computed
#'    style into terminal colors, attributes and border characters for explicit
#'    [capabilities][ink_terminal_capabilities()]; [ink_ansi_wrap()] serializes
#'    them as ANSI escape sequences.
#'
#' The stylesheet language is specified in `docs/language-spec-v0.1.md` in the
#' source repository.
#'
#' @section Stateless computation and sessions:
#' [ink_match()], [ink_compute()] and [ink_explain()] take a compiled stylesheet
#' and an element tree and keep nothing between calls. A style session
#' ([ink_session()] and the `ink_session_*()` functions) is an optional layer
#' for applications that query a tree repeatedly and change it a little at a
#' time: it caches computed styles and recomputes only what a change affects. A
#' session returns exactly what the stateless functions return.
#'
#' @section What inkr does not do:
#' inkr does not render widgets, draw borders, lay out or measure terminal
#' space, wrap text, move the cursor, handle input or detect the terminal. The
#' terminal adapter takes its capabilities as an explicit argument (it never
#' reads `TERM`, `COLORTERM` or `NO_COLOR`) and nothing in the package writes to
#' the terminal; displaying the result of [ink_ansi_wrap()] is up to the caller.
#'
#' @section Public interface and internal details:
#' The public interface is the exported functions listed above, the S3 classes
#' they return and the fields that their help pages document (for example
#' `code`, `detail`, `line` and `column` of the error conditions; `values` and
#' `provenance` of a computed style; the fields described in
#' [ink_stylesheet_ast], [ink_values], [ink_match], [ink_explain],
#' [ink_session] and [ink_terminal_style]). Everything else is an internal
#' implementation detail and may change without notice: unexported functions
#' (including prepared selectors, the selector index, cache entries and the
#' lexer and parser), and any field of an object that no help page documents.
#' Objects are plain lists with an S3 class and should be treated as read-only;
#' use the constructors and functions of the package to create or change them.
#'
#' @section Errors:
#' Each layer signals its own condition class, with a stable machine-readable
#' `code`: [ink_parse_error][ink_parse], [ink_compile_error], [ink_match_error],
#' [ink_compute_error], [ink_explain_error], [ink_session_error] and
#' [ink_terminal_error]. An argument of the wrong type given to [ink_parse()],
#' [ink_read()] or [ink_compile()] is an ordinary R error.
#'
#' @keywords internal
"_PACKAGE"
