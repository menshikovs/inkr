# Ink for R (`inkr`)

Ink for R is an independent, CSS-like styling language and engine for terminal
user interfaces written in R. Stylesheets use the `.inkr` extension.

The language is inspired by Textual CSS (TCSS) but has its own specification
and implementation. The package is a reusable styling engine: it does **not**
depend on `termr` or any other terminal UI framework.

> **Version 0.1.0.** The pipeline below is implemented and tested. The language is
> specified in [docs/language-spec-v0.1.md](docs/language-spec-v0.1.md).

## Overview

inkr works in separate steps, each consuming the output of the one before:

```text
 .inkr text
     |  ink_parse() / ink_read()       Parsing: text -> syntax tree
     v
 syntax tree
     |  ink_compile()                  Compilation: variables, typed values, property registry
     v
 compiled stylesheet  <-- ink_element(): a logical element tree
     |  ink_match()                    Selector matching and specificity
     v
 matched rules
     |  ink_compute() / ink_explain()  Cascade, inheritance, computed style
     v
 computed style
     |  ink_terminal_style()           Terminal style adaptation (explicit capabilities)
     v
 terminal style  -->  ink_ansi_wrap()  optional ANSI SGR text
```

* **Independent.** inkr does not depend on `termr` or on any TUI framework.
* **Not a renderer.** It does not draw widgets, lay out or measure terminal space, wrap
  text, move the cursor or handle input. The terminal adapter *describes* how a style can
  be shown (colors, attributes, border characters, spacing); a host decides where.
* **Explicit capabilities.** `ink_terminal_capabilities()` is plain data you supply; the
  environment (`TERM`, `COLORTERM`, `NO_COLOR`) is never read, and nothing writes to the
  terminal on its own.
* **Stateless or sessions.** `ink_match()`, `ink_compute()` and `ink_explain()` keep
  nothing between calls. A *session* (`ink_session()`) is an optional layer that caches
  computed styles for a tree that is queried repeatedly and changed a little at a time; it
  returns exactly what the stateless functions return.
* **Public interface.** The exported functions, the classes they return and the fields
  documented in their help pages (see `?inkr`). Anything else, such as unexported functions,
  prepared selectors and cache entries, is internal and may change.
* **Language.** The `.inkr` language is specified in
  [docs/language-spec-v0.1.md](docs/language-spec-v0.1.md); the README shows how to use the
  package, not the full syntax.

## Installation

```r
# install.packages("remotes")
remotes::install_github("menshikovs/inkr", ref = "v0.1.0")
```

## Usage

```r
library(inkr)

ast <- ink_parse("
  $primary: #7c3aed;

  Button.primary:focus {
      color: $primary;
      padding: 1 2;
  }
")
ast
#> <ink_stylesheet_ast> <text>: 2 statements
#>   [1] $primary: #7c3aed;  (2:3)
#>   [2] Button.primary:focus { 2 declarations }  (4:3)

rule <- ast$statements[[2]]
rule$selectors$selectors[[1]]$compounds[[1]]$classes
#> [1] "primary"
rule$declarations[[2]]$value   # raw text; variables are not resolved
#> [1] "1 2"

# Parse a file
ink_read(system.file("examples", "basic.inkr", package = "inkr"))
```

Malformed input signals a structured `ink_parse_error`:

```r
err <- tryCatch(
  ink_parse("Button {\n    color: red\n    padding: 1;\n}"),
  ink_parse_error = identity
)
err$code     #> "missing_semicolon"
err$line     #> 2
err$column   #> 15
conditionMessage(err)
#> "<text>:2:15: Missing ';' after the value of property 'color'"
```

## Compiling

`ink_compile()` resolves variables, checks every declaration against a property
registry, and turns values into typed objects:

```r
sheet <- ink_compile(ast)
sheet
#> <ink_compiled_stylesheet> <text>: 1 rule, 1 variable
#> variables:
#>   $primary = #7c3aed
#> [1] Button.primary:focus  (4:3)
#>     color: #7c3aed  <color>
#>     padding: top=1 right=2 bottom=1 left=2  <spacing>

sheet$rules[[1]]$declarations[[1]]$value$r   # typed value, not a string
#> [1] 124
```

Compilation does not match selectors or apply the cascade. Selectors are kept
as parsed, and declarations stay in source order, duplicates included
(`color: red; color: blue;` gives two declarations).

Invalid input signals a structured `ink_compile_error`:

```r
err <- tryCatch(
  ink_compile(ink_parse("Button {
    padding: 1 two;
}", source = "app.inkr")),
  ink_compile_error = identity
)
err$code           #> "invalid_spacing"
conditionMessage(err)
#> "app.inkr:2:14: Invalid padding value '1 two'. Expected 1 to 4 non-negative integers."
```

Variables, the value syntax and the built-in properties are specified in
[docs/language-spec-v0.1.md](docs/language-spec-v0.1.md). Unknown properties are errors;
properties of the built-in value types can be added from R:

```r
registry <- ink_register_property(
  ink_registry(), "row-stripe", type = "color", initial = "transparent"
)
ink_compile(ink_parse("DataTable { row-stripe: #242424; }"), registry)
```

Compile errors are `ink_compile_error`s with stable codes (`?ink_compile_error`). See
`inst/examples/variables.inkr` for a complete example.

## From stylesheet to computed style

The pipeline has five separate steps:

| Step | Function | What it does |
|------|----------|--------------|
| Parsing | `ink_parse()`, `ink_read()` | text to a syntax tree (AST) |
| Compiling | `ink_compile()` | resolves variables, validates properties, makes values typed |
| Matching | `ink_match()` | finds the rules whose selectors apply to one element |
| Cascading | (internal) | picks the winning declaration for each property |
| Computing | `ink_compute()` | resolves `inherit` / `initial`, applies inheritance and defaults |

```r
library(inkr)

sheet <- ink_compile(ink_parse("
  $primary: #7c3aed;

  Screen { color: white; background: #181825; }
  Container { padding: 1; }
  Button { color: inherit; background: blue; border: single white; }
  Button.primary { background: $primary; }
  Button:focus { text-style: bold; }
  #save { border: round green; }
"))

# A logical element tree. Only parents are stored.
screen <- ink_element("Screen", id = "main")
container <- ink_element("Container", classes = "panel", parent = screen)
button <- ink_element(
  "Button", id = "save", classes = "primary", states = "focus", parent = container
)
button
#> <ink_element> Button#save.primary:focus
#>   path: Screen#main > Container.panel > Button#save.primary:focus

ink_match(sheet, button)
#> <ink_match_result> 4 matching rules for Button#save.primary:focus
#>
#> [1] Button
#>     specificity: (0, 0, 1)
#>     rule: 3
#>
#> [2] Button.primary
#>     specificity: (0, 1, 1)
#>     rule: 4
#>
#> [3] Button:focus
#>     specificity: (0, 1, 1)
#>     rule: 5
#>
#> [4] #save
#>     specificity: (1, 0, 0)
#>     rule: 6

style <- ink_compute(sheet, button)
style
#> <ink_computed_style> Button#save.primary:focus
#>
#>   color       white
#>   background  #7c3aed
#>   text-style  bold
#>   padding     top=0 right=0 bottom=0 left=0
#>   margin      top=0 right=0 bottom=0 left=0
#>   border      round green
#>   width       auto
#>   height      auto

style$values$background        # a typed ink_color, not a string
style$provenance$color$origin  # "explicit_inherit": `color: inherit` took the parent's value
```

`ink_match()` only *selects* rules: if two matched rules set the same property
both are returned. The cascade then decides, per property, which declaration
wins. The button's `padding` is the initial value because the `Container`'s
padding does not inherit.

### Elements and matching

`ink_element()` describes a runtime component: `type`, `id`, `classes`, active
`states` and `parent`. It is unrelated to the `ink_node` classes of the syntax
tree. Identifiers follow stylesheet syntax and are case-sensitive; duplicates
in `classes` and `states` are dropped. Keeping IDs unique is up to the
application. Elements are plain lists and should be treated as immutable.

| Selector | Matches an element when |
|----------|-------------------------|
| `*` | always |
| `Button` | `type` is `"Button"` |
| `#save` | `id` is `"save"` |
| `.primary` | `classes` contains `"primary"` |
| `:focus` | `states` contains `"focus"` (any valid name works, e.g. `:loading`) |
| `Button.primary:focus` | all of the above hold for the same element |
| `Container Button` | some ancestor is a `Container` (descendant) |
| `Container > Button` | the direct parent is a `Container` (child) |
| `Button, Input` | either selector matches |

Selectors are matched right to left against the parent chain. Mixed
combinators are exact: `A > B C` matches `C` in `A > B > B > C`, even though the
nearest `B` ancestor has no `A` parent, because the matcher keeps every ancestor
that can satisfy each part of the selector.

### Specificity and the cascade

`ink_specificity()` returns the tuple `c(id, class, type)`: IDs, then classes
and pseudo-classes, then types. `*` and combinators count nothing. Tuples are
compared lexicographically, never as a weighted sum, so one ID always beats any
number of classes.

```r
ink_specificity(ast$statements[[2]]$selectors$selectors[[1]])
```

For each property independently:

1. the declaration with the higher specificity wins;
2. with equal specificity, the later rule wins;
3. within a rule, the later declaration wins.

For a selector list (`Button, #save`) the specificity used is that of the most
specific selector that actually matched the element. Only one stylesheet is
considered: there are no other origins, inline styles or `!important`.

### Inheritance, `inherit` and `initial`

For every property in the registry, `ink_compute()` uses, in order:

1. the winning declaration, if any (`inherit` and `initial` are resolved here);
2. otherwise the parent's *computed* value, if the property inherits (`color`
   and `text-style` among the built-ins, or the `inherits` flag of a custom
   property);
3. otherwise the registry's initial value.

Because the parent's computed value is used, a value set on a grandparent
reaches the child through parents that declare nothing. `inherit` forces
inheritance even for non-inherited properties (and gives the initial value on a
root element). `initial` always gives the registry's initial value. Ancestors
are processed iteratively from the root down, each once, so deep trees do not
hit R's recursion limit.

### Computed style and provenance

An `ink_computed_style` has `values` (every registered property, in registry
order, as typed values with no keywords or variables left), `provenance` (how
each value was obtained: `declaration`, `inherited`, `initial`,
`explicit_inherit` or `explicit_initial`, plus the winning rule, declaration
and selector index, specificity and source span where applicable) and the
`element`. See `?ink_computed_style`.

### Debugging styles

`ink_explain(sheet, element, property)` answers *why* an element has the value it
has. The three inspection functions divide the work:

| Function | Answers |
|----------|---------|
| `ink_match()` | which rules apply to this element? |
| `ink_compute()` | what is the final typed value of every property? |
| `ink_explain()` | why does this property have this value? |

```r
sheet <- ink_compile(ink_parse("
  Screen { color: green; }
  Button { color: white; }
  .primary { color: blue; }
  #save { color: green; }
"))
screen <- ink_element("Screen", id = "main")
panel <- ink_element("Container", parent = screen)
button <- ink_element("Button", id = "save", classes = "primary", parent = panel)

ink_match(sheet, button)                 # the 3 rules that apply (+ their specificity)
ink_compute(sheet, button)$values$color  # green
ink_explain(sheet, button, "color")
#> <ink_style_explanation>
#>
#> Element: Button#save.primary
#> Property: color
#> Computed: green
#> Origin: declaration
#>
#> Winner:
#>   #save
#>     color: green
#>     specificity: (1, 0, 0)
#>     rule 4, declaration 1
#>     source: <text>:5:11
#>
#> Overridden:
#>   Button
#>     color: white
#>     specificity: (0, 0, 1)
#>     reason: lower specificity
#>     rule 2, declaration 1
#>     source: <text>:3:12
#>
#>   .primary
#>     color: blue
#>     specificity: (0, 1, 0)
#>     reason: lower specificity
#>     rule 3, declaration 1
#>     source: <text>:4:14
```

For a value nobody declared, the explanation traces where it came from:

```r
ink_explain(sheet, ink_element("Label", parent = panel), "color")
#> <ink_style_explanation>
#>
#> Element: Label
#> Property: color
#> Computed: green
#> Origin: inherited
#>
#> No declaration for 'color' applies to this element.
#>
#> Resolution:
#>   Screen#main
#>     declaration: green (specificity (0, 0, 1), <text>:2:12)
#>
#>   Container
#>     inherited from Screen#main: green
#>
#>   Label
#>     inherited from Container: green
```

The result is structured data; printing is only a view of it:

* `value`: the final typed value, identical to `ink_compute(...)$values[[property]]`;
* `origin`: `declaration`, `inherited`, `initial`, `explicit_inherit` or
  `explicit_initial` (the same as the computed provenance);
* `winner` and `candidates`: every declaration for the property from the matching
  rules, in stylesheet order, with rule, declaration and selector indices,
  specificity, typed value, original text and source span. Each loser has a
  `reason`: `lower_specificity`, `earlier_rule` (equal specificity, later rule
  wins) or `earlier_declaration` (same rule, later declaration wins). Equal
  values still lose by priority;
* `resolution`: the chain of elements from the root, each with its own origin and
  value, and the `effective_source`, the element that originally supplied the
  value. An ancestor whose value was overridden further down is shown as
  context only. `inherit` and `initial` keep their winning declaration; an
  explicit `initial` ends the search at the registry value.

`ink_explain()` does not reimplement the cascade: it runs the same loop as
`ink_compute()` and records what happens, so it cannot disagree with the
computed style. Custom registered properties work without changes. See
`?ink_explain` for the full structure.

### Fast matching

Selectors are *prepared* once per stylesheet and matched *incrementally* down the
element chain, instead of searching the ancestors again for every element.

* A complex selector is split into compounds and combinators. For each element the
  engine keeps, per selector prefix, whether it matched the **parent** (needed for
  `>`) and whether it matched **some ancestor** (needed for a descendant space). The
  next element reads its parent's state and updates it with a few vectorized
  operations. Because the whole set of matching ancestors is remembered, not just
  the nearest, mixed selectors stay exact: `A > B C` still matches `C` in
  `A > B > B > C`. An element is never its own ancestor.
* A *selector index* files each selector under its rightmost compound (ID, else
  first class, else type, else a fallback list), so only selectors that could match
  an element are tested against it. The index can include selectors that turn out
  not to match, never the reverse; every candidate is still checked in full.
* Work per element depends on the stylesheet, not on the depth: a chain of D
  elements costs O(D) rather than O(D²). `ink_match()`, `ink_compute()` and
  `ink_explain()` all use it and return exactly what they returned before (the test
  suite compares them with the previous engine's recorded results, with the original
  matcher and with an exhaustive oracle).

On one machine, `ink_compute()` of a 4000-deep chain with a rule `Screen Filler`
took about 41 s before and about 2 s after; see `bench/benchmark-styles.R` and the
recorded runs in `bench/`. Timings depend on the machine; compare runs made together.

### Style sessions

For a tree that is queried repeatedly and changed a little at a time, a *session*
owns the tree and caches computed styles. The stateless functions above keep working
without it and give identical results.

```r
sheet <- ink_compile(ink_parse("
  Screen { color: white; }
  .active Button { color: yellow; }
"))

session <- ink_session(sheet)
ink_session_set(session, key = "root", type = "Screen")
ink_session_set(session, key = "panel", type = "Container", parent_key = "root")
ink_session_set(session, key = "button", type = "Button", classes = "primary",
                parent_key = "panel")

first <- ink_session_compute(session, "button")   # computes root, panel, button
second <- ink_session_compute(session, "button")  # cache hit
ink_session_stats(session)
#> <ink_session_stats> 3 nodes, 3 cached | hits 1, misses 1 | computed 3, invalidated 0

ink_session_set(session, "panel", classes = "active")  # an ancestor changes
third <- ink_session_compute(session, "button")
third$values$color                                     # now yellow
ink_session_stats(session)
#> <ink_session_stats> 3 nodes, 3 cached | hits 1, misses 2 | computed 5, invalidated 2
```

* **Identity.** Nodes are addressed by a *key* you choose. It is a session-local
  runtime identity, separate from the CSS `id` that `#save` matches (several nodes may
  share a CSS `id`). Nothing is guessed from an element's contents or address.
* **Updating.** `ink_session_set()` adds or updates a node. For an existing key only
  the arguments you pass change; `id = NULL`, `classes = character()`,
  `states = character()` and `parent_key = NULL` reset a field explicitly. A change
  is validated completely before anything is modified, so a failed call leaves the
  session untouched.
* **Invalidation.** Changing a node (including moving it to another parent) drops the
  cached styles of that node and *all its descendants*, because descendant selectors
  and inheritance can make any of them depend on it. Ancestors, siblings and other
  branches keep their caches, and a computation resumes from the nearest valid cached
  ancestor. Removing a node removes its whole subtree.
* **Cache entries** carry the node's revision and its parent's revision and are only
  used while both match, in addition to being dropped eagerly.
* **Sessions are independent.** A session holds its own copy of the stylesheet and
  never modifies it; two sessions never share nodes, caches or statistics.
* `ink_session_explain()` is `ink_explain()` for a node; it is not cached, so it can
  never be stale.

See `?ink_session` and `?ink_session_error`.

### Terminal Style Adapter

A computed style is platform-neutral. `ink_terminal_style()` adapts it to what a
particular terminal can show, and `ink_ansi_wrap()` serializes colors and text
attributes as ANSI SGR escape sequences. Neither draws anything, writes to the
terminal or depends on a TUI framework; a host decides where things go.

```r
sheet <- ink_compile(ink_parse("
  $accent: #7c3aed;

  Button.primary {
    color: white;
    background: $accent;
    text-style: bold;
    padding: 1 2;
    border: round $accent;
  }
"))

button <- ink_element(type = "Button", classes = "primary")
computed <- ink_compute(sheet, button)

capabilities <- ink_terminal_capabilities(color_depth = "truecolor", unicode = TRUE)
terminal <- ink_terminal_style(computed, capabilities = capabilities)
print(terminal)
#> <ink_terminal_style> truecolor, unicode
#>   foreground  palette16(7)
#>   background  rgb(124, 58, 237)
#>   text styles bold
#>   border      round  ╭─╮ / │ / ╰─╯  color rgb(124, 58, 237)
#>   padding     top=1 right=2 bottom=1 left=2
#>   margin      top=0 right=0 bottom=0 left=0
#>   width       auto
#>   height      auto

styled_text <- ink_ansi_wrap("Save", terminal)
cat(encodeString(styled_text), "\n")   # the escape sequences, shown literally
#> \033[1;37;48;2;124;58;237mSave\033[0m
cat(styled_text, "\n")          # optional demonstration: displays styled text on an ANSI terminal
```

The package never writes to the terminal on its own; `cat()` above is only there to
show the effect. (`white` is the ANSI color white, index 7, so it is sent as `37`.)

* **Capability profiles.** `ink_terminal_capabilities()` takes `color_depth`
  (`"truecolor"`, `"ansi256"`, `"ansi16"` or `"none"`), `unicode` and the supported
  `text_styles`. It is plain data: `TERM`, `COLORTERM` and `NO_COLOR` are never read,
  so a host that wants detection does it and passes the result in. One computed style
  can be adapted for any number of profiles.
* **Color quantization.** Truecolor keeps RGB exactly. For `ansi256` an RGB color goes
  to the nearest entry of the xterm 6x6x6 cube or its 24-step grey ramp (squared RGB
  distance; lower cube level on a channel tie; the cube wins a tie with the grey
  ramp), never to the theme-dependent colors 0-15. For `ansi16` it goes to the nearest
  of a fixed reference palette (lowest index on a tie). Named ANSI colors stay ANSI
  colors at every depth. `"none"` emits no color instructions, and colors are not
  replaced by black.
* **`default` vs `transparent`.** `default` is the terminal's own color (SGR `39`/`49`,
  sent only when the element's own declarations asked for it, not when a color was
  never set or only inherited);
  `transparent` is no fill. Both stay visible in the terminal style, and neither is
  black.
* **Alpha.** Terminals have no alpha, so a partially transparent color is composited
  onto a concrete RGB color before quantizing:
  `result = alpha * color + (1 - alpha) * underlying`. The underlying color is the
  style's own opaque background, or the explicit `matte` argument
  (`ink_terminal_style(computed, matte = "#181825")`). Alpha 0 on a background means no
  fill. If a color needs compositing and no concrete background is known, the error
  `unknown_alpha_background` is signalled; the terminal's real background is never guessed.
* **Text attributes.** `bold dim italic underline reverse strike` become SGR
  1, 2, 3, 4, 7, 9 in that order, limited to what the profile supports. The style keeps
  both the `requested` and the `enabled` attributes.
* **Borders.** `single`, `double`, `round` and `heavy` map to box-drawing characters
  (or `+ - |` when `unicode = FALSE`), kept separate from the border color. No box is
  drawn.
* **Spacing and size.** `padding` and `margin` are kept as `c(top, right, bottom, left)`
  in cells; `width` and `height` stay cell counts or `auto`. Nothing is measured or laid out.
* **ANSI wrapping.** `ink_ansi_wrap(text, style, reset = TRUE)` returns
  `ESC[<params>m` + text + `ESC[0m`; with `reset = FALSE` the attributes stay active
  until something else resets them. A style with nothing to apply returns the text
  unchanged. Control characters in `text` (ESC and the other C0 controls except tab and
  newline, DEL and the C1 controls) are removed, so untrusted text cannot inject
  terminal commands.
* **Custom properties** (such as `row-stripe`) are kept untouched in
  `extra_properties`, as their typed values; the adapter gives them no meaning.
* **Sessions.** Styles from `ink_compute()` and `ink_session_compute()` are adapted
  alike; sessions know nothing about terminals.

See `?ink_terminal_style`, `?ink_terminal_capabilities`, `?ink_ansi_wrap` and
`?ink_terminal_error`. Limitations: no automatic capability detection, no wide-character
or line-width handling, and no rendering.

### Limitations

* Only the first error is reported (variables are checked before rules); an error in a
  value that came from a variable points at the declaration using it.
* No custom value parsers, `unset`, `revert`, `!important`, inline styles or multiple
  stylesheets; see the language specification for the full list of unsupported syntax.
* No rendering: the terminal adapter produces colors, attributes, border characters and
  optional SGR strings, but nothing draws widgets, moves the cursor or lays out boxes, and
  the package never writes to the terminal on its own.
* `ink_compute()` itself is stateless and re-evaluates the whole path on every call;
  use a style session for repeated queries.
* Each layer signals its own condition class with a stable `code`: `ink_parse_error`,
  `ink_compile_error`, `ink_match_error`, `ink_compute_error`, `ink_explain_error`,
  `ink_session_error` and `ink_terminal_error`. Wrong argument types for `ink_parse()`,
  `ink_read()` and `ink_compile()` are ordinary R errors.
* `ink_explain()` explains one property per call.

See `inst/examples/cascade.inkr` for an annotated stylesheet.

## The `.inkr` language

[docs/language-spec-v0.1.md](docs/language-spec-v0.1.md) is the reference: syntax, selectors
and specificity, variables, the built-in properties and their value syntax, the cascade and
inheritance, unsupported features, and the error codes (with an example for each code a
stylesheet can raise). Its examples and tables are checked by the test suite.

## Documentation

See `?inkr` for the package overview and what is public, `?ink_parse` for the parse
errors, `?ink_stylesheet_ast` for the structure of the syntax tree and the source
location conventions,
`?ink_compile` for variable resolution and the compiled representation,
`?ink_values` for the typed values, `?ink_registry` for the property registry,
`?ink_compile_error` for compilation errors, `?ink_element`, `?ink_match` and
`?ink_specificity` for matching, `?ink_compute` and `?ink_computed_style`
for computed styles, `?ink_explain` for debugging them, `?ink_session` for cached,
incremental computation, and `?ink_terminal_style` for terminal output.

## Development

```r
roxygen2::roxygenise()   # regenerate man/ and NAMESPACE
devtools::test()         # or testthat::test_local()
```

Run `R CMD check --no-manual` for the full check. The tests that validate `README.md` and
`docs/language-spec-v0.1.md` need the source repository, so they are skipped when the tests
run against an installed or built copy of the package.

## License

Copyright 2026 Akims Mensikovs.

Licensed under the [Apache License 2.0](LICENSE).
