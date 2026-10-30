# Ink for R (`inkr`)

Ink for R is an independent, CSS-like styling language and engine for terminal
user interfaces written in R. Stylesheets use the `.inkr` extension.

The language is inspired by Textual CSS (TCSS) but has its own specification
and implementation. The package is a reusable styling engine: it does **not**
depend on `termr` or any other terminal UI framework.

> **Status: early development.** Stylesheets are parsed, compiled into typed
> declarations, matched against a logical element tree, run through the cascade
> and turned into computed styles (typed values for every property). Nothing is
> rendered: there is no terminal output, ANSI generation or layout yet.

## Installation

```r
# install.packages("remotes")
remotes::install_github("menshikovs/termr-ink")
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

### Variables

* Global to the stylesheet; forward references are allowed.
* May be used inside multi-part values and inside other variables.
* The last declaration of a name wins. Names are case-sensitive.
* Substitution is token-based (`$a` never matches the start of `$accent`).
* Undefined variables (`undefined_variable`) and cycles (`circular_variable`,
  with the chain `$a -> $b -> $c -> $a`) are errors, even in variables no rule
  uses.

### Values

| Type | Syntax | Notes |
|------|--------|-------|
| color | `#rgb`, `#rgba`, `#rrggbb`, `#rrggbbaa`; `black`, `red`, `green`, `yellow`, `blue`, `magenta`, `cyan`, `white` and `bright-` variants; `default`; `transparent` | Hex colors are stored as RGBA integers; ANSI names stay ANSI colors; `transparent` is not black |
| spacing | 1 to 4 non-negative integers | Stored as `top`, `right`, `bottom`, `left`. No `auto`, negatives or fractions (also for `margin`) |
| border | `none`, or `single`/`double`/`round`/`heavy` followed by a color | A color is required except for `none` |
| length | non-negative integer (cells) or `auto` | No `%`, `fr`, viewport units or `calc()` |
| text_style | `none`, or any of `bold dim italic underline reverse strike` | Stored in a fixed order, duplicates removed |

`inherit` and `initial` are accepted for every property and kept as markers
(`ink_keyword`); they are not resolved.

### Built-in properties

| Property | Type | Inherits | Initial |
|----------|------|----------|---------|
| `color` | color | yes | `default` |
| `background` | color | no | `transparent` |
| `text-style` | text_style | yes | `none` |
| `padding`, `margin` | spacing | no | `0` |
| `border` | border | no | `none` |
| `width`, `height` | length | no | `auto` |

Unknown properties are errors. Add your own with the built-in value types:

```r
registry <- ink_register_property(
  ink_registry(), "row-stripe", type = "color", initial = "transparent"
)
ink_compile(ink_parse("DataTable { row-stripe: #242424; }"), registry)
```

### Compile error codes

`undefined_variable`, `circular_variable`, `variable_too_large`,
`unknown_property`, `invalid_property_value`, `invalid_color`,
`invalid_spacing`, `invalid_border`, `invalid_length`, `invalid_text_style`,
`invalid_registry_definition`. See `?ink_compile_error`.

### Current limitations

* Only the first error is reported; variables are checked before rules.
* Errors in a value that came from a variable point at the declaration using
  it, not at the variable.
* No custom value parsers, percentages, `calc()`, `unset`, `revert` or
  `!important`.

See `inst/examples/variables.inkr` for a complete example.

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
ink_specificity(ast$statements[[1]]$selectors$selectors[[1]])
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

### Current limitations

* No rendering, terminal output, ANSI generation or layout.
* No caching: `ink_compute()` re-matches every element on the path each call.
* One stylesheet only; no `unset`, `revert`, `!important` or inline styles.
* Errors are `ink_match_error` (elements, matching) and `ink_compute_error`
  (computation); see `?ink_match_error` and `?ink_compute_error`.

See `inst/examples/cascade.inkr` for an annotated stylesheet.

## Language (initial subset)

```css
/* Comments cannot be nested. */

$primary: #7c3aed;          /* variables are declared at the top level */

Screen { background: #181825; }

Button.primary:focus {      /* type, class and pseudo-class */
    border: heavy $primary;
}

#results > .row:selected {  /* ID, child combinator */
    background: $primary;
}

Container Button,           /* descendant combinator, selector list */
* { padding: 0 1; }
```

* Selectors: type, `.class`, `#id`, `*`, compound (`Button.primary:focus`),
  pseudo-classes (parsed, never interpreted), descendant (`A B`) and child
  (`A > B`) combinators, and lists (`A, B`).
* Every declaration ends with `;`. Values must not be empty.
* Identifiers use ASCII letters, digits, `_` and `-`, and start with a letter
  or `_`.
* Not supported, and reported as errors rather than ignored: strings,
  functions such as `rgb()`, nested rules, `@import` and other at-rules, and
  `!important`.

Property values are kept as raw text; nothing is resolved or validated yet.

## Documentation

See `?ink_parse` for the syntax and error reference, `?ink_stylesheet_ast` for
the structure of the syntax tree and the source location conventions,
`?ink_compile` for variable resolution and the compiled representation,
`?ink_values` for the typed values, `?ink_registry` for the property registry,
`?ink_compile_error` for compilation errors, `?ink_element`, `?ink_match` and
`?ink_specificity` for matching, and `?ink_compute` and `?ink_computed_style`
for computed styles.

## Development

```r
roxygen2::roxygenise()   # regenerate man/ and NAMESPACE
devtools::test()         # or testthat::test_local()
```

## Package metadata

`DESCRIPTION` (author, maintainer e-mail) and `LICENSE` contain placeholders
that must be filled in by the package author before any release.
