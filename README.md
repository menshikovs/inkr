# Ink for R (`inkr`)

Ink for R is a CSS-like stylesheet engine for terminal interfaces, written in R.
Stylesheets use the `.inkr` extension. The language is inspired by Textual CSS
(TCSS) but has its own specification and implementation.

`inkr` is a reusable styling engine. It is independent of `termr` and of any
other terminal UI framework, and it does not draw anything: it turns a
stylesheet and a logical element tree into computed styles, and can describe
those styles for a terminal whose capabilities you state explicitly.

> **Version 0.1.0.** Documentation: <https://menshikovs.github.io/inkr/>

## Features

* **`.inkr` stylesheets**: parsing (`ink_parse()`, `ink_read()`) with source
  locations and structured errors.
* **Compilation** (`ink_compile()`): variables, typed values and a property
  registry that can be extended (`ink_register_property()`).
* **Selectors and specificity**: type, id, class, state, `*`, descendant and
  child combinators, selector lists (`ink_match()`, `ink_specificity()`).
* **Cascade and inheritance** producing **computed styles** (`ink_compute()`).
* **Style inspection** (`ink_explain()`): why does a property have its value?
* **Incremental sessions** (`ink_session()`): cached styles for trees that
  change a little at a time.
* **Terminal style adaptation** (`ink_terminal_capabilities()`,
  `ink_terminal_style()`) and **ANSI SGR encoding** (`ink_ansi_wrap()`).

## Installation

`inkr` is not on CRAN. Install the development version from GitHub:

```r
# install.packages("remotes")
remotes::install_github("menshikovs/inkr")
```

## Quick start

```r
library(inkr)

# 1. Parse and 2. compile a stylesheet
sheet <- ink_compile(ink_parse("
  $primary: #7c3aed;

  Screen { color: white; }
  Button { color: inherit; background: blue; }
  Button.primary { background: $primary; }
  #save { text-style: bold; }
"))

# 3. Describe an element in a logical tree
screen <- ink_element("Screen")
button <- ink_element("Button", id = "save", classes = "primary", parent = screen)

# 4. Compute its style
style <- ink_compute(sheet, button)
style
#> <ink_computed_style> Button#save.primary
#>
#>   color       white
#>   background  #7c3aed
#>   text-style  bold
#>   padding     top=0 right=0 bottom=0 left=0
#>   margin      top=0 right=0 bottom=0 left=0
#>   border      none
#>   width       auto
#>   height      auto

# 5. Inspect the result: why is the background purple?
ink_explain(sheet, button, "background")
#> <ink_style_explanation>
#>
#> Element: Button#save.primary
#> Property: background
#> Computed: #7c3aed
#> Origin: declaration
#>
#> Winner:
#>   Button.primary
#>     background: $primary  [= #7c3aed]
#>     specificity: (0, 1, 1)
#>     rule 3, declaration 1
#>     source: <text>:6:20
#>
#> Overridden:
#>   Button
#>     background: blue
#>     specificity: (0, 0, 1)
#>     reason: lower specificity
#>     rule 2, declaration 2
#>     source: <text>:5:28
```

## Terminal styles

A computed style is platform-neutral. You choose the terminal capabilities;
`inkr` never reads `TERM`, `COLORTERM` or `NO_COLOR`, and never writes to the
terminal on its own.

```r
caps <- ink_terminal_capabilities(color_depth = "ansi256", unicode = TRUE)
terminal <- ink_terminal_style(style, caps)
terminal
#> <ink_terminal_style> ansi256, unicode
#>   foreground  palette16(7)
#>   background  palette256(99)
#>   text styles bold
#>   border      none
#>   padding     top=0 right=0 bottom=0 left=0
#>   margin      top=0 right=0 bottom=0 left=0
#>   width       auto
#>   height      auto

encodeString(ink_ansi_wrap("Save", terminal))   # escape sequences shown literally
#> [1] "\\033[1;37;48;5;99mSave\\033[0m"
```

## Documentation

* Website: <https://menshikovs.github.io/inkr/>, with the function reference
  and the changelog.
* Vignettes (`browseVignettes("inkr")` once installed from a build that
  includes them):
  * [Getting started](https://menshikovs.github.io/inkr/articles/getting-started.html)
  * [Styling and cascade](https://menshikovs.github.io/inkr/articles/styling-and-cascade.html)
  * [Terminal styles](https://menshikovs.github.io/inkr/articles/terminal-styles.html)
* Language specification:
  [docs/language-spec-v0.1.md](https://github.com/menshikovs/inkr/blob/main/docs/language-spec-v0.1.md)
  (syntax, selectors, properties, cascade and error codes).
* Package overview: `?inkr`.
* Annotated examples: `system.file("examples", package = "inkr")`.

| Task | Functions |
|------|-----------|
| Parse | `ink_parse()`, `ink_read()` |
| Compile | `ink_compile()`, `ink_registry()`, `ink_register_property()` |
| Elements and matching | `ink_element()`, `ink_match()`, `ink_specificity()` |
| Computed styles | `ink_compute()`, `ink_explain()` |
| Sessions | `ink_session()`, `ink_session_set()`, `ink_session_remove()`, `ink_session_compute()`, `ink_session_explain()`, `ink_session_stats()` |
| Terminal | `ink_terminal_capabilities()`, `ink_terminal_style()`, `ink_ansi_wrap()` |

## Limitations

`inkr` is a styling engine, not a renderer. It does not draw widgets, lay out
or measure terminal space, wrap text, move the cursor, handle input or detect
terminal capabilities. Borders and spacing are reported as metadata for a host
application to use. Only one stylesheet is considered at a time; there are no
inline styles, `!important`, `unset` or `revert`. See the language
specification for the complete list.

## Contributing

Issues and pull requests are welcome at
<https://github.com/menshikovs/inkr/issues>. Development guidance (tests,
documentation, compatibility rules) is in
[agents/README.md](agents/README.md).

```r
roxygen2::roxygenise()   # regenerate man/ and NAMESPACE
devtools::test()         # or testthat::test_local()
```

The tests that validate `README.md` and `docs/language-spec-v0.1.md` need the
source repository, so they are skipped against an installed or built copy of
the package.

## License

Copyright 2026 Akims Mensikovs.

Licensed under the [Apache License 2.0](LICENSE).
