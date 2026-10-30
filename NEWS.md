# inkr 0.1.0

First stable release of Ink for R, an independent styling engine for terminal
user interfaces.

* Parse and compile `.inkr` stylesheets with typed properties and variables.
* Match selectors and resolve specificity, the cascade, computed styles and
  inheritance against logical element trees.
* Inspect style provenance and report structured diagnostics.
* Cache computed styles in incremental style sessions.
* Adapt styles to explicit terminal capabilities and encode text as ANSI SGR.
* Specify the supported language in `docs/language-spec-v0.1.md`.

The package does not render widgets, perform layout or detect terminal
capabilities, and has no dependency on `termr`.
