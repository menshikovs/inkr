## Submission

This is the first submission of 'inkr' to CRAN (version 0.1.0).

'inkr' is a CSS-like styling language and engine for terminal user
interfaces. It parses and compiles stylesheets, matches selectors against a
logical element tree, applies the cascade and inheritance to produce computed
styles, explains how each value was obtained, and adapts computed styles to an
explicitly supplied set of terminal capabilities for ANSI SGR output. It does
not render widgets, lay out a terminal, or detect terminal capabilities, and it
has no dependencies other than R itself (testthat is in Suggests). Its
distinguishing feature is a declarative stylesheet language with CSS-style
specificity, cascade and inheritance, plus inspection of style provenance.

## Test environments

* Windows 11 x64, R 4.6.1 (2026-06-24, ucrt), local: `R CMD check --as-cran`
  on the built tarball, including the vignettes and the PDF and HTML manuals.
* GitHub Actions, Ubuntu (ubuntu-latest), R-devel: `R CMD check --as-cran
  --no-manual` (no LaTeX on the runner): passed.
* GitHub Actions, Windows (windows-latest), R-devel: `R CMD check --as-cran
  --no-manual`: passed.

Not tested: macOS, the declared minimum R 4.0.0, win-builder and R-hub. The
package is pure R, with no compiled code and no system requirements.

## R CMD check results

0 errors | 0 warnings | 1 note

* checking CRAN incoming feasibility ... NOTE
  Maintainer: 'Akims Mensikovs <akimm7926@gmail.com>'
  New submission

This is the expected note for a first submission.

## Notes

* Tests take roughly two to two and a half minutes. They use fixed seeds, only
  temporary files, and do not use the network.
* The tests that check README.md and the language specification (docs/, which
  is excluded from the built package) are skipped when those files are not
  reachable from the installed package, as under `R CMD check`.
