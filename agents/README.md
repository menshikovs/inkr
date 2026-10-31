# agents/

Guidance for AI coding agents (and human contributors) working in this
repository. This directory is development tooling: it is excluded from the
built R package through `.Rbuildignore` and is never installed.

## Expectations

* Preserve backward compatibility. The exported functions, the classes they
  return and the fields documented in their help pages are the public
  interface (`?inkr`).
* Follow the language specification,
  [docs/language-spec-v0.1.md](../docs/language-spec-v0.1.md). Change the
  specification and its tests together with any behavior it describes.
* Run the relevant tests before committing, and the full suite
  (`testthat::test_local()`) before a release. Do not weaken or delete
  existing regression tests.
* Avoid unnecessary dependencies. The package has no runtime dependencies
  other than R.
* Keep the package independent of `termr` and of any other terminal UI
  framework. Terminal capabilities are always passed in explicitly; do not add
  autodetection.
* Edit roxygen comments, not `man/` or `NAMESPACE` by hand, and regenerate
  them with `roxygen2::roxygenise()`.
* Check a candidate source package with `R CMD build .` followed by
  `R CMD check --as-cran` on the tarball.
* Do not publish releases, move tags, push with force or submit to CRAN
  without explicit authorization from the maintainer.

Reusable task instructions, if any, are in [../skills/](../skills/README.md).
