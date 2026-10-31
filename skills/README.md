# skills/

Reusable instructions and workflows for developing Ink for R, written for
people and for AI coding assistants. A skill is a short, self-contained
document that describes how to carry out one recurring development task, for
example "regenerate the documentation" or "run the pre-submission checks".

This directory is development tooling. It is not R package code, is excluded
from the built package through `.Rbuildignore`, and is never installed.

Add a skill only when a task is repeated and the steps are easy to get wrong.
Each skill lives in its own file or subdirectory and should state when to use
it, the exact commands and the expected result. There are no skills yet; the
general working rules for the repository are in
[../agents/README.md](../agents/README.md).
