---
name: module-branch-tests
description: >-
  Runs teal.modules.clinical package tests filtered to one module with
  TESTING_DEPTH = 5 via devtools::test(). Use when the user asks to run tests for
  module X on its branch, verify tests pass for tm_* module, check shinytest2 or
  testthat for a single module, or use TESTING_DEPTH with test filter.
disable-model-invocation: true
---

# Module branch — filtered tests (`devtools::test`)

## Parameter

- **`X`** — Module token passed to **testthat**’s filter (e.g. `tm_t_tte`, `tm_t_shift_by_grade`). It should match how test files are named under `tests/testthat/` (substring match).

## Goal

On the **checked-out branch** for that module’s work (for example `X@picks_modules_migration@279-interactive_variables@main`), confirm whether **filtered tests pass** using project test depth **5**.

## Steps for the agent

1. **Repository**: `insightsengineering/teal.modules.clinical`. Ensure the working tree is on the branch the user cares about (typically the module’s feature branch).

2. **Run tests** from the package root in R:

   ```r
   options(TESTING_DEPTH = 5)
   devtools::test(filter = "X")
   ```

   Substitute **`X`** with the actual module string (same value the user gives).

   Equivalent one-liner:

   ```r
   options(TESTING_DEPTH = 5); devtools::test(filter = "X")
   ```

3. **Report**: Summarize pass/fail counts and paste or summarize the first failures with file paths (`tests/testthat/test-*`, `test-shinytest2-*`).

4. **Optional**: If tests fail, inspect the failing files and related `R/X.R` before proposing fixes—do not widen scope beyond fixing failing tests unless asked.

## Notes

- `filter` selects tests whose **file** names match the regular expression built from `filter` (see **devtools** / **testthat** documentation for the running version).
- Keep **`TESTING_DEPTH = 5`** unless the user specifies another depth.
- This skill does **not** change defaults or migration logic; it only **runs** tests on the current branch.
