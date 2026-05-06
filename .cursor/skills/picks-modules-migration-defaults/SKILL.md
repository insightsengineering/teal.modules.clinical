---
name: picks-modules-migration-defaults
description: >-
  Aligns teal.modules.clinical module defaults on picks migration branches with
  main. Use when the user names a module X (e.g. tm_t_tte), mentions
  picks_modules_migration, 279-interactive_variables, teal.picks vs
  teal.transform defaults, or asks to sync/revert module default parameters on a
  feature branch.
disable-model-invocation: true
---

# Picks modules migration — default parameters vs `main`

## Parameter

- **`X`** — Module basename used in git branch names and typically in `R/X.R` (examples: `tm_t_tte`, `tm_a_mmrm`, `tm_g_ci`).

## Task (verbatim procedure)

For the module **`X`**, check out branch **`X@picks_modules_migration@279-interactive_variables@main`**, and double-check that **default parameters** match **`main`**. If a default was **`NULL`** on **`main`**, revert it on the feature branch to **`NULL`**. If a default on **`main`** used **`teal.transform::`**, ensure the feature branch uses the corresponding **`teal.picks::`** equivalent.

## Steps for the agent

1. **Repository**: `insightsengineering/teal.modules.clinical` (this package).

2. **Checkout** (substitute `X`):
   - `git fetch origin`
   - `git checkout X@picks_modules_migration@279-interactive_variables@main`

3. **Locate defaults**: Open the module’s R sources (usually `R/X.R`; include companion files like `R/X_picks.R` if that is where exported defaults live).

4. **Compare to `main`** for the **same paths**:
   - Use `git show main:R/X.R` (and related paths) or an equivalent diff against **`main`** / **`origin/main`**.
   - Focus on **default values** of the exported module function’s parameters.

5. **Apply alignment rules**:
   - **`NULL` on `main`** → default must be **`NULL`** on the feature branch (restore original “no default” behavior).
   - **`teal.transform::…` on `main`** → on the feature branch use the **`teal.picks::`** equivalent that preserves the same intent for interactive picks.
   - Otherwise keep defaults consistent with **`main`** unless the migration explicitly requires a different but equivalent picks API.

6. **Verify**: Run targeted checks (e.g. `devtools::load_all()`, relevant tests for `X`) after edits.

## Notes

- Branch naming is literal: `X@picks_modules_migration@279-interactive_variables@main` (prefix **`X`** is the module token, not the word “X”).
- If **`main`** moved since the branch diverged, prefer **defaults as committed on `main` today** when the user asks for parity with **`main`**.
