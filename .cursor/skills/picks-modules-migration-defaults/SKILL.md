---
name: picks-modules-migration-defaults
description: >-
  Aligns teal.modules.clinical module defaults on picks migration branches with
  migration goals (teal.picks, preserve NULL/constants). Use when the user names
  a module X (e.g. tm_t_tte), mentions picks_modules_migration,
  279-interactive_variables, teal.picks vs teal.transform defaults, parentname,
  or asks to sync module default parameters on a feature branch.
disable-model-invocation: true
---

# Picks modules migration — default parameters vs `main`

## Parameter

- **`X`** — Module basename used in git branch names and typically in `R/X.R` (examples: `tm_t_tte`, `tm_a_mmrm`, `tm_g_ci`).

## Goal

The migration **replaces `teal.transform::` with `teal.picks::`** (or plain constants / `NULL`) in module APIs. When aligning defaults, **do not copy `teal.transform::` calls into feature-branch formals**—that would contradict the migration.

## Task (verbatim procedure)

For the module **`X`**, check out branch **`X@picks_modules_migration@279-interactive_variables@main`**, and double-check **default parameters** against **`main`** with the rules below.

## Steps for the agent

1. **Repository**: `insightsengineering/teal.modules.clinical` (this package).

2. **Checkout** (substitute `X`):
   - `git fetch origin`
   - `git checkout X@picks_modules_migration@279-interactive_variables@main`

3. **Locate defaults**: Open the module’s R sources (usually `R/X.R`; include companion files like `R/X_picks.R` if that is where exported defaults live).

4. **Compare to `main`** for the **same paths**:
   - Use `git show main:R/X.R` (and related paths) or an equivalent diff against **`main`** / **`origin/main`**.
   - Focus on **default values** of the exported module function’s parameters.

5. **Apply alignment rules** (migration-aware):

   - **`NULL` on `main`** → default must stay **`NULL`** on the feature branch (do not substitute picks placeholders).

   - **Pure constants on `main`** (string/number literals, simple vectors **without** `teal.transform::`) → keep the **same** constant defaults on the feature branch unless the picks API forces a different spelling.

   - **`teal.transform::…` inside a default on `main`** → on the feature branch express the **same user-facing intent** using **`teal.picks::`** (or a constant / `NULL`), **never** by leaving `teal.transform::` in the signature.

   - **Required parameters on `main`** (no default) → **no default** on the feature branch.

   - **`parentname`**: On `main`, signatures often use  
     `parentname = ifelse(inherits(arm_var, "data_extract_spec"), teal.transform::datanames_input(arm_var), "ADSL")`.  
     On the picks migration branch, **do not** preserve `teal.transform::datanames_input`. The usual picks-side default is the constant **`parentname = "ADSL"`** (same as the non-legacy branch of `main`), unless a concrete **`teal.picks::`** pattern is established for deriving parent names from specs.

6. **Verify**: Run targeted checks (e.g. `devtools::load_all()`, relevant tests for `X`) after edits.

## Notes

- Branch naming is literal: `X@picks_modules_migration@279-interactive_variables@main` (prefix **`X`** is the module token, not the word “X”).
- “Match `main`” means **match behavior and defaults at the user level**, not **verbatim** reuse of `teal.transform::` on migration branches.
