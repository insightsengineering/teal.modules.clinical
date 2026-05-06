---
name: picks-fixed-flag-migration
description: >-
  Replaces legacy fixed-flag checks (`isTRUE(param$fixed)` or `param$fixed`)
  with `teal.picks::is_pick_fixed(param)` in module code. Use when the user asks
  to verify/fix fixed handling for picks objects in tm_* modules.
disable-model-invocation: true
---

# Picks fixed-flag migration (`$fixed` -> `teal.picks::is_pick_fixed`)

## Parameter

- **`X`** — Module token / file stem (for example `tm_t_events_patyear`, `tm_t_exposure`), usually mapped to `R/X.R`.

## Goal

On the module branch for **`X`**, ensure fixed checks do not read `param$fixed` directly.
Prefer `teal.picks::is_pick_fixed(param)` consistently.

## Steps for the agent

1. **Repository**: `insightsengineering/teal.modules.clinical`.
2. **Checkout branch** for the module token:
   - `git fetch origin`
   - `git checkout X@picks_modules_migration@279-interactive_variables@main`
3. **Inspect module files**:
   - Start with `R/X.R`.
   - Include related files where the same module helpers/UI live.
4. **Find legacy fixed checks** (examples):
   - `isTRUE(param$fixed)`
   - `param$fixed`
5. **Replace with picks helper**:
   - `isTRUE(param$fixed)` -> `teal.picks::is_pick_fixed(param)`
   - `param$fixed` (when used as a fixed flag) -> `teal.picks::is_pick_fixed(param)`
6. **Safety rule**:
   - Apply replacements only when `param` is a picks object (`variables`, `values`, or `picks`).
   - Do **not** rewrite unrelated list/data-frame `$fixed` fields.
7. **Verify**:
   - Parse touched R files.
   - Run targeted tests for `X` if requested.

## Notes

- `teal.picks::is_pick_fixed()` is the canonical API and avoids dependence on internal object layout.
- Prefer explicit namespacing (`teal.picks::`) in package code for clarity.
