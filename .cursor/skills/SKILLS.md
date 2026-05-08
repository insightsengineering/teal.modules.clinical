# Cursor skills (`teal.modules.clinical`)

Skills are defined under `.cursor/skills/<skill-name>/SKILL.md`. Invoke by name (see YAML `name` in each file) or by describing the workflow in chat.

| Skill | Summary |
|-------|---------|
| [picks-modules-migration-defaults](picks-modules-migration-defaults/SKILL.md) | Align exported module defaults with `main` / picks migration rules on feature branches (`teal.picks`, constants, `NULL`). |
| [module-branch-tests](module-branch-tests/SKILL.md) | Run `options(TESTING_DEPTH = 5)` then `devtools::test(filter = "X")` for one module on the current branch. |
| [picks-fixed-flag-migration](picks-fixed-flag-migration/SKILL.md) | Replace `isTRUE(param$fixed)` / `param$fixed` checks with `teal.picks::is_pick_fixed(param)` in module code. |

## Adding a skill

1. Add a directory `.cursor/skills/<skill-name>/` with `SKILL.md` (YAML frontmatter + instructions).
2. Register it in this file with a one-line summary and a link to `SKILL.md`.
