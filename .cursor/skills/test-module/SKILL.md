---
name: test-module
description: >-
  Compares a teal.modules.clinical module example on main vs
  279-interactive_variables: waits for outputs, screenshots module Show R Code
  modal and Report preview (with reporter cards expanded), writes a markdown
  summary comparing module output, report content, and reproducible code. Use
  when the user invokes test-module or wants branch parity checks for tm_*
  modules with screenshots and a written report file.
disable-model-invocation: true
---

# test-module — branch comparison for one module (`X`)

## Input

- **`X`** — Module name: the exported `tm_*` function (e.g. `tm_t_glm_counts`). Use it to locate `R/` source, roxygen `@examples`, and `tests/testthat/` files.

## Branches (defaults — override if the user specifies otherwise)

- **Branch A — `main`**
- **Branch B — `279-interactive_variables`** (confirm `origin/279-interactive_variables` / `279-interactive_variables@main` exists and fetch if needed.)

## Repository

Use the **`teal.modules.clinical`** package root (where `DESCRIPTION` and `R/` live). If the skill runs from a monorepo, `cd` into that package directory first.

## Goals (verbatim checklist from the user)

For **`X`**, compare the **module example** on **`main`** and on **`279-interactive_variables@main`**. Run the Shiny apps from those two branches and determine:

1. If you get the **same code** when you click **Show R Code**.
2. Whether you are **able to add a card to the report**.
3. Whether the module is **presenting the same information**.
4. Whether you can **play with the encoding** so the module shows **different output**, and whether the **same change in encodings** for the two implementations **behaves the same**.
5. Capture **screenshots** of **both** apps as specified below **and** write the **markdown summary** (including both example code blocks at the end).

## Recommended git approach

Prefer **two git worktrees** (one per branch) so both apps can run without losing local work, or **sequential checkouts** after `git stash push`. Never force-push or change git config.

```bash
git fetch origin main 279-interactive_variables 2>/dev/null || true
git worktree add ../teal.modules.clinical-main main
# Feature branch: existing clone or second worktree
```

## Running the example app (each branch)

1. `cd` to package root on that branch; ensure dependencies (`devtools::install_deps()`, `pkgload::load_all()`).
2. Run the **`@examples`** for **`X`** (`example(X, local = TRUE)`, temp script from roxygen, etc.). Avoid setting env var **`SHINY_PORT`** (it confuses Shiny); pass **`port`** only inside **`shiny::runApp(app, host = "0.0.0.0", port = …)`**.
3. Use distinct ports (e.g. **3838** = `main`, **3840** = **`279-interactive_variables`**).

## Waiting before screenshots (required)

Do **not** screenshot the module while a busy state is visible (e.g. toast **“Computing…”**, **“Loading data…”**, or an empty output panel still pending).

- **Browser / MCP**: After navigation, **wait** until those indicators **disappear** and the **primary output** (table, plot, etc.) is **visible** (or a clear empty-state message replaces loading). Retry with short polls (1–3 s) rather than one long blind wait.
- **Programmatic (preferred for reliability)**: Use **`shinytest2::AppDriver`** / **`teal::TealAppDriver`** (see **`tests/testthat/helper-TealAppDriver.R`** and **`init_teal_app_driver`**) and call **`$wait_for_idle()`** (and/or **`$wait_for_js()`** on a selector for the rendered output) before any screenshot.

Only then take **module output** screenshots (see below).

## Screenshot set (each branch — repeat for `main` and `279-interactive_variables`)

Use **browser MCP** or **headless Chrome** only if you can satisfy **wait** rules; otherwise prefer **TealAppDriver** + screenshot API where available.

| Step | What to capture |
|------|-----------------|
| **A. Module with output** | Full viewport: **encoding panel + rendered output** after computing **finished**. |
| **B. Show R Code modal** | Open **Show R code**; wait until the **modal** is visible (header typically **“Show R Code”**). Screenshot the **modal** (code area legible). **Copy** the modal’s R code text for the markdown appendix. |
| **C. Report → Show Report** | Add a card (**+ Add to Report**), open **Report → Show Report** / report preview (`#teal-preview_report-preview_button` or equivalent). **Expand every collapsed reporter accordion / teal card** (e.g. click **`.accordion-button.collapsed`** inside `#teal-preview_report-preview_content-reporter_cards`) so **card body text and table fragments are visible**, not just collapsed headers. Wait for paint, then screenshot. |

**Naming files** (example pattern — stay consistent):

- `<package>/.test-module-screenshots/<X>-main-module-output.png`
- `<package>/.test-module-screenshots/<X>-main-show-r-code.png`
- `<package>/.test-module-screenshots/<X>-main-report-show-report.png`
- Same triple for **`279`** (e.g. `<X>-279-module-output.png`, …).

Optional: repeat **A** after a **encoding** change (same change on both branches) for parity checks.

## Markdown summary file (required)

Create **one** markdown file per run, for example:

**`<teal.modules.clinical>/.test-module-reports/<YYYY-MM-DD>-<X>-main-vs-279.md`**

Programmatic runs should, where possible, **compare captured text** (module output region, expanded report card region, **Show R Code** modal) between branches after **whitespace normalization**. State clearly:

1. **Module output** — Whether the **same** rendered output (table/plot text) appears for the **same default encodings** (Same / Different / Unable to verify), with a one-line reason.
2. **Teal report content** — Whether **report preview** text (expanded cards) is the **same** or **different** (Same / Different).
3. **Code & reproducibility** — Whether **Show R Code** text is **identical**; if not, whether it is still **plausibly the same analysis** (e.g. same `ANL` rows and same model call) vs **materially different** pipelines. Say whether the two code paths **should produce the same numerical results** given identical inputs, or **not** (e.g. different merge/filter code).

Use this **structure**:

```markdown
# test-module: <X> — main vs 279-interactive_variables

## Summary

- **Module output (main vs 279):** Same / Different — …
- **Teal report preview (expanded cards):** Same / Different — …
- **Show R Code / reproducibility:** Identical / Different — … (same results expected? yes / no / unclear …)
- **Add to Report / Show Report:** …
- **Encodings (optional):** …

## Screenshots

| Branch | Module output | Show R Code modal | Report → Show Report |
|--------|---------------|-------------------|----------------------|
| main | ![…](./.test-module-screenshots/…) | … | … |
| 279-interactive_variables | … | … | … |

(Use image links or bullet list of paths if tables are awkward.)

---

## Appendix: example app source (`@examples`)

### Branch `main`

```r
<Paste the @examples R code for X from `git show main:R/<file>.R` or the file on the main worktree —
from `data <-` / `library` through `shinyApp` / `runApp`, without roxygen #’ prefixes.>
```

### Branch `279-interactive_variables`

```r
<Same for the feature branch — must reflect the actual example on that branch.>
```

### Notes on the appendix

- Obtain code with **`git show <branch>:R/<filename>.R`** and extract only the **`@examples`** block, or read **`man/<X>.Rd`** **on each worktree** after `devtools::document()` if needed.
- The appendix is the **example used to launch the app**, not necessarily the same strings as **Show R Code** inside the running app (those reflect merged analysis pipelines).

## Report back (chat)

In the user’s chat message, give a **short pointer**: path to the **markdown file**, list of **screenshot paths**, and **one paragraph** echoing the summary (do not duplicate the full appendix in chat unless asked).

## Related project skill

- **`module-branch-tests`** — `devtools::test(filter = "X")` with **`TESTING_DEPTH`**. **`test-module`** is interactive/visual + written report; not a substitute for **`devtools::test`**.
