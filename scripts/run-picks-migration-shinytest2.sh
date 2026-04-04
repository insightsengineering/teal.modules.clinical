#!/usr/bin/env bash
# Run shinytest2 files (TESTING_DEPTH = 5) for each per-module picks migration branch.
#
# Shared test helpers (e.g. tests/testthat/helper-TealAppDriver.R) should live on
#   picks_modules_migration@279-interactive_variables@main
# Merge or rebase that branch into each tm_*@picks_modules_migration@... branch
# before relying on this script, so picks helpers match the integration branch.
#
# Usage: from repo root (teal.modules.clinical):
#   chmod +x scripts/run-picks-migration-shinytest2.sh
#   ./scripts/run-picks-migration-shinytest2.sh
#
# Requires: git, Rscript, devtools, Chrome/Chromium for shinytest2.
set -euo pipefail
ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$ROOT"
export NOT_CRAN=true
SUFFIX='@picks_modules_migration@279-interactive_variables@main'
mapfile -t BRANCHES < <(git branch --format='%(refname:short)' | grep "${SUFFIX}\$" | grep -v "^picks_modules_migration@" | sort)
for branch in "${BRANCHES[@]}"; do
  module="${branch%%@picks_modules_migration@*}"
  filter="shinytest2-${module}"
  testf="tests/testthat/test-${filter}.R"
  echo "======== ${branch} (filter=${filter}) ========"
  if [[ ! -f "${testf}" ]]; then
    echo "SKIP: no ${testf} (e.g. tm_t_glm_counts has no shinytest2 file)"
    continue
  fi
  git checkout -q "${branch}"
  Rscript -e "options(TESTING_DEPTH = 5L); Sys.setenv(NOT_CRAN = 'true'); devtools::load_all('.'); devtools::test(pkg = '.', filter = '${filter}', stop_on_failure = FALSE)" || echo "FAILED: ${branch}"
  echo ""
done
echo "Done. Restore your branch with: git checkout - <your-branch>"
