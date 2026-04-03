"""Rewrite shinytest2 files: teal.picks inputs need badge open via get/set_teal_picks_slot."""
from __future__ import annotations

import re
import sys
from pathlib import Path


def transform(text: str) -> str:
    text = text.replace(
        'app_driver$get_text("a.nav-link.active")',
        'app_driver$get_text(".teal-modules-tree a.module-button.active")',
    )

    text = re.sub(
        r"app_driver\$get_active_module_input\(\"([\w]+)-(variables|values)-selected\"\)",
        r'as.vector(get_teal_picks_slot(app_driver, "\1", "\2"))',
        text,
    )

    def repl_set(m: re.Match[str]) -> str:
        indent, pid, slot, rest = m.group(1), m.group(2), m.group(3), m.group(4).strip()
        if rest.endswith(", wait_ = FALSE"):
            inner = rest[: -len(", wait_ = FALSE")].strip()
            if inner != "character(0)":
                raise ValueError(f"Unexpected wait_=FALSE combo: {rest!r}")
            return (
                f'{indent}set_teal_picks_slot(app_driver, "{pid}", "{slot}", '
                f"NULL, wait = FALSE)"
            )
        return f'{indent}set_teal_picks_slot(app_driver, "{pid}", "{slot}", {rest})'

    text = re.sub(
        r"^(\s*)app_driver\$set_active_module_input\(\"([\w]+)-(variables|values)-selected\","
        r"\s*(.*)\)\s*$",
        repl_set,
        text,
        flags=re.MULTILINE,
    )
    return text


def main() -> None:
    path = Path(sys.argv[1])
    old = path.read_text(encoding="utf-8")
    new = transform(old)
    if old == new:
        print(f"No changes: {path}")
    else:
        path.write_text(new, encoding="utf-8", newline="\n")
        print(f"Updated: {path}")


if __name__ == "__main__":
    main()
