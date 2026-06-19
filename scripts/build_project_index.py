from pathlib import Path
import csv, json, re
from datetime import datetime

ROOT = Path.cwd()
OUT = ROOT / ".project-index"
OUT.mkdir(exist_ok=True)

SKIP_DIRS = {".git", ".project-index", "__pycache__", ".Rproj.user", "renv", "node_modules"}

def keep(p):
    return not any(part in SKIP_DIRS for part in p.parts)

files = [p for p in ROOT.rglob("*") if p.is_file() and keep(p.relative_to(ROOT))]

with (OUT / "tree.txt").open("w") as f:
    for p in sorted(files):
        f.write(str(p.relative_to(ROOT)) + "\n")

with (OUT / "files.csv").open("w", newline="") as f:
    w = csv.writer(f)
    w.writerow(["path", "suffix", "size_bytes"])
    for p in sorted(files):
        w.writerow([p.relative_to(ROOT), p.suffix, p.stat().st_size])

func_re = re.compile(r"^\s*([A-Za-z0-9_.]+)\s*<-\s*function\s*\(")
with (OUT / "functions.csv").open("w", newline="") as f:
    w = csv.writer(f)
    w.writerow(["file", "line", "function"])
    for p in sorted(files):
        if p.suffix.lower() == ".r":
            for i, line in enumerate(p.read_text(errors="ignore").splitlines(), 1):
                m = func_re.match(line)
                if m:
                    w.writerow([p.relative_to(ROOT), i, m.group(1)])

with (OUT / "shiny_pages.csv").open("w", newline="") as f:
    w = csv.writer(f)
    w.writerow(["path", "role"])
    roles = {
        "app.R": "main_shiny_entry",
        "global.R": "global_setup",
        "ui.R": "ui",
        "server.R": "server",
        "run.R": "launcher",
    }
    for name, role in roles.items():
        for p in ROOT.rglob(name):
            if keep(p.relative_to(ROOT)):
                w.writerow([p.relative_to(ROOT), role])

with (OUT / "logs.csv").open("w", newline="") as f:
    w = csv.writer(f)
    w.writerow(["path", "size_bytes"])
    for p in sorted(files):
        if "log" in p.name.lower():
            w.writerow([p.relative_to(ROOT), p.stat().st_size])

(OUT / "README.md").write_text(f"""# Project Index

Generated: {datetime.now().isoformat(timespec='seconds')}

Start here before reading source files.

Core files:
- tree.txt: compact file list
- files.csv: file inventory
- functions.csv: R function index
- shiny_pages.csv: Shiny entry/global/page files
- logs.csv: log files
- agent_notes.md: running notes for agents

Agent rule:
Read this folder first, then open only the relevant source files.
""")

notes = OUT / "agent_notes.md"
if not notes.exists():
    notes.write_text("# Agent Notes\n\n- Keep this file updated after meaningful code changes.\n")

meta = {
    "generated": datetime.now().isoformat(timespec="seconds"),
    "root": str(ROOT),
    "file_count": len(files),
}
(OUT / "last_scan.json").write_text(json.dumps(meta, indent=2))
