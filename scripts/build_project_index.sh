#!/usr/bin/env bash
set -euo pipefail
cd "$(dirname "$0")/.."
python3 scripts/build_project_index.py
echo "Project index rebuilt at .project-index/"
