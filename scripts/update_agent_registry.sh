#!/bin/bash
set -euo pipefail

OUT="docs/agent-state/registry.md"
TASK_DIR="docs/agent-state/active-tasks"

mkdir -p docs/agent-state

{
  echo "# Agent Task Registry"
  echo
  echo "Last Updated:"
  date
  echo
  echo "| Agent | Task ID | Status | Branch | Task |"
  echo "|---|---|---|---|---|"

  for f in "$TASK_DIR"/*.md; do
    [ -f "$f" ] || continue
    agent="$(basename "$f" .md)"
    task_id="$(awk '/^Task ID:/{getline; print; exit}' "$f")"
    status="$(awk '/^Current Status:/{getline; print; exit}' "$f")"
    branch="$(awk '/^Current Branch:/{getline; print; exit}' "$f")"
    task="$(awk '/^Task:/{getline; print; exit}' "$f")"

    [ -z "$task_id" ] && task_id="TBD"
    [ -z "$status" ] && status="TBD"
    [ -z "$branch" ] && branch="TBD"
    [ -z "$task" ] && task="TBD"

    echo "| $agent | $task_id | $status | $branch | $task |"
  done
} > "$OUT"

echo "Updated $OUT"
