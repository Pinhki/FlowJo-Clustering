#!/bin/bash
set -euo pipefail

AGENT="${1:-}"
shift || true
TASK="$*"

if [ -z "$AGENT" ] || [ -z "$TASK" ]; then
  echo "Usage: ./scripts/start_task.sh codex \"Task description\""
  exit 1
fi

case "$AGENT" in
  codex|cursor|gemini|chatgpt) ;;
  *) echo "Unknown agent: $AGENT"; exit 1 ;;
esac

mkdir -p docs/agent-state/active-tasks
TASK_ID="$(date +%Y%m%d-%H%M%S)-${AGENT}"

cat > "docs/agent-state/active-tasks/${AGENT}.md" <<EOT
# Active Task - ${AGENT}

Last Updated:
$(date)

Task ID:
${TASK_ID}

Task:
${TASK}

Current Status:
planning

Current Branch:
$(git branch --show-current)

Files In Scope:

Latest Progress:

Next Action:

Success Criteria:

Known Risks:

EOT

./scripts/update_agent_registry.sh 2>/dev/null || true
echo "Task initialized for ${AGENT}: ${TASK_ID}"
