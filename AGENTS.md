## Task Context Bootstrap

## Shared Shiny Style Template

Before reusable Shiny UI/CSS/style work, check:

`/Users/rongzhenyu/Downloads/R_pipeline/shared_shiny_style/README.md`

Canonical CSS template:

`/Users/rongzhenyu/Downloads/R_pipeline/shared_shiny_style/Pinhki-shiny-style.css`

If a future styling change touches shared tokens, reusable classes, shell layout, zoom, sidebars, dragbars, overlays, plot containers, dark mode, or cross-app UI behavior, ask whether it should update the global template.

Before source-file reads:

1. Read your own agent task file under `docs/agent-state/active-tasks/<agent>.md`.
2. Read `docs/agent-state/registry.md`.
3. Read `.project-index/README.md`.
4. Read `.project-index/agent_notes.md`.
5. Use `.project-index/files.csv`, `.project-index/functions.csv`, and `.project-index/shiny_pages.csv` to locate relevant files.
6. Then open only the relevant source files.

Project historical notes live in `.project-index/agent_notes.md`.
Current task state lives in `docs/agent-state/active-tasks/<agent>.md`.


## Agent Coordination

Before modifying files:

1. Read `docs/agent-state/registry.md`.
2. Read `docs/agent-state/branch-owners.md`.
3. Read your own task file under `docs/agent-state/active-tasks/<agent>.md`.
4. Do not overwrite another agent's task file.
5. If current branch owner differs from your agent identity, treat the branch as shared state.

Refresh the registry after changing an active task:

    ./scripts/update_agent_registry.sh


# AGENTS.md

## Project navigation rule

Before reading source files, read:
1. .project-index/README.md
2. .project-index/tree.txt
3. .project-index/functions.csv
4. .project-index/shiny_pages.csv
5. .project-index/agent_notes.md

Use the index to locate relevant files, then open only the minimum source files needed.

## Git safety rule

Before editing, run: git status --short

After editing, run:
- git diff --stat
- git diff

## R/Shiny project map

Known important locations:
- Contents/app.R
- Contents/global.R
- Contents/run.R
- Contents/R/
- Contents/www/

## Index rebuild

Run: ./scripts/build_project_index.sh

After a meaningful feature/fix checkpoint, run at most one compact maintenance pass: rebuild the project index only if files were added, removed, renamed, or navigation changed, and update `.project-index/agent_notes.md` only when a durable intention, reusable contract, or future-edit warning was learned.

---

## Token-Safe Rediscovery Prevention

Before reading source files or running broad inspection commands:

1. Check existing project state first:
   - `docs/agent-state/registry.md`
   - `docs/agent-state/active-tasks/`
   - `.project-index/agent_notes.md`
   - `.project-index/files.csv`
   - `.project-index/functions.csv`
   - `.project-index/shiny_pages.csv`

2. Use summaries and indexes before source reads.

3. Do not reread a file already inspected in the same session unless the user explicitly asks for a reread.

4. Prefer targeted lookup commands:
   - `rg`
   - `grep`
   - `sed -n 'start,endp'`
   - index CSV lookup

   over broad output commands:
   - `cat`
   - `ls -R`
   - `find` over the whole project
   - full-file reads

5. Every investigation should end by updating or creating a compact finding note under:
   - `docs/agent-state/findings/`

6. New sessions should read existing finding notes before reopening original source files.

7. If the task can be answered from agent-state or project-index, answer from those sources and avoid source-file reads.

---

## Fresh Change Priority

Before relying on project-index, findings, previous summaries, or prior session knowledge:

1. Check current repository changes:
   - `git status --short`
   - `git diff --stat`

2. For any modified file relevant to the task:
   - Inspect `git diff -- <file>` first.

3. Recent uncommitted changes take priority over:
   - findings
   - project-index
   - prior session notes
   - remembered structure

4. If the diff provides sufficient context, avoid reopening the entire file.

5. If additional context is required:
   - use targeted reads (`sed -n`, `rg`, `grep`)
   - avoid full-file reads unless necessary

Decision order:

Fresh changes
→ Git diff
→ Findings
→ Project index
→ Source file reads

---

## Active Task Handoff

At the start of each session or task continuation:

1. Read `docs/agent-state/registry.md`.
2. Read the current agent file under `docs/agent-state/active-tasks/`, for example:
   - `codex.md`
   - `chatgpt.md`
   - `cursor.md`
   - `gemini.md`

3. Continue from the recorded `Latest Progress` and `Next Action`.

4. Before ending a task, update the relevant active-task file with:
   - current objective
   - files in scope
   - latest progress
   - next action
   - known risks

5. Do not rediscover facts already captured in the active-task file.


---

## Command Output

Protect context usage. Any command with unknown or potentially large output must be byte-capped.

Default pattern:
COMMAND 2>&1 | head -c 4000

Use targeted inspection before broad output:
rg PATTERN
grep PATTERN FILE
sed -n 'START,ENDp' FILE
git diff -- FILE

Expand output only when the capped result shows that more context is required.

Avoid unbounded output from:
- find
- ls -R
- cat on unknown-size files
- full logs
- broad sqlite dumps
- broad git diffs


## Project Index and CocoIndex

Before broad file reads:
1. Read .project-index/README.md
2. Check .project-index/tree.txt and relevant CSV indexes
3. Use CocoIndex semantic search when available
4. Open only targeted files

Checkpoint-based project-index maintenance:

./scripts/build_project_index.sh

Protect context usage. Any command with unknown, noisy, or potentially large output must be byte-capped.

Default pattern:

COMMAND 2>&1 | head -c 4000
