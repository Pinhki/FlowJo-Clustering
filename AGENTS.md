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

After meaningful code changes, rebuild the index and update .project-index/agent_notes.md.
