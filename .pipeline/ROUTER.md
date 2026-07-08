# Project Router

Search order for agents:

1. Read AGENTS.md
2. Read existing project router if present:
   - docs/project-index/project-map-router.md
   - docs/project-index/
   - ./shiny-index
   - scripts/query_project_index.R
3. Read .project-index/README.md when present
4. Use CocoIndex semantic search when available
5. Open targeted source files only

Rule:
Existing project-specific indexing is canonical. .pipeline_tools should adapt to it, not replace it.
