# FlowJo -> Single-Cell Selector Mapping

| Legacy selector / structure | Unified selector / structure |
|---|---|
| `.topbar`, `.topbar-inner`, custom router nav | `page_navbar()` with Bootstrap `.navbar` contract + `navbar.css` |
| `div.content-wrap` page body | `.app-page` / `.plotting-page` wrappers inside navbar panels |
| inline footer in `app.R` | `wan_lab_footer()` with `.footer` / `.footer-inner` |
| top-action buttons (`open_settings`, `save_state`, `load_state_btn`) | right sidebar module controls (same behavior, shared sidebar container classes) |
| page root `tagList(...)` | page root `div.card-page-wrap(...)` |
| gate notice with router link | gate notice text aligned to tab-navigation model |
| `www/app.css` monolith | `www/global.css`, `www/navbar.css`, `www/footer.css`, `www/custom_bslib.css` |

## Notes
- Functional Shiny input IDs that drive server logic are preserved.
- New wrappers/classes were added to align with single-cell styling/script contract.

## Code Search

Use Semble before grep/read when locating relevant code.

Examples:
```bash
uvx --from "semble[mcp]" semble search "clustering" .
uvx --from "semble[mcp]" semble find-related path/to/file.R 42 .
```
## Command Output

Protect context usage. **Any command with unknown or potentially large output must be byte-capped.**

Default pattern:

```bash
COMMAND 2>&1 | head -c 4000
```

## Session Update Check

At the start of every session, read this file:

`/Users/rongzhenyu/Downloads/R_pipeline/Token Saving.md`

If it contains new instructions not already in this `agents.md`, copy those new instructions into this `agents.md`.
