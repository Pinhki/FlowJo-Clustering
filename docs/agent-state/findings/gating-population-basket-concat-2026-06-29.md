# Gating Population Basket + Concatenation - 2026-06-29

- Added durable population basket state under `shared$populations`.
- Gating can add the active gate or selected Navigator gates to the basket.
- Right sidebar now reviews/edits/removes basket rows.
- Concatenation now uses gate memberships from basket rows, supports all-events vs capped sampling, and exports original values plus numeric annotation channels as FCS + mapping CSV zip.
- Gate membership recompute now uses transformed coordinates so drawn gates, counts, basket rows, concat, and export agree.
- Checks passed: touched R parses, `node --check Contents/www/gating_canvas.js`, `scripts/check_gating_helpers.R`, WSP import smoke on the Cytek folder, one-file real-FCS export smoke, Shiny HTTP startup.
- Browser automation screenshot was skipped because Playwright browser binaries are not installed; HTTP startup passed.
