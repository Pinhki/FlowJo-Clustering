# Gating Navigator - 2026-06-29

- Scope: `Contents/R/interface/page_gating.R`, `Contents/R/global/gating_helpers.R`, `Contents/R/global/shared_store.R`, `Contents/www/gating_canvas.js`, `Contents/www/global.css`, `scripts/check_gating_helpers.R`.
- Added a FlowJo-style Gating `Navigator` pill tab beside `Draw`, with linked sample/group pane, hybrid indented gate table, quick sample/level buttons, Cmd/Shift gate multi-select, drag/drop copy, overwrite confirmation, group sync toggle, and compact cascading preview windows.
- Gate copies are normal gate objects with metadata: `source_gate_id`, `applied_sample_id`, and optional `sync_group_id`. Original gates remain global by default; copied gates only apply to their target sample.
- Group sync propagates geometry, name, and color across synced copies that share the same `sync_group_id` and source key.
- Checks: R parse for Gating page/helpers/store, `node --check Contents/www/gating_canvas.js`, `Rscript scripts/check_gating_helpers.R`, and local Shiny startup HTTP smoke on `127.0.0.1:3838`.
