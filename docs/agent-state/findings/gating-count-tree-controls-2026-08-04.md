# Gating count-tree controls — 2026-08-04

- Population Counts renders a fold button only when a visible row has applicable child rows; collapsed IDs live in a page-local reactive set and descendant filtering leaves shared gate data untouched.
- Count-row color swatches now open `motion_color_picker_ui(..., mode = "single")` in a modal. Apply writes the normalized six-digit hex through `mark_gate_modified_or_sync()`, preserving the existing synced/sample-modified behavior.
- The count table remains non-paginated and uses `.Pinhki-bounded-scroll.Pinhki-scroll-fade` on its DataTables scroll body.
- Validation passed: `Rscript` parse, scoped `git diff --check`, and synthetic `shiny::testServer` control-event smoke.
