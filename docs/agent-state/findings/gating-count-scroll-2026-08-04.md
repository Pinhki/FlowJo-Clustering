# Gating population count scroll — 2026-08-04

- Population Counts now disables DataTables paging and length controls and uses a responsive bounded scroll body.
- The body receives `Pinhki-bounded-scroll` and `Pinhki-scroll-fade`; the existing `adaptive_plot_workspace.js` scroll-state updater supplies the edge classes used by the transparent global fade.
- R parse and scoped `git diff --check` pass.
