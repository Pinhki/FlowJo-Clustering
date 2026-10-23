# Workspace first-landing busy overlay

- Workspace navigation sends a page-loading message that shows the global busy overlay while the tree and details outputs are rendered.
- The Workspace navbar click also starts the overlay immediately; the server message remains the authoritative readiness wait.
- The browser-side wait is tokenized, so a previous Workspace wait cannot hide a later page's overlay after navigation changes.
- While the wait is active, the client reasserts the overlay if another route's late hide message removes it before Workspace output readiness.
- Gating preload progress is page-owned: its notification helper and queue/flush paths check `gating_tab_active()` before showing or updating the native progress bar.
- Keep background preparation notifications on Gating; do not globally remove Shiny notifications on every route change.
