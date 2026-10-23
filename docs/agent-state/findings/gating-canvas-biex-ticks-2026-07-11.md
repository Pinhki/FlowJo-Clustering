# Gating canvas biex ticks — 2026-07-11

- Root cause: canvas ticks were generated from the full sample data range, while the canvas rendered a separate manual/auto visible transformed range. Client filtering then removed mismatched major ticks and labels.
- Fix: `canvas_axis_ticks()` now inverse-transforms the visible canvas endpoints to establish the raw range, builds raw decade ticks, and transforms every tick through the exact active axis setting. Raw labels remain paired with those transformed positions.
- The client-side range filter was reverted; it now only rejects non-finite server tick values.
- Checks: `page_gating.R` parses and `node --check Contents/www/gating_canvas.js` passes.
