# Gating Polygon + Histogram Preview - 2026-06-29

- Scope: `Contents/www/gating_canvas.js`, `Contents/R/interface/page_gating.R`.
- Polygon root cause: browser `dblclick` closed any draft polygon once it had at least 3 vertices, draft polygon drawing reused the saved-polygon path that visually closed 3+ point drafts, and R treated any 3+ point draft as saveable geometry.
- Fix: close polygon through one JS `closePolygon()` path, close on second click of a double-click or first-point click, draw draft polygons open, and require explicit polygon close before `Save Gate` can use polygon geometry.
- Histogram fix: axis modal preview is taller, uses a less-smoothed 4096-point density curve instead of bins, draws connected L axes, labels sparse x ticks in raw intensity values with plotmath powers such as `10^3`, adds 2-9-per-decade minor ticks, and removes the pink raw-0 guide.
- Histogram follow-up: preview height is now 460px, y-axis tick labels are hidden, the y label is vertical, scale choices stay inline with wider spacing, and min/max are controlled by a padded non-scientific range slider.
- Biex range fix: saved transformed display ranges are inverse-mapped back to raw values for the modal slider, and the preview x-limits live-update from that slider before Apply.
- Checks: `node --check Contents/www/gating_canvas.js`; `Rscript -e "invisible(parse(file='Contents/R/interface/page_gating.R')); cat('OK page_gating parse\n')"`.
