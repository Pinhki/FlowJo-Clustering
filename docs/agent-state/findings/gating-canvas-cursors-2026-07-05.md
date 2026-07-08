# Gating Canvas Cursors - 2026-07-05

- Symptom: non-active plots still showed a crosshair, and draggable gate/label targets also looked like drawing space.
- Fix: `gating_canvas.js` now updates the overlay cursor from the actual interaction state.
- Behavior: read-only/inactive canvas shows pointer, active empty drawing space shows crosshair, draggable gate handles/bodies/labels show grab, and active drags show grabbing.
