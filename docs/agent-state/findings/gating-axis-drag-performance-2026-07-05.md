# Gating Axis And Drag Performance - 2026-07-05

- Symptom: after selecting/saving a gate, the left preview stopped following X/Y axis dropdown changes.
- Cause: left preview payload passed the active gate as `axis_gate_id`, forcing `preview_data_for_gate()` to reuse the selected gate's saved channels instead of `plot_axis_pair("left")`.
- Follow-up: when the left plot is the parent context for an active child gate, it must use the active child's saved X/Y channels so the parent plot shows the axis pair that gated out that child.
- Fix: left preview now uses its own plot axes again; overlays still filter out saved gates whose channels do not match the current view.
- Symptom: first gate drag/resize could snap back, and later adjustment felt delayed.
- Cause: pointerdown selection/activation could trigger a server payload while the browser was still dragging, replacing client-side geometry mid-drag.
- Fix: `gating_canvas.js` ignores incoming payload setup while `s.drag` is active, preserving immediate client-side geometry until the drag commit reaches Shiny.
