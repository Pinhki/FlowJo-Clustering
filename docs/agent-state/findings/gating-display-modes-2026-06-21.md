# Gating Display Modes - 2026-06-21

- Implemented in `Contents/R/interface/page_gating.R`: dot, pseudocolor, contour, color by gate/population, and color by marker.
- Gate colors are stored on gate objects and edited with compact `colourpicker::colourInput()` rows, matching the Single Cell Data Browser metadata-level swatch pattern.
- Helper additions in `Contents/R/global/gating_helpers.R`: `flowjo_gate_palette()`, `flowjo_gate_color()`, and `flowjo_gate_plot_groups()`.
- Smoke rendered all five modes from reference FCS `Brain Dura SG and pLN_8-3 Brain IgRP-Kd.fcs` using a 50k-row preview.
- Remaining correctness issue: manual gate preview coordinates are transformed, but `shared_store.R` count recomputation still uses raw `e$df`. Counts need transform-aware recomputation or raw-space geometry storage.
