# Gating Canvas Labels - 2026-07-05

- Added canvas gate labels with gate name plus `% parent` from `active_sample_stats()` / `flowjo_gate_stats()`.
- Label position is stored as `gate$label_pos` in data coordinates and updated through `input$gate_canvas_label`.
- Label hit-testing runs before gate handles/body so dragging text moves only the label, not gate geometry.
- Synced group peers copy `label_pos` through the existing `sync_gate_peers()` path.
