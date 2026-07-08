# Gating Sibling Navigation Disabled - 2026-07-05

- Symptom: sibling navigation buttons appeared active even when there was no sibling population to navigate to.
- Fix: `preview_plots` now computes sibling availability from `flowjo_gate_children()` for the active gate's parent and renders both sibling buttons with `disabled` when fewer than two siblings exist.
