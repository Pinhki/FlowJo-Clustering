# Gating Population Scale Preview Correction - 2026-07-05

User correction:
- Autoscale should be channel-level, not population-local.
- Do not make each visible parent/current population independently autoscale without asking first.

Wrong attempted change reverted:
- A population-local autoscale change in `Contents/R/interface/page_gating.R` was reverted.
- The unrelated canvas gutter change was also reverted.

Check:
- Ask the user for the exact intended channel-level scaling behavior before further edits.

Follow-up:
- User asked for a better range-scaling algorithm.
- Implemented a channel-level autoscale refinement in `Contents/R/interface/page_gating.R`: 0.1%-99.9% robust range, zero-preserving FSC/SSC lower bound, saved gate geometry inclusion, and 4% padding instead of the previous half-IQR padding that created large empty borders on broad FSC/SSC clouds.
