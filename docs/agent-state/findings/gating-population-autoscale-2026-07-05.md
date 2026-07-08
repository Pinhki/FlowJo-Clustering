# Gating Population Autoscale - 2026-07-05

- Symptom: entering/showing a gated population could crop events because canvas ranges were based on full-sample values or stale saved channel ranges.
- Fix: main canvas payload ranges now use the visible parent-gated preview data (`pd$df`) with the existing 0-99.5% cutoff, visible gate geometry, and robust boxplot-style padding: `max(8% displayed range, 0.5 * IQR, 4% visible data span)`.
- Correction: autoscale persists as a channel-level auto range in `shared$transforms$auto_scales()`, not in `shared$transforms$scales()` because `scales()` represents manual/user axis customization.
- Manual axis modal remains the only path that should replace the manual channel range; canvas payloads use manual range first, then auto range, then a fresh robust 0.1%-99.9% computed range.
- User confirmed first-land preview scaling looks correct on 2026-07-05. Preserve the model: channel-level autoscale should include as many cells as practical while excluding outliers; FSC/SSC may anchor at zero, but marker biex/logicle axes should stay display-friendly and not be forced to raw zero when that compresses visible data. Histogram behavior remains unverified and should be tested against the current plot/view, including dynamic range updates when biex parameters change.
