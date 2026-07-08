# Feature Plot Axis Scale Finding - 2026-06-21

- Existing FlowJo/logicle code was split between `Contents/R/global/flowjo_pipeline_helpers.R` (`flowjo_logicle_transform`) and `Contents/R/interface/page_feature.R` per-marker transform settings.
- The fake arrow axis came from `apply_theme(..., theme_choice = "UMAP Arrows")` in `Contents/R/global/utils_helpers.R`, which blanks normal axis text/ticks/lines.
- Current fix keeps that arrow path only when both Feature Plot axes are embedding axes; non-UMAP axes use real ggplot axes and tick marks.
- Legend and axis labels now use `flowjo_mfi_labels()` in `Contents/R/global/axis_scale_helpers.R` so representative large values render like `4 x 10^6` instead of `4e+06`.
- The Gating tab label points to the existing batch isolation/concatenation page; interactive drawn gates were not found or added.
