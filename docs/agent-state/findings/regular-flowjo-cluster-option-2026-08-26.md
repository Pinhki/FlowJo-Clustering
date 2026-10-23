# Regular FlowJo Cluster option — 2026-08-26

- The Cluster page now offers `FlowJo biexponential (regular FlowJo)` as a signal transformation choice while preserving estimated `flowCore` Logicle, arcsinh, and none.
- Generic fallback parameters are explicit: channel range 4096, maximum value 262144, pos 4.5, neg 0, and width basis -10.
- Analysis objects created from an imported WSP retain `wsp_axis_transforms`; the Cluster option uses valid per-marker FlowJo settings when enabled and falls back to the generic parameters otherwise.
- Run parameters persist the selected FlowJo settings. The heatmap and 3D upgrade reuse those parameters, so heatmap medians use the same event-level transform as clustering.
- Clustering scaling remains a separate control. No FlowSOM-specific 0–1024 normalization was added.
