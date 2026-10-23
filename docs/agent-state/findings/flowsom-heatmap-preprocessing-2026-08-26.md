# FlowSOM and heatmap preprocessing trace — 2026-08-26

- Computed Cluster runs transform selected raw marker columns in `page_umap_flowsom.R` with the chosen Logicle, arcsinh, or none option, then apply the chosen z-score, robust, min-max 0–1, or no clustering scale.
- The same prepared matrix is passed to UMAP and `flowsom_cluster(..., scale_data = FALSE)`. Inside `flowsom_cluster()`, `FlowSOM::FlowSOM(..., scale = FALSE)` is also explicit. FlowSOM therefore performs the SOM/clustering, not the app's normalization or transformation.
- The cluster heatmap starts again from the saved run's original marker columns, reapplies the run's signal transformation, computes per-cluster marker medians, and only then applies the selected row/column z-score or min-max display standardization. This display standardization does not alter the saved clustering.
- No marker/MFI conversion to a 0–1024 range exists in the current clustering or heatmap path. The only fixed min-max clustering option is 0–1; heatmap min-max standardization is likewise 0–1 internally.
