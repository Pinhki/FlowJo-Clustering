# Upload Landing + Defaults

- FCS upload now routes to `gating_tab` only when the newly loaded sample has no UMAP columns and the current workspace has no gate tree and no imported WSP population report.
- Gating page defaults now prefer `FSC-A` for X and `SSC-A` for Y when those channels are present, and the display mode starts on pseudocolor.
- Feature Plots now prefer `FSC-A`/`SSC-A` as the first axes, render with `coord_fixed(ratio = 1)`, and keep the gradient legend inside the plot panel.
