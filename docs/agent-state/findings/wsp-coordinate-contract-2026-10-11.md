# WSP coordinate contract

CytoML's executed GatingSet stores transformed events and gate coordinates. The app canvas expects compensated raw values and applies WSP transforms itself. Import must request `gs_pop_get_data(..., inverse.transform = TRUE)` and inverse-transform each gate with its own sample's transformation functions.

Real Cytek CD8/CD4 rectangle: XML X -2536 to 1516306, Y -3379 to 4184554; GatingSet X 8.364 to 236.984, Y 0.590 to 256.977. Treating the latter as raw caused double transformation.

Validation: `scripts/check_wsp_file_resolution.R` checks real transform-parameter round trips; `scripts/check_gating_plot_export.js` checks label repulsion and exports. Live visual verification requires restart and fresh WSP import. Existing imported state is not migrated.
