# Release verification: 2026-10-23

This public preview was checked on macOS with R 4.4.3 and flowCore 2.18.0.

| Check | Observed result |
| --- | --- |
| Fresh GitHub checkout | App opened with no sample data using existing installed R dependencies. |
| Flag-sensitive startup | `Rscript --vanilla Contents/run.R` opened after fixing script-path discovery. |
| Public GvHD example | 1,006 of 3,420 events selected by the stated demonstration rectangle; independent calculation agrees. |
| `scripts/check_analysis_review.R` | Reviewed populations, unchanged basket, rejected empty/stale inputs, retained provenance and selected-cluster export checks passed. |
| `scripts/check_gating_plot_export.js` | Density sizing, SVG marks/gates and 2,048-pixel PNG checks passed. |

The startup fix locates `--file=` by name instead of assuming an argument position. The entrypoint now loads `here` after its installation loop, so a missing `here` package is not loaded before the bootstrap can install it.

The current [import-screen screenshot](images/flowjo-import.png) was captured from the fresh checkout on 2026-10-23. It contains no sample data.

This is a source preview, not an offline installer. Installation from an empty dependency library, Windows execution, a full imported-workspace round trip and every analysis module are not established by these checks.

## Maintenance evidence

The repository retains the original code history. Current reproducible maintenance evidence includes the entrypoint fix, reviewed-analysis-input checks, plot-export checks and the public example. These demonstrate code maintenance; they do not establish an external user count. No external adoption number is claimed.
