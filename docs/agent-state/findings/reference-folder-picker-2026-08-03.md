# Reference folder picker — 2026-08-03

- `shinyFiles` was not installed, so it was added to the existing CRAN/core UI package bootstrap in `Contents/global.R` and `Contents/run.R`.
- Import / Export now uses `shinyDirButton`/`shinyDirChoose` with named shortcuts for Home, existing Desktop/Documents/Downloads folders, Cloud Storage providers, iCloud Drive, and mounted volumes.
- The selected directory is copied into the visible `ref_path` field; selecting a new directory clears the prior inventory until Scan Reference runs.
- UI generation smoke passed and confirmed the `sF` resource path is registered.
