# Gating Group Management Finding - 2026-07-04

## Decision
- Manual sample groups are the V1 unit for applying gates across files.
- A group stores name, color, sample IDs, and a sync default; samples may belong to multiple groups.
- Applying gates to a group is explicit: Copy creates independent per-sample copies, Sync creates linked peer gates.

## Implementation Notes
- Synced peers are matched by `sync_group_id`, `applied_sample_id`, and stable lineage (`group_lineage_id` / `source_gate_id`).
- Gate-based sample switching is valid only for synced peer sets; local/root/unsynced gates do not switch files.
- Group subtree application must remap parent IDs per target sample so copied child gates point to copied parents.
- Editing synced peers propagates name/color/geometry. Editing unsynced group copies marks them `sample-modified`.

## Verification
- `Rscript scripts/check_gating_helpers.R` passes with synced-peer navigation coverage.
- Parse checks passed for `Contents/R/global/gating_helpers.R`, `Contents/R/global/shared_store.R`, `Contents/R/interface/page_workspace.R`, `Contents/R/interface/page_gating.R`, `Contents/global.R`, `Contents/app.R`, and `Contents/run.R`.

## Remaining Risk
- Needs browser smoke with real FCS files for Workspace group CRUD and Navigator drag/drop Copy vs Sync behavior.
