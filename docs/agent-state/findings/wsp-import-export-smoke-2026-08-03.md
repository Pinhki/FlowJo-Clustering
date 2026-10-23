# WSP import/export smoke — 2026-08-03

- Current fixture: `/Users/rongzhenyu/Library/CloudStorage/Box-Box/RY Result Box/Flow Data - Symphony/Brain/8.3 Brain/RY NY83 Tet Test OtherTetramers in different tissues/19-Jun-2026.wsp`.
- `flowjo_reference_inventory(dirname(wsp))` found 17 files: 1 WSP and 16 FCS.
- `flowjo_wsp_import_report(..., execute = FALSE)` passed: group `pLN`, 4 samples, 16 population paths, non-null GatingSet.
- `flowjo_wsp_import_report(..., execute = TRUE)` also passed with the same 4 samples and 16 populations.
- `flowWorkspace::save_gs()` failed for the `execute = FALSE` GatingSet but passed for the `execute = TRUE` GatingSet, staging 11 entries; this is the serialization prerequisite used by CytoML export.
- A Cytek concatenated WSP with two valid samples but empty gate trees failed with CytoML's default `include_empty_tree = FALSE`; enabling `include_empty_tree = TRUE` imports both samples and the root population successfully.
- The Cytek fixture's XML contains absolute `DataSet uri` paths for `concat_1_Tet Neg.fcs` and `concat_1_Tet Pos.fcs`; both resolve to root-level files. `Raw` and `Unmixed` are separate child directories, but those exact WSP sample names do not resolve to either child in this fixture.
- The selected full Cytek WSP records nine absolute `DataSet uri` paths under `Unmixed`; recursive import from the parent previously collided with duplicate Raw files. `flowjo_wsp_data_path()` now honors the valid URI directory first, and the WSP imports successfully with 9 samples and 118 populations.
- `flowjo_wsp_export_report()` reached `CytoML::gatingset_to_flowjo()` but returned `ok = FALSE` because CytoML requires the Docker daemon for export; Docker is not running on this machine. No output WSP was created.
- The Import / Export page now recursively inventories FCS/WSP files, lets the user choose a WSP by relative path, passes the selected reference root for nested FCS resolution, imports with `execute = TRUE`, and clears stale WSP state when the folder is rescanned.
- The import control is now WSP-only: users select the actual `.wsp` file, then import it; no folder selector or WSP dropdown is required for this workflow. The selected file's recorded dataset URIs remain the source of truth for FCS resolution.
- The reference table had still been fed by the old parent-folder inventory, so sibling WSPs appeared after selection. It now shows only the selected WSP and its recorded FCS references; the Cytek fixture passes with 1 WSP + 9 FCS rows, all under `Unmixed`.
- A runtime error remained because the server-side export control called `ns()` without a module namespace binding. Adding `ns <- session$ns` fixes it; focused `shiny::testServer()` checks pass for both empty and usable WSP report states.
- Full recheck passed: exact Cytek import resolves 9 URI references to `Unmixed`, loads 9 samples and 118 populations, the WSP-only table excludes sibling WSPs, and the module renders both import/export states. Export reaches CytoML but remains environment-blocked by the inactive Docker daemon.
- Important scope gap: this currently validates WSP parsing/reporting only. The imported GatingSet is not yet adapted into `shared$fcs` registry/active data or the app's editable gate tree/memberships, so other pages can remain empty after a successful report import.
- Resolved the integration gap: WSP import now populates shared FCS entries, app gates, exact per-sample memberships, and a sample group. The real page observer test passes with 9 samples and 117 non-root gates.
- Follow-up readiness fix: active Gating preparation is deferred until after the browser flushes a global busy overlay, and `active_sample_stats` is guarded until that preparation completes. Default preload scope is now current sample only, so a 9-sample WSP does not queue 24 contexts before the page becomes usable.
- WSP import no longer eagerly recomputes all imported gates across all samples; the adapter's exact memberships are reused by the Gating cache. The focused deferred Gating smoke passed, and all edited R files parse cleanly.

Next action: start Docker and use the new Export WSP control to complete the temporary-output round-trip; then reopen the exported WSP if the export succeeds.
