# Compensation library/editor — 2026-08-04

- The shared store now owns a named `compensation_library`; matrix records contain ID, display name, current/original matrix, channel names, source, sample IDs, signature, and timestamps.
- FCS entries retain the assigned matrix ID and mapped marker-space matrix. WSP raw detector names remain in `compensation_raw_channels`; normal FCS channel/marker maps are normalized on registration.
- Editing is guarded by finite numeric values and matrix invertibility. Updates propagate to every assigned sample and clear stale preprocessing outputs. Duplicate, reset, assign, and delete operations are exposed through `shared$compensation`.
- The Compensation page uses the existing card/inline-note design, a DT cell editor, explicit sample assignment, and a 180 ms debounced deterministic 6,000-event before/after scatter preview.
- Workspace save/load includes the library. WSP export remaps marker-space matrices back to native GatingSet detector names (`Comp-*` where applicable) before calling FlowWorkspace/CytoML.
- Validation passed with the selected Cytek WSP: repeated 26×26 matrices deduplicated to one library record, a manual copy assigned to one sample and applied to 500 events, restored library count preserved, and export preparation accepted all 9 native matrices.
- External limitation remains: writing the final `.wsp` still requires the configured CytoML/Docker or local `gs-to-flowjo` runtime.
