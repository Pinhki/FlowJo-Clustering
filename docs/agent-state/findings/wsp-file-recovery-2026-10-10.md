# WSP file recovery — 2026-10-10

- Import / Export puts WSP results first and replaces raw metadata with import status and an optional source-file check.
- Missing FCS references open a recovery modal; Locate missing files reopens it after cancellation. Replacements require matching filenames and readable FCS content.
- Import is disabled until all references resolve. Server checks readiness again; running imports reject duplicate execution and selection changes. Success is green and disabled; failures allow retry.
- Exact resolved paths are used through temporary WSP/FCS symlink staging. Original files are untouched; app entries retain resolved source paths. Duplicate reference basenames fail explicitly rather than choosing one silently.
- Check: Rscript scripts/check_wsp_file_resolution.R covers missing references, invalid replacement rejection, valid replacement selection, staging, original-file preservation, and import button states.
- Real Cytek fixture imported through mapping: 9 samples, 130 population paths. CytoML warned of different gating tree structures; Gating-page behavior is not verified by this check.
- Remaining: live native picker/browser check, responsive layout check, and full app import/gating smoke.

- Deferred import fix: withReactiveDomain alone does not provide a reactive consumer. The callback now isolates all shared reactive reads/writes. Regression invokes the captured callback with no reactive context and verifies samples/transforms/gates/memberships/groups are applied; removing the fix reproduces the reported error.

- Gating foreground preview had the same deferred-context defect after successful import. Its callback now isolates readiness, payload preparation, shared gate reads, and preload state. The check exercises the actual parsed callback with minimal state, covers inactive-tab cancellation, and reproduces the error when isolation is removed. Full live Gating/large-workspace verification remains.

- Gating overlay completion now uses an explicit post-draw client acknowledgement with a per-send token and module namespace. Current acknowledgements dismiss the overlay; stale ones are ignored. Errors clear the pending token and hide immediately. R contract checks and the actual JavaScript acknowledgement block passed; live browser verification remains.
