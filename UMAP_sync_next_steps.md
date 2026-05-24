# FlowJo UMAP Sync - Read-Only Handoff

Date: 2026-05-19
Scope lock: No file edits except `.md`.

## Current status (from code inspection)
- `Contents/R/interface/page_umap_flowsom.R` already contains:
  - 2D/3D toggle (`view_mode`)
  - 3D color selector (`color_by_ui`)
  - 2D fixed ratio (`coord_fixed(ratio = 1)`)
  - legend on top
  - reduced title size
  - 2D PDF/SVG + 3D bundle export wiring
- `Contents/www/global.css` already contains `card-subsection-title` token/class.
- `Contents/run.R` and `Contents/global.R` already include `plotly`, `htmlwidgets`, `svglite`.

## Remaining issues to fix (no edits applied yet)
1. Channel picker visual contract still not exact:
- Need gray rounded container around the full channel-picker region.
- Need white, borderless checkbox look.
- Need compact row height/text and keep `max-height: 50svh` scroll.

2. Subsection hierarchy still needs final parity check against scRNA:
- `Features & UMAP` / `FlowSOM` sizing, weight, spacing, and color need side-by-side confirmation.

3. Need runtime validation (not just parse):
- Busy overlay immediate display on Run.
- 3D mode render with metadata color fields.
- 2D/3D export workflows.

## Exact verification checklist before release
1. Visual checks
- Compare FlowJo vs scRNA for card title and subsection title metrics.
- Verify channel picker row height, checkbox appearance, and 50svh scroll behavior.

2. UMAP behavior checks
- Run UMAP+FlowSOM and confirm fixed 1:1 appearance (no squeeze).
- Confirm legend is top and wraps correctly for many clusters.
- Confirm preview title size is smaller and consistent.

3. 3D behavior checks
- Switch to 3D and verify interaction (rotate/zoom).
- Confirm default color by `FlowSOM_Cluster`.
- Confirm metadata fields appear only when present.

4. Export checks
- 2D PDF and SVG download and open.
- 3D ZIP contains HTML + `_files` and loads offline.

## Questions that need explicit decisions before further code edits
1. For the channel picker container gray, do you want exact same hex as scRNA panel background, or a local override for FlowJo only?
2. For checkbox style, should unchecked be fully white with no stroke, and checked use the current dark square with white check?
3. For subsection text, should we enforce one shared token source in scRNA and have FlowJo import/copy it exactly, or keep duplicate tokens synced manually?
4. Do you want me to only prepare markdown patch instructions for scRNA too, or request permission later to apply scRNA file edits directly?

## Proposed next step when you unlock edits
- Apply only the picker CSS deltas first.
- Run parse check + runtime smoke check.
- Then apply cross-app style token sync in scRNA.
