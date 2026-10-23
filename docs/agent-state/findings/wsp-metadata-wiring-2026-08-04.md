# WSP metadata wiring finding — 2026-08-04

Fixture: `/Users/rongzhenyu/Library/CloudStorage/Box-Box/RY Result Box/Flow Data - Cytek/RY_Human_BET 095 7 Tetramers Dual Color 06-17-2026/RY_Human_BET 095 7 Tetramers Dual Color 06-17-2026.wsp`

- The importer now copies WSP workspace/sample/group/population metadata into the report, entries, gates, and group slots.
- Recorded `DataSet` URIs resolve all nine samples to existing `/Unmixed/*.fcs` files; recursive basename matching is only a fallback.
- Each entry keeps exact CytoML FlowJo biexponential settings (raw-channel and marker aliases), XML linear transform settings, transform metadata, and a mapped 26×26 compensation matrix.
- Each imported gate keeps its WSP path, count/annotation/graph/gate metadata, per-sample geometry, and exact per-sample membership vectors.
- Raw XML attribute maps for samples, groups, populations, gates, and GraphEnvironment are retained for future UI/export use rather than discarded during conversion.
- The full fixture smoke passed with 9 entries, 117 gates, 2 sample groups, 117 population paths, 58 transform keys, 26×26 compensation, all paths existing, all membership lists present, and zero numerical error against CytoML’s FlowJo transform.
- This WSP has no unique `gating:id` attribute on its population nodes; app-generated deterministic gate IDs and full population paths are used instead.
- Remaining external limitation: WSP export still depends on the local CytoML export environment (Docker/local `gs-to-flowjo` support); this wiring does not add that external runtime.
