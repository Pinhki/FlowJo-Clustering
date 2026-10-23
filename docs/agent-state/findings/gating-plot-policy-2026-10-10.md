# Gating plot policy — 2026-10-10

- User explicitly retains edge accumulation and cached channel ranges. Neither behavior was changed.
- Linear axes use additive raw-value major ticks and fifth-step subticks; every rendered major retains a numeric label beyond 10^6. Tick positions use the active transform.
- Density uses a fixed 1024-square grid independent of panel pixel size; palette and event sampling policy remain unchanged.
- Gating preview frames are square, with no highlight shadow. Selected gates retain thicker strokes but no glow.
- Each main preview has SVG/PNG export and an Axis text checkbox. Off removes tick numbers/channel names only; axes, tick marks, saved gates, and gate labels remain. Exports exclude draft/edit handles. SVG records vector marks; PNG targets 2048 pixels on the longest side.
- Checks: scripts/check_wsp_file_resolution.R; scripts/check_gating_plot_export.js; JS syntax; diff whitespace.
- Existing Chrome verification could not run because mcporter is unavailable. The Node check uses DOM/canvas doubles; native PNG encoding and live appearance remain unverified.

- Gating sidebar clicks now dispatch through the existing population-aware sample switch. On a missing corresponding population they clear stale gate/template selection; preview initialization chooses an applicable gate. Other pages retain selection-only clicks.
- Fresh sessions default to all-sample background preloading of the current plot pair and counts. Explicit/restored scopes remain authoritative. Cache-disabled or not-yet-prepared contexts still use foreground preparation. Focused checks cover click dispatch, fallback selection clearing, and sample scope. Live switching latency remains unverified.
