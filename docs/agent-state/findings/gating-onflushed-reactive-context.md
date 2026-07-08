# Gating onFlushed Reactive Context

Date: 2026-07-04

Finding:
`session$onFlushed()` callbacks must not build gating canvas payloads by reading reactive values directly. Shiny can run those callbacks outside a reactive consumer, causing:

`Operation not allowed without an active reactive context`

Fix pattern:
Build payload lists inside the observer or event handler, then let `onFlushed()` only send the prebuilt messages.

Touched area:
- `Contents/R/interface/page_gating.R`

Verification:
- `Rscript -e "invisible(parse(...)); cat('OK parse')"` passed for touched UI files.
- `Rscript scripts/check_gating_helpers.R` passed.
- Direct Shiny smoke served HTML on `127.0.0.1:7792`.
- `Rscript Contents/run.R` reached `Listening on http://127.0.0.1:4961` with no reactive-context fatal error.
