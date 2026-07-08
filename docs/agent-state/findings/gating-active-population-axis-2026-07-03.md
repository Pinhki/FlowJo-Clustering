# Gating Active Population + Axis Change Fix - 2026-07-03

## Finding

Manual Gating preview filtering used `active_parent_keep()` in `Contents/R/interface/page_gating.R`. That helper skipped saved parent gates whose saved X/Y axes did not match the currently displayed X/Y axes. After entering a child population and changing axes for the next gate, the helper returned `NULL`, so preview points were shown from the root population even though `shared$gating$active_gate_id()` still pointed at the child.

## Change

- Removed the current-axis filter inside `active_parent_keep()` so selected population membership follows the saved gate hierarchy regardless of the axes currently displayed.
- Renamed the left status box to `Viewing population / next gate parent` and added explicit switch text.
- Clicking a Gate Tree row now updates both `shared$gating$active_gate_id()` and `canvas_selected_gate()`.
- Active Gate Tree row now gets a visible `.active` border/inset highlight.

## Check

- `Rscript -e "invisible(parse(file='Contents/R/interface/page_gating.R')); cat('OK page_gating parse\n')"`
