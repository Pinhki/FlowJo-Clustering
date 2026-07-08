# Gating Smart Naming - 2026-07-05

- Added smart default gate names in `Contents/R/interface/page_gating.R`.
- Rules: FSC/SSC scatter gates suggest `Non-debris`, FSC-A vs FSC-H/W pairs suggest `Singlets` regardless of midpoint signs, negative viability markers suggest `Live cells`, and general gates use marker +/- based on visible midpoint signs.
- Marker labels prefer channel-map marker names and strip simple `-A/-H/-W` suffixes.
- Duplicate sibling gate names now warn and block save/replace; names are not auto-numbered.
