# Gating Active Plot Highlight - 2026-07-05

- Updated `.gate-history-plot.active` in `Contents/www/global.css` from an inset-only ring to the shared card-highlight style: outer accent ring, soft accent glow, and subtle card shadow.
- The color remains the current `var(--accent, #2563eb)`.
- Active plots keep the outer ring only; the base 1px border is transparent while active to avoid a double-border frame.
