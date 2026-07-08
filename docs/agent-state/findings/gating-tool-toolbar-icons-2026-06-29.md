# Gating Tool Toolbar Icons

- Moved the gate tool selector out of the left control column and into the preview canvas wrapper.
- The selector now renders as icon-only radio buttons in a top-right overlay, while preserving the existing `input$gate_type` server contract.
- No server-side gate behavior changed; this is a presentation-only UI tweak.
- 2026-07-03: Locked `.gate-toolbar` radio cells to fixed 42px icon hitboxes and fixed icon metrics so the selected pill/hidden radio input cannot shift toolbar icons.
