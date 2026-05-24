# spinner_overlay.R
# Global busy overlay (single-cell style, without ASCII/meme media).

overlay_deps <- function() {
  shiny::tagList(
    shiny::singleton(
      shiny::tags$head(
        shiny::tags$style(shiny::HTML("
          .so-backdrop {
            position: fixed;
            inset: 0;
            z-index: 9999;
            display: none;
            align-items: center;
            justify-content: center;
            background: rgba(2, 6, 23, 0.55);
            backdrop-filter: blur(6px);
          }
          .so-card {
            width: min(92vw, 560px);
            min-height: 220px;
            padding: 22px 24px;
            border-radius: 24px;
            background: #ffffff;
            border: 1px solid #e2e8f0;
            box-shadow: 0 20px 54px rgba(15, 23, 42, 0.30);
            display: flex;
            flex-direction: column;
            align-items: center;
            justify-content: center;
            gap: 14px;
            color: #0f172a;
            text-align: center;
            font-family: system-ui, -apple-system, BlinkMacSystemFont, 'Segoe UI', sans-serif;
          }
          .so-spinner {
            width: 70px;
            height: 70px;
            border-radius: 50%;
            border: 6px solid rgba(148, 163, 184, 0.32);
            border-top-color: #3b82f6;
            animation: so-spin 0.9s linear infinite;
          }
          .so-message {
            font-size: 18px;
            font-weight: 600;
            color: #0f172a;
            line-height: 1.35;
          }
          .so-tip {
            font-size: 13px;
            color: #475569;
            line-height: 1.35;
            max-width: 460px;
          }
          body.app-dark-mode .so-card {
            background: #1f1f1f;
            border-color: #323232;
            color: #e5e7eb;
            box-shadow: 0 24px 58px rgba(0, 0, 0, 0.52);
          }
          body.app-dark-mode .so-message { color: #f3f4f6; }
          body.app-dark-mode .so-tip { color: #cbd5e1; }
          body.app-dark-mode .so-spinner {
            border-color: rgba(148, 163, 184, 0.26);
            border-top-color: #93c5fd;
          }
          @keyframes so-spin { to { transform: rotate(360deg); } }
        ")),
        shiny::tags$script(shiny::HTML("
          window.overlayShow = function(id, text, tip){
            var root = document.getElementById(id);
            if (!root) return;
            var msgEl = root.querySelector('.so-message');
            var tipEl = root.querySelector('.so-tip');
            if (msgEl && typeof text === 'string' && text.length) msgEl.textContent = text;
            if (tipEl) {
              if (typeof tip === 'string' && tip.length) {
                tipEl.textContent = tip;
                tipEl.style.display = '';
              } else {
                tipEl.style.display = 'none';
              }
            }
            root.style.display = 'flex';
          };
          window.overlayHide = function(id){
            var root = document.getElementById(id);
            if (!root) return;
            root.style.display = 'none';
          };
        "))
      )
    )
  )
}

overlay_ui <- function(id = "global_overlay", default_text = "Working...") {
  shiny::tags$div(
    id = id,
    class = "so-backdrop",
    shiny::tags$div(
      class = "so-card",
      shiny::tags$div(class = "so-spinner"),
      shiny::tags$div(class = "so-message", default_text),
      shiny::tags$div(class = "so-tip", style = "display:none;")
    )
  )
}

overlay_show <- function(text = NULL, id = "global_overlay", session = shiny::getDefaultReactiveDomain(), tip = NULL) {
  msg <- if (is.null(text) || !nzchar(text)) "Working..." else as.character(text)
  tip_txt <- if (is.null(tip) || !nzchar(tip)) "" else as.character(tip)
  if (!is.null(session)) {
    session$sendCustomMessage("overlay_show", list(id = id, text = msg, tip = tip_txt))
  } else {
    shinyjs::runjs(sprintf("window.overlayShow(%s,%s,%s);", jsonlite::toJSON(id, auto_unbox = TRUE), jsonlite::toJSON(msg, auto_unbox = TRUE), jsonlite::toJSON(tip_txt, auto_unbox = TRUE)))
  }
}

overlay_hide <- function(id = "global_overlay", session = shiny::getDefaultReactiveDomain()) {
  if (!is.null(session)) {
    session$sendCustomMessage("overlay_hide", list(id = id))
  } else {
    shinyjs::runjs(sprintf("window.overlayHide(%s);", jsonlite::toJSON(id, auto_unbox = TRUE)))
  }
}

overlay_hide_when_output_ready <- function(output_id, timeout_ms = 120000L, id = "global_overlay", session = shiny::getDefaultReactiveDomain()) {
  if (is.null(session) || is.null(output_id) || !nzchar(output_id)) return(invisible(NULL))
  session$sendCustomMessage("overlay_hide_when_output_ready", list(
    id = id,
    output_id = output_id,
    timeout_ms = as.integer(timeout_ms)
  ))
  invisible(NULL)
}
