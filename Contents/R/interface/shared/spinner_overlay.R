# spinner_overlay.R
# Global busy overlay (single-cell style, without ASCII/meme media).

overlay_deps <- function() {
  shiny::tagList(
    shiny::singleton(
      shiny::tags$head(
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
          document.addEventListener('change', function(event){
            var input = event.target;
            if (!input || input.type !== 'file' || !input.closest('.sidebar-fcs-upload')) return;
            if (!input.files || input.files.length === 0) return;
            window.overlayShow(
              'global_overlay',
              input.files.length === 1 ? 'Reading FCS file...' : 'Reading FCS files...',
              'Large cytometry files can take a moment to import.'
            );
          }, true);
        "))
      )
    )
  )
}

overlay_ui <- function(id = "global_overlay", default_text = "Working...") {
  shiny::tags$div(
    id = id,
    class = "so-backdrop Pinhki-busy-backdrop",
    shiny::tags$div(
      class = "so-card Pinhki-busy-card",
      shiny::tags$div(class = "so-spinner Pinhki-busy-spinner"),
      shiny::tags$div(class = "so-message Pinhki-busy-message", default_text),
      shiny::tags$div(class = "so-tip Pinhki-busy-tip", style = "display:none;")
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
