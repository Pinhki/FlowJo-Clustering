# spinner_overlay.R
# Global busy overlay (single-cell style, without ASCII/meme media).

overlay_deps <- function() {
  shiny::tagList(
    shiny::singleton(
      shiny::tags$head(
        shiny::tags$script(shiny::HTML("
          window.overlayShow = function(id, text, tip, progress, progressLabel){
            var root = document.getElementById(id);
            if (!root) return;
            var msgEl = root.querySelector('.so-message');
            var tipEl = root.querySelector('.so-tip');
            var spinnerEl = root.querySelector('.so-spinner');
            var progressEl = root.querySelector('.so-progress');
            var progressFill = root.querySelector('.so-progress-fill');
            var progressText = root.querySelector('.so-progress-label');
            if (msgEl && typeof text === 'string' && text.length) msgEl.textContent = text;
            if (tipEl) {
              if (typeof tip === 'string' && tip.length) {
                tipEl.textContent = tip;
                tipEl.style.display = '';
              } else {
                tipEl.style.display = 'none';
              }
            }
            if (progressEl && progressFill) {
              if (progress === null || typeof progress === 'undefined' || progress === false) {
                progressEl.style.display = 'none';
                progressEl.classList.remove('is-indeterminate');
                if (spinnerEl) spinnerEl.style.display = '';
              } else {
                progressEl.style.display = '';
                if (progress === true || progress === 'indeterminate') {
                  progressEl.classList.add('is-indeterminate');
                  progressFill.style.width = '42%';
                  if (spinnerEl) spinnerEl.style.display = '';
                } else {
                  progressEl.classList.remove('is-indeterminate');
                  var pct = Math.max(0, Math.min(100, Number(progress) || 0));
                  progressFill.style.width = pct + '%';
                  if (spinnerEl) spinnerEl.style.display = 'none';
                }
              }
            }
            if (progressText) {
              if (typeof progressLabel === 'string' && progressLabel.length) {
                progressText.textContent = progressLabel;
                progressText.style.display = '';
              } else {
                progressText.style.display = 'none';
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
      shiny::tags$div(class = "so-tip Pinhki-busy-tip", style = "display:none;"),
      shiny::tags$div(
        class = "so-progress Pinhki-busy-progress",
        style = "display:none;",
        shiny::tags$div(class = "so-progress-fill Pinhki-busy-progress-fill")
      ),
      shiny::tags$div(class = "so-progress-label Pinhki-busy-progress-label", style = "display:none;")
    )
  )
}

overlay_show <- function(text = NULL, id = "global_overlay", session = shiny::getDefaultReactiveDomain(), tip = NULL, progress = NULL, progress_label = NULL) {
  msg <- if (is.null(text) || !nzchar(text)) "Working..." else as.character(text)
  tip_txt <- if (is.null(tip) || !nzchar(tip)) "" else as.character(tip)
  progress_value <- if (is.null(progress)) NULL else progress
  progress_txt <- if (is.null(progress_label) || !nzchar(progress_label)) "" else as.character(progress_label)
  if (!is.null(session)) {
    session$sendCustomMessage("overlay_show", list(id = id, text = msg, tip = tip_txt, progress = progress_value, progressLabel = progress_txt))
  } else {
    message("[overlay] show skipped because no Shiny session is active")
  }
}

overlay_hide <- function(id = "global_overlay", session = shiny::getDefaultReactiveDomain()) {
  if (!is.null(session)) {
    session$sendCustomMessage("overlay_hide", list(id = id))
  } else {
    message("[overlay] hide skipped because no Shiny session is active")
  }
}

overlay_hide_when_output_ready <- function(output_id, timeout_ms = 120000L, id = "global_overlay", ready_selector = NULL, ready_token = NULL, session = shiny::getDefaultReactiveDomain()) {
  if (is.null(session) || is.null(output_id) || !nzchar(output_id)) return(invisible(NULL))
  session$sendCustomMessage("overlay_hide_when_output_ready", list(
    id = id,
    output_id = output_id,
    ready_selector = ready_selector,
    ready_token = ready_token,
    timeout_ms = as.integer(timeout_ms)
  ))
  invisible(NULL)
}
