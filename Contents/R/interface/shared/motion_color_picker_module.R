# Reusable Motion-inspired color picker module for Shiny.
# Exposes a stable parsed payload for downstream plotting modules.

motion_color_picker_dep <- function() {
  htmltools::htmlDependency(
    name = "motion-color-picker",
    version = "1.0.0",
    src = c(file = "www"),
    script = "js/motion-color-picker.vendored.min.js",
    stylesheet = "css/motion-color-picker.css"
  )
}

motion_color_picker_ui <- function(id, label = NULL, mode = c("single", "palette"), ...) {
  mode <- match.arg(mode)
  ns <- shiny::NS(id)
  dots <- list(...)

  initial_hex <- as.character(dots$initial_hex %||% "#3B82F6")
  initial_alpha <- as.numeric(dots$initial_alpha %||% 1)
  initial_alpha <- if (is.finite(initial_alpha)) max(0, min(1, initial_alpha)) else 1
  palette_n <- as.integer(dots$palette_n %||% 6L)
  if (is.na(palette_n) || palette_n < 2L) palette_n <- 6L
  interpolation <- as.character(dots$interpolation %||% "hsl")

  htmltools::tagList(
    motion_color_picker_dep(),
    htmltools::tags$div(
      id = ns("picker"),
      class = "motion-color-picker",
      `data-shiny-input` = ns("value"),
      `data-mode` = mode,
      `data-initial-hex` = initial_hex,
      `data-initial-alpha` = sprintf("%.4f", initial_alpha),
      `data-palette-n` = as.character(palette_n),
      `data-interpolation` = interpolation,
      `data-label` = as.character(label %||% "")
    )
  )
}

motion_color_picker_server <- function(id, ...) {
  shiny::moduleServer(id, function(input, output, session) {
    dots <- list(...)

    .clamp <- function(x, lo, hi) {
      x <- suppressWarnings(as.numeric(x))
      if (!is.finite(x)) return(lo)
      max(lo, min(hi, x))
    }

    .norm_hex <- function(x) {
      x <- toupper(trimws(as.character(x %||% "")))
      if (!nzchar(x)) return("#3B82F6")
      if (!startsWith(x, "#")) x <- paste0("#", x)
      if (!grepl("^#[0-9A-F]{6}$", x)) return("#3B82F6")
      x
    }

    .as_num_vec <- function(x, n = 3L) {
      if (is.null(x)) return(rep(NA_real_, n))
      out <- suppressWarnings(as.numeric(unlist(x, use.names = FALSE)))
      if (!length(out)) out <- rep(NA_real_, n)
      if (length(out) < n) out <- c(out, rep(NA_real_, n - length(out)))
      out[seq_len(n)]
    }

    .default_payload <- function() {
      list(
        hex = "#3B82F6",
        alpha = 1,
        rgb = c(59, 130, 246),
        hsl = c(217, 91, 60),
        palette = c("#1D4ED8", "#3B82F6", "#60A5FA", "#93C5FD", "#BFDBFE", "#DBEAFE"),
        meta = list(mode = "single", source = "init", timestamp_ms = as.numeric(Sys.time()) * 1000)
      )
    }

    .parse_payload <- function(raw) {
      parsed <- raw
      if (is.character(raw) && length(raw) == 1L) {
        parsed <- tryCatch(jsonlite::fromJSON(raw, simplifyVector = TRUE), error = function(e) NULL)
      }
      if (is.null(parsed) || !is.list(parsed)) return(.default_payload())

      meta <- parsed$meta
      if (is.null(meta) || !is.list(meta)) meta <- list()

      out <- list(
        hex = .norm_hex(parsed$hex),
        alpha = .clamp(parsed$alpha, 0, 1),
        rgb = .as_num_vec(parsed$rgb, 3L),
        hsl = .as_num_vec(parsed$hsl, 3L),
        palette = toupper(as.character(parsed$palette %||% character(0))),
        meta = list(
          mode = as.character(meta$mode %||% parsed$mode %||% "single"),
          source = as.character(meta$source %||% "interaction"),
          timestamp_ms = .clamp(meta$timestamp_ms %||% as.numeric(Sys.time()) * 1000, 0, Inf)
        )
      )

      if (!length(out$palette)) out$palette <- .default_payload()$palette
      out$palette <- out$palette[grepl("^#[0-9A-F]{6}$", out$palette)]
      if (!length(out$palette)) out$palette <- .default_payload()$palette

      out$rgb[1] <- .clamp(out$rgb[1], 0, 255)
      out$rgb[2] <- .clamp(out$rgb[2], 0, 255)
      out$rgb[3] <- .clamp(out$rgb[3], 0, 255)
      out$hsl[1] <- .clamp(out$hsl[1], 0, 360)
      out$hsl[2] <- .clamp(out$hsl[2], 0, 100)
      out$hsl[3] <- .clamp(out$hsl[3], 0, 100)

      out
    }

    value <- shiny::reactive({
      .parse_payload(input$value)
    })

    if (!is.null(dots$reset_trigger)) {
      shiny::observeEvent(dots$reset_trigger(), {
        session$sendCustomMessage("motionColorPicker:cmd", list(id = session$ns("picker"), cmd = "reset"))
      }, ignoreInit = TRUE)
    }

    if (!is.null(dots$randomize_trigger)) {
      shiny::observeEvent(dots$randomize_trigger(), {
        session$sendCustomMessage("motionColorPicker:cmd", list(id = session$ns("picker"), cmd = "randomize"))
      }, ignoreInit = TRUE)
    }

    value
  })
}
