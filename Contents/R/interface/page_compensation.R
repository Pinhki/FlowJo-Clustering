compensation_page_ui <- function(id) {
  ns <- NS(id)
  div(class = "card-page-wrap",
    card_page_header("Compensation / Transforms", subtitle = "Manage spillover matrices, assign them to samples, and check the effect before preprocessing."),
    fluidRow(
      column(
        4,
        card_page_card("Matrix library",
          uiOutput(ns("library_summary")),
          selectInput(ns("matrix_id"), "Matrix", choices = NULL),
          div(class = "d-flex gap-2 flex-wrap",
            actionButton(ns("duplicate_matrix"), "Duplicate for editing", class = "btn btn-primary"),
            actionButton(ns("reset_matrix"), "Reset to original", class = "btn btn-outline-secondary"),
            actionButton(ns("delete_matrix"), "Delete", class = "btn btn-outline-danger")
          ),
          textInput(ns("matrix_name"), "Matrix name"),
          actionButton(ns("save_matrix_name"), "Save name", class = "btn btn-sm btn-outline-secondary")
        ),
        card_page_card("Sample assignment",
          uiOutput(ns("assignment_ui"))
        ),
        card_page_card("Preview controls",
          selectInput(ns("preview_x"), "X channel", choices = NULL),
          selectInput(ns("preview_y"), "Y channel", choices = NULL),
          div(class = "Pinhki-inline-note", "The preview uses a deterministic sample of events and refreshes shortly after an edit.")
        )
      ),
      column(
        8,
        card_page_card("Spillover matrix",
          div(class = "Pinhki-inline-note", "Edit numeric cells directly. Duplicate a WSP matrix first if you want to keep its original values. Updates are rejected if the matrix cannot be inverted safely."),
          DT::dataTableOutput(ns("spillover_table"))
        ),
        card_page_card("Compensation preview",
          plotOutput(ns("comp_preview"), height = "470px"),
          uiOutput(ns("preview_status"))
        )
      )
    )
  )
}

compensation_page_server <- function(id, shared) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    active_entry <- reactive({
      active_id <- shared$fcs$active_id()
      shared$fcs$registry()[[active_id %||% ""]]
    })
    matrix_library <- reactive(shared$compensation$library())
    matrix_ids <- reactive(names(matrix_library()))
    selected_matrix_id <- reactive({
      ids <- matrix_ids()
      if (!length(ids)) return(NULL)
      requested <- input$matrix_id %||% ""
      if (requested %in% ids) requested else ids[[1]]
    })
    selected_matrix <- reactive({
      id <- selected_matrix_id()
      if (is.null(id)) return(NULL)
      matrix_library()[[id]]
    })

    output$library_summary <- renderUI({
      library <- matrix_library()
      if (!length(library)) return(div(class = "Pinhki-inline-note", "No compensation matrix is available yet."))
      id <- selected_matrix_id()
      assigned <- sum(vapply(shared$fcs$registry(), function(entry) identical(entry$compensation_id %||% NULL, id), logical(1)))
      div(
        class = "Pinhki-inline-note",
        sprintf("%d matrix%s stored; %d sample%s use the selected matrix.", length(library), if (length(library) == 1L) "" else "es", assigned, if (assigned == 1L) "" else "s")
      )
    })

    observe({
      library <- matrix_library()
      ids <- names(library)
      choices <- if (length(ids)) {
        stats::setNames(ids, vapply(library, function(spec) {
          channels <- length(spec$channels %||% colnames(spec$matrix) %||% character())
          assigned <- sum(vapply(shared$fcs$registry(), function(entry) identical(entry$compensation_id %||% NULL, spec$id %||% ""), logical(1)))
          sprintf("%s (%d channels, %d samples)", spec$name %||% spec$id, channels, assigned)
        }, character(1)))
      } else character()
      current <- input$matrix_id %||% ""
      selected <- if (current %in% ids) current else if (length(ids)) ids[[1]] else character()
      updateSelectInput(session, "matrix_id", choices = choices, selected = selected)
    })

    observeEvent(selected_matrix_id(), {
      spec <- selected_matrix()
      updateTextInput(session, "matrix_name", value = spec$name %||% spec$id %||% "")
    }, ignoreInit = FALSE)

    output$assignment_ui <- renderUI({
      entry <- active_entry()
      library <- matrix_library()
      id <- selected_matrix_id()
      if (is.null(entry)) return(div(class = "Pinhki-inline-note", "Add and select an FCS sample first."))
      if (is.null(id) || is.null(library[[id]])) return(div(class = "Pinhki-inline-note", "Import or create a compensation matrix first."))
      registry <- shared$fcs$registry()
      sample_choices <- stats::setNames(names(registry), vapply(registry, function(item) item$name %||% item$id, character(1)))
      assigned <- names(registry)[vapply(registry, function(item) identical(item$compensation_id %||% NULL, id), logical(1))]
      selected <- unique(c(shared$fcs$active_id(), assigned))
      tagList(
        div(class = "Pinhki-inline-note", tags$b("Active sample: "), entry$name %||% entry$id),
        selectizeInput(
          ns("assign_samples"),
          "Assign selected matrix to samples",
          choices = sample_choices,
          selected = selected,
          multiple = TRUE,
          options = list(plugins = list("remove_button"), placeholder = "Choose samples")
        ),
        actionButton(ns("assign_matrix"), "Assign matrix", class = "btn btn-primary w-100"),
        div(class = "Pinhki-inline-note", "Assignment replaces the selected samples' active compensation matrix and invalidates stale preprocessing results.")
      )
    })

    matrix_table_data <- reactive({
      spec <- selected_matrix()
      if (is.null(spec) || is.null(spec$matrix)) return(data.frame(Channel = character(), stringsAsFactors = FALSE))
      matrix_value <- as.matrix(spec$matrix)
      data.frame(Channel = rownames(matrix_value) %||% colnames(matrix_value), as.data.frame(matrix_value, check.names = FALSE), check.names = FALSE, stringsAsFactors = FALSE)
    })

    output$spillover_table <- DT::renderDataTable({
      data <- matrix_table_data()
      if (!nrow(data)) return(NULL)
      numeric_columns <- if (ncol(data) > 1L) seq.int(2L, ncol(data)) else integer()
      widget <- DT::datatable(
        data,
        rownames = FALSE,
        editable = list(target = "cell"),
        selection = "none",
        options = list(
          dom = "t",
          paging = FALSE,
          ordering = FALSE,
          searching = FALSE,
          scrollX = TRUE,
          scrollY = "430px",
          autoWidth = FALSE,
          columnDefs = list(list(className = "dt-center", targets = 0))
        )
      )
      if (length(numeric_columns)) DT::formatRound(widget, columns = numeric_columns, digits = 5) else widget
    })

    table_proxy <- DT::dataTableProxy(ns("spillover_table"), session = session)
    refresh_matrix_table <- function() {
      try(DT::replaceData(table_proxy, matrix_table_data(), resetPaging = FALSE, rownames = FALSE), silent = TRUE)
      invisible(NULL)
    }

    observeEvent(input$spillover_table_cell_edit, {
      edit <- input$spillover_table_cell_edit
      id <- selected_matrix_id()
      spec <- selected_matrix()
      req(id, spec, edit$row, edit$col)
      row <- suppressWarnings(as.integer(edit$row))
      col <- suppressWarnings(as.integer(edit$col)) - 1L
      if (!is.finite(row) || !is.finite(col) || col < 1L || row < 1L || row > nrow(spec$matrix) || col > ncol(spec$matrix)) {
        refresh_matrix_table()
        return()
      }
      value <- suppressWarnings(as.numeric(edit$value))
      if (length(value) != 1L || !is.finite(value)) {
        showNotification("Enter one finite numeric compensation value.", type = "warning")
        refresh_matrix_table()
        return()
      }
      candidate <- spec$matrix
      candidate[row, col] <- value
      if (!isTRUE(shared$compensation$update(id, candidate, source = "manual"))) {
        showNotification("That edit would make the compensation matrix unsafe to invert.", type = "warning")
        refresh_matrix_table()
      }
    }, ignoreInit = TRUE)

    observeEvent(input$duplicate_matrix, {
      spec <- selected_matrix()
      req(spec)
      library <- matrix_library()
      suffix <- length(library) + 1L
      preferred <- paste0(spec$id, "_manual_", suffix)
      while (preferred %in% names(library)) {
        suffix <- suffix + 1L
        preferred <- paste0(spec$id, "_manual_", suffix)
      }
      new_id <- shared$compensation$add(spec$matrix, preferred_id = preferred, name = paste0(spec$name %||% spec$id, " (manual)"), source = "manual")
      if (!is.null(new_id)) {
        updateSelectInput(session, "matrix_id", selected = new_id)
        showNotification("Duplicated the matrix. You can now edit the copy without changing the original.", type = "message")
      }
    }, ignoreInit = TRUE)

    observeEvent(input$reset_matrix, {
      id <- selected_matrix_id()
      req(id)
      if (isTRUE(shared$compensation$reset(id))) showNotification("Matrix reset to its original values.", type = "message")
      else showNotification("The original matrix is not available for this entry.", type = "warning")
    }, ignoreInit = TRUE)

    observeEvent(input$save_matrix_name, {
      id <- selected_matrix_id()
      spec <- selected_matrix()
      req(id, spec)
      name <- trimws(input$matrix_name %||% "")
      if (!nzchar(name)) {
        showNotification("Enter a matrix name first.", type = "warning")
        return()
      }
      if (isTRUE(shared$compensation$update(id, spec$matrix, name = name, source = spec$source %||% "manual"))) showNotification("Matrix name saved.", type = "message")
    }, ignoreInit = TRUE)

    observeEvent(input$delete_matrix, {
      id <- selected_matrix_id()
      req(id)
      if (isTRUE(shared$compensation$remove(id))) showNotification("Matrix removed from the library.", type = "message")
      else showNotification("A matrix can only be deleted after it is unassigned from every sample.", type = "warning")
    }, ignoreInit = TRUE)

    observeEvent(input$assign_matrix, {
      id <- selected_matrix_id()
      req(id)
      sample_ids <- input$assign_samples %||% character()
      if (!length(sample_ids)) {
        showNotification("Choose at least one sample to assign.", type = "warning")
        return()
      }
      if (isTRUE(shared$compensation$assign(id, sample_ids))) showNotification("Compensation assignment updated.", type = "message")
    }, ignoreInit = TRUE)

    preview_channels <- reactive({
      entry <- active_entry()
      spec <- selected_matrix()
      if (is.null(entry) || is.null(spec$matrix)) return(character())
      raw <- entry$raw_exprs %||% entry$df
      intersect(colnames(raw), colnames(spec$matrix))
    })

    observe({
      channels <- preview_channels()
      current_x <- input$preview_x %||% ""
      current_y <- input$preview_y %||% ""
      selected_x <- if (length(channels) && current_x %in% channels) current_x else if (length(channels)) channels[[1]] else character()
      selected_y <- if (length(channels) && current_y %in% channels) current_y else if (length(channels)) channels[[min(2L, length(channels))]] else character()
      updateSelectInput(session, "preview_x", choices = channels, selected = selected_x)
      updateSelectInput(session, "preview_y", choices = channels, selected = selected_y)
    })

    preview_state <- reactive({
      entry <- active_entry()
      spec <- selected_matrix()
      channels <- preview_channels()
      if (is.null(entry) || is.null(spec) || length(channels) < 2L) return(list(error = "Choose a sample with at least two compensation channels."))
      x_channel <- input$preview_x %||% channels[[1]]
      y_channel <- input$preview_y %||% channels[[2]]
      if (!(x_channel %in% channels) || !(y_channel %in% channels) || identical(x_channel, y_channel)) return(list(error = "Choose two different channels for the preview."))
      raw <- as.data.frame(entry$raw_exprs %||% entry$df)
      keep_channels <- intersect(colnames(spec$matrix), colnames(raw))
      if (length(keep_channels) < 2L) return(list(error = "The selected matrix does not match this sample's channel names."))
      n_events <- nrow(raw)
      if (!n_events) return(list(error = "The active sample has no events to preview."))
      sample_size <- min(6000L, n_events)
      indices <- if (sample_size == n_events) seq_len(n_events) else unique(round(seq(1, n_events, length.out = sample_size)))
      sampled <- raw[indices, keep_channels, drop = FALSE]
      compensated <- tryCatch(flowjo_compensate_exprs(as.matrix(sampled), spec$matrix), error = function(e) NULL)
      if (is.null(compensated)) return(list(error = "The selected compensation matrix could not be applied to this sample."))
      list(
        sample_name = entry$name %||% entry$id,
        matrix_name = spec$name %||% spec$id,
        x = x_channel,
        y = y_channel,
        before = sampled,
        after = as.data.frame(compensated),
        n_events = n_events,
        n_preview = nrow(sampled),
        x_setting = (entry$wsp_axis_transforms %||% list())[[x_channel]] %||% NULL,
        y_setting = (entry$wsp_axis_transforms %||% list())[[y_channel]] %||% NULL
      )
    })
    preview_state_debounced <- shiny::debounce(preview_state, 180)

    plot_panel <- function(data, x, y, title, x_setting = NULL, y_setting = NULL) {
      x_values <- flowjo_transform_column(data[[x]], x_setting)
      y_values <- flowjo_transform_column(data[[y]], y_setting)
      keep <- is.finite(x_values) & is.finite(y_values)
      if (sum(keep) < 2L) {
        plot.new()
        text(0.5, 0.5, "Not enough finite events", cex = 0.9)
        return(invisible(NULL))
      }
      plot(x_values[keep], y_values[keep], pch = 16, cex = 0.35, col = grDevices::rgb(38, 99, 235, 95, maxColorValue = 255),
        xlab = x, ylab = y, main = title)
      invisible(NULL)
    }

    output$comp_preview <- renderPlot({
      state <- preview_state_debounced()
      op <- par(mfrow = c(1, 2), mar = c(4.1, 4.1, 2.3, 0.8))
      on.exit(par(op), add = TRUE)
      if (!is.null(state$error)) {
        plot.new()
        text(0.5, 0.5, state$error, cex = 0.95)
        return()
      }
      plot_panel(state$before, state$x, state$y, "Before compensation", state$x_setting, state$y_setting)
      plot_panel(state$after, state$x, state$y, "After compensation", state$x_setting, state$y_setting)
    }, res = 96)

    output$preview_status <- renderUI({
      state <- preview_state_debounced()
      if (!is.null(state$error)) return(div(class = "Pinhki-inline-note", state$error))
      div(class = "Pinhki-inline-note", sprintf("%s · %s · previewing %s of %s events.", state$sample_name, state$matrix_name, format(state$n_preview, big.mark = ","), format(state$n_events, big.mark = ",")))
    })
  })
}
