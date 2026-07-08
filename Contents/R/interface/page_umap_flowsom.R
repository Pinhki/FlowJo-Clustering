umap_flowsom_page_ui <- function(id) {
  ns <- NS(id)
  div(class = "card-page-wrap analysis-cluster-page",
    card_page_header(
      "Cluster",
      subtitle = "Create named clustering runs from an analysis object, or import FlowSOM cluster labels."
    ),
    uiOutput(ns("object_header")),
    fluidRow(class = "analysis-cluster-layout",
      column(
        4,
        class = "analysis-controls-col",
        card_page_card(
          "Create Run",
          div(class = "app-tabs-wrap",
            navset_pill(
              id = ns("cluster_mode"),
              nav_panel(
                "Compute",
                textInput(ns("run_name"), "Run name", value = ""),
                div(class = "Pinhki-inline-note", tags$b("Name required. "), "Runs are saved under the selected analysis object and keep their parameters fixed."),
                uiOutput(ns("channels_picker")),
                numericInput(ns("seed"), "Seed", value = 42, min = 1, step = 1),
                numericInput(ns("neighbors"), "UMAP n_neighbors", value = 15, min = 2, step = 1),
                numericInput(ns("min_dist"), "UMAP min_dist", value = 0.3, min = 0, max = 1, step = 0.05),
                selectInput(ns("metric"), "UMAP metric", choices = c("euclidean", "manhattan", "cosine"), selected = "euclidean"),
                checkboxInput(ns("scale"), "Z-score scale channels", value = TRUE),
                tags$div(class = "card-subsection-title", "FlowSOM"),
                numericInput(ns("xdim"), "SOM xdim", value = 10, min = 3, step = 1),
                numericInput(ns("ydim"), "SOM ydim", value = 10, min = 3, step = 1),
                numericInput(ns("rlen"), "Training iterations (rlen)", value = 10, min = 1, step = 1),
                numericInput(ns("k"), "Meta-clusters (k)", value = 20, min = 2, step = 1),
                actionButton(ns("run"), "Run UMAP + FlowSOM", class = "btn btn-primary w-100")
              ),
              nav_panel(
                "Import",
                textInput(ns("import_run_name"), "Run name", value = ""),
                fileInput(ns("flowsom_file"), "Upload FlowSOM CSV", accept = ".csv"),
                div(class = "Pinhki-inline-note", tags$b("Imported run. "), "The CSV must have the same row count as the selected analysis object."),
                actionButton(ns("import_run"), "Import FlowSOM Run", class = "btn btn-primary w-100")
              )
            )
          )
        ),
        card_page_card("Object Notes", uiOutput(ns("object_notes")))
      ),
      column(
        8,
        class = "analysis-preview-col",
        card_page_card(
          "Preview & Export",
          uiOutput(ns("run_selector")),
          uiOutput(ns("run_notice")),
          radioButtons(ns("view_mode"), "View mode", choices = c("2D" = "2d", "3D interactive" = "3d"), selected = "2d", inline = TRUE),
          uiOutput(ns("axis_ui")),
          uiOutput(ns("color_by_ui")),
          uiOutput(ns("preview_plot_ui")),
          hr(),
          uiOutput(ns("export_ui"))
        )
      )
    )
  )
}

umap_flowsom_page_server <- function(id, shared) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    run_state <- reactiveVal("idle")

    active_object <- reactive(shared$analysis$active())

    active_run <- reactive({
      obj <- active_object()
      if (is.null(obj) || is.null(obj$active_run_id)) return(NULL)
      obj$runs[[obj$active_run_id]]
    })

    run_name_exists <- function(obj, name) {
      run_names <- vapply(obj$runs %||% list(), function(x) x$name %||% "", character(1))
      name %in% run_names
    }

    available_sources <- reactive({
      choices <- character(0)
      raw <- shared$df$raw()
      if (!is.null(raw) && ncol(raw) > 0) choices["FCS event table"] <- "fcs"
      batch <- shared$pipeline$concat_data()
      if (!is.null(batch) && ncol(batch) > 0) choices["Batch concatenation"] <- "batch"
      choices
    })

    show_create_object_modal <- function(source = NULL) {
      choices <- available_sources()
      selected <- source %||% unname(choices)[1]
      if (!length(choices)) {
        showModal(modalDialog(
          title = "Create Analysis Object",
          div(class = "Pinhki-inline-note", tags$b("No source data available. "), "Import FCS data or build a batch concatenation first."),
          footer = tagList(modalButton("Close"))
        ))
        return()
      }
      showModal(modalDialog(
        title = "Create Analysis Object",
        textInput(ns("new_object_name"), "Object name", value = ""),
        selectInput(ns("new_object_source"), "Source", choices = choices, selected = selected),
        div(class = "Pinhki-inline-note", tags$b("Snapshot saved. "), "The object keeps a full copy of the selected source data for reproducible named runs."),
        footer = tagList(modalButton("Cancel"), actionButton(ns("confirm_create_object"), "Create Object", class = "btn btn-primary")),
        easyClose = TRUE
      ))
    }

    output$object_header <- renderUI({
      objs <- shared$analysis$objects()
      if (!length(objs)) {
        return(div(class = "analysis-object-header",
          div(class = "Pinhki-inline-note", tags$b("Create an analysis object first. "), "Use Import or Batch Concatenation to prepare data, then create an object for clustering."),
          actionButton(ns("create_object"), "Create Analysis Object", class = "btn btn-primary")
        ))
      }
      choices <- stats::setNames(names(objs), vapply(objs, function(x) x$name %||% x$id, character(1)))
      div(class = "analysis-object-header",
        selectInput(ns("active_object"), NULL, choices = choices, selected = shared$analysis$active_id(), width = "280px"),
        actionButton(ns("create_object"), "New Object", class = "btn btn-outline-primary")
      )
    })

    observeEvent(input$create_object, {
      show_create_object_modal()
    })

    observeEvent(input$confirm_create_object, {
      name <- trimws(input$new_object_name %||% "")
      source <- input$new_object_source %||% ""
      if (!nzchar(name)) {
        showNotification("Object name is required.", type = "warning")
        return()
      }
      tryCatch({
        obj <- shared$analysis$create(name = name, source = source)
        removeModal()
        showNotification(sprintf("Created analysis object: %s", obj$name), type = "message")
      }, error = function(e) {
        showNotification(conditionMessage(e), type = "error", duration = 10)
      })
    })

    observeEvent(input$active_object, {
      req(input$active_object)
      shared$analysis$set_active(input$active_object)
    }, ignoreInit = TRUE)

    output$object_notes <- renderUI({
      obj <- active_object()
      if (is.null(obj)) {
        return(div(class = "Pinhki-inline-note", tags$b("No object selected. "), "Create or import an analysis object before clustering."))
      }
      tagList(
        div(class = "Pinhki-inline-note",
          tags$b(obj$source_badge %||% "Object"), " source, ",
          format(nrow(obj$data), big.mark = ","), " events, ",
          length(obj$markers %||% character(0)), " numeric markers."
        ),
        if (nzchar(obj$notes %||% "")) p(class = "control-note", obj$notes)
      )
    })

    output$channels_picker <- renderUI({
      obj <- active_object()
      if (is.null(obj)) {
        return(div(class = "Pinhki-inline-note", tags$b("No analysis object. "), "Create an object before selecting markers."))
      }
      markers <- obj$markers %||% character(0)
      selected <- intersect(obj$selected_markers %||% flowjo_default_cluster_markers(markers), markers)
      checkboxGroupInput(ns("channels"), "Markers for clustering", choices = markers, selected = selected, width = "100%")
    })

    output$run_selector <- renderUI({
      obj <- active_object()
      runs <- obj$runs %||% list()
      if (is.null(obj) || !length(runs)) return(NULL)
      choices <- stats::setNames(names(runs), vapply(runs, function(x) x$name %||% x$id, character(1)))
      div(class = "analysis-run-header",
        selectInput(ns("active_run"), "Selected run", choices = choices, selected = obj$active_run_id, width = "280px")
      )
    })

    observeEvent(input$active_run, {
      req(input$active_run)
      shared$analysis$set_active_run(run_id = input$active_run)
    }, ignoreInit = TRUE)

    output$run_notice <- renderUI({
      if (identical(run_state(), "running")) {
        return(div(class = "Pinhki-inline-note", tags$b("Running UMAP + FlowSOM. "), "The named run will appear in the preview when finished."))
      }
      if (is.null(active_object())) {
        return(div(class = "Pinhki-inline-note", tags$b("No analysis object. "), "Create or import an object before clustering."))
      }
      if (is.null(active_run())) {
        return(div(class = "Pinhki-inline-note", tags$b("No run yet. "), "Use Compute or Import to create the first named run for this object."))
      }
      NULL
    })

    run_df <- reactive({
      run <- active_run()
      if (is.null(run)) return(NULL)
      run$result$df
    })

    output$preview_plot_ui <- renderUI({
      df <- run_df()
      if (is.null(df)) return(NULL)
      if (identical(input$view_mode, "3d")) return(withSpinner(plotlyOutput(ns("plot3d"), height = "72vh")))
      withSpinner(plotOutput(ns("plot"), height = "72vh"))
    })

    observeEvent(input$run, {
      obj <- active_object()
      if (is.null(obj)) {
        showNotification("Create an analysis object before running clustering.", type = "error")
        return()
      }
      run_name <- trimws(input$run_name %||% "")
      if (!nzchar(run_name)) {
        showNotification("Run name is required.", type = "warning")
        return()
      }
      if (isTRUE(run_name_exists(obj, run_name))) {
        showNotification("Run name must be unique within this object.", type = "warning")
        return()
      }
      sel_channels <- intersect(input$channels %||% character(0), names(obj$data))
      if (length(sel_channels) < 2) {
        showNotification("Select at least 2 markers before running UMAP + FlowSOM.", type = "warning")
        return()
      }

      run_state("running")
      on.exit(run_state("idle"), add = TRUE)
      overlay_show("Running UMAP + FlowSOM...", tip = "This can take time on large analysis objects.")
      on.exit(overlay_hide(), add = TRUE)

      withProgress(message = "Computing UMAP + FlowSOM", value = 0, {
        df <- obj$data
        n_neighbors <- as.integer(input$neighbors %||% 15)
        min_dist <- as.numeric(input$min_dist %||% 0.3)
        seed_i <- as.integer(input$seed %||% 42)
        metric_i <- as.character(input$metric %||% "euclidean")
        xdim_i <- as.integer(input$xdim %||% 10)
        ydim_i <- as.integer(input$ydim %||% 10)
        rlen_i <- as.integer(input$rlen %||% 10)
        k_i <- as.integer(input$k %||% 20)
        mat <- as.matrix(df[, sel_channels, drop = FALSE])
        if (isTRUE(input$scale)) mat <- scale(mat)

        incProgress(0.35, detail = "Running UMAP")
        set.seed(seed_i)
        um <- uwot::umap(X = mat, n_neighbors = n_neighbors, min_dist = min_dist, metric = metric_i, verbose = FALSE)
        colnames(um) <- c("UMAP_1", "UMAP_2")

        incProgress(0.45, detail = "Running FlowSOM")
        fs_res <- flowsom_cluster(df = df, features = sel_channels, xdim = xdim_i, ydim = ydim_i, rlen = rlen_i, seed = seed_i, scale_data = isTRUE(input$scale), k = k_i)

        out <- cbind(df, as.data.frame(um))
        out$FlowSOM_Cluster <- factor(fs_res$cluster)
        params <- list(seed = seed_i, neighbors = n_neighbors, min_dist = min_dist, metric = metric_i, feats = sel_channels, xdim = xdim_i, ydim = ydim_i, rlen = rlen_i, k = fs_res$k_used %||% k_i, scale = isTRUE(input$scale))
        shared$analysis$add_run(name = run_name, method = "UMAP + FlowSOM", result = list(df = out), params = params)
      })
    })

    observeEvent(input$import_run, {
      obj <- active_object()
      req(obj, input$flowsom_file)
      run_name <- trimws(input$import_run_name %||% "")
      if (!nzchar(run_name)) {
        showNotification("Run name is required.", type = "warning")
        return()
      }
      if (isTRUE(run_name_exists(obj, run_name))) {
        showNotification("Run name must be unique within this object.", type = "warning")
        return()
      }
      fs_df <- read.csv(input$flowsom_file$datapath, check.names = FALSE)
      if (nrow(fs_df) != nrow(obj$data)) {
        showNotification(sprintf("Row mismatch: object=%d vs FlowSOM CSV=%d", nrow(obj$data), nrow(fs_df)), type = "error", duration = 10)
        return()
      }
      cluster_col <- colnames(fs_df)[grepl("FlowSOM|Cluster", colnames(fs_df), ignore.case = TRUE)][1] %||% colnames(fs_df)[2]
      req(cluster_col)
      out <- obj$data
      out$FlowSOM_Cluster <- as.factor(fs_df[[cluster_col]])
      shared$analysis$add_run(name = run_name, method = "Imported FlowSOM", result = list(df = out), params = list(cluster_col = cluster_col))
      showNotification("Imported FlowSOM run.", type = "message")
    })

    output$color_by_ui <- renderUI({
      if (!identical(input$view_mode, "3d")) return(NULL)
      df <- run_df()
      req(df)
      cols <- unique(c("FlowSOM_Cluster", names(df)[vapply(df, function(x) length(unique(as.character(x))) > 1 && length(unique(as.character(x))) <= 128, logical(1))]))
      selectInput(ns("color_by"), "3D color by", choices = cols, selected = "FlowSOM_Cluster")
    })

    output$axis_ui <- renderUI({
      df <- run_df()
      if (is.null(df) || !identical(input$view_mode, "2d")) return(NULL)
      numeric_cols <- names(df)[vapply(df, is.numeric, logical(1))]
      if (all(c("UMAP_1", "UMAP_2") %in% numeric_cols)) return(NULL)
      if (length(numeric_cols) < 2) {
        return(div(class = "Pinhki-inline-note", tags$b("No numeric axes available. "), "Imported runs need at least two numeric columns for 2D preview."))
      }
      fluidRow(
        column(6, selectInput(ns("x_axis"), "X axis", choices = numeric_cols, selected = numeric_cols[1])),
        column(6, selectInput(ns("y_axis"), "Y axis", choices = numeric_cols, selected = numeric_cols[2]))
      )
    })

    build_2d_plot <- reactive({
      df <- run_df()
      req(df, "FlowSOM_Cluster" %in% names(df))
      x_axis <- if ("UMAP_1" %in% names(df)) "UMAP_1" else input$x_axis
      y_axis <- if ("UMAP_2" %in% names(df)) "UMAP_2" else input$y_axis
      req(x_axis, y_axis, x_axis %in% names(df), y_axis %in% names(df))
      ggplot(df, aes(.data[[x_axis]], .data[[y_axis]], color = .data[["FlowSOM_Cluster"]])) +
        geom_point(size = shared$params$point_size(), alpha = shared$params$point_alpha(), shape = shared$params$point_shape(), stroke = shared$params$point_stroke()) +
        coord_fixed(ratio = 1) +
        theme_minimal(base_size = 11) +
        theme(legend.position = "top") +
        labs(x = x_axis, y = y_axis, color = "FlowSOM cluster", title = active_run()$name %||% "Cluster run")
    })

    output$plot <- renderPlot({
      req(identical(input$view_mode, "2d"))
      build_2d_plot()
    }, res = 120)

    output$plot3d <- renderPlotly({
      req(identical(input$view_mode, "3d"))
      df <- run_df()
      req(df, "UMAP_1" %in% names(df), "UMAP_2" %in% names(df), "FlowSOM_Cluster" %in% names(df))
      color_by <- input$color_by %||% "FlowSOM_Cluster"
      if (!color_by %in% names(df)) color_by <- "FlowSOM_Cluster"
      plotly::plot_ly(data = df, x = ~.data[["UMAP_1"]], y = ~.data[["UMAP_2"]], z = ~as.numeric(.data[["FlowSOM_Cluster"]]), color = ~as.factor(.data[[color_by]]), type = "scatter3d", mode = "markers", marker = list(size = 2.2, opacity = 0.78)) %>%
        plotly::layout(scene = list(xaxis = list(title = "UMAP 1"), yaxis = list(title = "UMAP 2"), zaxis = list(title = "Cluster Index"))) %>%
        plotly::config(displaylogo = FALSE)
    })

    output$export_ui <- renderUI({
      if (is.null(active_object())) {
        return(div(class = "Pinhki-inline-note", tags$b("Nothing to export. "), "Create or import an analysis object first."))
      }
      downloadButton(ns("download_bundle"), "Download Analysis Bundle", class = "btn btn-primary")
    })

    output$download_bundle <- downloadHandler(
      filename = function() paste0("analysis_object_", Sys.Date(), ".zip"),
      content = function(file) shared$analysis$export_bundle(file = file)
    )
  })
}
