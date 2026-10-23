cluster_default_name <- function(kind) {
  sprintf("%s — %s", kind, format(Sys.time(), "%Y-%m-%d %H-%M-%S"))
}

cluster_available_name <- function(kind, existing = character(0)) {
  base <- cluster_default_name(kind)
  if (!base %in% existing) return(base)
  i <- 2L
  while (sprintf("%s (%d)", base, i) %in% existing) i <- i + 1L
  sprintf("%s (%d)", base, i)
}

cluster_natural_order <- function(x) {
  numeric_x <- suppressWarnings(as.numeric(as.character(x)))
  if (length(x) && all(is.finite(numeric_x))) return(order(numeric_x, na.last = TRUE))
  order(tolower(as.character(x)), na.last = TRUE)
}

cluster_color_value <- function(colors, key, fallback = "#808080") {
  if (is.null(colors) || !length(key) || is.na(key) || !key %in% names(colors)) return(fallback)
  value <- unname(colors[[key]])
  if (!length(value) || is.na(value) || !nzchar(value)) fallback else value
}

cluster_scale_matrix <- function(mat, method = "zscore") {
  method <- method %||% "zscore"
  if (identical(method, "none")) return(mat)
  center <- if (identical(method, "robust")) apply(mat, 2, stats::median, na.rm = TRUE) else colMeans(mat, na.rm = TRUE)
  spread <- switch(method,
    robust = apply(mat, 2, stats::mad, na.rm = TRUE),
    minmax = apply(mat, 2, function(x) diff(range(x, finite = TRUE))),
    apply(mat, 2, stats::sd, na.rm = TRUE))
  if (identical(method, "minmax")) center <- apply(mat, 2, min, na.rm = TRUE)
  spread[!is.finite(spread) | spread == 0] <- 1
  sweep(sweep(mat, 2, center, "-"), 2, spread, "/")
}

cluster_transform_matrix <- function(mat, method = "logicle", cofactor = 150,
                                     logicle_auto = TRUE, t = 262144, w = 0.5, m = 4.5, a = 0,
                                     flowjo_channel_range = 4096, flowjo_max_value = 262144,
                                     flowjo_pos = 4.5, flowjo_neg = 0, flowjo_width_basis = -10,
                                     flowjo_settings = list(), flowjo_use_wsp = TRUE) {
  method <- tolower(method %||% "logicle")
  if (method %in% c("flowjo", "flowjo_biexp")) {
    default_setting <- flowjo_default_biexp_setting(
      channel_range = flowjo_channel_range, max_value = flowjo_max_value,
      pos = flowjo_pos, neg = flowjo_neg, width_basis = flowjo_width_basis
    )
    if (is.null(default_setting)) stop("FlowJo biexponential parameters are invalid.")
    out <- mat
    for (j in seq_len(ncol(mat))) {
      marker <- colnames(mat)[[j]] %||% ""
      setting <- if (isTRUE(flowjo_use_wsp)) flowjo_settings[[marker]] else NULL
      if (is.null(setting) || !identical(setting$type %||% NULL, "biex") ||
          !identical(setting$engine %||% NULL, "flowjo")) {
        setting <- default_setting
      }
      values <- flowjo_transform_column(mat[, j], setting)
      if (is.null(values) || length(values) != nrow(mat)) {
        stop(sprintf("FlowJo biexponential transform failed for marker '%s'.", marker))
      }
      out[, j] <- values
    }
    return(out)
  }
  if (identical(method, "none")) return(mat)
  if (identical(method, "arcsinh")) return(asinh(mat / max(as.numeric(cofactor), .Machine$double.eps)))
  if (identical(method, "cytof")) stop("The CyTOF preset is reserved for a future shared workflow.")
  if (isTRUE(logicle_auto)) {
    transformed <- flowjo_logicle_transform(as.data.frame(mat))$data
    return(as.matrix(transformed[, colnames(mat), drop = FALSE]))
  }
  tf <- flowCore::logicleTransform(t = as.numeric(t), w = as.numeric(w), m = as.numeric(m), a = as.numeric(a))
  apply(mat, 2, tf@.Data)
}

umap_flowsom_page_ui <- function(id) {
  ns <- NS(id)
  div(class = "card-page-wrap analysis-cluster-page plotting-page",
    fluidRow(class = "analysis-cluster-title-row",
      column(8, card_page_header("Cluster", subtitle = "Create named clustering runs from an analysis object, or import FlowSOM cluster labels.")),
      column(4, class = "analysis-object-selector-wrap", uiOutput(ns("object_header")))
    ),
    fluidRow(class = "analysis-cluster-layout Pinhki-plot-workspace",
      column(
        3,
        class = "analysis-controls-col plotting-left-panel Pinhki-plot-inputs",
        card_page_card(
          "Create Run",
          div(class = "app-tabs-wrap",
            navset_pill(
              id = ns("cluster_mode"),
              nav_panel(
                "Compute",
                textInput(ns("run_name"), "Run name", value = cluster_default_name("Cluster Run")),
                div(class = "Pinhki-inline-note", tags$b("Name required. "), "Runs are saved under the selected analysis object and keep their parameters fixed."),
                uiOutput(ns("channels_picker")),
                tags$div(class = "card-subsection-title", "Data transformation"),
                selectInput(ns("transform_method"), "Transformation", choices = c(
                  "FlowJo biexponential (regular FlowJo)" = "flowjo_biexp",
                  "Estimated Logicle (flowCore)" = "logicle",
                  "Arcsinh" = "arcsinh", "None" = "none",
                  "CyTOF preset (coming later)" = "cytof"
                ), selected = "logicle"),
                div(class = "Pinhki-control-note", tags$b("FlowJo biexponential. "), "Standard FlowJo transform. Keeps negatives and compresses highs. This is a signal transform, not statistical normalization. Logicle is the existing flowCore option; arcsinh is for comparison; none keeps stored values."),
                conditionalPanel(sprintf("input['%s'] == 'flowjo_biexp'", ns("transform_method")),
                  checkboxInput(ns("flowjo_use_wsp"), "Use imported WSP channel settings when available", value = TRUE),
                  div(class = "Pinhki-control-note", tags$b("Settings. "), "Checked: WSP values win per marker. Unchecked: fields below apply to all markers. Missing WSP values use fields below. Run settings are reused by the heatmap."),
                  div(class = "cluster-logicle-advanced",
                    numericInput(ns("flowjo_channel_range"), "Channel range", value = 4096, min = 1, step = 1),
                    numericInput(ns("flowjo_max_value"), "Maximum value", value = 262144, min = 1, step = 1),
                    numericInput(ns("flowjo_pos"), "Positive decades (pos)", value = 4.5, min = 0.1, step = 0.1),
                    numericInput(ns("flowjo_neg"), "Negative range (neg)", value = 0, min = 0, step = 0.1),
                    numericInput(ns("flowjo_width_basis"), "Width basis", value = -10, min = -1000000, max = -0.000001, step = 1)
                  )),
                conditionalPanel(sprintf("input['%s'] == 'arcsinh'", ns("transform_method")),
                  numericInput(ns("arcsinh_cofactor"), "Arcsinh cofactor", value = 150, min = 0.001, step = 1),
                  div(class = "Pinhki-control-note", "Larger cofactors compress low signals less. Cofactor 150 is a common flow-cytometry comparison setting, not a FlowJo UMAP option.")),
                conditionalPanel(sprintf("input['%s'] == 'logicle'", ns("transform_method")),
                  checkboxInput(ns("logicle_auto"), "Estimate Logicle parameters per marker", value = TRUE),
                  div(class = "Pinhki-control-note", "Recommended: estimate each marker from its observed distribution."),
                  conditionalPanel(sprintf("!input['%s']", ns("logicle_auto")),
                    div(class = "cluster-logicle-advanced",
                      numericInput(ns("logicle_t"), "T (top range)", 262144, min = 1),
                      numericInput(ns("logicle_w"), "W (linear width)", 0.5, min = 0, step = 0.1),
                      numericInput(ns("logicle_m"), "M (decades)", 4.5, min = 0.1, step = 0.1),
                      numericInput(ns("logicle_a"), "A (extra negative range)", 0, min = 0, step = 0.1)
                    ),
                    div(class = "Pinhki-control-note", "Advanced shared override: T sets the top data range, W the near-zero linear region, M the displayed logarithmic span, and A extra negative range."))),
                tags$div(class = "card-subsection-title", "Clustering scaling"),
                selectInput(ns("scale_method"), "Scaling after transformation", choices = c(
                  "Z-score (recommended)" = "zscore", "Robust median / MAD" = "robust",
                  "Min–max (0–1)" = "minmax", "None" = "none"
                ), selected = "zscore"),
                div(class = "Pinhki-control-note", tags$ul(
                  tags$li(tags$b("Z-score: "), "balances marker means and variation; the strongest general default for distance-based clustering."),
                  tags$li(tags$b("Robust median/MAD: "), "similar balancing but less influenced by rare extreme events."),
                  tags$li(tags$b("Min–max: "), "forces every marker into 0–1, but can be sensitive to outliers."),
                  tags$li(tags$b("None: "), "preserves transformed magnitudes, so high-range markers can dominate distances.")
                )),
                numericInput(ns("seed"), "Seed", value = 42, min = 1, step = 1),
                numericInput(ns("neighbors"), "UMAP n_neighbors", value = 15, min = 2, step = 1),
                numericInput(ns("min_dist"), "UMAP min_dist", value = 0.3, min = 0, max = 1, step = 0.05),
                selectInput(ns("metric"), "UMAP metric", choices = c("euclidean", "manhattan", "cosine"), selected = "euclidean"),
                tags$div(class = "card-subsection-title", "FlowSOM"),
                numericInput(ns("xdim"), "SOM xdim", value = 10, min = 3, step = 1),
                numericInput(ns("ydim"), "SOM ydim", value = 10, min = 3, step = 1),
                numericInput(ns("rlen"), "Training iterations (rlen)", value = 10, min = 1, step = 1),
                numericInput(ns("k"), "Meta-clusters (k)", value = 20, min = 2, step = 1),
                actionButton(ns("run"), "Run UMAP + FlowSOM", class = "btn btn-primary w-100")
              ),
              nav_panel(
                "Import",
                textInput(ns("import_run_name"), "Run name", value = cluster_default_name("Imported Run")),
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
        9,
        class = "analysis-preview-col plot_area_card_style Pinhki-plot-preview",
        card_page_card("UMAP Preview",
          div(class = "cluster-preview-card-top",
            uiOutput(ns("run_selector")),
          actionButton(ns("update_preview"), "Update plot", icon = icon("sync-alt"), class = "btn btn-primary")
          ),
          div(class = "Pinhki-plot-controls-wrap cluster-preview-controls", uiOutput(ns("display_controls"))),
          uiOutput(ns("run_notice")),
          div(class = "Pinhki-plot-body-reserve", uiOutput(ns("preview_plot_ui")))
        ),
        card_page_card("Cluster Composition",
          div(class = "Pinhki-plot-controls-wrap cluster-composition-controls", uiOutput(ns("composition_controls"))),
          uiOutput(ns("composition_notice")),
          plotOutput(ns("composition_plot"), height = "460px")
        ),
        card_page_card("Cluster × Marker Heatmap",
          div(class = "Pinhki-plot-controls-wrap cluster-heat-controls",
            selectInput(ns("heat_normalize"), "Standardization", choices = c(
              "Row z-score" = "row_z", "Row min–max" = "row_minmax",
              "Column z-score" = "col_z", "Column min–max" = "col_minmax", "None / transformed median" = "none"
            ), selected = "row_z"),
            div(class = "Pinhki-control-note heat-normalize-note cluster-heat-normalize-note",
              div(class = "cluster-heat-normalize-item", tags$b("Row z-score: "), "compare markers within each cluster; normalize each cluster."),
              div(class = "cluster-heat-normalize-item", tags$b("Row min–max: "), "compare markers within each cluster; scale each cluster."),
              div(class = "cluster-heat-normalize-item", tags$b("Column z-score: "), "compare clusters within each marker; normalize each marker."),
              div(class = "cluster-heat-normalize-item", tags$b("Column min–max: "), "compare clusters within each marker; scale each marker."),
              div(class = "cluster-heat-normalize-item", tags$b("None / transformed median: "), "show each cluster’s median marker value after the saved run transformation.")),
            actionButton(ns("heat_reset_order"), "Reset order", class = "btn btn-sm btn-outline-secondary")
          ),
          uiOutput(ns("cluster_heatmap")),
          div(class = "Pinhki-control-note heat-normalize-note cluster-heat-interpretation-note",
            tags$b("How to interpret this heatmap"),
            tags$ul(
              tags$li(tags$b("Biological interpretation: "), "use transformed marker signal before clustering scaling. Transformation controls extreme values while preserving relative intensity."),
              tags$li(tags$b("Clustering scaling: "), "makes markers numerically comparable for UMAP and FlowSOM. A scaled value is relative to this dataset, not absolute expression."),
              tags$li(tags$b("Heatmap standardization: "), "row and column options are display-only. They emphasize different patterns without changing the saved clustering result."),
              tags$li(tags$b("Clustering-input view: "), "use this advanced view to audit exactly what the algorithms received.")))
        ),
        card_page_card("Export", uiOutput(ns("export_ui")))
      )
    )
  )
}

umap_flowsom_page_server <- function(id, shared) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    run_state <- reactiveVal("idle")
    cluster_colors <- reactiveVal(list())
    color_presets <- reactiveVal(list())
    active_palette <- reactiveVal("Sci Publication Color palette")
    editing_category <- reactiveVal(NULL)
    picker_value <- motion_color_picker_server("cluster_motion_picker")
    applied_structure <- reactiveVal(list())
    marker_table_proxy <- DT::dataTableProxy("marker_table", session = session)

    active_object <- reactive(shared$analysis$active())

    active_run <- reactive({
      obj <- active_object()
      if (is.null(obj) || is.null(obj$active_run_id)) return(NULL)
      obj$runs[[obj$active_run_id]]
    })
    active_run_key <- reactive({
      obj <- active_object()
      paste(obj$id %||% "", obj$active_run_id %||% "", sep = "::")
    })

    run_name_exists <- function(obj, name) {
      run_names <- vapply(obj$runs %||% list(), function(x) x$name %||% "", character(1))
      name %in% run_names
    }

    available_sources <- reactive({
      choices <- c("Review population basket" = "basket")
      raw <- shared$df$raw()
      if (!is.null(raw) && ncol(raw) > 0) choices["Current sample — Root / all events"] <- "fcs"
      choices
    })

    show_create_object_modal <- function(source = NULL) {
      choices <- available_sources()
      selected <- source %||% unname(choices)[1]
      if (!length(choices)) {
        showModal(modalDialog(
          title = "Create Analysis Object",
          div(class = "Pinhki-inline-note", tags$b("No source data available. "), "Import FCS data, then add gated populations to the basket."),
          footer = tagList(modalButton("Close"))
        ))
        return()
      }
      showModal(modalDialog(
        title = "Create Analysis Object",
        textInput(ns("new_object_name"), "Object name", value = cluster_available_name("Analysis Object", vapply(shared$analysis$objects(), function(x) x$name %||% "", character(1)))),
        selectInput(ns("new_object_source"), "Source", choices = choices, selected = selected),
        uiOutput(ns("object_population_review")),
        div(class = "Pinhki-inline-note", "Creation saves a snapshot of only the reviewed inputs. Basket contents remain unchanged."),
        footer = tagList(modalButton("Cancel"), uiOutput(ns("object_create_control"), inline = TRUE)),
        easyClose = TRUE
      ))
    }

    output$object_population_review <- renderUI({
      if (identical(input$new_object_source, "fcs")) {
        entry <- shared$fcs$registry()[[shared$fcs$active_id()]]
        return(div(tags$b(entry$name %||% "Current sample"), p("Root / all events"),
          p(sprintf("%s cells", format(nrow(shared$df$raw()), big.mark = ",")))))
      }
      items <- shared$populations$items()
      if (!length(items)) return(tagList(p("No populations selected. Add gated populations to the basket."),
        actionButton(ns("review_go_gating"), "Go to Gating")))
      tbl <- shared$populations$table()
      labels <- vapply(items, function(item) {
        row <- tbl[tbl$id == item$id, , drop = FALSE]
        path <- flowjo_gate_path(shared$gating$gates(), item$gate_id)
        sprintf("%s | %s | %s cells", row$sample_name[[1]], path, format(row$events[[1]], big.mark = ","))
      }, character(1))
      checkboxGroupInput(ns("object_population_ids"), "Include in this object",
        choices = stats::setNames(names(items), labels),
        selected = names(items)[vapply(items, function(item) isTRUE(item$include), logical(1))])
    })
    output$object_create_control <- renderUI({
      ids <- input$object_population_ids %||% character()
      tbl <- shared$populations$table()
      selected <- tbl[tbl$id %in% ids, , drop = FALSE]
      ready <- if (identical(input$new_object_source, "fcs")) nrow(shared$df$raw() %||% data.frame()) > 0 else nrow(selected) > 0 && all(selected$events > 0)
      tagList(if (!identical(input$new_object_source, "fcs")) span(sprintf("%d populations; %s cell rows", nrow(selected), format(sum(selected$events), big.mark = ","))),
        actionButton(ns("confirm_create_object"), "Create Object", class = "btn btn-primary", disabled = if (!ready) "disabled" else NULL))
    })
    observeEvent(input$review_go_gating, {
      removeModal()
      updateNavbarPage(session$rootScope(), "main_nav", selected = "gating_tab")
    })

    output$object_header <- renderUI({
      objs <- shared$analysis$objects()
      if (!length(objs)) {
        return(div(class = "analysis-object-header",
          div(class = "Pinhki-inline-note", tags$b("Create an analysis object first. "), "Review gated populations from the basket, or explicitly choose Root / all events."),
          actionButton(ns("create_object"), "Create Analysis Object", class = "btn btn-primary")
        ))
      }
      choices <- stats::setNames(names(objs), vapply(objs, function(x) x$name %||% x$id, character(1)))
      div(class = "analysis-object-header",
        selectInput(ns("active_object"), NULL, choices = choices, selected = shared$analysis$active_id(), width = "100%"),
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
        if (!source %in% c("basket", "fcs")) stop("Choose an analysis input source.")
        obj <- shared$analysis$create(name = name, source = source, population_ids = if (identical(source, "basket")) input$object_population_ids %||% character() else NULL)
        metadata_counts <- vapply(intersect(c("sample_name", "sample_id", "population_label", "population_set", "gate_name", "gate_path"), names(obj$data)),
          function(field) length(unique(as.character(obj$data[[field]]))), integer(1))
        message(sprintf(
          "[cluster metadata trace] created object source=%s; rows=%d; %s",
          source, nrow(obj$data), paste(sprintf("%s=%d", names(metadata_counts), metadata_counts), collapse = "; ")
        ))
        removeModal()
        source_label <- switch(source, basket = "Population basket", batch = "Batch concatenation", "FCS event table")
        showNotification(sprintf("Created %s from %s.", obj$name, source_label), type = "message")
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
        if (nrow(obj$provenance$selection %||% data.frame())) tags$details(tags$summary("Input populations"),
          tags$ul(lapply(seq_len(nrow(obj$provenance$selection)), function(i) {
            row <- obj$provenance$selection[i, ]
            tags$li(sprintf("%s | %s | %s cells", row$sample_name, row$gate_path, format(row$events, big.mark = ",")))
          }))),
        if (nzchar(obj$notes %||% "")) p(class = "control-note", obj$notes)
      )
    })

    marker_table_data <- reactive({
      obj <- active_object()
      req(obj)
      markers <- obj$markers %||% character(0)
      cm <- obj$channel_map %||% data.frame(channel = markers, marker = markers, stringsAsFactors = FALSE)
      channel <- cm$channel[match(markers, cm$marker)]
      channel[is.na(channel) | !nzchar(channel)] <- markers[is.na(channel) | !nzchar(channel)]
      type <- ifelse(grepl("^FSC|^SSC", markers, ignore.case = TRUE), "Scatter",
        ifelse(grepl("^TIME$", markers, ignore.case = TRUE), "Time",
          ifelse(grepl("LIVE|DEAD|VIAB", markers, ignore.case = TRUE), "Viability",
            ifelse(grepl("EVENT|LENGTH|WIDTH|CENTER|OFFSET", markers, ignore.case = TRUE), "Technical", ""))))
      marker_html <- ifelse(nzchar(type), sprintf('<span class="cluster-marker-technical">%s <span class="cluster-marker-badge">%s</span></span>', htmltools::htmlEscape(markers), type), htmltools::htmlEscape(markers))
      data.frame(Marker = marker_html, Channel = channel, marker_key = markers, check.names = FALSE, stringsAsFactors = FALSE)
    })

    output$channels_picker <- renderUI({
      if (is.null(active_object())) return(div(class = "Pinhki-inline-note", tags$b("No analysis object. "), "Create an object before selecting markers."))
      tagList(
        div(class = "cluster-marker-toolbar",
          actionButton(ns("markers_recommended"), "Recommended", class = "btn btn-sm btn-outline-primary"),
          actionButton(ns("markers_all"), "Select all", class = "btn btn-sm btn-outline-secondary"),
          actionButton(ns("markers_clear"), "Clear", class = "btn btn-sm btn-outline-secondary"),
          textOutput(ns("marker_selected_count"), inline = TRUE)
        ),
        div(class = "cluster-marker-table-scroll Pinhki-scroll-fade", DT::dataTableOutput(ns("marker_table")))
      )
    })

    output$marker_table <- DT::renderDataTable({
      tbl <- marker_table_data()
      obj <- active_object()
      selected <- match(intersect(obj$selected_markers %||% flowjo_default_cluster_markers(obj$markers), obj$markers), tbl$marker_key)
      DT::datatable(tbl[, c("Marker", "Channel"), drop = FALSE], escape = FALSE, rownames = FALSE,
        selection = list(mode = "multiple", selected = selected),
        options = list(dom = "t", paging = FALSE, scrollY = "300px", scrollCollapse = TRUE, ordering = FALSE, autoWidth = TRUE))
    })

    selected_marker_names <- reactive({
      tbl <- marker_table_data()
      rows <- input$marker_table_rows_selected %||% integer(0)
      tbl$marker_key[intersect(rows, seq_len(nrow(tbl)))]
    })

    observeEvent(input$marker_table_rows_selected, {
      obj <- active_object()
      req(obj)
      shared$analysis$set_selected_markers(obj$id, selected_marker_names())
    }, ignoreInit = TRUE)

    output$marker_selected_count <- renderText(sprintf("%d selected", length(selected_marker_names())))

    observeEvent(input$markers_recommended, {
      tbl <- marker_table_data()
      DT::selectRows(marker_table_proxy, match(flowjo_default_cluster_markers(tbl$marker_key), tbl$marker_key))
    })
    observeEvent(input$markers_all, DT::selectRows(marker_table_proxy, seq_len(nrow(marker_table_data()))))
    observeEvent(input$markers_clear, DT::selectRows(marker_table_proxy, integer(0)))

    output$run_selector <- renderUI({
      obj <- active_object()
      if (is.null(obj)) return(NULL)
      runs <- obj$runs %||% list()
      if (!length(runs)) return(NULL)
      choices <- stats::setNames(names(runs), vapply(runs, function(x) x$name %||% x$id, character(1)))
      div(class = "analysis-run-header",
        selectInput(ns("active_run"), "Selected run", choices = choices, selected = obj$active_run_id, width = "100%")
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

    # Agent note: Keep the run data reactive to the active run itself; dynamic selectors may not emit
    # input$active_run when a newly-created run becomes active, which would otherwise hide Split by.
    run_df <- reactive({
      run <- active_run()
      if (is.null(run)) return(NULL)
      df <- as.data.frame(run$result$df, stringsAsFactors = FALSE)
      obj <- active_object()
      if (is.null(obj) || !nrow(df)) return(df)

      # Older analysis objects were saved before row-level sample metadata was
      # carried into the run snapshot. Keep those runs facetable without
      # rewriting their stored result; new batch objects still use their own
      # sample/gate/keyword columns below.
      restored <- character()
      object_data <- tryCatch(as.data.frame(obj$data, stringsAsFactors = FALSE), error = function(e) NULL)
      if (!is.null(object_data) && nrow(object_data) == nrow(df) && ncol(object_data)) {
        # Preserve row order: analysis runs are generated from the object
        # snapshot, so these columns are safe to carry back into an older run
        # that stored only clustering/UMAP output.
        for (nm in setdiff(names(object_data), names(df))) {
          value <- object_data[[nm]]
          if (!is.list(value)) {
            df[[nm]] <- value
            restored <- c(restored, nm)
          }
        }
      }
      if (length(restored)) {
        message(sprintf("[cluster split trace] restored object metadata columns: %s", paste(restored, collapse = ", ")))
      }
      mapping <- shared$pipeline$isolated_populations()$mapping %||% data.frame()
      if (!all(c("SampleID", "PopulationID") %in% names(df))) {
        items <- shared$populations$items()
        items <- items[vapply(items, function(item) isTRUE(item$include), logical(1))]
        registry <- shared$fcs$registry()
        memberships <- shared$gating$memberships()
        item_counts <- vapply(items, function(item) {
          member <- memberships[[item$sample_id]][[item$gate_id]]
          if (is.logical(member)) sum(member, na.rm = TRUE) else length(unique(member[is.finite(member)]))
        }, integer(1))
        if (length(items) && sum(item_counts) == nrow(df)) {
          df$sample_name <- unlist(Map(function(item, count) {
            entry <- registry[[item$sample_id]] %||% list()
            rep(entry$name %||% item$sample_id, count)
          }, items, item_counts), use.names = FALSE)
          df$population_label <- unlist(Map(function(item, count) {
            rep(item$label %||% item$gate_id, count)
          }, items, item_counts), use.names = FALSE)
          message(sprintf(
            "[cluster split trace] recovered metadata by exact basket row blocks: items=%d; rows=%d",
            length(items), sum(item_counts)
          ))
        } else if (length(items)) {
          message(sprintf(
            "[cluster split trace] basket row recovery refused: basket rows=%d; run rows=%d",
            sum(item_counts), nrow(df)
          ))
        }
      }
      if (!nrow(mapping) && all(c("SampleID", "PopulationID") %in% names(df))) {
        items <- shared$populations$items()
        items <- items[vapply(items, function(item) isTRUE(item$include), logical(1))]
        registry <- shared$fcs$registry()
        if (length(items) && length(registry)) {
          mapping <- do.call(rbind, lapply(seq_along(items), function(i) {
            item <- items[[i]]
            sample_index <- match(item$sample_id, names(registry))
            sample_entry <- registry[[item$sample_id]] %||% list()
            basket_label <- item$label %||% item$gate_id
            data.frame(
              PopulationID = i,
              SampleID = sample_index,
              sample_name = sample_entry$name %||% item$sample_id,
              population_label = basket_label,
              stringsAsFactors = FALSE
            )
          }))
          message(sprintf("[cluster split trace] rebuilt mapping from %d current basket items", nrow(mapping)))
        }
      }
      if (nrow(mapping)) {
        if (!"sample_name" %in% names(df) && "SampleID" %in% names(df) && all(c("SampleID", "sample_name") %in% names(mapping))) {
          df$sample_name <- mapping$sample_name[match(df$SampleID, mapping$SampleID)]
        }
        if (!"population_label" %in% names(df) && "PopulationID" %in% names(df) && all(c("PopulationID", "population_label") %in% names(mapping))) {
          df$population_label <- mapping$population_label[match(df$PopulationID, mapping$PopulationID)]
        }
        message(sprintf(
          "[cluster split trace] recovered exported metadata: sample_name=%d; population_label=%d",
          length(unique(stats::na.omit(df$sample_name %||% character()))),
          length(unique(stats::na.omit(df$population_label %||% character())))
        ))
      }
      if ("population_label" %in% names(df)) {
        basket_name <- as.character(df$population_label)
        path_shaped <- grepl("/|\\s>\\s|::", basket_name)
        basket_name[path_shaped] <- trimws(sub("^.*(?:/|\\s>\\s|::)", "", basket_name[path_shaped], perl = TRUE))
        df$basket_name <- basket_name
      }
      df
    })

    gate_split_context <- reactive({
      obj <- active_object()
      df <- run_df()
      if (is.null(obj) || is.null(df) || !nrow(df) || "gate_name" %in% names(df)) return(NULL)
      sample_id <- obj$source_id
      if (length(sample_id) != 1L || is.na(sample_id) || !nzchar(sample_id)) sample_id <- shared$fcs$active_id()
      if (length(sample_id) != 1L || is.na(sample_id) || !nzchar(sample_id)) return(NULL)
      gates <- shared$gating$gates()
      memberships <- shared$gating$memberships()[[sample_id]]
      if (!length(gates) || !length(memberships)) return(NULL)
      gate_ids <- intersect(names(gates), names(memberships))
      gate_ids <- gate_ids[vapply(gate_ids, function(id) {
        member <- memberships[[id]]
        (is.logical(member) && length(member) == nrow(df)) ||
          (is.numeric(member) && length(member) && all(is.na(member) | is.finite(member)) &&
            (length(member) == nrow(df) || all(member[is.finite(member)] >= 1 & member[is.finite(member)] <= nrow(df))))
      }, logical(1))]
      if (!length(gate_ids)) return(NULL)
      list(sample_id = sample_id, gates = gates, memberships = memberships, gate_ids = gate_ids)
    })

    expand_gate_split <- function(df, structure) {
      fields <- c(structure$color_by %||% "", structure$split_col %||% "", structure$split_row %||% "")
      if (!".gate_name" %in% fields) return(df)
      context <- gate_split_context()
      if (is.null(context)) return(df)
      pieces <- lapply(context$gate_ids, function(id) {
        member <- context$memberships[[id]]
        keep <- if (is.logical(member) || (length(member) == nrow(df) && all(is.na(member) | member %in% c(0, 1)))) {
          which(!is.na(member) & member)
        } else {
          unique(as.integer(member[is.finite(member) & member >= 1 & member <= nrow(df)]))
        }
        if (!length(keep)) return(NULL)
        out <- df[keep, , drop = FALSE]
        out$.gate_name <- context$gates[[id]]$name %||% id
        out
      })
      pieces <- Filter(Negate(is.null), pieces)
      if (!length(pieces)) return(df)
      out <- do.call(rbind, pieces)
      rownames(out) <- NULL
      message(sprintf("[cluster split trace] expanded %d events into %d gate-panel rows across %d gates", nrow(df), nrow(out), length(pieces)))
      out
    }

    output$preview_plot_ui <- renderUI({
      df <- run_df()
      if (is.null(df)) return(NULL)
      if (identical(applied_structure()$view_mode, "3d") && !"UMAP_3" %in% names(df)) return(
        div(class="Pinhki-inline-note", tags$b("3D unavailable. "), "This run does not contain a real UMAP_3 coordinate."))
      width_probe <- tags$script(HTML(sprintf("(function(){var id='%s', root=document.getElementById('%s'); if(!root||root.dataset.widthProbe)return; root.dataset.widthProbe='1'; var last=0,timer; var send=function(){clearTimeout(timer);timer=setTimeout(function(){var width=Math.round(root.getBoundingClientRect().width);if(Math.abs(width-last)<12)return;last=width;if(window.Shiny)Shiny.setInputValue(id,width,{priority:'event'});},200);}; send(); if(window.ResizeObserver)new ResizeObserver(send).observe(root);})();", ns("preview_width"), ns("preview_plot_ui"))))
      plot_side <- max(520L, as.integer(input$preview_width %||% 720L))
      if (identical(applied_structure()$view_mode, "3d")) return(tagList(width_probe, withSpinner(plotlyOutput(ns("plot3d"), height = paste0(plot_side, "px")))))
      tagList(width_probe, withSpinner(plotOutput(ns("plot"), height = paste0(plot_side, "px"))))
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
      sel_channels <- intersect(selected_marker_names(), names(obj$data))
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
        transform_method <- input$transform_method %||% "logicle"
        scale_method <- input$scale_method %||% "zscore"
        flowjo_use_wsp <- isTRUE(input$flowjo_use_wsp %||% TRUE)
        flowjo_channel_range <- as.numeric(input$flowjo_channel_range %||% 4096)
        flowjo_max_value <- as.numeric(input$flowjo_max_value %||% 262144)
        flowjo_pos <- as.numeric(input$flowjo_pos %||% 4.5)
        flowjo_neg <- as.numeric(input$flowjo_neg %||% 0)
        flowjo_width_basis <- as.numeric(input$flowjo_width_basis %||% -10)
        mat_transformed <- tryCatch(cluster_transform_matrix(
          as.matrix(df[, sel_channels, drop = FALSE]), method = transform_method,
          cofactor = input$arcsinh_cofactor %||% 150,
          logicle_auto = isTRUE(input$logicle_auto),
          t = input$logicle_t %||% 262144, w = input$logicle_w %||% 0.5,
          m = input$logicle_m %||% 4.5, a = input$logicle_a %||% 0,
          flowjo_channel_range = flowjo_channel_range, flowjo_max_value = flowjo_max_value,
          flowjo_pos = flowjo_pos, flowjo_neg = flowjo_neg, flowjo_width_basis = flowjo_width_basis,
          flowjo_settings = obj$transform_settings %||% list(), flowjo_use_wsp = flowjo_use_wsp
        ), error = function(e) {
          showNotification(conditionMessage(e), type = "error", duration = 10)
          NULL
        })
        if (is.null(mat_transformed)) return()
        mat <- cluster_scale_matrix(mat_transformed, scale_method)
        mat[!is.finite(mat)] <- 0

        incProgress(0.35, detail = "Running UMAP")
        set.seed(seed_i)
        um <- uwot::umap(X = mat, n_neighbors = n_neighbors, min_dist = min_dist, metric = metric_i, n_components = 3L, verbose = FALSE)
        colnames(um) <- c("UMAP_1", "UMAP_2", "UMAP_3")

        incProgress(0.45, detail = "Running FlowSOM")
        clustering_df <- df
        clustering_df[, sel_channels] <- mat
        fs_res <- flowsom_cluster(df = clustering_df, features = sel_channels, xdim = xdim_i, ydim = ydim_i, rlen = rlen_i, seed = seed_i, scale_data = FALSE, k = k_i)

        out <- cbind(df, as.data.frame(um))
        out$FlowSOM_Cluster <- factor(fs_res$cluster)
        metadata_counts <- vapply(intersect(c("sample_name", "sample_id", "population_label", "population_set", "gate_name", "gate_path"), names(out)),
          function(field) length(unique(as.character(out[[field]]))), integer(1))
        message(sprintf(
          "[cluster metadata trace] saving run rows=%d; %s",
          nrow(out), paste(sprintf("%s=%d", names(metadata_counts), metadata_counts), collapse = "; ")
        ))
        params <- list(seed = seed_i, neighbors = n_neighbors, min_dist = min_dist, metric = metric_i,
          feats = sel_channels, xdim = xdim_i, ydim = ydim_i, rlen = rlen_i, k = fs_res$k_used %||% k_i,
          transform_method = transform_method, arcsinh_cofactor = input$arcsinh_cofactor %||% 150,
          logicle_auto = isTRUE(input$logicle_auto), logicle_t = input$logicle_t %||% 262144,
          logicle_w = input$logicle_w %||% 0.5, logicle_m = input$logicle_m %||% 4.5,
          logicle_a = input$logicle_a %||% 0, scale_method = scale_method,
          flowjo_use_wsp = flowjo_use_wsp, flowjo_channel_range = flowjo_channel_range,
          flowjo_max_value = flowjo_max_value, flowjo_pos = flowjo_pos,
          flowjo_neg = flowjo_neg, flowjo_width_basis = flowjo_width_basis,
          scale = identical(scale_method, "zscore"))
        shared$analysis$add_run(name = run_name, method = "UMAP + FlowSOM", result = list(df = out), params = params)
        names_now <- vapply(shared$analysis$active()$runs %||% list(), function(x) x$name %||% "", character(1))
        updateTextInput(session, "run_name", value = cluster_available_name("Cluster Run", names_now))
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
      for (nm in intersect(c("UMAP_1", "UMAP_2", "UMAP_3"), names(fs_df))) out[[nm]] <- fs_df[[nm]]
      shared$analysis$add_run(name = run_name, method = "Imported FlowSOM", result = list(df = out), params = list(cluster_col = cluster_col))
      names_now <- vapply(shared$analysis$active()$runs %||% list(), function(x) x$name %||% "", character(1))
      updateTextInput(session, "import_run_name", value = cluster_available_name("Imported Run", names_now))
      showNotification("Imported FlowSOM run.", type = "message")
    })

    split_choices <- reactive({
      df <- run_df()
      req(df)
      obj <- active_object()
      req(obj)
      marker_columns <- unique(c(obj$markers %||% character(), obj$channel_map$channel %||% character()))
      reserved <- unique(c("FlowSOM_Cluster", "UMAP_1", "UMAP_2", "UMAP_3", marker_columns))
      eligible <- setdiff(names(df), reserved)
      eligible <- eligible[vapply(df[eligible], function(x) {
        if (is.list(x) && !is.data.frame(x)) return(FALSE)
        values <- if (is.numeric(x)) x[is.finite(x)] else x[!is.na(x) & nzchar(trimws(as.character(x)))]
        n_values <- length(unique(as.character(values)))
        n_values >= 2L && n_values <= 256L
      }, logical(1))]
      preferred <- grepl("sample|gate|population|condition|group|batch|file|source|keyword", eligible, ignore.case = TRUE)
      out <- unique(c(eligible[preferred], eligible[!preferred]))
      if ("sample_name" %in% out) out <- setdiff(out, "sample_id")
      # Agent note: population_label is the name users assigned in the population basket;
      # do not expose source gate names/paths as competing split concepts for basket data.
      if ("basket_name" %in% out) out <- setdiff(out, c("population_label", "gate_name", "gate_path"))
      message(sprintf("[cluster split trace] run columns=%d; eligible split columns=%d: %s", ncol(df), length(out), paste(out, collapse = ", ")))
      out
    })

    output$display_controls <- renderUI({
      active_run_key()
      df <- isolate(run_df()); if (is.null(df)) return(NULL)
      run <- isolate(active_run())
      saved <- run$visualization %||% list()
      split_options <- split_choices()
      if (!length(split_options)) message("[cluster split trace] rendering Split by controls with None only; no eligible metadata columns found")
      split_labels <- split_options
      split_labels[split_options == "basket_name"] <- "Basket name"
      color_options <- split_options
      color_labels <- split_labels
      color_cols <- c("FlowSOM cluster" = "FlowSOM_Cluster", stats::setNames(color_options, color_labels))
      color_cols <- color_cols[!duplicated(unname(color_cols))]
      splits <- c("None" = "", stats::setNames(split_options, split_labels))
      tagList(
        div(class = "Pinhki-plot-controls-row cluster-structure-controls", fluidRow(
          column(4, div(class = "Pinhki-plot-controls-item", selectInput(ns("color_by"), "Color by", color_cols, selected = saved$color_by %||% "FlowSOM_Cluster"))),
          column(4, div(class = "Pinhki-plot-controls-item", selectInput(ns("split_col"), "Split by (columns)", splits, selected = saved$split_col %||% ""))),
          column(4, div(class = "Pinhki-plot-controls-item", selectInput(ns("split_row"), "Split by (rows)", splits, selected = saved$split_row %||% "")))
        )),
        div(class = "Pinhki-plot-controls-row", fluidRow(
          column(4, div(class = "Pinhki-plot-controls-item", radioButtons(ns("view_mode"), "View mode", choices = c("2D" = "2d", "3D interactive" = "3d"), selected = saved$view_mode %||% "2d", inline = TRUE))),
          column(4, div(class = "Pinhki-plot-controls-item", uiOutput(ns("palette_controls")))),
          column(4, div(class = "Pinhki-plot-controls-item", textInput(ns("plot_title"), "Plot title", value = saved$plot_title %||% run$name %||% "Cluster run")))
        )),
        div(class = "Pinhki-plot-controls-row", fluidRow(
          column(4, div(class = "Pinhki-plot-controls-item", sliderInput(ns("point_size"), "Point size", .1, 5, saved$point_size %||% .7, .1))),
          column(4, div(class = "Pinhki-plot-controls-item", sliderInput(ns("point_alpha"), "Opacity", .05, 1, saved$point_alpha %||% .85, .05))),
          column(4, div(class = "Pinhki-plot-controls-item", conditionalPanel(sprintf("input['%s'] == '3d'", ns("view_mode")), checkboxInput(ns("see_through"), "See-through dots", FALSE))))
        )),
        div(class = "Pinhki-plot-controls-row", fluidRow(
          column(12, div(class = "Pinhki-plot-controls-item", uiOutput(ns("axis_ui"))))
        )),
        div(class = "cluster-color-editor", uiOutput(ns("color_swatch_grid")))
      )
    })

    split_input_choices <- reactive({
      options <- split_choices()
      labels <- options
      labels[options == "basket_name"] <- "Basket name"
      c("None" = "", stats::setNames(options, labels))
    })

    composition_group_count <- reactive({
      df <- run_df()
      field <- input$composition_group %||% ""
      if (is.null(df) || !nzchar(field) || !field %in% names(df)) return(1L)
      values <- as.character(df[[field]])
      values[is.na(values) | !nzchar(trimws(values))] <- "Unknown"
      length(unique(values))
    })

    output$composition_controls <- renderUI({
      active_run_key()
      run <- isolate(active_run())
      if (is.null(run)) return(NULL)
      saved <- run$visualization %||% list()
      splits <- split_input_choices()
      current <- function(id, saved_value, default) isolate(input[[id]]) %||% saved_value %||% default
      group_selected <- current("composition_group", saved$composition_group, "")
      mode_selected <- current("composition_mode", saved$composition_mode, "stacked")
      if (composition_group_count() < 2L) mode_selected <- "stacked"
      mode_choices <- if (composition_group_count() < 2L) c("Stacked only" = "stacked") else c("Alluvial + stacked" = "alluvial", "Stacked only" = "stacked")
      tagList(
        div(class = "cluster-composition-primary", fluidRow(
          column(6, selectInput(ns("composition_split"), "Split by", splits,
            selected = current("composition_split", saved$composition_split %||% saved$split_col, ""))),
          column(6, selectInput(ns("composition_group"), "Group by", splits, selected = group_selected))
        )),
        div(class = "cluster-composition-primary", fluidRow(
          column(4, selectInput(ns("composition_mode"), "Chart mode", mode_choices, selected = mode_selected)),
          column(4, selectInput(ns("composition_metric"), "Metric", c("Percentage" = "percent", "Cell count" = "count"), selected = current("composition_metric", saved$composition_metric, "percent"))),
          column(4, selectInput(ns("composition_order"), "Cluster order", c("Abundance" = "abundance", "Numeric" = "numeric", "Reverse numeric" = "reverse_numeric"), selected = current("composition_order", saved$composition_order, "abundance")))
        )),
        tags$details(class = "cluster-composition-aesthetics",
          tags$summary("Aesthetics"),
          div(class = "cluster-composition-aesthetic-grid",
            sliderInput(ns("composition_ribbon_alpha"), "Ribbon opacity", .1, 1, current("composition_ribbon_alpha", saved$composition_ribbon_alpha, .45), .05),
            sliderInput(ns("composition_ribbon_width"), "Ribbon width", .05, .8, current("composition_ribbon_width", saved$composition_ribbon_width, .28), .01),
            sliderInput(ns("composition_ribbon_waist"), "Ribbon curve", .05, .45, current("composition_ribbon_waist", saved$composition_ribbon_waist, .22), .01),
            sliderInput(ns("composition_separator"), "Ribbon separator", 0, 1, current("composition_separator", saved$composition_separator, .18), .05),
            sliderInput(ns("composition_bar_width"), "Bar width", .2, 1, current("composition_bar_width", saved$composition_bar_width, .85), .05),
            sliderInput(ns("composition_fill_alpha"), "Fill opacity", .1, 1, current("composition_fill_alpha", saved$composition_fill_alpha, 1), .05),
            sliderInput(ns("composition_border_width"), "Border width", 0, 1, current("composition_border_width", saved$composition_border_width, .15), .05),
            textInput(ns("composition_border_color"), "Border color", current("composition_border_color", saved$composition_border_color, "#4D4D4D"))
          )
        )
      )
    })

    output$composition_notice <- renderUI({
      if (composition_group_count() >= 2L) return(NULL)
      div(class = "Pinhki-inline-note", tags$b("Alluvial needs at least two groups. "), "Choose a Group by field with two or more values; stacked composition remains available.")
    })

    output$palette_controls <- renderUI({
      choices <- c("Sci Publication Color palette", "Okabe-Ito", "Dark 2", "Set 3", "Viridis", names(color_presets()))
      selected <- active_palette(); if (!selected %in% choices) selected <- choices[[1]]
      custom <- selected %in% names(color_presets())
      tagList(
        selectInput(ns("palette"), "Palette", choices, selected = selected),
        if (custom) div(class = "cluster-preset-actions",
          actionButton(ns("rename_preset"), NULL, icon = icon("pen"), title = "Rename custom preset", class = "btn btn-sm btn-outline-secondary"),
          actionButton(ns("delete_preset"), NULL, icon = icon("trash"), title = "Delete custom preset", class = "btn btn-sm btn-outline-danger"))
      )
    })

    color_categories <- reactive({
      df <- run_df(); req(df)
      field <- input$color_by %||% "FlowSOM_Cluster"
      if (!field %in% names(df)) field <- "FlowSOM_Cluster"
      values <- unique(as.character(df[[field]][!is.na(df[[field]])]))
      values[cluster_natural_order(values)]
    })

    output$color_swatch_grid <- renderUI({
      cats <- color_categories(); cols <- unlist(cluster_colors())
      if (!length(cats)) return(NULL)
      div(class = "cluster-color-grid", lapply(cats, function(category) {
        col <- cluster_color_value(cols, category)
        tags$button(type = "button",
          tagList(tags$span(class = "cluster-color-dot", style = sprintf("--cluster-color:%s", col)),
            tags$span(class = "cluster-color-label", title = category, category)),
          class = "cluster-color-swatch", title = category, `data-category` = category,
          onclick = sprintf("Shiny.setInputValue('%s', this.dataset.category, {priority:'event'})", ns("edit_color_category")))
      }))
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

    reference_palette_seed <- c(
      # Agent note: these seeds mirror shared_shiny_style/palettes/color_palettes.R;
      # keep the source colors and OKLCH maximin selection synchronized so large
      # cluster counts remain distinguishable instead of becoming random.
      "#96C3D8", "#5D9BBE", "#F5B375", "#C0937E", "#67A59B", "#A4D38E", "#4A9D47", "#F19294", "#E45A5F", "#3477A9", "#BDA7CB", "#684797", "#9983B7", "#CD9A99", "#DD4B52", "#DA8F6F", "#F58135",
      "#80B9D1", "#6CA6B3", "#A3CAAE", "#88C784", "#BCABAA", "#EC9772", "#D7C79A", "#EEB982", "#6CA0AA", "#C8AE7E", "#A4B08A", "#8CB26D", "#D08D7B", "#E35F64", "#B392BD", "#8069AA", "#E2CE97", "#C3936B", "#AC623A", "#E05446", "#DDA98B", "#EB9580", "#F4AF7E",
      "#84709E", "#BC763B", "#C67628", "#F2AE74", "#FABC91", "#F2CAAC", "#F6DECB", "#C6A89A", "#BC232E", "#F48C84", "#F29381", "#EA614E", "#EF5326", "#F06E27", "#F98626", "#DE8C32", "#8DC58B", "#66A540", "#2D521C", "#C1C58C", "#8B9666", "#647752", "#84985C", "#CCE189", "#D1DF8B", "#F0EE9E", "#F4EAAF", "#F0BECA"
    )

    palette_oklab <- function(hex) {
      if (!requireNamespace("farver", quietly = TRUE)) return(NULL)
      farver::convert_colour(t(grDevices::col2rgb(hex)), from = "rgb", to = "oklab")
    }

    palette_oklch <- function(hex) {
      if (!requireNamespace("farver", quietly = TRUE)) return(NULL)
      farver::convert_colour(t(grDevices::col2rgb(hex)), from = "rgb", to = "oklch")
    }

    palette_hex_from_oklch <- function(lch) {
      if (!requireNamespace("farver", quietly = TRUE) || !nrow(lch)) return(character())
      rgb <- farver::convert_colour(as.matrix(lch), from = "oklch", to = "rgb")
      keep <- apply(rgb, 1, function(x) all(is.finite(x)) && all(x >= 0 & x <= 255))
      if (!any(keep)) return(character())
      grDevices::rgb(rgb[keep, 1], rgb[keep, 2], rgb[keep, 3], maxColorValue = 255)
    }

    palette_distinct_seeds <- function(hex, min_distance = 0.035) {
      hex <- unique(toupper(hex))
      lab <- palette_oklab(hex)
      if (is.null(lab) || nrow(lab) < 2) return(hex)
      keep <- integer()
      for (i in seq_len(nrow(lab))) {
        if (!length(keep) || all(sqrt(rowSums(sweep(lab[keep, , drop = FALSE], 2, lab[i, ], "-", check.margin = FALSE)^2)) >= min_distance)) keep <- c(keep, i)
      }
      hex[keep]
    }

    palette_maximin <- function(hex, n) {
      if (n < 1) return(character())
      hex <- palette_distinct_seeds(hex)
      if (!requireNamespace("farver", quietly = TRUE)) return(grDevices::colorRampPalette(hex)(n))
      # Add a dense OKLCH candidate field only when the supplied figure colors
      # are insufficient; invalid out-of-gamut colors are discarded.
      if (length(hex) < n) {
        grid <- expand.grid(
          l = c(.50, .58, .66, .74, .82),
          c = c(.09, .13, .17, .21),
          h = seq(0, 355, by = 5),
          KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE
        )
        hex <- palette_distinct_seeds(c(hex, palette_hex_from_oklch(grid)), min_distance = 0.02)
      }
      lab <- palette_oklab(hex)
      selected <- integer()
      remaining <- seq_len(nrow(lab))
      while (length(selected) < min(n, nrow(lab)) && length(remaining)) {
        if (!length(selected)) pick <- remaining[1]
        else {
          distance <- vapply(remaining, function(i) min(sqrt(rowSums(sweep(lab[selected, , drop = FALSE], 2, lab[i, ], "-", check.margin = FALSE)^2))), numeric(1))
          pick <- remaining[which.max(distance)]
        }
        selected <- c(selected, pick)
        remaining <- setdiff(remaining, pick)
      }
      out <- hex[selected]
      lch <- palette_oklch(out)
      if (is.null(lch) || !nrow(lch)) return(out)
      hue <- lch[, "h"]
      hue[!is.finite(hue)] <- 360
      # Agent note: cluster numbers must follow a readable hue progression;
      # saturation/chroma breaks ties within a hue instead of leaving maximin order visible.
      out[order(hue, -lch[, "c"], lch[, "l"], na.last = TRUE)]
    }

    palette_values <- function(name, n) {
      if (n < 1) return(character())
      if (identical(name, "Sci Publication Color palette")) return(palette_maximin(reference_palette_seed, n))
      if (identical(name, "Viridis")) return(scales::viridis_pal()(n))
      base <- switch(name, "Dark 2" = RColorBrewer::brewer.pal(8, "Dark2"), "Set 3" = RColorBrewer::brewer.pal(12, "Set3"),
        c("#0072B2", "#E69F00", "#009E73", "#CC79A7", "#D55E00", "#56B4E9", "#F0E442", "#000000"))
      grDevices::colorRampPalette(base)(n)
    }

    observeEvent(active_run_key(), {
      run <- isolate(active_run()); req(run)
      cls <- unique(as.character(run$result$df$FlowSOM_Cluster))
      cls <- cls[cluster_natural_order(cls)]
      saved <- run$cluster_colors %||% character()
      defaults <- stats::setNames(palette_values("Sci Publication Color palette", length(cls)), cls)
      # Agent note: old builds initialized every picker to #4477AA; repair only that unmistakable corrupt map.
      corrupt_blue <- length(saved) > 1L && all(toupper(unname(saved)) == "#4477AA")
      if (corrupt_blue) saved <- character()
      cluster_colors(stats::setNames(as.list(ifelse(cls %in% names(saved), saved[cls], defaults)), cls))
      viz <- run$visualization %||% list()
      color_presets(viz$color_presets %||% list())
      saved_palette <- viz$active_palette %||% viz$palette %||% "Sci Publication Color palette"
      if (identical(saved_palette, "Science reference (OKLCH)")) saved_palette <- "Sci Publication Color palette"
      active_palette(saved_palette)
      applied_structure(list(
        view_mode = viz$view_mode %||% "2d", color_by = viz$color_by %||% "FlowSOM_Cluster",
        split_col = viz$split_col %||% "", split_row = viz$split_row %||% "",
        x_axis = viz$x_axis, y_axis = viz$y_axis
      ))
    }, ignoreInit = FALSE)

    observeEvent(input$update_preview, {
      message(sprintf(
        "[cluster split trace] applying color=%s; columns=%s; rows=%s",
        input$color_by %||% "FlowSOM_Cluster",
        input$split_col %||% "",
        input$split_row %||% ""
      ))
      applied_structure(list(
        view_mode = input$view_mode %||% "2d", color_by = input$color_by %||% "FlowSOM_Cluster",
        split_col = input$split_col %||% "", split_row = input$split_row %||% "",
        x_axis = input$x_axis, y_axis = input$y_axis
      ))
    })

    observeEvent(list(input$split_col, input$split_row), {
      state <- applied_structure()
      state$split_col <- input$split_col %||% ""
      state$split_row <- input$split_row %||% ""
      applied_structure(state)
    }, ignoreInit = TRUE)

    observeEvent(input$color_by, {
      state <- applied_structure(); state$color_by <- input$color_by %||% "FlowSOM_Cluster"; applied_structure(state)
      cats <- isolate(color_categories()); cols <- isolate(cluster_colors())
      missing <- setdiff(cats, names(cols))
      if (length(missing)) {
        fresh <- palette_values(if (active_palette() %in% c("Sci Publication Color palette", "Okabe-Ito", "Dark 2", "Set 3", "Viridis")) active_palette() else "Sci Publication Color palette", length(cats))
        names(fresh) <- cats
        for (x in missing) cols[[x]] <- fresh[[x]]
        cluster_colors(cols)
      }
    }, ignoreInit = TRUE)

    visualization_state <- reactive({
      req(active_run())
      list(
        view_mode = input$view_mode %||% "2d", color_by = input$color_by %||% "FlowSOM_Cluster",
        split_col = input$split_col %||% "", split_row = input$split_row %||% "",
        x_axis = input$x_axis, y_axis = input$y_axis, palette = active_palette(), active_palette = active_palette(),
        color_presets = color_presets(),
        point_size = input$point_size %||% .7, point_alpha = input$point_alpha %||% .85,
        plot_title = input$plot_title %||% active_run()$name, see_through = isTRUE(input$see_through),
        # Agent note: heatmap-only display changes must not invalidate the active run;
        # isolate them here so sorting/standardization does not redraw UMAP or composition.
        heat_normalize = isolate(input$heat_normalize %||% "row_z"),
        heat_row_order = isolate(input$heat_row_order %||% character()),
        heat_col_order = isolate(input$heat_col_order %||% character()),
        composition_split = input$composition_split %||% "",
        composition_group = input$composition_group %||% "",
        composition_mode = input$composition_mode %||% "stacked",
        composition_metric = input$composition_metric %||% "percent",
        composition_order = input$composition_order %||% "abundance",
        composition_ribbon_alpha = input$composition_ribbon_alpha %||% .45,
        composition_ribbon_width = input$composition_ribbon_width %||% .28,
        composition_ribbon_waist = input$composition_ribbon_waist %||% .22,
        composition_separator = input$composition_separator %||% .18,
        composition_bar_width = input$composition_bar_width %||% .85,
        composition_fill_alpha = input$composition_fill_alpha %||% 1,
        composition_border_width = input$composition_border_width %||% .15,
        composition_border_color = input$composition_border_color %||% "#4D4D4D"
      )
    })
    observeEvent(visualization_state(), {
      obj <- active_object(); run <- active_run(); req(obj, run)
      shared$analysis$set_run_visualization(obj$id, run$id, visualization_state())
    }, ignoreInit = TRUE)

    current_cluster_colors <- reactive(unlist(cluster_colors()))
    observeEvent(current_cluster_colors(), {
      obj <- active_object(); run <- active_run(); req(obj, run)
      shared$analysis$set_run_colors(obj$id, run$id, current_cluster_colors())
    }, ignoreInit = TRUE)

    observeEvent(input$palette, {
      choice <- input$palette %||% "Sci Publication Color palette"; if (identical(choice, active_palette())) return()
      active_palette(choice)
      cats <- color_categories()
      if (choice %in% names(color_presets())) cluster_colors(as.list(color_presets()[[choice]]))
      else {
        fresh <- palette_values(choice, length(cats)); names(fresh) <- cats; cluster_colors(as.list(fresh))
      }
    }, ignoreInit = TRUE)

    observeEvent(input$edit_color_category, {
      category <- input$edit_color_category; req(category)
      editing_category(category)
      showModal(modalDialog(
        title = div(class = "cluster-picker-title", "Color for ", tags$span(title = category, category)),
        motion_color_picker_ui(ns("cluster_motion_picker"), mode = "single", initial_hex = cluster_color_value(current_cluster_colors(), category)),
        footer = tagList(modalButton("Cancel"), actionButton(ns("apply_category_color"), "Apply", class = "btn btn-primary")),
        easyClose = FALSE
      ))
    })

    next_custom_name <- function(existing) {
      i <- 1L; repeat { out <- if (i == 1L) "Custom" else paste("Custom", i); if (!out %in% existing) return(out); i <- i + 1L }
    }
    observeEvent(input$apply_category_color, {
      category <- editing_category(); value <- picker_value(); req(category, value$hex)
      cols <- cluster_colors(); cols[[category]] <- value$hex; cluster_colors(cols)
      presets <- color_presets(); name <- next_custom_name(names(presets)); presets[[name]] <- unlist(cols)
      color_presets(presets); active_palette(name); removeModal()
    })

    observeEvent(input$rename_preset, {
      old <- active_palette(); req(old %in% names(color_presets()))
      showModal(modalDialog("Rename custom preset", textInput(ns("preset_name"), "Name", old),
        footer = tagList(modalButton("Cancel"), actionButton(ns("confirm_rename_preset"), "Rename", class = "btn btn-primary"))))
    })
    observeEvent(input$confirm_rename_preset, {
      old <- active_palette(); new <- trimws(input$preset_name %||% ""); presets <- color_presets()
      if (!nzchar(new) || (new != old && new %in% c("Sci Publication Color palette", "Okabe-Ito", "Dark 2", "Set 3", "Viridis", names(presets)))) {
        showNotification("Preset names must be non-blank and unique.", type = "warning"); return()
      }
      presets[[new]] <- presets[[old]]; if (new != old) presets[[old]] <- NULL
      color_presets(presets); active_palette(new); removeModal()
    })
    observeEvent(input$delete_preset, {
      old <- active_palette(); presets <- color_presets(); req(old %in% names(presets))
      choices <- c("Sci Publication Color palette", "Okabe-Ito", "Dark 2", "Set 3", "Viridis", names(presets)); idx <- match(old, choices)
      presets[[old]] <- NULL; color_presets(presets); next_choice <- choices[max(1L, idx - 1L)]; active_palette(next_choice)
      cats <- color_categories(); if (next_choice %in% names(presets)) cluster_colors(as.list(presets[[next_choice]])) else {
        fresh <- palette_values(next_choice, length(cats)); names(fresh) <- cats; cluster_colors(as.list(fresh))
      }
    })

    build_2d_plot <- eventReactive(list(applied_structure(), input$active_object, input$active_run, input$preview_width,
      input$point_size, input$point_alpha, input$palette, input$plot_title, input$color_by,
      current_cluster_colors()), {
      df <- run_df()
      req(df, "FlowSOM_Cluster" %in% names(df))
      structure <- applied_structure()
      x_axis <- if ("UMAP_1" %in% names(df)) "UMAP_1" else structure$x_axis
      y_axis <- if ("UMAP_2" %in% names(df)) "UMAP_2" else structure$y_axis
      req(x_axis, y_axis, x_axis %in% names(df), y_axis %in% names(df))
      color_by <- structure$color_by %||% "FlowSOM_Cluster"
      if (!color_by %in% names(df)) color_by <- "FlowSOM_Cluster"
      p <- ggplot(df, aes(.data[[x_axis]], .data[[y_axis]], color = .data[[color_by]])) +
        geom_point(size = input$point_size %||% .7, alpha = input$point_alpha %||% .85, shape = shared$params$point_shape(), stroke = shared$params$point_stroke()) +
        coord_fixed(ratio = 1) +
        theme_minimal(base_size = 11) +
        theme(
          legend.position = "none",
          panel.grid = element_blank(),
          axis.line = element_line(color = "#64748b", linewidth = 0.4),
          plot.title = element_text(hjust = 0.5, family = "Arial", face = "bold", size = 10)
        ) +
        labs(x = x_axis, y = y_axis, color = color_by, title = input$plot_title %||% active_run()$name)
      values <- current_cluster_colors(); cats <- unique(as.character(df[[color_by]]))
      if (all(cats %in% names(values))) p <- p + scale_color_manual(values = values, na.value = "#9CA3AF")
      row <- structure$split_row %||% ""; col <- structure$split_col %||% ""
      if (!row %in% names(df)) row <- ""
      if (!col %in% names(df)) col <- ""
      if (nzchar(row) && identical(row, col)) row <- ""
      split_trace <- function(field) {
        if (!nzchar(field) || !field %in% names(df)) return("None")
        values <- unique(as.character(df[[field]]))
        values <- values[!is.na(values) & nzchar(trimws(values))]
        sprintf("%s (%d: %s)", field, length(values), paste(utils::head(values, 12L), collapse = " | "))
      }
      message(sprintf(
        "[cluster split trace] plotting columns=%s; rows=%s; plot rows=%d",
        split_trace(col), split_trace(row), nrow(df)
      ))
      # Agent note: facet variables are stored as strings in saved run state; inject symbols here so ggplot facets the columns instead of treating `.data` as a facet value.
      if (nzchar(row) && nzchar(col)) p <- p + facet_grid(rows = vars(!!rlang::sym(row)), cols = vars(!!rlang::sym(col)))
      else if (nzchar(row)) p <- p + facet_wrap(vars(!!rlang::sym(row)), ncol = 1)
      else if (nzchar(col)) p <- p + facet_wrap(vars(!!rlang::sym(col)))
      p
    }, ignoreNULL = FALSE)

    output$plot <- renderPlot({
      req(identical(applied_structure()$view_mode, "2d"))
      build_2d_plot()
    }, res = 120)

    output$plot3d <- renderPlotly({
      req(identical(applied_structure()$view_mode, "3d"))
      df <- run_df()
      req(df, all(c("UMAP_1", "UMAP_2", "UMAP_3") %in% names(df)), "FlowSOM_Cluster" %in% names(df))
      structure <- applied_structure()
      color_by <- structure$color_by %||% "FlowSOM_Cluster"
      if (!color_by %in% names(df)) color_by <- "FlowSOM_Cluster"
      opacity <- if (isTRUE(input$see_through)) input$point_alpha %||% .85 else 1
      colors <- current_cluster_colors()
      plotly::plot_ly(data = df, x = ~UMAP_1, y = ~UMAP_2, z = ~UMAP_3, color = ~as.factor(.data[[color_by]]), colors = colors, type = "scatter3d", mode = "markers", marker = list(size = input$point_size %||% 2.2, opacity = opacity)) %>%
        plotly::layout(title = list(text = paste0("<b>", htmltools::htmlEscape(input$plot_title %||% active_run()$name), "</b>"), x = 0.5, xanchor = "center", font = list(family = "Arial", size = 12)), showlegend = FALSE, scene = list(xaxis = list(title = "UMAP 1"), yaxis = list(title = "UMAP 2"), zaxis = list(title = "UMAP 3"))) %>%
        plotly::config(displaylogo = FALSE)
    })

    composition_data <- reactive({
      df <- run_df(); req(df, "FlowSOM_Cluster" %in% names(df))
      group_field <- input$composition_group %||% ""
      if (!group_field %in% names(df)) group_field <- ""
      split_field <- input$composition_split %||% ""
      if (!split_field %in% names(df)) split_field <- ""

      display_values <- function(field, fallback) {
        if (!nzchar(field)) return(factor(rep(fallback, nrow(df)), levels = fallback))
        raw <- df[[field]]
        values <- as.character(raw)
        values[is.na(values) | !nzchar(trimws(values))] <- "Unknown"
        levels_out <- if (is.factor(raw)) as.character(levels(raw)) else unique(values[cluster_natural_order(values)])
      levels_out <- unique(c(levels_out[levels_out %in% values], if ("Unknown" %in% values) "Unknown"))
        factor(values, levels = levels_out)
      }

      plot_df <- data.frame(
        facet = display_values(split_field, "All events"),
        group = display_values(group_field, "All events"),
        cluster = as.character(df$FlowSOM_Cluster),
        stringsAsFactors = FALSE
      )
      plot_df$cluster[is.na(plot_df$cluster) | !nzchar(trimws(plot_df$cluster))] <- "Unknown"
      counts <- dplyr::count(plot_df, .data$facet, .data$group, .data$cluster, name = "n")
      totals <- counts |>
        dplyr::group_by(.data$facet, .data$group) |>
        dplyr::mutate(percent = 100 * .data$n / sum(.data$n)) |>
        dplyr::ungroup()
      order_mode <- input$composition_order %||% "abundance"
      cluster_levels <- unique(as.character(totals$cluster))
      if (identical(order_mode, "abundance")) {
        cluster_levels <- totals |>
          dplyr::group_by(.data$cluster) |>
          dplyr::summarise(total = sum(.data$n), .groups = "drop") |>
          dplyr::arrange(dplyr::desc(.data$total)) |>
          dplyr::pull(.data$cluster)
      } else {
        cluster_levels <- cluster_levels[cluster_natural_order(cluster_levels)]
        if (identical(order_mode, "reverse_numeric")) cluster_levels <- rev(cluster_levels)
      }
      totals$cluster <- factor(as.character(totals$cluster), levels = cluster_levels)
      totals$value <- if (identical(input$composition_metric %||% "percent", "count")) totals$n else totals$percent
      list(data = totals, split_field = split_field, group_field = group_field)
    })

    output$composition_plot <- renderPlot({
      bundle <- composition_data(); d <- bundle$data
      req(nrow(d))
      mode <- input$composition_mode %||% "stacked"
      if (identical(mode, "alluvial")) {
        validate(need(requireNamespace("ggalluvial", quietly = TRUE), "The ggalluvial package is required for alluvial mode."))
        validate(need(length(unique(as.character(d$group))) >= 2L, "Choose a Group by field with at least two values for alluvial mode."))
      }
      border_color <- input$composition_border_color %||% "#4D4D4D"
      if (!grepl("^#(?:[A-Fa-f0-9]{3}|[A-Fa-f0-9]{6}|[A-Fa-f0-9]{8})$", border_color)) border_color <- "#4D4D4D"
      p <- ggplot(d, aes(x = .data$group, y = .data$value, fill = .data$cluster))
      if (identical(mode, "alluvial")) {
        p <- p +
          ggalluvial::geom_alluvium(aes(stratum = .data$cluster, alluvium = .data$cluster),
            alpha = input$composition_ribbon_alpha %||% .45,
            width = input$composition_ribbon_width %||% .28,
            knot.pos = input$composition_ribbon_waist %||% .22,
            color = scales::alpha("#FFFFFF", input$composition_separator %||% .18), linewidth = input$composition_separator %||% .18) +
          ggalluvial::geom_stratum(aes(stratum = .data$cluster),
            width = input$composition_ribbon_width %||% .28,
            color = border_color, linewidth = input$composition_border_width %||% .15,
            alpha = input$composition_fill_alpha %||% 1)
      } else {
        p <- p + geom_col(width = input$composition_bar_width %||% .85,
          color = border_color, linewidth = input$composition_border_width %||% .15,
          alpha = input$composition_fill_alpha %||% 1)
      }
      colors <- current_cluster_colors()
      if (all(levels(d$cluster) %in% names(colors))) p <- p + scale_fill_manual(values = colors[levels(d$cluster)], drop = FALSE)
      # Agent note: Composition has one independent split dimension; keep its panels in one horizontal row.
      if (nzchar(bundle$split_field)) p <- p + facet_wrap(vars(.data$facet), nrow = 1)
      metric <- input$composition_metric %||% "percent"
      p <- p +
        labs(x = if (nzchar(bundle$group_field)) bundle$group_field else NULL,
          y = if (identical(metric, "count")) "Cells" else "Percentage", fill = "Cluster") +
        theme_minimal(base_family = "Arial", base_size = 11) +
        theme(panel.grid.major.x = element_blank(), panel.grid.minor = element_blank(),
          panel.grid.major.y = element_line(color = "#D7DEE8", linewidth = .35),
          axis.line = element_line(color = "#667085", linewidth = .45),
          axis.ticks = element_line(color = "#667085", linewidth = .4),
          axis.ticks.length = grid::unit(3, "pt"),
          axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
          legend.position = "none", plot.margin = margin(12, 14, 12, 12))
      # Agent note: do not use hard scale limits here; floating-point stack totals can
      # slightly exceed 100 and ggplot would drop the top segment instead of clipping it.
      if (identical(metric, "percent")) p <- p +
        scale_y_continuous(labels = function(x) paste0(x, "%"), expand = expansion(mult = c(0, .03))) +
        coord_cartesian(ylim = c(0, 100), clip = "on")
      p
    }, res = 120)

    heat_matrix <- reactive({
      df <- run_df(); run <- active_run(); req(df, run, "FlowSOM_Cluster" %in% names(df))
      markers <- intersect(run$params$feats %||% active_object()$selected_markers %||% character(), names(df)); req(length(markers))
      signal <- cluster_transform_matrix(as.matrix(df[, markers, drop = FALSE]),
        method = run$params$transform_method %||% "none",
        cofactor = run$params$arcsinh_cofactor %||% 150,
        logicle_auto = run$params$logicle_auto %||% TRUE,
        t = run$params$logicle_t %||% 262144, w = run$params$logicle_w %||% 0.5,
        m = run$params$logicle_m %||% 4.5, a = run$params$logicle_a %||% 0,
        flowjo_channel_range = run$params$flowjo_channel_range %||% 4096,
        flowjo_max_value = run$params$flowjo_max_value %||% 262144,
        flowjo_pos = run$params$flowjo_pos %||% 4.5,
        flowjo_neg = run$params$flowjo_neg %||% 0,
        flowjo_width_basis = run$params$flowjo_width_basis %||% -10,
        flowjo_settings = active_object()$transform_settings %||% list(),
        flowjo_use_wsp = run$params$flowjo_use_wsp %||% TRUE)
      cls <- unique(as.character(df$FlowSOM_Cluster))
      cls <- cls[cluster_natural_order(cls)]
      med <- sapply(cls, function(cl) vapply(seq_along(markers), function(j) stats::median(signal[df$FlowSOM_Cluster == cl, j], na.rm=TRUE), numeric(1)))
      z <- t(med)
      rownames(z) <- cls; colnames(z) <- markers
      norm <- input$heat_normalize %||% "row_z"
      scale_axis <- function(x, margin, minmax = FALSE) {
        apply_fun <- function(v) {
          ok <- is.finite(v); if (!any(ok)) return(rep(NA_real_, length(v)))
          if (minmax) { lo <- min(v[ok]); hi <- max(v[ok]); if (hi == lo) return(replace(v, ok, 0)); return((v - lo) / (hi - lo)) }
          s <- stats::sd(v[ok]); if (!is.finite(s) || s == 0) return(replace(v, ok, 0)); (v - mean(v[ok])) / s
        }
        if (margin == 1L) t(apply(x, 1, apply_fun)) else apply(x, 2, apply_fun)
      }
      z <- switch(norm, row_z = scale_axis(z, 1), row_minmax = scale_axis(z, 1, TRUE),
        col_z = scale_axis(z, 2), col_minmax = scale_axis(z, 2, TRUE), z)
      row_order <- input$heat_row_order %||% active_run()$visualization$heat_row_order %||% character()
      col_order <- input$heat_col_order %||% active_run()$visualization$heat_col_order %||% character()
      row_order <- c(intersect(row_order, rownames(z)), setdiff(rownames(z), row_order))
      col_order <- c(intersect(col_order, colnames(z)), setdiff(colnames(z), col_order))
      z[row_order, col_order, drop = FALSE]
    })

    observeEvent(input$heat_reset_order, {
      session$sendCustomMessage("clusterHeatmap:reset", list(id = ns("cluster_heatmap_grid")))
    })

    output$cluster_heatmap <- renderUI({
      z <- heat_matrix(); req(length(z))
      finite <- z[is.finite(z)]; rng <- if (length(finite)) range(finite) else c(0, 1)
      if (diff(rng) == 0) rng <- rng + c(-.5, .5)
      sky <- c("#304B9A", "#256FCC", "#4793EC", "#7AC5FE", "#95DAFE", "#BCCBF6", "#FEA4C4", "#FEAA9B", "#FEA364", "#FCF7CF")
      ramp <- grDevices::colorRampPalette(sky)(256)
      color_for <- function(x) if (!is.finite(x)) "#9CA3AF" else ramp[pmax(1L, pmin(256L, 1L + floor(255 * (x - rng[1]) / diff(rng))))]
      cols <- current_cluster_colors()
      label_chars <- max(1L, nchar(rownames(z), type = "width"), na.rm = TRUE)
      label_width <- max(52L, min(124L, 25L + 7L * label_chars))
      header <- tags$div(class = "cluster-heat-row cluster-heat-header",
        tags$button(type = "button", class = "cluster-heat-corner cluster-heat-axis-label cluster-heat-sort-clusters",
          `data-axis` = "cluster", title = "Click to sort cluster IDs", "Cluster"),
        lapply(colnames(z), function(marker) tags$button(type = "button", draggable = "true", class = "cluster-heat-axis-label cluster-heat-col-label", `data-axis` = "col", `data-key` = marker, title = marker, tags$span(marker))))
      rows <- lapply(seq_len(nrow(z)), function(i) {
        cluster <- rownames(z)[i]
        tags$div(class = "cluster-heat-row", `data-row-key` = cluster,
          tags$button(type = "button", draggable = "true", class = "cluster-heat-axis-label cluster-heat-row-label", `data-axis` = "row", `data-key` = cluster, title = cluster,
            tags$span(class = "cluster-heat-cluster-dot", style = sprintf("--cluster-color:%s", cluster_color_value(cols, cluster))), tags$span(class = "cluster-heat-name", cluster)),
          lapply(seq_len(ncol(z)), function(j) tags$div(class = "cluster-heat-cell", `data-row-key` = cluster, `data-col-key` = colnames(z)[j], `data-value` = if (is.finite(z[i,j])) format(z[i,j], scientific = FALSE, digits = 15) else "NaN", style = sprintf("--heat-color:%s", color_for(z[i,j]))))
        )
      })
      tagList(
        div(id = ns("cluster_heatmap_grid"), class = paste("cluster-heatmap-grid", if (ncol(z) > 8) "heat-many-cols" else ""), style = sprintf("--heat-cols:%d;--cluster-label-width:%dpx", ncol(z), label_width),
          div(class = "cluster-heat-scroll", header, rows)),
        div(class = "cluster-heat-legend",
          span(format(rng[1], digits = 3)), span(class = "cluster-heat-gradient"), span(format(rng[2], digits = 3)),
          span(class = "cluster-heat-missing", tags$i(), "Missing"))
      )
    })

    observeEvent(input$upgrade_3d, {
      obj <- active_object(); run <- active_run(); df <- run_df(); req(obj, run, df)
      feats <- intersect(run$params$feats %||% obj$selected_markers, names(df))
      if (length(feats) < 2) { showNotification("At least two original clustering markers are required to upgrade this run.", type="error"); return() }
      mat <- cluster_transform_matrix(as.matrix(df[, feats, drop = FALSE]),
        method = run$params$transform_method %||% "none",
        cofactor = run$params$arcsinh_cofactor %||% 150,
        logicle_auto = run$params$logicle_auto %||% TRUE,
        t = run$params$logicle_t %||% 262144, w = run$params$logicle_w %||% 0.5,
        m = run$params$logicle_m %||% 4.5, a = run$params$logicle_a %||% 0,
        flowjo_channel_range = run$params$flowjo_channel_range %||% 4096,
        flowjo_max_value = run$params$flowjo_max_value %||% 262144,
        flowjo_pos = run$params$flowjo_pos %||% 4.5,
        flowjo_neg = run$params$flowjo_neg %||% 0,
        flowjo_width_basis = run$params$flowjo_width_basis %||% -10,
        flowjo_settings = obj$transform_settings %||% list(),
        flowjo_use_wsp = run$params$flowjo_use_wsp %||% TRUE)
      mat <- cluster_scale_matrix(mat, run$params$scale_method %||% if (isTRUE(run$params$scale)) "zscore" else "none")
      mat[!is.finite(mat)] <- 0
      set.seed(as.integer(run$params$seed %||% 42))
      um <- uwot::umap(mat, n_neighbors=as.integer(run$params$neighbors %||% 15), min_dist=as.numeric(run$params$min_dist %||% .3), metric=run$params$metric %||% "euclidean", n_components=3L, verbose=FALSE)
      out <- df; out[,c("UMAP_1","UMAP_2","UMAP_3")] <- um
      name <- cluster_available_name(paste0(run$name %||% "Cluster Run", " — 3D"), vapply(obj$runs, function(x) x$name %||% "", character(1)))
      shared$analysis$add_run(name=name, method=paste(run$method %||% "Cluster", "3D upgrade"), result=list(df=out), params=modifyList(run$params %||% list(), list(n_components=3L)))
      showNotification("Created a 3D upgraded copy.", type="message")
    })

    output$export_ui <- renderUI({
      obj <- active_object()
      if (is.null(obj)) {
        return(div(class = "Pinhki-inline-note", tags$b("Nothing to export. "), "Create or import an analysis object first."))
      }
      bundle_button <- downloadButton(ns("download_bundle"), "Download Analysis Bundle", class = "btn btn-primary")
      if (is.null(active_run())) {
        return(tagList(bundle_button,
          div(class = "Pinhki-control-note", "Create or select a clustering run to enable plot and data exports.")))
      }
      tagList(bundle_button,
        downloadButton(ns("download_plot"), "Combined plot PNG"), downloadButton(ns("download_plot_pdf"), "Combined plot PDF"),
        downloadButton(ns("download_plot_data"), "Plotted data CSV"), downloadButton(ns("download_panels"), "Individual panels ZIP"))
    })

    output$download_plot <- downloadHandler(filename=function() "cluster_umap.png", content=function(file) {
      # Agent note: Shiny supplies an extensionless temporary path; specify the
      # device explicitly so ggsave does not fail before the browser download starts.
      ggplot2::ggsave(file, plot = build_2d_plot(), device = "png", width = 10, height = 8, dpi = 300)
    })
    output$download_plot_pdf <- downloadHandler(filename = function() "cluster_umap.pdf", content = function(file) {
      # Keep PDF export explicit as well; the Shiny target path has no extension.
      ggplot2::ggsave(file, plot = build_2d_plot(), device = grDevices::pdf, width = 10, height = 8, units = "in")
    })
    output$download_plot_data <- downloadHandler(filename = function() "cluster_umap_data.csv", content = function(file) {
      df <- run_df()
      req(df, nrow(df) > 0)
      utils::write.csv(as.data.frame(df, stringsAsFactors = FALSE), file, row.names = FALSE)
    })
    output$download_panels <- downloadHandler(filename = function() "cluster_umap_panels.zip", content = function(file) {
      df <- run_df()
      req(df, nrow(df) > 0)
      structure <- applied_structure()
      split <- unique(c(structure$split_row %||% "", structure$split_col %||% ""))
      split <- intersect(split[nzchar(split)], names(df))
      tmp <- tempfile("cluster-panels-")
      dir.create(tmp, recursive = TRUE, showWarnings = FALSE)
      on.exit(unlink(tmp, recursive = TRUE, force = TRUE), add = TRUE)
      if (!length(split)) {
        ggplot2::ggsave(file.path(tmp, "all.png"), plot = build_2d_plot(), device = "png", width = 8, height = 7, dpi = 300)
      } else {
        keys <- unique(df[, split, drop = FALSE])
        panel_i <- 0L
        for (i in seq_len(nrow(keys))) {
          keep <- rep(TRUE, nrow(df))
          for (nm in split) {
            value <- keys[[nm]][i]
            if (is.na(value)) keep <- keep & is.na(df[[nm]])
            else keep <- keep & !is.na(df[[nm]]) & as.character(df[[nm]]) == as.character(value)
          }
          if (!any(keep)) next
          panel_i <- panel_i + 1L
          p <- build_2d_plot() %+% df[keep, , drop = FALSE]
          ggplot2::ggsave(file.path(tmp, paste0("panel_", panel_i, ".png")), plot = p, device = "png", width = 8, height = 7, dpi = 300)
        }
      }
      old <- setwd(tmp)
      on.exit(setwd(old), add = TRUE)
      zip_file <- paste0(file, ".zip")
      utils::zip(zipfile = zip_file, files = list.files(tmp, all.files = FALSE))
      if (!file.rename(zip_file, file)) stop("Could not finalize the panel ZIP download.")
    })

    output$download_bundle <- downloadHandler(
      filename = function() paste0("analysis_object_", Sys.Date(), ".zip"),
      content = function(file) shared$analysis$export_bundle(file = file)
    )
  })
}
