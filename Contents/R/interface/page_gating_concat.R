flowjo_pipeline_page_ui <- function(id) {
  ns <- NS(id)
  div(
    class = "card-page-wrap",
    card_page_header(
      "Batch Concatenation",
      subtitle = "FlowJo-style preprocessing, isolate populations with checkboxes, then concatenate selected cells for analysis."
    ),
    tags$style(".flowjo-umap-scroll{max-height:72vh;overflow:auto}.flowjo-umap-scroll .shiny-plot-output{min-width:720px}"),
    fluidRow(
      column(
        4,
        card_page_card(
          "Preprocess",
          actionButton(ns("run_preprocess"), "Run Preprocess (Comp + Logicle)", class = "btn btn-primary"),
          div(style = "height:8px"),
          verbatimTextOutput(ns("preprocess_status"))
        ),
        card_page_card(
          "Population Basket",
          DT::dataTableOutput(ns("basket_counts")),
          div(style = "height:8px"),
          downloadButton(ns("export_concat"), "Export FCS + Mapping", class = "btn btn-outline-secondary w-100")
        ),
        card_page_card(
          "Concatenation",
          radioButtons(
            ns("concat_mode"),
            "Concatenate mode",
            choices = c("Cap per population" = "cap", "All events" = "all"),
            selected = "cap",
            inline = TRUE
          ),
          numericInput(ns("balanced_n"), "Event cap per sample + gate", value = 5000, min = 100, step = 500),
          actionButton(ns("build_concat"), "Build Concatenated Object", class = "btn btn-success"),
          div(style = "height:8px"),
          verbatimTextOutput(ns("concat_status"))
        )
      ),
      column(
        8,
        card_page_card(
          "Analysis Handoff",
          uiOutput(ns("analysis_handoff"))
        ),
        card_page_card(
          "Parity Validation (Internal)",
          DT::dataTableOutput(ns("parity_table"))
        )
      )
    )
  )
}

flowjo_pipeline_page_server <- function(id, shared) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    table_order_source <- reactiveVal("dropdown sort")

    sample_table <- reactive(shared$fcs$table())

    observeEvent(input$run_preprocess, {
      reg <- shared$fcs$registry()
      req(length(reg) > 0)
      ids <- names(reg)
      req(length(ids) > 0)

      withProgress(message = "Preprocessing selected FCS", value = 0, {
        per_sample <- list()
        all_rows <- list()
        for (i in seq_along(ids)) {
          id_i <- ids[[i]]
          e <- reg[[id_i]]
          pr <- flowjo_preprocess_entry(e, mode = "exact")
          per_sample[[id_i]] <- pr

          d <- pr$transformed
          d$sample_id <- id_i
          d$sample_name <- e$name %||% id_i
          d$event_id <- seq_len(nrow(d))
          d$condition <- e$sample_meta$condition %||% NA_character_
          d$batch <- e$sample_meta$batch %||% NA_character_
          d$timepoint <- e$sample_meta$timepoint %||% NA_character_
          d$subject <- e$sample_meta$subject %||% NA_character_
          all_rows[[id_i]] <- d
          incProgress(1 / length(ids), detail = paste("Processed", e$name %||% id_i))
        }

        merged <- data.table::rbindlist(all_rows, use.names = TRUE, fill = TRUE)
        shared$pipeline$preprocess_result(list(
          mode = "exact",
          per_sample = per_sample,
          raw = lapply(per_sample, `[[`, "raw"),
          compensated = lapply(per_sample, `[[`, "compensated"),
          transformed = as.data.frame(merged)
        ))

        first_id <- ids[[1]]
        first_pr <- per_sample[[first_id]]
        parity <- flowjo_parity_metrics(first_pr$raw, first_pr$transformed, intersect(colnames(first_pr$raw), colnames(first_pr$transformed)))
        shared$pipeline$parity_result(parity)
      })
    })

    output$preprocess_status <- renderText({
      pr <- shared$pipeline$preprocess_result()
      if (is.null(pr)) return("Not run")
      sprintf("Mode: %s\nSamples: %d\nRows: %d\nColumns: %d", pr$mode, length(pr$per_sample), nrow(pr$transformed), ncol(pr$transformed))
    })

    output$basket_counts <- DT::renderDataTable({
      tbl <- shared$populations$table()
      if (!nrow(tbl)) return(NULL)
      DT::datatable(tbl[, c("include", "population_set", "label", "sample_name", "events", "percent_parent", "sync_status"), drop = FALSE],
        options = list(pageLength = 8, scrollX = TRUE), rownames = FALSE)
    })

    observeEvent(input$build_concat, {
      cc <- flowjo_population_concat(shared$fcs$registry(), shared$gating$gates(), shared$gating$memberships(), shared$populations$items(), mode = input$concat_mode %||% "cap", cap = input$balanced_n %||% 5000L)
      dat <- cc$data
      marker_cols <- cc$markers
      shared$pipeline$isolated_populations(list(mapping = cc$mapping))
      shared$pipeline$concat_object(flowjo_build_concat_object(dat, marker_cols = marker_cols))
      shared$pipeline$concat_markers(marker_cols)
      shared$pipeline$concat_data(dat)
    })

    output$concat_status <- renderText({
      sce <- shared$pipeline$concat_object()
      if (is.null(sce)) return("Not built")
      sprintf("Cells: %d\nMarkers: %d", ncol(sce), nrow(sce))
    })

    output$analysis_handoff <- renderUI({
      dat <- shared$pipeline$concat_data()
      if (is.null(dat)) {
        return(div(class = "Pinhki-inline-note", tags$b("Build a concatenated object first. "), "After batch concatenation is ready, create a named analysis object for Cluster runs."))
      }
      tagList(
        div(class = "Pinhki-inline-note",
          tags$b("Batch object ready. "),
          format(nrow(dat), big.mark = ","), " events can be saved as an analysis object."
        ),
        actionButton(ns("create_analysis_object"), "Create Analysis Object", class = "btn btn-primary")
      )
    })

    observeEvent(input$create_analysis_object, {
      dat <- shared$pipeline$concat_data()
      req(dat)
      showModal(modalDialog(
        title = "Create Analysis Object",
        textInput(ns("batch_analysis_name"), "Object name", value = ""),
        div(class = "Pinhki-inline-note", tags$b("Snapshot saved. "), "The analysis object keeps a full copy of this batch concatenation."),
        footer = tagList(modalButton("Cancel"), actionButton(ns("confirm_batch_analysis"), "Create Object", class = "btn btn-primary")),
        easyClose = TRUE
      ))
    })

    observeEvent(input$confirm_batch_analysis, {
      name <- trimws(input$batch_analysis_name %||% "")
      if (!nzchar(name)) {
        showNotification("Object name is required.", type = "warning")
        return()
      }
      tryCatch({
        obj <- shared$analysis$create(name = name, source = "batch")
        removeModal()
        shiny::updateNavbarPage(session = shared$session, inputId = "main_nav", selected = "umap_tab")
        showNotification(sprintf("Created analysis object: %s", obj$name), type = "message")
      }, error = function(e) {
        showNotification(conditionMessage(e), type = "error", duration = 10)
      })
    })

    output$marker_picker <- renderUI({
      req(shared$pipeline$concat_markers())
      mk <- shared$pipeline$concat_markers()
      checkboxGroupInput(ns("cluster_markers"), "Markers for clustering", choices = mk, selected = flowjo_default_cluster_markers(mk))
    })

    current_cluster_signature <- reactive({
      dat <- shared$pipeline$concat_data()
      req(dat)
      list(
        marker_cols = intersect(input$cluster_markers %||% character(0), names(dat)),
        seed = as.integer(input$cluster_seed %||% 42),
        xdim = as.integer(input$cluster_xdim %||% 5),
        ydim = as.integer(input$cluster_ydim %||% 5),
        rlen = as.integer(input$cluster_rlen %||% 10),
        row_count = nrow(dat),
        columns = names(dat),
        metadata_counts = vapply(intersect(c("sample_name", "sample_id", "gate_name", "gate_path", "population_label", "condition"), names(dat)),
                                 function(x) length(unique(dat[[x]])), integer(1))
      )
    })

    recluster_ready <- reactive({
      cl <- shared$pipeline$cluster_result()
      if (is.null(cl) || is.null(cl$run_signature) || is.null(cl$flowsom$codes)) return(FALSE)
      sig <- current_cluster_signature()
      old <- cl$run_signature
      identical(old$marker_cols, sig$marker_cols) &&
        identical(old$seed, sig$seed) &&
        identical(old$xdim, sig$xdim) &&
        identical(old$ydim, sig$ydim) &&
        identical(old$rlen, sig$rlen) &&
        identical(old$row_count, sig$row_count) &&
        identical(old$columns, sig$columns) &&
        identical(old$metadata_counts, sig$metadata_counts)
    })

    output$recluster_button <- renderUI({
      actionButton(ns("recluster_cluster"), "Recluster same UMAP", class = "btn btn-outline-primary", disabled = if (isTRUE(recluster_ready())) NULL else "disabled")
    })

    output$recluster_hint <- renderUI({
      if (isTRUE(recluster_ready())) return(NULL)
      helpText("Selected markers or input changed. Run FlowSOM + UMAP again to create a new embedding.")
    })

    output$export_concat <- downloadHandler(
      filename = function() paste0("flowjo_concatenated_populations_", Sys.Date(), ".zip"),
      content = function(file) {
        dat <- shared$pipeline$concat_data()
        mk <- shared$pipeline$concat_markers()
        map <- shared$pipeline$isolated_populations()$mapping
        if (is.null(dat)) {
          cc <- flowjo_population_concat(shared$fcs$registry(), shared$gating$gates(), shared$gating$memberships(), shared$populations$items(), mode = input$concat_mode %||% "cap", cap = input$balanced_n %||% 5000L)
          dat <- cc$data; mk <- cc$markers; map <- cc$mapping
        }
        flowjo_export_population_fcs_zip(dat, mk, map, file)
      }
    )

    observeEvent(input$run_cluster, {
      dat <- shared$pipeline$concat_data()
      req(!is.null(dat), nrow(dat) > 10)
      mk <- intersect(input$cluster_markers %||% character(0), names(dat))
      req(length(mk) >= 2)

      cl <- flowjo_cluster_concat(
        dat,
        marker_cols = mk,
        seed = as.integer(input$cluster_seed %||% 42),
        k = as.integer(input$cluster_k %||% 20),
        xdim = as.integer(input$cluster_xdim %||% 5),
        ydim = as.integer(input$cluster_ydim %||% 5),
        rlen = as.integer(input$cluster_rlen %||% 10)
      )
      shared$pipeline$cluster_result(cl)
    })

    observeEvent(input$recluster_cluster, {
      req(isTRUE(recluster_ready()))
      cl <- flowjo_recluster_concat(shared$pipeline$cluster_result(), k = as.integer(input$cluster_k %||% 20), seed = as.integer(input$cluster_seed %||% 42))
      shared$pipeline$cluster_result(cl)
    })

    cluster_display_data <- reactive({
      cl <- shared$pipeline$cluster_result()
      req(cl)
      d <- cl$data
      if (!"population_label" %in% names(d)) d$population_label <- d$gate_name %||% "Population"
      d$population_label <- ifelse(is.na(d$population_label) | !nzchar(d$population_label), sub(".*/", "", d$gate_path %||% d$gate_name), d$population_label)
      d$.hover <- sprintf(
        "Sample: %s<br>Population: %s<br>Gate path: %s<br>Cluster: %s<br>UMAP 1: %.3f<br>UMAP 2: %.3f<br>UMAP 3: %.3f",
        d$sample_name %||% "", d$population_label %||% "", d$gate_path %||% "", d$FlowSOM_Cluster, d$UMAP_1, d$UMAP_2, d$UMAP_3
      )
      mode <- input$cluster_umap_downsample_mode %||% "auto"
      limit <- if (identical(mode, "auto")) 25000L else if (identical(mode, "n")) as.integer(input$cluster_umap_downsample_n %||% 20000L) else nrow(d)
      if (!identical(mode, "all") && nrow(d) > limit) {
        set.seed(1729L)
        grp <- interaction(d$population_label, d$sample_name %||% "", drop = TRUE)
        keep <- unlist(tapply(seq_len(nrow(d)), grp, function(ii) sample(ii, max(1L, floor(length(ii) * limit / nrow(d))))))
        keep <- unique(keep)[seq_len(min(length(unique(keep)), limit))]
        attr(d[keep, , drop = FALSE], "downsampled") <- TRUE
        return(d[keep, , drop = FALSE])
      }
      attr(d, "downsampled") <- FALSE
      d
    })

    output$cluster_downsample_note <- renderUI({
      d <- cluster_display_data()
      if (!isTRUE(attr(d, "downsampled"))) return(NULL)
      div(class = "text-muted small", "UMAP display is downsampled by population for visualization only. Analysis summaries still use all events.")
    })

    output$cluster_umap_panel <- renderUI({
      req(input$cluster_umap_view_mode)
      if (identical(input$cluster_umap_view_mode, "3d")) {
        plotly::plotlyOutput(ns("cluster_umap3d"), height = "560px")
      } else {
        plotOutput(ns("cluster_umap"), height = "620px")
      }
    })

    output$cluster_umap <- renderPlot({
      d <- cluster_display_data()
      req(nrow(d) > 0)
      split_by <- input$cluster_split_by %||% "population_label"
      p <- ggplot2::ggplot(d, ggplot2::aes(UMAP_1, UMAP_2, color = FlowSOM_Cluster)) +
        ggplot2::geom_point(size = 0.5, alpha = 0.7) +
        ggplot2::coord_equal() +
        ggplot2::theme_minimal(base_size = 11) +
        ggplot2::labs(title = "Concatenated clustering", color = "FlowSOM", x = "UMAP 1", y = "UMAP 2")
      row_split <- input$cluster_split_rows %||% "none"
      col_split <- input$cluster_split_cols %||% "none"
      if (!row_split %in% names(d)) row_split <- "none"
      if (!col_split %in% names(d)) col_split <- "none"
      if (!split_by %in% names(d)) split_by <- "none"
      if (!identical(row_split, "none") || !identical(col_split, "none")) {
        p + ggplot2::facet_grid(stats::as.formula(sprintf("%s ~ %s", if (identical(row_split, "none")) "." else row_split, if (identical(col_split, "none")) "." else col_split)))
      } else if (!identical(split_by, "none") && split_by %in% names(d)) {
        p + ggplot2::facet_wrap(stats::as.formula(paste("~", split_by)))
      } else {
        p
      }
    })

    output$cluster_umap3d <- plotly::renderPlotly({
      d <- cluster_display_data()
      req(nrow(d) > 0)
      plotly::plot_ly(
        d, x = ~UMAP_1, y = ~UMAP_2, z = ~UMAP_3, color = ~FlowSOM_Cluster,
        text = ~.hover, hoverinfo = "text", type = "scatter3d", mode = "markers",
        marker = list(size = 2, opacity = 0.75)
      ) |>
        plotly::layout(scene = list(xaxis = list(title = "UMAP 1"), yaxis = list(title = "UMAP 2"), zaxis = list(title = "UMAP 3")))
    })

    output$cluster_freq <- DT::renderDataTable({
      cl <- shared$pipeline$cluster_result()
      req(cl)
      DT::datatable(cl$sample_freq, options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE)
    })

    observeEvent(input$cluster_order, table_order_source("dropdown sort"), ignoreInit = TRUE)
    observeEvent(input$cluster_marker_summary_rows_current, table_order_source("table sort"), ignoreInit = TRUE)

    output$cluster_marker_help <- renderText({
      switch(input$cluster_marker_view %||% "median",
             median = "Transformed-but-unscaled cluster median. Closest MFI-like view.",
             delta_parent = "Cluster median minus selected parent baseline. Useful when markers are high everywhere but still differ.",
             pct_parent = "Percent shift from selected parent baseline.",
             iqr = "Within-cluster spread. Helps judge whether a cluster is tight or heterogeneous.")
    })

    marker_summary_long <- reactive({
      cl <- shared$pipeline$cluster_result()
      req(cl)
      flowjo_cluster_summary(cl$data, cl$marker_values, cl$marker_cols, baseline = input$cluster_parent_baseline %||% "all_analysis_cells")
    })

    dropdown_cluster_order <- reactive({
      cl <- shared$pipeline$cluster_result()
      sm <- marker_summary_long()
      req(cl, nrow(sm) > 0)
      clusters <- unique(as.character(cl$data$FlowSOM_Cluster))
      numeric_order <- clusters[order(suppressWarnings(as.numeric(as.character(clusters))), clusters, na.last = TRUE)]
      cent <- cl$data |>
        dplyr::group_by(FlowSOM_Cluster) |>
        dplyr::summarise(UMAP_1 = stats::median(UMAP_1, na.rm = TRUE), UMAP_2 = stats::median(UMAP_2, na.rm = TRUE), .groups = "drop")
      out <- tryCatch({
        switch(input$cluster_order %||% "umap_proximity",
               umap_xy = as.character(cent$FlowSOM_Cluster[order(cent$UMAP_1, cent$UMAP_2)]),
               umap_hclust = as.character(cent$FlowSOM_Cluster[stats::hclust(stats::dist(cent[, c("UMAP_1", "UMAP_2")]))$order]),
               marker_hclust = {
                 wide <- tidyr::pivot_wider(sm, id_cols = FlowSOM_Cluster, names_from = marker, values_from = median)
                 mat <- as.matrix(wide[, setdiff(names(wide), "FlowSOM_Cluster"), drop = FALSE])
                 mat[!is.finite(mat)] <- 0
                 as.character(wide$FlowSOM_Cluster[stats::hclust(stats::dist(mat))$order])
               },
               umap_proximity = {
                 z <- stats::cmdscale(stats::dist(cent[, c("UMAP_1", "UMAP_2")]), k = 1)
                 as.character(cent$FlowSOM_Cluster[order(z[, 1])])
               },
               cluster_number = numeric_order,
               numeric_order)
      }, error = function(e) numeric_order)
      unique(c(out, numeric_order))
    })

    cluster_marker_wide <- reactive({
      sm <- marker_summary_long()
      cl <- shared$pipeline$cluster_result()
      view <- input$cluster_marker_view %||% "median"
      req(nrow(sm) > 0, view %in% names(sm))
      sm$value <- sm[[view]]
      order_vec <- dropdown_cluster_order()
      wide <- sm |>
        dplyr::mutate(FlowSOM_Cluster = factor(as.character(FlowSOM_Cluster), levels = order_vec)) |>
        dplyr::arrange(FlowSOM_Cluster) |>
        dplyr::select(FlowSOM_Cluster, marker, value) |>
        tidyr::pivot_wider(names_from = marker, values_from = value)
      markers <- intersect(cl$marker_cols, names(wide))
      wide <- wide[, c("FlowSOM_Cluster", markers), drop = FALSE]
      rows_current <- input$cluster_marker_summary_rows_current
      heat_order <- as.character(wide$FlowSOM_Cluster)
      if (identical(table_order_source(), "table sort") && length(rows_current)) {
        rows_current <- rows_current[rows_current <= nrow(wide)]
        heat_order <- as.character(wide$FlowSOM_Cluster[rows_current])
      }
      list(table = wide, heat_order = heat_order, long = sm, view = view, markers = markers)
    })

    output$cluster_order_status <- renderText({
      paste("Current order:", table_order_source())
    })

    output$cluster_marker_heatmap <- plotly::renderPlotly({
      x <- cluster_marker_wide()
      req(length(x$markers) > 0, length(x$heat_order) > 0)
      sm <- x$long |>
        dplyr::mutate(FlowSOM_Cluster = factor(as.character(FlowSOM_Cluster), levels = x$heat_order)) |>
        dplyr::filter(!is.na(FlowSOM_Cluster), marker %in% x$markers) |>
        dplyr::arrange(FlowSOM_Cluster, match(marker, x$markers))
      z <- matrix(sm[[x$view]], nrow = length(x$heat_order), byrow = TRUE)
      hover <- matrix(sprintf(
        "Cluster: %s<br>Marker: %s<br>%s: %.1f<br>Absolute median: %.1f<br>Delta from parent: %.1f<br>Percent change from parent: %.1f%%<br>IQR: %.1f<br>n: %d<br>Parent baseline: %s",
        sm$FlowSOM_Cluster, sm$marker, x$view, sm[[x$view]], sm$median, sm$delta_parent, sm$pct_parent, sm$iqr, sm$n, input$cluster_parent_baseline %||% "all_analysis_cells"
      ), nrow = length(x$heat_order), byrow = TRUE)
      colors <- if (x$view %in% c("delta_parent", "pct_parent")) "RdBu" else "Viridis"
      plotly::plot_ly(x = x$markers, y = x$heat_order, z = z, text = hover, hoverinfo = "text", type = "heatmap", colors = colors) |>
        plotly::layout(xaxis = list(title = ""), yaxis = list(title = "Cluster"))
    })

    output$cluster_marker_summary <- DT::renderDataTable({
      wide <- cluster_marker_wide()
      value_cols <- setdiff(names(wide$table), "FlowSOM_Cluster")
      display <- wide$table
      if (identical(wide$view, "pct_parent")) {
        display[value_cols] <- lapply(display[value_cols], function(x) sprintf("%.1f%%", x))
      } else {
        display[value_cols] <- lapply(display[value_cols], function(x) round(x, 1))
      }
      DT::datatable(display, options = list(pageLength = 20, scrollX = TRUE), rownames = FALSE, selection = "none")
    })

    output$parity_table <- DT::renderDataTable({
      p <- shared$pipeline$parity_result()
      if (is.null(p)) return(NULL)
      DT::datatable(p, options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE)
    })
  })
}
