flowjo_pipeline_page_ui <- function(id) {
  ns <- NS(id)
  div(
    class = "card-page-wrap",
    card_page_header(
      "Population Isolation + Concatenation",
      subtitle = "FlowJo-style preprocessing, isolate populations with checkboxes, concatenate selected cells, then cluster."
    ),
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
          "Isolation Filters",
          uiOutput(ns("sample_picker")),
          uiOutput(ns("group_col_picker")),
          uiOutput(ns("group_value_picker")),
          actionButton(ns("apply_isolation"), "Apply Isolation", class = "btn btn-secondary"),
          div(style = "height:8px"),
          DT::dataTableOutput(ns("isolation_counts"))
        ),
        card_page_card(
          "Concatenation",
          radioButtons(
            ns("concat_mode"),
            "Concatenate mode",
            choices = c("Balanced per sample" = "balanced", "All cells" = "all"),
            selected = "balanced",
            inline = TRUE
          ),
          numericInput(ns("balanced_n"), "Balanced cells per sample", value = 5000, min = 500, step = 500),
          actionButton(ns("build_concat"), "Build Concatenated Object", class = "btn btn-success"),
          div(style = "height:8px"),
          verbatimTextOutput(ns("concat_status"))
        )
      ),
      column(
        8,
        card_page_card(
          "Clustering",
          uiOutput(ns("marker_picker")),
          numericInput(ns("cluster_seed"), "Seed", value = 42, min = 1, step = 1),
          numericInput(ns("cluster_k"), "Target meta-clusters (k)", value = 20, min = 2, step = 1),
          actionButton(ns("run_cluster"), "Run FlowSOM + PhenoGraph", class = "btn btn-primary"),
          div(style = "height:8px"),
          plotOutput(ns("cluster_umap"), height = "520px"),
          hr(),
          h5("Sample-level frequencies"),
          DT::dataTableOutput(ns("cluster_freq")),
          hr(),
          h5("Cluster marker summary"),
          DT::dataTableOutput(ns("cluster_marker_summary"))
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

    sample_table <- reactive({
      tbl <- shared$fcs$table()
      if (nrow(tbl) == 0) return(NULL)
      tbl
    })

    output$sample_picker <- renderUI({
      tbl <- sample_table()
      if (is.null(tbl)) return(helpText("Add FCS files from the right sidebar first."))
      checkboxGroupInput(
        ns("selected_samples"),
        "Samples",
        choices = stats::setNames(tbl$id, tbl$name),
        selected = tbl$id
      )
    })

    output$group_col_picker <- renderUI({
      req(shared$pipeline$preprocess_result())
      df <- shared$pipeline$preprocess_result()$transformed
      likely <- names(df)[grepl("sample|group|condition|batch|donor|patient|subject|time", names(df), ignore.case = TRUE)]
      if (!length(likely)) likely <- names(df)
      selectInput(ns("group_col"), "Group column", choices = likely, selected = likely[1])
    })

    output$group_value_picker <- renderUI({
      req(shared$pipeline$preprocess_result(), input$group_col)
      df <- shared$pipeline$preprocess_result()$transformed
      vals <- unique(as.character(df[[input$group_col]]))
      checkboxGroupInput(ns("group_vals"), "Group values", choices = vals, selected = vals)
    })

    observeEvent(input$run_preprocess, {
      reg <- shared$fcs$registry()
      req(length(reg) > 0)
      ids <- input$selected_samples %||% names(reg)
      ids <- intersect(ids, names(reg))
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

    observeEvent(input$apply_isolation, {
      req(shared$pipeline$preprocess_result())
      df <- shared$pipeline$preprocess_result()$transformed
      sample_ids <- input$selected_samples %||% unique(df$sample_id)
      gcol <- input$group_col %||% "sample_name"
      gvals <- input$group_vals %||% unique(as.character(df[[gcol]]))

      keep <- df$sample_id %in% sample_ids & as.character(df[[gcol]]) %in% as.character(gvals)
      iso <- df[keep, , drop = FALSE]
      iso$selected_population <- paste0(gcol, "=", as.character(iso[[gcol]]))

      shared$pipeline$isolated_populations(list(
        data = iso,
        group_col = gcol,
        group_values = gvals,
        sample_ids = sample_ids
      ))
    })

    output$isolation_counts <- DT::renderDataTable({
      iso <- shared$pipeline$isolated_populations()
      if (is.null(iso)) return(NULL)
      iso$data |>
        dplyr::count(sample_name, selected_population, name = "cells") |>
        DT::datatable(options = list(pageLength = 8, scrollX = TRUE), rownames = FALSE)
    })

    observeEvent(input$build_concat, {
      iso <- shared$pipeline$isolated_populations()
      req(!is.null(iso), nrow(iso$data) > 0)

      dat <- iso$data
      mode <- input$concat_mode %||% "balanced"
      if (identical(mode, "balanced")) {
        n_target <- as.integer(input$balanced_n %||% 5000)
        dat <- dat |>
          dplyr::group_by(sample_id) |>
          dplyr::slice_sample(n = min(dplyr::n(), n_target)) |>
          dplyr::ungroup()
      }

      num_cols <- names(dat)[vapply(dat, is.numeric, logical(1))]
      drop_cols <- c("event_id")
      marker_cols <- setdiff(num_cols, drop_cols)

      shared$pipeline$concat_object(flowjo_build_concat_object(dat, marker_cols = marker_cols))
      shared$pipeline$concat_markers(marker_cols)
      shared$pipeline$concat_data(dat)
    })

    output$concat_status <- renderText({
      sce <- shared$pipeline$concat_object()
      if (is.null(sce)) return("Not built")
      sprintf("Cells: %d\nMarkers: %d", ncol(sce), nrow(sce))
    })

    output$marker_picker <- renderUI({
      req(shared$pipeline$concat_markers())
      mk <- shared$pipeline$concat_markers()
      checkboxGroupInput(ns("cluster_markers"), "Markers for clustering", choices = mk, selected = mk)
    })

    observeEvent(input$run_cluster, {
      dat <- shared$pipeline$concat_data()
      req(!is.null(dat), nrow(dat) > 10)
      mk <- intersect(input$cluster_markers %||% character(0), names(dat))
      req(length(mk) >= 2)

      cl <- flowjo_cluster_concat(dat, marker_cols = mk, seed = as.integer(input$cluster_seed %||% 42), k = as.integer(input$cluster_k %||% 20))
      shared$pipeline$cluster_result(cl)
    })

    output$cluster_umap <- renderPlot({
      cl <- shared$pipeline$cluster_result()
      req(cl)
      ggplot2::ggplot(cl$data, ggplot2::aes(UMAP_1, UMAP_2, color = FlowSOM_Cluster)) +
        ggplot2::geom_point(size = 0.5, alpha = 0.7) +
        ggplot2::theme_minimal(base_size = 11) +
        ggplot2::labs(title = "Concatenated clustering", color = "FlowSOM")
    })

    output$cluster_freq <- DT::renderDataTable({
      cl <- shared$pipeline$cluster_result()
      req(cl)
      DT::datatable(cl$sample_freq, options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE)
    })

    output$cluster_marker_summary <- DT::renderDataTable({
      cl <- shared$pipeline$cluster_result()
      req(cl)
      DT::datatable(cl$marker_summary, options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE)
    })

    output$parity_table <- DT::renderDataTable({
      p <- shared$pipeline$parity_result()
      if (is.null(p)) return(NULL)
      DT::datatable(p, options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE)
    })
  })
}
