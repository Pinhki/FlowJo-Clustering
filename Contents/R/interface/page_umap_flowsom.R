# R/page_umap_flowsom.R
umap_flowsom_page_ui <- function(id) {
  ns <- NS(id)
  div(class = "card-page-wrap",
    card_page_header(
      "UMAP + FlowSOM (Compute)",
      subtitle = "Pick channels, compute UMAP with uwot, cluster with FlowSOM (no Seurat), then optionally send to FlowSOM page."
    ),
    uiOutput(ns("gate_notice")),
    fluidRow(
      column(3, card_page_card("Run Configuration",
             tags$div(class = "card-subsection-title", "Features & UMAP"),
             uiOutput(ns("channels_picker")),
             p(class = "control-note", "Choose markers used to build the UMAP map. More relevant markers usually give clearer separation."),
             numericInput(ns("seed"), "Seed", value = 42, min = 1, step = 1),
             p(class = "control-note", "Recommended seed: 42 for reproducible runs and consistent comparison across iterations."),
             numericInput(ns("neighbors"), "UMAP n_neighbors", value = 15, min = 2, step = 1),
             p(class = "control-note", "Controls local vs global structure. Lower = local detail; higher = smoother global pattern."),
             numericInput(ns("min_dist"), "UMAP min_dist", value = 0.3, min = 0, max = 1, step = 0.05),
             p(class = "control-note", "Controls how tightly points can pack. Lower = tighter clusters; higher = more spread-out layout."),
             selectInput(ns("metric"), "UMAP metric", choices = c("euclidean","manhattan","cosine"), selected = "euclidean"),
             p(class = "control-note", "Distance rule used to compare cells. Use cosine when marker scales differ a lot."),
             checkboxInput(ns("scale"), "Z-score scale channels", value = TRUE),
             p(class = "control-note", "Standardizes markers to comparable scale so high-range channels do not dominate."),
             radioButtons(ns("view_mode"), "View mode", choices = c("2D" = "2d", "3D (interactive)" = "3d"), selected = "2d", inline = TRUE),
             p(class = "control-note", "Switch between static 2D and interactive 3D preview."),
             uiOutput(ns("color_by_ui")),
             hr(),
             tags$div(class = "card-subsection-title", "FlowSOM"),
             numericInput(ns("xdim"), "SOM xdim", value = 10, min = 3, step = 1),
             p(class = "control-note", "Grid width for initial SOM map. Larger grid can capture finer patterns but may be noisier."),
             numericInput(ns("ydim"), "SOM ydim", value = 10, min = 3, step = 1),
             p(class = "control-note", "Grid height for initial SOM map. Together with xdim, this sets SOM map size."),
             numericInput(ns("rlen"), "Training iterations (rlen)", value = 10, min = 1, step = 1),
             p(class = "control-note", "How long FlowSOM trains. Higher can stabilize clusters but takes longer."),
             numericInput(ns("k"), "Meta-clusters (k)", value = 20, min = 2, step = 1),
             p(class = "control-note", "Final number of cluster groups. Higher k gives finer splits; lower k gives broader groups."),
             actionButton(ns("run"), "Run UMAP + FlowSOM", class = "btn btn-primary"),
             br(), br(),
             actionButton(ns("send"), "Send clusters to FlowSOM page", class = "btn btn-success")
      )),
      column(9, tagList(
        card_page_card("Preview",
             uiOutput(ns("run_notice")),
             uiOutput(ns("preview_plot_ui"))
        ),
        card_page_card("Export", uiOutput(ns("export_ui")))
      ))
    )
  )
}

umap_flowsom_page_server <- function(id, shared) {

  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    run_state <- reactiveVal("idle")
    default_channels_signature <- reactiveVal(NULL)
    # Gate message (requires FCS)
    output$gate_notice <- renderUI({
      if (is.null(shared$df$raw())) {
        div(class="alert alert-info",
            strong("No data yet — "), "Please add an FCS from the right sidebar."
        )
      } else NULL
    })

    candidate_group_cols <- reactive({
      df <- shared$df$raw()
      if (is.null(df)) return(character(0))
      nms <- colnames(df)
      if (is.null(nms) || !length(nms)) return(character(0))
      # likely concatenated FCS group/sample fields
      likely <- nms[grepl("sample|group|condition|batch|donor|patient|id", nms, ignore.case = TRUE)]
      is_discrete <- vapply(df[nms], function(x) {
        ux <- unique(as.character(x))
        length(ux) > 1 && length(ux) <= 128
      }, logical(1))
      keep <- intersect(nms[is_discrete], likely)
      unique(keep)
    })

    output$color_by_ui <- renderUI({
      req(input$view_mode)
      if (!identical(input$view_mode, "3d")) return(NULL)
      cols <- c("FlowSOM_Cluster", candidate_group_cols())
      cols <- unique(cols)
      selectInput(ns("color_by"), "3D color by", choices = cols, selected = "FlowSOM_Cluster")
    })
    # Populate channel choices from shared data
    output$channels_picker <- renderUI({
      num_choices <- shared$choices$numeric()
      req(num_choices)
      sig <- paste(num_choices, collapse = "||")
      current <- isolate(input$channels %||% character(0))
      keep <- intersect(current, num_choices)

      choice_names <- lapply(num_choices, function(nm) {
        tags$div(class = "channel-card-option", nm)
      })

      pick_default_channels <- function(df, ch_names, max_n = 14L) {
        if (is.null(df) || !nrow(df) || !length(ch_names)) return(head(ch_names, min(length(ch_names), max_n)))

        score_one <- function(ch) {
          x <- df[[ch]]
          if (is.null(x)) return(-Inf)

          ch_low <- tolower(ch)
          s <- 0

          # Strong positive signal for marker-like channels.
          if (grepl("^(cd\\d+|ccr\\d+|cxcr\\d+|pd\\d+|ctla\\d+|hla|ifn|tnf|il\\d+|foxp\\d+|ki67|cd45ra|cd45ro)", ch_low)) s <- s + 3

          # Strong negative signal for known non-UMAP feature families.
          if (grepl("(^|[_\\-\\s])(fsc|ssc)([_\\-\\s]|$)", ch_low)) s <- s - 6
          if (grepl("time|event|index|acq|acquisition", ch_low)) s <- s - 5
          if (grepl("live\\s*dead|viability|debris|singlet|doublet|dna", ch_low)) s <- s - 4
          if (grepl("sample|group|condition|batch|donor|patient|subject|barcode|well|file|origin|meta", ch_low)) s <- s - 5
          if (grepl("(^|[_\\-\\s])(w|h|a|width|height|area|length)([_\\-\\s]|$)", ch_low)) s <- s - 2

          # Data-driven checks (smart fallback beyond names).
          if (!is.numeric(x)) return(s - 8)
          x_ok <- x[is.finite(x)]
          if (length(x_ok) < 10) return(s - 6)

          uq <- length(unique(x_ok))
          uq_ratio <- uq / length(x_ok)
          iqr_x <- stats::IQR(x_ok)

          # Likely metadata/ID-ish numeric columns: very low unique ratio.
          if (uq_ratio < 0.005) s <- s - 5
          if (uq_ratio < 0.02) s <- s - 2

          # Flat/near-flat channels are poor for UMAP.
          if (!is.finite(iqr_x) || iqr_x == 0) s <- s - 4
          if (is.finite(iqr_x) && iqr_x > 0) s <- s + 1

          s
        }

        scores <- vapply(ch_names, score_one, numeric(1))
        ord <- order(scores, decreasing = TRUE, na.last = TRUE)
        ranked <- ch_names[ord]
        ranked_scores <- scores[ord]

        # Keep channels with at least neutral score first.
        picked <- ranked[ranked_scores >= 0]
        if (!length(picked)) picked <- ranked[ranked_scores > -2]
        if (!length(picked)) picked <- ranked

        head(unique(picked), min(length(picked), max_n))
      }

      # Apply smart defaults once per dataset/channel set refresh, then preserve manual edits.
      is_new_signature <- !identical(default_channels_signature(), sig)
      if (is_new_signature || !length(keep)) {
        keep <- pick_default_channels(shared$df$raw(), num_choices, max_n = 14L)
        if (length(keep) < 2) keep <- head(num_choices, min(10, length(num_choices)))
        default_channels_signature(sig)
      }

      div(
        class = "channel-card-picker-wrap",
        tags$div(class = "channel-picker-title", "Channels"),
        div(
          class = "channel-card-picker",
          checkboxGroupInput(
            ns("channels"),
            label = NULL,
            choiceNames = choice_names,
            choiceValues = num_choices,
            selected = keep,
            width = "100%"
          )
        )
      )
    })

    output$run_notice <- renderUI({
      st <- run_state()
      if (identical(st, "running")) {
        return(
          div(class = "alert alert-warning",
              tags$strong("Running UMAP + FlowSOM... "),
              "Please wait. Results will appear automatically when compute finishes."
          )
        )
      }
      if (is.null(shared$umap$df())) {
        return(
          div(class = "alert alert-info",
              tags$strong("Need to run compute first. "),
              "Click ",
              tags$code("Run UMAP + FlowSOM"),
              " to generate UMAP coordinates and clusters for this preview card."
          )
        )
      }
      NULL
    })

    output$preview_plot_ui <- renderUI({
      req(input$view_mode)
      if (identical(input$view_mode, "3d")) {
        return(withSpinner(plotlyOutput(ns("plot3d"), height = "80vh")))
      }
      withSpinner(plotOutput(ns("plot"), height = "80vh"))
    })
    
    # Run UMAP + FlowSOM
    observeEvent(input$run, {
      if (is.null(shared$df$raw())) {
        showNotification("No FCS data loaded. Please add an FCS file first.", type = "error", duration = 6)
        return(invisible(NULL))
      }
      sel_channels <- input$channels %||% character(0)
      if (length(sel_channels) < 2) {
        showNotification("Select at least 2 channels before running UMAP + FlowSOM.", type = "warning", duration = 6)
        return(invisible(NULL))
      }
      run_state("running")
      on.exit(run_state("idle"), add = TRUE)
      overlay_show(
        text = "Running UMAP + FlowSOM...",
        tip = "This can take time on large FCS files. The preview updates automatically when finished."
      )
      on.exit(overlay_hide(), add = TRUE)

      withProgress(message = "Computing UMAP + FlowSOM", value = 0, {
        df <- shared$df$raw()
        feats <- intersect(sel_channels, colnames(df))
        validate(need(length(feats) >= 2, "Select at least two channels."))

        n_neighbors <- as.integer(input$neighbors %||% 15)
        min_dist <- as.numeric(input$min_dist %||% 0.3)
        seed_i <- as.integer(input$seed %||% 42)
        metric_i <- as.character(input$metric %||% "euclidean")
        xdim_i <- as.integer(input$xdim %||% 10)
        ydim_i <- as.integer(input$ydim %||% 10)
        rlen_i <- as.integer(input$rlen %||% 10)
        k_i <- as.integer(input$k %||% 20)

        validate(need(is.finite(n_neighbors) && n_neighbors >= 2, "UMAP n_neighbors must be >= 2."))
        validate(need(is.finite(min_dist) && min_dist >= 0 && min_dist <= 1, "UMAP min_dist must be between 0 and 1."))
        validate(need(is.finite(seed_i) && seed_i >= 1, "Seed must be >= 1."))
        validate(need(is.finite(xdim_i) && xdim_i >= 2, "SOM xdim must be >= 2."))
        validate(need(is.finite(ydim_i) && ydim_i >= 2, "SOM ydim must be >= 2."))
        validate(need(is.finite(rlen_i) && rlen_i >= 1, "Training iterations (rlen) must be >= 1."))
        validate(need(is.finite(k_i) && k_i >= 2, "Meta-clusters (k) must be >= 2."))

        mat <- as.matrix(df[, feats, drop = FALSE])
        if (isTRUE(input$scale)) mat <- scale(mat)

        incProgress(0.35, detail = "Running UMAP")
        set.seed(seed_i)
        um <- uwot::umap(
          X = mat,
          n_neighbors = n_neighbors,
          min_dist = min_dist,
          metric = metric_i,
          verbose = FALSE
        )
        colnames(um) <- c("UMAP_1","UMAP_2")

        incProgress(0.45, detail = "Running FlowSOM clustering")
        fs_res <- flowsom_cluster(
          df = df, features = feats,
          xdim = xdim_i, ydim = ydim_i, rlen = rlen_i,
          seed = seed_i, scale_data = isTRUE(input$scale), k = k_i
        )

        incProgress(0.20, detail = "Saving results")
        out <- cbind(df, as.data.frame(um))
        out$FlowSOM_Cluster <- factor(fs_res$cluster)

        shared$umap$df(out)
        shared$umap$params(list(
          seed = seed_i, neighbors = n_neighbors, min_dist = min_dist, metric = metric_i,
          feats = feats, xdim = xdim_i, ydim = ydim_i, rlen = rlen_i,
          k = fs_res$k_used %||% k_i, scale = isTRUE(input$scale)
        ))
        if (!identical(fs_res$k_used %||% k_i, k_i)) {
          showNotification(
            sprintf("k was adjusted to %d to fit SOM grid size (%d x %d).", fs_res$k_used, xdim_i, ydim_i),
            type = "warning", duration = 6
          )
        }
        if (identical(fs_res$meta_method %||% "consensus", "kmeans_fallback")) {
          showNotification(
            "Consensus clustering failed on this dataset; used k-means fallback for meta-clustering.",
            type = "warning", duration = 7
          )
        }
        if (identical(fs_res$meta_method %||% "", "direct_kmeans_fallback")) {
          showNotification(
            "FlowSOM core run failed on this dataset; used direct k-means fallback to keep results available.",
            type = "warning", duration = 8
          )
        }
        try(overlay_hide_when_output_ready(ns("plot"), timeout_ms = 120000L), silent = TRUE)
      })
    })

    build_2d_plot <- reactive({
      req(shared$umap$df())
      df <- shared$umap$df()
      ggplot(df, aes(.data[["UMAP_1"]], .data[["UMAP_2"]], color = .data[["FlowSOM_Cluster"]])) +
        geom_point(size = shared$params$point_size(), alpha = shared$params$point_alpha(), shape = shared$params$point_shape(), stroke = shared$params$point_stroke()) +
        coord_fixed(ratio = 1) +
        theme_minimal(base_size = 11) +
        theme(
          plot.title = element_text(size = 13, face = "bold"),
          legend.position = "top",
          legend.title = element_text(size = 10),
          legend.text = element_text(size = 9)
        ) +
        guides(color = guide_legend(override.aes = list(size = 3, alpha = 1), ncol = 6)) +
        labs(x = "UMAP 1", y = "UMAP 2", color = "FlowSOM meta-cluster", title = "UMAP colored by FlowSOM clusters")
    })

    output$plot <- renderPlot({
      req(input$view_mode)
      if (!identical(input$view_mode, "2d")) return(invisible(NULL))
      build_2d_plot()
    }, res = 120)

    output$plot3d <- renderPlotly({
      req(input$view_mode)
      if (!identical(input$view_mode, "3d")) return(NULL)
      req(shared$umap$df())
      df <- shared$umap$df()
      color_by <- input$color_by %||% "FlowSOM_Cluster"
      if (!color_by %in% colnames(df)) color_by <- "FlowSOM_Cluster"
      pal <- scales::hue_pal()(length(unique(df[[color_by]])))
      names(pal) <- unique(as.character(df[[color_by]]))

      plotly::plot_ly(
        data = df,
        x = ~.data[["UMAP_1"]],
        y = ~.data[["UMAP_2"]],
        z = ~as.numeric(.data[["FlowSOM_Cluster"]]),
        color = ~as.factor(.data[[color_by]]),
        colors = pal,
        type = "scatter3d",
        mode = "markers",
        marker = list(size = 2.2, opacity = 0.78)
      ) %>%
        plotly::layout(
          title = list(text = "UMAP 3D interactive view", font = list(size = 13)),
          legend = list(orientation = "h", x = 0, y = 1.08),
          scene = list(
            xaxis = list(title = "UMAP 1"),
            yaxis = list(title = "UMAP 2"),
            zaxis = list(title = "Cluster Index")
          )
        ) %>%
        plotly::config(displaylogo = FALSE)
    })

    output$export_ui <- renderUI({
      req(input$view_mode)
      if (identical(input$view_mode, "3d")) {
        return(
          tagList(
            p(class = "control-note", "3D export generates an interactive HTML bundle (.zip) with required assets."),
            textInput(ns("umap3d_export_name"), "3D export filename stem", value = "umap3d_export"),
            downloadButton(ns("download_umap3d_bundle"), "Download 3D Bundle (.zip)", class = "btn btn-sm")
          )
        )
      }
      tagList(
        p(class = "control-note", "2D export uses current UMAP styling and fixed 1:1 ratio."),
        downloadButton(ns("download_umap_pdf"), "Download 2D PDF", class = "btn btn-sm"),
        tags$span(" "),
        downloadButton(ns("download_umap_svg"), "Download 2D SVG", class = "btn btn-sm")
      )
    })

    output$download_umap_pdf <- downloadHandler(
      filename = function() paste0("UMAP_2D_", Sys.Date(), ".pdf"),
      content = function(file) {
        ggsave(file, plot = build_2d_plot(), device = grDevices::cairo_pdf, width = 8, height = 8, dpi = 300)
      }
    )

    output$download_umap_svg <- downloadHandler(
      filename = function() paste0("UMAP_2D_", Sys.Date(), ".svg"),
      content = function(file) {
        svglite::svglite(file = file, width = 8, height = 8)
        print(build_2d_plot())
        grDevices::dev.off()
      }
    )

    observeEvent(input$download_umap3d_bundle, {
      overlay_show("Building 3D UMAP bundle...")
    }, ignoreInit = TRUE)

    output$download_umap3d_bundle <- downloadHandler(
      filename = function() paste0(gsub("[^A-Za-z0-9_.-]+", "_", input$umap3d_export_name %||% "umap3d_export"), "_bundle.zip"),
      content = function(file) {
        on.exit(overlay_hide(), add = TRUE)
        req(shared$umap$df())
        df <- shared$umap$df()
        color_by <- input$color_by %||% "FlowSOM_Cluster"
        if (!color_by %in% colnames(df)) color_by <- "FlowSOM_Cluster"
        pal <- scales::hue_pal()(length(unique(df[[color_by]])))
        names(pal) <- unique(as.character(df[[color_by]]))

        stem <- gsub("[^A-Za-z0-9_.-]+", "_", input$umap3d_export_name %||% "umap3d_export")
        widget <- plotly::plot_ly(
          data = df,
          x = ~.data[["UMAP_1"]],
          y = ~.data[["UMAP_2"]],
          z = ~as.numeric(.data[["FlowSOM_Cluster"]]),
          color = ~as.factor(.data[[color_by]]),
          colors = pal,
          type = "scatter3d",
          mode = "markers",
          marker = list(size = 2.2, opacity = 0.78)
        ) %>%
          plotly::layout(
            title = list(text = "UMAP 3D interactive view", font = list(size = 13)),
            legend = list(orientation = "h", x = 0, y = 1.08),
            scene = list(
              xaxis = list(title = "UMAP 1"),
              yaxis = list(title = "UMAP 2"),
              zaxis = list(title = "Cluster Index")
            )
          ) %>%
          plotly::config(displaylogo = FALSE)

        out_dir <- tempfile(pattern = "umap3d_bundle_")
        dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
        html_path <- file.path(out_dir, paste0(stem, ".html"))
        htmlwidgets::saveWidget(widget, file = html_path, selfcontained = FALSE, title = "UMAP 3D interactive view")

        old_wd <- getwd()
        on.exit(setwd(old_wd), add = TRUE)
        setwd(out_dir)
        bundle_items <- c(basename(html_path), paste0(stem, "_files"))
        bundle_items <- bundle_items[file.exists(bundle_items)]
        utils::zip(zipfile = file, files = bundle_items)
      }
    )
    
    observeEvent(input$send, {
      req(shared$umap$df())
      shared$flowsom$df(shared$umap$df())
      shared$flowsom$order(sort(as.numeric(levels(shared$umap$df()[["FlowSOM_Cluster"]]))))
      showNotification("UMAP + FlowSOM sent to FlowSOM page.", type = "message")
    })
  })
}
