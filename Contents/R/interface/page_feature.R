# R/page_feature.R
feature_page_ui <- function(id) {
  ns <- NS(id)
  div(class = "card-page-wrap",
    card_page_header("Feature Plots"),
    uiOutput(ns("gate_notice")),   # <— add this line
    fluidRow(
      column(3, card_page_card("Controls", uiOutput(ns("feature_controls")))),
      column(9, card_page_card("Preview & Export",
             withSpinner(plotOutput(ns("plot"), height = "80vh")),
             hr(),
             downloadButton(ns("download_pdf"), "Download PDF"),
             downloadButton(ns("download_svg"), "Download SVG")))
    )
  )
}

feature_page_server <- function(id, shared) {
  
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    active_feature_data <- reactive({
      obj <- shared$analysis$active()
      if (!is.null(obj)) return(obj$data)
      shared$df$processed()
    })

    active_feature_numeric <- reactive({
      obj <- shared$analysis$active()
      if (!is.null(obj)) return(obj$markers %||% names(obj$data)[vapply(obj$data, is.numeric, logical(1))])
      shared$choices$numeric()
    })

    active_feature_facets <- reactive({
      df <- active_feature_data()
      if (is.null(df)) return(character(0))
      names(df)[vapply(df, function(x) !is.numeric(x), logical(1))]
    })

    # Gate message (requires FCS)
    output$gate_notice <- renderUI({
      obj <- shared$analysis$active()
      if (!is.null(obj)) {
        return(div(class = "Pinhki-inline-note", tags$b("Analysis object: "), obj$name))
      }
      if (is.null(shared$df$raw())) {
        div(class = "Pinhki-inline-note",
            tags$b("No analysis object. "), "Create an object or add an FCS from the right sidebar."
        )
      } else {
        num_cols <- shared$choices$numeric() %||% colnames(shared$df$raw())
        has_umap <- any(grepl("UMAP", num_cols, ignore.case = TRUE))
        if (!has_umap) {
          div(
            class = "Pinhki-inline-note",
            strong("UMAP columns not found in this FCS. "),
            "Run UMAP first, then come back to Feature Plots.",
            div(style = "margin-top: 6px;",
                actionButton(ns("go_umap_tab"), "Go to Cluster", class = "btn btn-sm btn-primary")
            )
          )
        } else NULL
      }
    })

    observeEvent(input$go_umap_tab, {
      app_nav_select(shared$session, "umap_tab")
    }, ignoreInit = TRUE)
    # ---- Transform data (using saved settings) ----
    observe({
      req(shared$df$raw())
      shared$df$processed(flowjo_transform_data(shared$df$raw(), shared$transforms$settings()))
    })
    
    # ---- Page controls ----
    output$feature_controls <- renderUI({
      req(active_feature_numeric())
      all_cols <- active_feature_numeric()
      ap <- shared$axis_pairs()
      default_x <- all_cols[1]; default_y <- all_cols[2]
      fsc_idx <- match(TRUE, grepl("^FSC-A$", all_cols, ignore.case = TRUE))
      ssc_idx <- match(TRUE, grepl("^SSC-A$", all_cols, ignore.case = TRUE))
      if (!is.na(fsc_idx)) default_x <- all_cols[[fsc_idx]]
      if (!is.na(ssc_idx)) default_y <- all_cols[[ssc_idx]]
      if (nrow(ap) > 0 && is.na(fsc_idx) && is.na(ssc_idx)) {
        dr <- ap[grepl("UMAP|TSNE|T-SNE|DIM|PC", ap$x, ignore.case = TRUE), ]
        if (nrow(dr) > 0) { default_x <- dr$x[1]; default_y <- dr$y[1] }
      }
      default_scale <- flowjo_default_axis_scale_type(default_x)
      if (default_scale == "none") default_scale <- flowjo_default_axis_scale_type(default_y)
      saved_feature_scale <- shared$transforms$feature_scale() %||% list(type = default_scale)
      non_struct <- all_cols[!grepl("FSC|SSC|UMAP|TSNE|Time|Event", all_cols, ignore.case=TRUE)]
      default_marker <- if (length(non_struct) > 0) non_struct[1] else all_cols[1]
      
      tagList(
        h4("Axes & Markers"),
        selectInput(ns("yvar"), "Y", all_cols, selected = default_y),
        selectInput(ns("xvar"), "X", all_cols, selected = default_x),
        selectizeInput(ns("markers"), "Markers to color", all_cols, selected = default_marker, multiple = TRUE),
        hr(),
        h4("Axis Scale"),
        radioButtons(
          ns("axis_scale_type"),
          "Scale",
          choices = c("Linear" = "none", "Log1p" = "log1p", "Biex/logicle" = "biex"),
          selected = saved_feature_scale$type %||% default_scale,
          inline = TRUE
        ),
        conditionalPanel(
          sprintf("input['%s'] == 'biex'", ns("axis_scale_type")),
          numericInput(ns("axis_w"), "Width (w)", value = saved_feature_scale$w %||% 0.5, min = 0, step = 0.1),
          numericInput(ns("axis_p"), "Positive decades (m)", value = saved_feature_scale$p %||% 4.5, min = 1, step = 0.1),
          numericInput(ns("axis_n"), "Negative decades (a)", value = saved_feature_scale$n %||% 0, min = 0, step = 0.1)
        ),
        hr(),
        h4("Facetting"),
        selectInput(ns("facet1"), "Facet (cols)", choices = c("None", active_feature_facets())),
        selectInput(ns("facet2"), "Facet (rows)", choices = c("None", active_feature_facets())),
        numericInput(ns("ncol"), "Columns per page", value = 1, min = 1, max = 10),
        hr(),
        h4("Export"),
        numericInput(ns("exp_scale"), "Scale factor", value = 1, min = 0.5, max = 3, step = 0.1),
        radioButtons(ns("exp_unit"), "Units", inline = TRUE, choices = c("cm","in"), selected = "cm"),
        numericInput(ns("exp_w"), "Width", value = 15),
        numericInput(ns("exp_h"), "Height", value = 12)
      )
    })
    
    # Keep axis labels synced with shared
    observeEvent(input$xvar, shared$params$xlab(input$xvar), ignoreInit = TRUE)
    observeEvent(input$yvar, shared$params$ylab(input$yvar), ignoreInit = TRUE)

    observe({
      # Agent note: Feature Plots scale is page-local; do not write it into channel transforms or workspace import will lose gating/channel scale choices.
      req(input$axis_scale_type)
      scale_setting <- flowjo_axis_scale_setting(
        input$axis_scale_type %||% "none",
        input$axis_w %||% 0.5,
        input$axis_p %||% 4.5,
        input$axis_n %||% 0
      )
      shared$transforms$feature_scale(scale_setting %||% list(type = "none"))
    })
    
    # ---- Plot ----
    render_one <- function(gene, df, xvar, yvar, scale_factor) {
      safe_num <- function(x, default) {
        y <- suppressWarnings(as.numeric(x))
        if (length(y) != 1 || is.na(y) || !is.finite(y)) default else y
      }
      safe_int <- function(x, default) {
        y <- suppressWarnings(as.integer(round(as.numeric(x))))
        if (length(y) != 1 || is.na(y)) default else y
      }
      safe_flag <- function(x, default = FALSE) {
        if (is.logical(x) && length(x) == 1 && !is.na(x)) return(x)
        if (is.character(x) && length(x) == 1) {
          lx <- tolower(trimws(x))
          if (lx %in% c("true", "t", "1", "yes", "y")) return(TRUE)
          if (lx %in% c("false", "f", "0", "no", "n")) return(FALSE)
        }
        default
      }
      safe_chr <- function(x, default) {
        if (length(x) < 1 || is.null(x)) return(default)
        y <- as.character(x[[1]])
        if (!nzchar(y) || is.na(y)) default else y
      }

      transform_settings <- shared$transforms$settings()
      cur_trans <- shared$transforms$feature_scale() %||% list(type = "none")
      legend_labels <- flowjo_axis_labeler(cur_trans)
      color_vals <- flowjo_transform_column(df[[gene]], cur_trans)
      scales_override <- shared$transforms$scales()[[gene]]
      rng <- if (!is.null(scales_override)) flowjo_transform_column(scales_override, cur_trans) else range(color_vals, na.rm=TRUE)
      if (any(!is.finite(rng)) || length(rng) != 2 || rng[1] == rng[2]) {
        rng <- range(color_vals, na.rm = TRUE)
      }

      use_mid <- safe_flag(shared$params$use_mid_color(), FALSE)
      low_col <- safe_chr(shared$params$low_color(), "#2300AD")
      mid_col <- safe_chr(shared$params$mid_color(), "white")
      high_col <- safe_chr(shared$params$high_color(), "#FF8D0A")
      cols <- if (use_mid) c(low_col, mid_col, high_col) else c(low_col, high_col)

      shape_v  <- safe_int(shared$params$point_shape(), 19)
      size_v   <- safe_num(shared$params$point_size(), 1) * safe_num(scale_factor, 1)
      alpha_v  <- min(max(safe_num(shared$params$point_alpha(), 0.8), 0), 1)
      stroke_v <- max(safe_num(shared$params$point_stroke(), 0), 0)
      color_map <- safe_chr(shared$params$color_map(), "color")
      color_map <- if (identical(color_map, "fill")) "fill" else "color"

      keep <- is.finite(df[[xvar]]) & is.finite(df[[yvar]]) & is.finite(color_vals)
      dff <- df[keep, , drop = FALSE]
      dff$.feature_value <- color_vals[keep]
      validate(need(nrow(dff) > 0, "No finite rows available for plotting after filtering."))

      p <- ggplot(dff, aes(.data[[xvar]], .data[[yvar]]))
        if (identical(color_map, "fill")) {
        p <- p + geom_point(
          aes(fill = .data$.feature_value),
          color = "black",
          shape = shape_v,
          size = size_v,
          alpha = alpha_v,
          stroke = stroke_v
        ) + 
          scale_fill_gradientn(colors = cols, limits = rng, oob = squish, name = "MFI", labels = legend_labels)
      } else {
        p <- p + geom_point(
          aes(color = .data$.feature_value),
          shape = shape_v,
          size = size_v,
          alpha = alpha_v,
          stroke = stroke_v
        ) +
          scale_color_gradientn(colors = cols, limits = rng, oob = squish, name = "MFI", labels = legend_labels)
      }
      p +
        scale_x_continuous(labels = flowjo_axis_labeler(transform_settings[[xvar]])) +
        scale_y_continuous(labels = flowjo_axis_labeler(transform_settings[[yvar]])) +
        coord_fixed(ratio = 1) +
        labs(title = gene, x = NULL, y = NULL) +
        theme(
          legend.position = c(0.82, 0.82),
          legend.justification = c(1, 1),
          legend.background = element_rect(fill = scales::alpha("white", 0.82), color = "#d1d5db"),
          legend.key = element_rect(fill = "white", color = NA)
        )
    }
    
    output$plot <- renderPlot({
      req(active_feature_data(), input$markers, input$xvar, input$yvar)
      df <- active_feature_data()
      validate(
        need(is.data.frame(df), "Processed data is not available as a data frame."),
        need(input$xvar %in% colnames(df), "Selected X axis is not available in the active FCS."),
        need(input$yvar %in% colnames(df), "Selected Y axis is not available in the active FCS.")
      )
      markers <- input$markers[input$markers %in% colnames(df)]
      markers <- markers[sapply(markers, function(m) is.numeric(df[[m]]))]
      validate(need(length(markers) > 0, "No valid numeric marker selected for plotting."))
      width <- session$clientData[[ns("plot") %>% paste0("_width")]] %||% 800
      sf <- width / 800
      
      plots <- lapply(markers, function(g) render_one(g, df, input$xvar, input$yvar, sf))
      p <- wrap_plots(plots, ncol = input$ncol)
      
      theme_choice <- flowjo_feature_theme_choice(input$xvar, input$yvar, shared$params$plot_theme())
      real_axes <- !(flowjo_is_embedding_axis(input$xvar) && flowjo_is_embedding_axis(input$yvar))
      p <- apply_theme(
        p, scale_factor = sf,
        theme_choice   = theme_choice,
        title_size     = shared$params$title_size(),
        axis_title_size= shared$params$axis_title_size(),
        axis_text_size = shared$params$axis_text_size(),
        xlab = shared$params$xlab(),
        ylab = shared$params$ylab()
      )
      if (isTRUE(real_axes)) p <- p + flowjo_real_axis_theme(sf)
      p
    }, res = 120)
    
    # ---- Exports ----
    output$download_pdf <- downloadHandler(
      filename = function() paste0("Feature_Plots_", Sys.Date(), ".pdf"),
      content  = function(file) {
        req(active_feature_data(), input$markers, input$xvar, input$yvar)
        df <- active_feature_data()
        markers <- input$markers[input$markers %in% colnames(df)]
        markers <- markers[sapply(markers, function(m) is.numeric(df[[m]]))]
        validate(need(length(markers) > 0, "No valid numeric marker selected for plotting."))
        plots <- lapply(markers, function(g) render_one(g, df, input$xvar, input$yvar, input$exp_scale))
        p <- wrap_plots(plots, ncol = input$ncol)
        theme_choice <- flowjo_feature_theme_choice(input$xvar, input$yvar, shared$params$plot_theme())
        real_axes <- !(flowjo_is_embedding_axis(input$xvar) && flowjo_is_embedding_axis(input$yvar))
        p <- apply_theme(p, scale_factor = input$exp_scale, theme_choice = theme_choice,
                         title_size = shared$params$title_size(), axis_title_size = shared$params$axis_title_size(),
                         axis_text_size = shared$params$axis_text_size(), xlab = shared$params$xlab(), ylab = shared$params$ylab())
        if (isTRUE(real_axes)) p <- p + flowjo_real_axis_theme(input$exp_scale)
        ggsave(file, plot = p, device = "pdf", width = input$exp_w, height = input$exp_h, units = input$exp_unit, scale = 1, dpi = 300)
      }
    )
    output$download_svg <- downloadHandler(
      filename = function() paste0("Feature_Plots_", Sys.Date(), ".svg"),
      content  = function(file) {
        req(active_feature_data(), input$markers, input$xvar, input$yvar)
        df <- active_feature_data()
        markers <- input$markers[input$markers %in% colnames(df)]
        markers <- markers[sapply(markers, function(m) is.numeric(df[[m]]))]
        validate(need(length(markers) > 0, "No valid numeric marker selected for plotting."))
        plots <- lapply(markers, function(g) render_one(g, df, input$xvar, input$yvar, input$exp_scale))
        p <- wrap_plots(plots, ncol = input$ncol)
        theme_choice <- flowjo_feature_theme_choice(input$xvar, input$yvar, shared$params$plot_theme())
        real_axes <- !(flowjo_is_embedding_axis(input$xvar) && flowjo_is_embedding_axis(input$yvar))
        p <- apply_theme(p, scale_factor = input$exp_scale, theme_choice = theme_choice,
                         title_size = shared$params$title_size(), axis_title_size = shared$params$axis_title_size(),
                         axis_text_size = shared$params$axis_text_size(), xlab = shared$params$xlab(), ylab = shared$params$ylab())
        if (isTRUE(real_axes)) p <- p + flowjo_real_axis_theme(input$exp_scale)
        ggsave(file, plot = p, device = "svg", width = input$exp_w, height = input$exp_h, units = input$exp_unit, scale = 1)
      }
    )
  })
}
