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
    # Gate message (requires FCS)
    output$gate_notice <- renderUI({
      if (is.null(shared$df$raw())) {
        div(class="alert alert-info",
            strong("No data yet — "), "Please add an FCS from the right sidebar."
        )
      } else {
        num_cols <- shared$choices$numeric() %||% colnames(shared$df$raw())
        has_umap <- any(grepl("UMAP", num_cols, ignore.case = TRUE))
        if (!has_umap) {
          div(
            class = "alert alert-warning",
            strong("UMAP columns not found in this FCS. "),
            "Run UMAP first, then come back to Feature Plots.",
            div(style = "margin-top: 6px;",
                actionButton(ns("go_umap_tab"), "Go to UMAP + FlowSOM", class = "btn btn-sm btn-primary")
            )
          )
        } else NULL
      }
    })

    observeEvent(input$go_umap_tab, {
      shiny::updateNavbarPage(session = session, inputId = "main_nav", selected = "umap_tab")
    }, ignoreInit = TRUE)
    # ---- Transform data (using saved settings) ----
    observe({
      req(shared$df$raw())
      df <- shared$df$raw()
      settings <- shared$transforms$settings()
      for (marker in names(settings)) {
        if (!(marker %in% colnames(df))) next
        s <- settings[[marker]]
        if (s$type == "log1p") {
          df[[marker]] <- log1p(df[[marker]])
        } else if (s$type == "biex") {
          if (is.null(s$w) || is.null(s$p) || is.null(s$n)) next
          if ((2*s$w + s$n) >= s$p) next
          lg <- logicleTransform(w=s$w, m=s$p, a=s$n)
          df[[marker]] <- lg(df[[marker]])
        }
      }
      shared$df$processed(df)
    })
    
    # ---- Page controls ----
    output$feature_controls <- renderUI({
      req(shared$choices$numeric())
      all_cols <- shared$choices$numeric()
      ap <- shared$axis_pairs()
      default_x <- all_cols[1]; default_y <- all_cols[2]
      if (nrow(ap) > 0) {
        dr <- ap[grepl("UMAP|TSNE|T-SNE|DIM|PC", ap$x, ignore.case = TRUE), ]
        if (nrow(dr) > 0) { default_x <- dr$x[1]; default_y <- dr$y[1] }
      }
      non_struct <- all_cols[!grepl("FSC|SSC|UMAP|TSNE|Time|Event", all_cols, ignore.case=TRUE)]
      default_marker <- if (length(non_struct) > 0) non_struct[1] else all_cols[1]
      
      tagList(
        h4("Axes & Markers"),
        selectInput(ns("xvar"), "X", all_cols, selected = default_x),
        selectInput(ns("yvar"), "Y", all_cols, selected = default_y),
        selectizeInput(ns("markers"), "Markers to color", all_cols, selected = default_marker, multiple = TRUE),
        hr(),
        h4("Facetting"),
        selectInput(ns("facet1"), "Facet (cols)", choices = c("None", shared$choices$facet())),
        selectInput(ns("facet2"), "Facet (rows)", choices = c("None", shared$choices$facet())),
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

      cur_trans <- shared$transforms$settings()[[gene]] %||% list(type="None")
      legend_labels <- waiver()
      if (cur_trans$type == "biex") {
        trans_func <- logicleTransform(w=cur_trans$w, m=cur_trans$p, a=cur_trans$n)
        inv_func   <- inverseLogicleTransform(trans=trans_func)
        legend_labels <- function(breaks_x) {
          orig <- inv_func(breaks_x)
          sapply(orig, function(val) if (val <= 0) as.character(round(val,1)) else as.expression(bquote(10^.(round(log10(val),1)))))
        }
      }
      scales_override <- shared$transforms$scales()[[gene]]
      rng <- if (!is.null(scales_override)) scales_override else range(df[[gene]], na.rm=TRUE)
      if (any(!is.finite(rng)) || length(rng) != 2 || rng[1] == rng[2]) {
        rng <- range(df[[gene]], na.rm = TRUE)
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

      keep <- is.finite(df[[xvar]]) & is.finite(df[[yvar]]) & is.finite(df[[gene]])
      dff <- df[keep, , drop = FALSE]
      validate(need(nrow(dff) > 0, "No finite rows available for plotting after filtering."))

      p <- ggplot(dff, aes(.data[[xvar]], .data[[yvar]]))
      if (identical(color_map, "fill")) {
        p <- p + geom_point(
          aes(fill = .data[[gene]]),
          color = "black",
          shape = shape_v,
          size = size_v,
          alpha = alpha_v,
          stroke = stroke_v
        ) +
          scale_fill_gradientn(colors = cols, limits = rng, oob = squish, name = "MFI", labels = legend_labels)
      } else {
        p <- p + geom_point(
          aes(color = .data[[gene]]),
          shape = shape_v,
          size = size_v,
          alpha = alpha_v,
          stroke = stroke_v
        ) +
          scale_color_gradientn(colors = cols, limits = rng, oob = squish, name = "MFI", labels = legend_labels)
      }
      p + coord_fixed() + labs(title = gene, x = NULL, y = NULL)
    }
    
    output$plot <- renderPlot({
      req(shared$df$processed(), input$markers, input$xvar, input$yvar)
      df <- shared$df$processed()
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
      
      apply_theme(
        p, scale_factor = sf,
        theme_choice   = shared$params$plot_theme(),
        title_size     = shared$params$title_size(),
        axis_title_size= shared$params$axis_title_size(),
        axis_text_size = shared$params$axis_text_size(),
        xlab = shared$params$xlab(),
        ylab = shared$params$ylab()
      )
    }, res = 120)
    
    # ---- Exports ----
    output$download_pdf <- downloadHandler(
      filename = function() paste0("Feature_Plots_", Sys.Date(), ".pdf"),
      content  = function(file) {
        req(shared$df$processed(), input$markers, input$xvar, input$yvar)
        df <- shared$df$processed()
        markers <- input$markers[input$markers %in% colnames(df)]
        markers <- markers[sapply(markers, function(m) is.numeric(df[[m]]))]
        validate(need(length(markers) > 0, "No valid numeric marker selected for plotting."))
        plots <- lapply(markers, function(g) render_one(g, df, input$xvar, input$yvar, input$exp_scale))
        p <- wrap_plots(plots, ncol = input$ncol)
        p <- apply_theme(p, scale_factor = input$exp_scale, theme_choice = shared$params$plot_theme(),
                         title_size = shared$params$title_size(), axis_title_size = shared$params$axis_title_size(),
                         axis_text_size = shared$params$axis_text_size(), xlab = shared$params$xlab(), ylab = shared$params$ylab())
        ggsave(file, plot = p, device = "pdf", width = input$exp_w, height = input$exp_h, units = input$exp_unit, scale = 1, dpi = 300)
      }
    )
    output$download_svg <- downloadHandler(
      filename = function() paste0("Feature_Plots_", Sys.Date(), ".svg"),
      content  = function(file) {
        req(shared$df$processed(), input$markers, input$xvar, input$yvar)
        df <- shared$df$processed()
        markers <- input$markers[input$markers %in% colnames(df)]
        markers <- markers[sapply(markers, function(m) is.numeric(df[[m]]))]
        validate(need(length(markers) > 0, "No valid numeric marker selected for plotting."))
        plots <- lapply(markers, function(g) render_one(g, df, input$xvar, input$yvar, input$exp_scale))
        p <- wrap_plots(plots, ncol = input$ncol)
        p <- apply_theme(p, scale_factor = input$exp_scale, theme_choice = shared$params$plot_theme(),
                         title_size = shared$params$title_size(), axis_title_size = shared$params$axis_title_size(),
                         axis_text_size = shared$params$axis_text_size(), xlab = shared$params$xlab(), ylab = shared$params$ylab())
        ggsave(file, plot = p, device = "svg", width = input$exp_w, height = input$exp_h, units = input$exp_unit, scale = 1)
      }
    )
  })
}
