# R/page_flowsom.R
flowsom_page_ui <- function(id) {
  ns <- NS(id)
  div(class = "card-page-wrap",
    card_page_header(
      "FlowSOM Clusters",
      subtitle = "Upload FlowSOM CSV exported from FlowJo (or send clustering from the UMAP page)."
    ),
    uiOutput(ns("gate_notice")),
    card_page_card("FlowSOM Import", fileInput(ns("flowsom_file"), "Upload FlowSOM CSV", accept = ".csv")),
    fluidRow(
      column(3, card_page_card("Controls", uiOutput(ns("controls")))),
      column(9, card_page_card("Preview & Export",
             withSpinner(plotOutput(ns("plot"), height = "80vh")),
             hr(),
             downloadButton(ns("download_pdf"), "Download PDF"),
             downloadButton(ns("download_svg"), "Download SVG")))
    )
  )
}

flowsom_page_server <- function(id, shared) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    # Gate message (requires FCS)
    output$gate_notice <- renderUI({
      if (is.null(shared$df$raw())) {
        div(class="alert alert-info",
            strong("No data yet — "), "Please add an FCS from the right sidebar."
        )
      } else NULL
    })
    observeEvent(input$flowsom_file, {
      req(shared$df$raw(), input$flowsom_file)
      showNotification("Merging FlowSOM CSV with FCS rows...", type = "message")
      fs_df <- read.csv(input$flowsom_file$datapath, check.names = FALSE)
      main  <- shared$df$raw()
      if (nrow(fs_df) != nrow(main)) {
        showNotification(sprintf("Row mismatch: FCS=%d vs FlowSOM CSV=%d", nrow(main), nrow(fs_df)), type="error", duration=10)
        return()
      }
      cluster_col <- colnames(fs_df)[2]
      merged <- main
      merged[[cluster_col]] <- as.factor(fs_df[[cluster_col]])
      shared$flowsom$df(merged)
      shared$flowsom$order(sort(as.numeric(levels(merged[[cluster_col]]))))
      showNotification("FlowSOM merged.", type = "message")
    })
    
    output$controls <- renderUI({
      req(shared$choices$numeric())
      all_cols <- shared$choices$numeric()
      ap <- shared$axis_pairs()
      default_x <- all_cols[1]; default_y <- all_cols[2]
      if (nrow(ap) > 0) {
        dr <- ap[grepl("UMAP|TSNE|T-SNE|DIM|PC", ap$x, ignore.case = TRUE), ]
        if (nrow(dr) > 0) { default_x <- dr$x[1]; default_y <- dr$y[1] }
      }
      tagList(
        h4("Axes"),
        selectInput(ns("xvar"), "X", all_cols, selected = default_x),
        selectInput(ns("yvar"), "Y", all_cols, selected = default_y),
        hr(),
        h4("Cluster Colors & Order"),
        uiOutput(ns("color_picker_ui")),
        hr(),
        h4("Export"),
        numericInput(ns("exp_scale"), "Scale", value = 1, min = 0.5, max = 3, step = 0.1),
        radioButtons(ns("exp_unit"), "Units", inline = TRUE, choices = c("cm","in"), selected = "cm"),
        numericInput(ns("exp_w"), "Width", value = 15),
        numericInput(ns("exp_h"), "Height", value = 12)
      )
    })
    
    # dynamic color pickers for clusters
    output$color_picker_ui <- renderUI({
      req(shared$flowsom$df())
      fs_df <- shared$flowsom$df()
      cluster_col <- colnames(fs_df)[grepl("FlowSOM", colnames(fs_df))][1]
      if (is.null(cluster_col)) cluster_col <- colnames(fs_df)[grepl("Cluster", colnames(fs_df))][1]
      req(cluster_col)
      ids <- sort(as.numeric(levels(fs_df[[cluster_col]])))
      if (is.null(shared$flowsom$order())) shared$flowsom$order(ids)
      pal <- colorRampPalette(brewer.pal(9,"Set1"))(length(ids))
      lapply(shared$flowsom$order(), function(id) {
        current <- input[[paste0("fs_color_", id)]]
        default <- pal[which(ids == id)]
        final   <- current %||% default
        div(style="display:flex; align-items:center; margin-bottom:-8px;",
            div(style="flex-grow:1;", colourInput(ns(paste0("fs_color_", id)), label = paste("Cluster", id), value = final)),
            div(style="flex-shrink:0; margin-left:10px;",
                actionButton(ns(paste0("up_", id)),   "", icon = icon("arrow-up"),   style="padding:2px 6px;"),
                actionButton(ns(paste0("down_", id)), "", icon = icon("arrow-down"), style="padding:2px 6px;")
            )
        )
      })
    })
    
    # wire reordering
    observe({
      req(shared$flowsom$df())
      fs_df <- shared$flowsom$df()
      cluster_col <- colnames(fs_df)[grepl("FlowSOM", colnames(fs_df))][1]
      if (is.null(cluster_col)) cluster_col <- colnames(fs_df)[grepl("Cluster", colnames(fs_df))][1]
      req(cluster_col)
      ids <- sort(as.numeric(levels(fs_df[[cluster_col]])))
      if (is.null(shared$flowsom$order())) shared$flowsom$order(ids)
      swap <- function(v,i,j){ tmp<-v[i]; v[i]<-v[j]; v[j]<-tmp; v }
      lapply(ids, function(id) {
        observeEvent(input[[paste0("up_", id)]],   { cur<-shared$flowsom$order(); idx<-which(cur==id); if (idx>1) shared$flowsom$order(swap(cur, idx, idx-1)) }, ignoreInit=TRUE)
        observeEvent(input[[paste0("down_", id)]], { cur<-shared$flowsom$order(); idx<-which(cur==id); if (idx<length(cur)) shared$flowsom$order(swap(cur, idx, idx+1)) }, ignoreInit=TRUE)
      })
    })
    
    # ---- Plot ----
    plot_build <- function(df, xvar, yvar, scale_factor) {
      cluster_col <- colnames(df)[grepl("FlowSOM", colnames(df))][1]
      if (is.null(cluster_col)) cluster_col <- colnames(df)[grepl("Cluster", colnames(df))][1]
      req(cluster_col)
      
      ord <- shared$flowsom$order()
      df[[cluster_col]] <- factor(df[[cluster_col]], levels = ord)
      
      # colors
      req(input[[ns(paste0("fs_color_", ord[1]))]])
      cluster_colors <- sapply(ord, function(id) input[[ns(paste0("fs_color_", id))]])
      names(cluster_colors) <- as.character(ord)
      
      p <- ggplot(df, aes(.data[[xvar]], .data[[yvar]]))
      gargs <- list(shape = shared$params$point_shape(),
                    size  = shared$params$point_size()*scale_factor,
                    alpha = shared$params$point_alpha(),
                    stroke= shared$params$point_stroke())
      if (shared$params$color_map() == "fill") {
        p <- p + geom_point(aes(fill = .data[[cluster_col]]), color="black", !!!gargs) +
          scale_fill_manual(values = cluster_colors, name = "FlowSOM Cluster")
      } else {
        p <- p + geom_point(aes(color = .data[[cluster_col]]), !!!gargs) +
          scale_color_manual(values = cluster_colors, name = "FlowSOM Cluster")
      }
      p <- p + coord_fixed() + labs(title = shared$params$title(), x = shared$params$xlab(), y = shared$params$ylab())
      apply_theme(p, scale_factor = scale_factor,
                  theme_choice = shared$params$plot_theme(),
                  title_size = shared$params$title_size(),
                  axis_title_size = shared$params$axis_title_size(),
                  axis_text_size = shared$params$axis_text_size(),
                  xlab = shared$params$xlab(), ylab = shared$params$ylab())
    }
    
    output$plot <- renderPlot({
      req(shared$flowsom$df(), input$xvar, input$yvar)
      df <- shared$flowsom$df()
      width <- session$clientData[[ns("plot") %>% paste0("_width")]] %||% 800
      sf <- width / 800
      plot_build(df, input$xvar, input$yvar, sf)
    }, res = 120)
    
    # Exports
    output$download_pdf <- downloadHandler(
      filename = function() paste0("FlowSOM_", Sys.Date(), ".pdf"),
      content  = function(file) {
        p <- plot_build(shared$flowsom$df(), input$xvar, input$yvar, input$exp_scale)
        ggsave(file, plot = p, device = "pdf", width = input$exp_w, height = input$exp_h, units = input$exp_unit, scale = 1, dpi = 300)
      }
    )
    output$download_svg <- downloadHandler(
      filename = function() paste0("FlowSOM_", Sys.Date(), ".svg"),
      content  = function(file) {
        p <- plot_build(shared$flowsom$df(), input$xvar, input$yvar, input$exp_scale)
        ggsave(file, plot = p, device = "svg", width = input$exp_w, height = input$exp_h, units = input$exp_unit, scale = 1)
      }
    )
  })
}
