# Population export follows analysis; this page never creates analysis inputs.
flowjo_pipeline_page_ui <- function(id) {
  ns <- NS(id)
  div(class = "card-page-wrap",
    card_page_header("Population Export", subtitle = "Select populations from an analysis run, then export their original marker values."),
    card_page_card("Analysis source", uiOutput(ns("export_source"))),
    card_page_card("Populations to export", uiOutput(ns("export_populations")),
      radioButtons(ns("concat_mode"), "Export sampling", choices = c("All events" = "all", "Cap per sample + cluster" = "cap"), selected = "all"),
      numericInput(ns("balanced_n"), "Event cap per sample + cluster", value = 5000, min = 1, step = 100),
      uiOutput(ns("export_control")))
  )
}

flowjo_pipeline_page_server <- function(id, shared) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    output$export_source <- renderUI({
      objects <- shared$analysis$objects()
      if (!length(objects)) return(p("Create an analysis object and run clustering first."))
      tagList(selectInput(ns("export_object"), "Analysis object", choices = stats::setNames(names(objects), vapply(objects, function(x) x$name, character(1))), selected = shared$analysis$active_id()),
        uiOutput(ns("export_run_ui")))
    })
    export_object <- reactive(shared$analysis$objects()[[input$export_object %||% ""]])
    output$export_run_ui <- renderUI({
      obj <- export_object()
      if (is.null(obj) || !length(obj$runs)) return(p("This object has no analysis runs. Run clustering first."))
      selectInput(ns("export_run"), "Run", choices = stats::setNames(names(obj$runs), vapply(obj$runs, function(x) x$name, character(1))), selected = obj$active_run_id)
    })
    export_data <- reactive({
      obj <- export_object()
      run <- obj$runs[[input$export_run %||% ""]]
      if (is.null(run$result$df) || !"FlowSOM_Cluster" %in% names(run$result$df)) return(NULL)
      run$result$df
    })
    output$export_populations <- renderUI({
      dat <- export_data()
      if (is.null(dat)) return(p("Select a run with cluster populations."))
      counts <- table(as.character(dat$FlowSOM_Cluster))
      checkboxGroupInput(ns("export_clusters"), "Include clusters", choices = stats::setNames(names(counts), sprintf("Cluster %s | %s cells", names(counts), format(as.integer(counts), big.mark = ","))), selected = character())
    })
    output$export_control <- renderUI({
      dat <- export_data()
      selected <- input$export_clusters %||% character()
      n <- if (is.null(dat)) 0L else sum(as.character(dat$FlowSOM_Cluster) %in% selected)
      if (!n) return(p("Select at least one cluster to export."))
      tagList(p(sprintf("%s selected cell rows before export sampling", format(n, big.mark = ","))),
        downloadButton(ns("export_concat"), "Export FCS + Mapping", class = "btn btn-primary"))
    })
    output$export_concat <- downloadHandler(
      filename = function() paste0("analysis_populations_", Sys.Date(), ".zip"),
      content = function(file) {
        obj <- export_object(); dat <- export_data()
        if (is.null(dat)) stop("Select a clustering run.")
        selected <- input$export_clusters %||% character()
        dat <- dat[as.character(dat$FlowSOM_Cluster) %in% selected, , drop = FALSE]
        if (!nrow(dat)) stop("Select populations to export.")
        if (!"sample_id" %in% names(dat)) dat$sample_id <- obj$source_id
        clusters <- sort(unique(as.character(dat$FlowSOM_Cluster)))
        samples <- unique(as.character(dat$sample_id))
        dat$PopulationID <- match(as.character(dat$FlowSOM_Cluster), clusters)
        dat$SampleID <- match(as.character(dat$sample_id), samples)
        # Agent note: Export samples original run rows; no compensation or clustering transforms are reapplied here.
        groups <- split(seq_len(nrow(dat)), interaction(dat$SampleID, dat$PopulationID, drop = TRUE))
        if (identical(input$concat_mode, "cap")) {
          cap <- as.integer(input$balanced_n)
          if (!is.finite(cap) || cap < 1) stop("Event cap must be positive.")
          groups <- lapply(groups, function(rows) if (length(rows) > cap) sample(rows, cap) else rows)
        }
        mapping <- do.call(rbind, lapply(groups, function(rows) data.frame(
          SampleID = dat$SampleID[rows[1]], sample_id = dat$sample_id[rows[1]],
          PopulationID = dat$PopulationID[rows[1]], cluster = as.character(dat$FlowSOM_Cluster[rows[1]]),
          events = length(rows), object_name = obj$name, run_name = obj$runs[[input$export_run]]$name,
          stringsAsFactors = FALSE)))
        dat <- dat[unlist(groups, use.names = FALSE), , drop = FALSE]
        flowjo_export_population_fcs_zip(dat, obj$markers, mapping, file)
      }
    )
  })
}
