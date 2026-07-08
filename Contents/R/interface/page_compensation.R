compensation_page_ui <- function(id) {
  ns <- NS(id)
  div(class = "card-page-wrap",
    card_page_header("Compensation / Transforms", subtitle = "Spillover matrix and channel transform controls."),
    fluidRow(
      column(4, card_page_card("Active Sample", uiOutput(ns("sample_info")))),
      column(8, card_page_card("Spillover Matrix", DT::dataTableOutput(ns("spillover_table"))))
    )
  )
}

compensation_page_server <- function(id, shared) {
  moduleServer(id, function(input, output, session) {
    active_entry <- reactive({
      id <- shared$fcs$active_id()
      reg <- shared$fcs$registry()
      if (is.null(id) || is.null(reg[[id]])) return(NULL)
      reg[[id]]
    })
    output$sample_info <- renderUI({
      e <- active_entry()
      if (is.null(e)) return(helpText("Add and select an FCS file first."))
      tagList(strong(e$name), div(class = "gate-node-meta", sprintf("%s cells, %s channels", format(e$cells, big.mark = ","), e$channels)))
    })
    output$spillover_table <- DT::renderDataTable({
      e <- active_entry()
      if (is.null(e) || is.null(e$spillover)) return(NULL)
      DT::datatable(as.data.frame(e$spillover), options = list(scrollX = TRUE, pageLength = 12))
    })
  })
}
