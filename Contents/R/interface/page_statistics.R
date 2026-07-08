statistics_page_ui <- function(id) {
  ns <- NS(id)
  div(class = "card-page-wrap",
    card_page_header("Statistics", subtitle = "Population counts and percent of parent."),
    card_page_card("Gate Statistics", DT::dataTableOutput(ns("stats_table")))
  )
}

statistics_page_server <- function(id, shared) {
  moduleServer(id, function(input, output, session) {
    output$stats_table <- DT::renderDataTable({
      stats <- shared$gating$stats()
      if (!nrow(stats)) return(NULL)
      DT::datatable(stats, options = list(pageLength = 20, scrollX = TRUE), rownames = FALSE)
    })
  })
}
