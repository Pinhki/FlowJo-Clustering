# R/page_import.R
import_page_ui <- function(id) {
  ns <- NS(id)
  div(
    class = "welcome-page-wrap",
    fluidRow(
      class = "welcome-row-single",
      column(
        width = 12,
        class = "welcome-card-col",
        div(
          class = "card welcome-block",
          div(class = "welcome-block-title", icon("list-ol"), "Import & Instructions"),
          tags$ol(
            class = "welcome-steps",
            tags$li("Add FCS from the right sidebar (FCS Files panel), then set the active dataset."),
            tags$li("Explore Feature Plots to color by markers, facet by metadata, and export figures."),
            tags$li("Cluster with FlowSOM or run UMAP + FlowSOM, then continue styling/export.")
          )
        )
      )
    ),
    fluidRow(
      class = "welcome-row-single",
      column(
        width = 12,
        class = "welcome-card-col",
        div(
          class = "card welcome-block welcome-updates-block",
          div(class = "welcome-block-title", icon("lightbulb"), "What's New"),
          tags$ul(
            class = "welcome-update-list",
            tags$li("Fixed the inaccuracy of biexponential legends."),
            tags$li("Improved consistency between the preview and exported PDF."),
            tags$li("Corrected spelling and grammar."),
            tags$li("[Experimental] Save/Load workspace is available from the right sidebar.")
          ),
          p(class = "welcome-update-link", "Tip: adjust point and label sizes before export when needed.")
        )
      )
    )
  )
}

import_page_server <- function(id, shared) {
  moduleServer(id, function(input, output, session) {
    # Import is handled from the right sidebar FCS manager.
  })
}
