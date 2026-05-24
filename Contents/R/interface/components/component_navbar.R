wan_lab_navbar <- function(id, theme, import_ui, feature_ui, flowsom_ui, umap_ui, pipeline_ui, right_sidebar_ui, APP_VERSION = "0.1.0") {
  app_page <- function(x) div(class = "app-page", x)
  plotting_page <- function(x) div(class = "app-page plotting-page", x)
  .nav_spacer_safe <- function() {
    ns <- get0("nav_spacer", envir = asNamespace("bslib"), inherits = FALSE, ifnotfound = NULL)
    if (is.function(ns)) return(ns())
    if (!is.null(ns)) return(ns)
    tags$div(style = "flex:1 1 auto;")
  }

  page_navbar(
    id = id,
    theme = theme,
    title = div(
      style = "display: flex; flex-wrap: wrap; align-items: baseline; row-gap: 0; column-gap: 10px; line-height: 1.2; white-space: normal;",
      span("FlowJo Re-plotting Tool", style = "font-weight: bold; font-size: 1.1rem; white-space: nowrap;"),
      span(sprintf("v%s", APP_VERSION), style = "font-size: 0.75rem; opacity: 0.8; white-space: nowrap;")
    ),
    fillable = TRUE,
    header = tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "global.css"),
      tags$link(rel = "stylesheet", type = "text/css", href = "navbar.css"),
      tags$link(rel = "stylesheet", type = "text/css", href = "custom_bslib.css")
    ),
    sidebar = sidebar(
      id = "app_right_sidebar",
      position = "right",
      width = 320,
      open = "open",
      bg = "#ffffff",
      right_sidebar_ui
    ),
    nav_panel("Import", value = "import_tab", app_page(import_ui)),
    nav_panel("Feature Plots", value = "feature_tab", plotting_page(feature_ui)),
    nav_panel("FlowSOM", value = "flowsom_tab", plotting_page(flowsom_ui)),
    nav_panel("UMAP + FlowSOM", value = "umap_tab", plotting_page(umap_ui)),
    nav_panel("Population Isolation", value = "pipeline_tab", plotting_page(pipeline_ui)),
    .nav_spacer_safe(),
    nav_item(actionLink("nav_dark_mode_toggle", "Dark mode", icon = icon("moon"))),
    nav_item(actionLink("nav_help", "Help", icon = icon("question-circle")))
  )
}
