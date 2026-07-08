wan_lab_navbar <- function(id, theme, import_ui, workspace_ui, compensation_ui, gating_ui, stats_ui, layout_ui, import_export_ui, feature_ui, umap_ui, pipeline_ui, right_sidebar_ui, APP_VERSION = "0.1.0") {
  app_page <- function(x) div(class = "app-page", x)
  plotting_page <- function(x) div(class = "app-page plotting-page", x)
  .nav_spacer_safe <- function() {
    ns <- get0("nav_spacer", envir = asNamespace("bslib"), inherits = FALSE, ifnotfound = NULL)
    if (is.function(ns)) return(ns())
    if (!is.null(ns)) return(ns)
    tags$div(style = "flex:1 1 auto;")
  }
  .nav_title <- function(label, icon_name) {
    tagList(
      icon(icon_name, class = "wl-nav-icon"),
      span(label, class = "wl-nav-label")
    )
  }

  page_navbar(
    id = id,
    theme = theme,
    title = div(
      class = "wl-navbar-brand-title",
      span("FlowJo Re-plotting Tool", class = "wl-navbar-brand-main"),
      span(sprintf("v%s", APP_VERSION), class = "wl-navbar-brand-version")
    ),
    fillable = TRUE,
    header = tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "global.css"),
      tags$link(rel = "stylesheet", type = "text/css", href = "navbar.css"),
      tags$link(rel = "stylesheet", type = "text/css", href = "custom_bslib.css"),
      tags$script(HTML(
        "(function(){
          function bindRail(nav){
            if (!nav || nav.dataset.wlRailBound === 'true') return;
            nav.dataset.wlRailBound = 'true';
            nav.addEventListener('mouseenter', function(){ nav.classList.add('wl-nav-expanded'); });
            nav.addEventListener('mouseover', function(){ nav.classList.add('wl-nav-expanded'); });
            nav.addEventListener('mouseleave', function(){ nav.classList.remove('wl-nav-expanded'); });
            nav.addEventListener('focusin', function(){ nav.classList.add('wl-nav-expanded'); });
            nav.addEventListener('focusout', function(){ setTimeout(function(){ if (!nav.contains(document.activeElement)) nav.classList.remove('wl-nav-expanded'); }, 0); });
          }
          function init(){ document.querySelectorAll('.navbar').forEach(bindRail); }
          if (document.readyState === 'loading') document.addEventListener('DOMContentLoaded', init);
          else init();
          document.addEventListener('shiny:bound', init);
        })();"
      ))
    ),
    sidebar = sidebar(
      id = "app_right_sidebar",
      position = "right",
      width = 320,
      open = "open",
      bg = "#ffffff",
      right_sidebar_ui
    ),
    nav_panel(.nav_title("Import", "file-import"), value = "import_tab", app_page(import_ui)),
    nav_menu(.nav_title("Cytometry", "filter"),
      nav_panel("Workspace", value = "workspace_tab", app_page(workspace_ui)),
      nav_panel("Compensation / Transforms", value = "compensation_tab", plotting_page(compensation_ui)),
      nav_panel("Gating", value = "gating_tab", plotting_page(gating_ui)),
      nav_panel("Statistics", value = "statistics_tab", app_page(stats_ui)),
      nav_panel("Layout Editor", value = "layout_tab", app_page(layout_ui)),
      nav_panel("Import / Export", value = "import_export_tab", app_page(import_export_ui))
    ),
    nav_menu(.nav_title("Analysis / Clustering", "chart-area"),
      nav_panel("Batch Concatenation", value = "pipeline_tab", plotting_page(pipeline_ui)),
      nav_panel("Cluster", value = "umap_tab", plotting_page(umap_ui)),
      nav_panel("Feature Plots", value = "feature_tab", plotting_page(feature_ui))
    ),
    .nav_spacer_safe(),
    nav_item(actionLink("nav_dark_mode_toggle", .nav_title("Dark mode", "moon"))),
    nav_item(actionLink("nav_help", .nav_title("Help", "question-circle")))
  )
}
