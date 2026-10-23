source("global.R")
APP_VERSION <- "0.1.0"
PROJECT_ROOT <- if (identical(basename(getwd()), "Contents")) dirname(getwd()) else getwd()
LOCAL_SETTINGS_FILE <- file.path(PROJECT_ROOT, "local_settings", "flowjo_settings.json")
LOCAL_SETTINGS_DEFAULTS <- list(
  plot_cache_enabled = TRUE,
  plot_event_limit_enabled = TRUE,
  plot_event_limit = 75000L,
  plot_cache_mode = "settings",
  plot_cache_max_contexts = 50L,
  plot_preload_enabled = TRUE,
  plot_preload_after_load = TRUE,
  plot_preload_scope = "current"
)

load_local_settings <- function(path = LOCAL_SETTINGS_FILE) {
  if (!file.exists(path)) return(list(values = LOCAL_SETTINGS_DEFAULTS, warning = NULL))
  vals <- tryCatch(jsonlite::read_json(path, simplifyVector = TRUE), error = function(e) e)
  if (inherits(vals, "error") || !is.list(vals)) {
    return(list(values = LOCAL_SETTINGS_DEFAULTS, warning = "Local settings could not be read; defaults were restored."))
  }
  list(values = utils::modifyList(LOCAL_SETTINGS_DEFAULTS, vals), warning = NULL)
}

save_local_settings <- function(shared, path = LOCAL_SETTINGS_FILE) {
  dir.create(dirname(path), showWarnings = FALSE, recursive = TRUE)
  vals <- lapply(names(LOCAL_SETTINGS_DEFAULTS), function(nm) shared$params[[nm]]())
  names(vals) <- names(LOCAL_SETTINGS_DEFAULTS)
  jsonlite::write_json(vals, path, auto_unbox = TRUE, pretty = TRUE)
}

apply_local_settings <- function(shared, vals) {
  for (nm in intersect(names(vals), names(LOCAL_SETTINGS_DEFAULTS))) {
    if (!is.null(shared$params[[nm]])) shared$params[[nm]](vals[[nm]])
  }
}

reset_performance_settings <- function(shared) {
  apply_local_settings(shared, LOCAL_SETTINGS_DEFAULTS)
}

reset_plot_settings <- function(shared) {
  reset_performance_settings(shared)
  shared$params$plot_theme("UMAP Arrows")
  shared$params$point_size(1)
  shared$params$point_alpha(0.8)
  shared$params$point_shape(19)
  shared$params$point_stroke(0)
  shared$params$color_map("color")
  shared$params$low_color("#2300AD")
  shared$params$high_color("#FF8D0A")
  shared$params$use_mid_color(FALSE)
  shared$params$mid_color("white")
  shared$params$title("")
  shared$params$xlab("")
  shared$params$ylab("")
  shared$params$title_size(20)
  shared$params$axis_title_size(14)
  shared$params$axis_text_size(12)
}

my_theme <- bs_theme(
  version = 5,
  bootswatch = "flatly",
  primary = "#4b5563",
  secondary = "#9ca3af"
)

ui <- tagList(
  shinyjs::useShinyjs(),
  overlay_deps(),
  overlay_ui("global_overlay"),
  tags$head(
    includeScript("www/adaptive_plot_workspace.js"),
    tags$script(HTML("
      (function () {
        if (/Mac|iPhone|iPad|iPod/.test(navigator.platform || '')) {
          document.documentElement.classList.add('Pinhki-platform-mac');
        }
        function syncPinhkiTabPills() {
          var navs = document.querySelectorAll(
            '.Pinhki-tabs, .tabbable > .nav.nav-tabs, .tabbable > .nav.nav-pills, ' +
            '.app-tabs-wrap .nav.nav-tabs, .app-tabs-wrap .nav.nav-pills'
          );
          for (var i = 0; i < navs.length; i++) {
            var nav = navs[i];
            if (!nav) continue;
            nav.classList.add('Pinhki-tab-pill-track', 'app-tab-pill-track');
            if (!nav.dataset.PinhkiPillScrollBound) {
              nav.addEventListener('scroll', syncPinhkiTabPills, { passive: true });
              nav.dataset.PinhkiPillScrollBound = '1';
            }
            var active = nav.querySelector('li.active > a, .nav-item.active > a, .nav-link.active');
            if (!active) {
              nav.style.setProperty('--Pinhki-tab-pill-w', '0px');
              nav.style.setProperty('--app-tab-pill-w', '0px');
              continue;
            }
            var activeItem = active.closest('li, .nav-item');
            var x = ((activeItem ? activeItem.offsetLeft : 0) + active.offsetLeft) + nav.scrollLeft;
            var w = active.offsetWidth;
            nav.style.setProperty('--Pinhki-tab-pill-x', (Number.isFinite(x) ? x : 0) + 'px');
            nav.style.setProperty('--Pinhki-tab-pill-w', (Number.isFinite(w) ? w : 0) + 'px');
            nav.style.setProperty('--app-tab-pill-x', (Number.isFinite(x) ? x : 0) + 'px');
            nav.style.setProperty('--app-tab-pill-w', (Number.isFinite(w) ? w : 0) + 'px');
          }
        }
        function syncLayoutHeightVars() {
          var nav = document.querySelector('.navbar');
          var footer = document.querySelector('.footer');
          var navRect = nav ? nav.getBoundingClientRect() : null;
          var isDesktopRail = !!(
            navRect &&
            window.innerWidth > 991 &&
            window.matchMedia('(orientation: landscape)').matches &&
            navRect.height >= (window.innerHeight * 0.75) &&
            navRect.width <= 140
          );
          var navH = navRect ? Math.ceil(navRect.height || 56) : 56;
          if (isDesktopRail) navH = 0;
          var footerH = footer ? Math.ceil(footer.getBoundingClientRect().height || 0) : 0;
          document.documentElement.style.setProperty('--nav-h', navH + 'px');
          if (navRect) document.documentElement.style.setProperty('--nav-rail-h', Math.ceil(navRect.height || 0) + 'px');
          document.documentElement.style.setProperty('--footer-h', footerH + 'px');
        }
        function collapseSidebarInPortrait() {
          var isPortrait = window.matchMedia('(max-width: 991.98px), (orientation: portrait)').matches;
          if (!isPortrait) return;
          var sidebar = document.querySelector('.bslib-sidebar-layout.sidebar-right');
          if (!sidebar || sidebar.classList.contains('sidebar-collapsed')) return;
          var toggle = sidebar.querySelector('.collapse-toggle, .bslib-sidebar-toggle');
          if (toggle) toggle.click();
        }
        window.addEventListener('load', syncLayoutHeightVars);
        window.addEventListener('load', syncPinhkiTabPills);
        window.addEventListener('resize', function(){
          syncLayoutHeightVars();
          syncPinhkiTabPills();
          collapseSidebarInPortrait();
        });
        document.addEventListener('shiny:connected', function(){
          syncLayoutHeightVars();
          syncPinhkiTabPills();
          collapseSidebarInPortrait();
        });
        document.addEventListener('shown.bs.tab', syncPinhkiTabPills);
        document.addEventListener('shiny:bound', syncPinhkiTabPills);
        document.addEventListener('shiny:value', syncPinhkiTabPills);
        document.addEventListener('shown.bs.collapse', syncLayoutHeightVars);
        setTimeout(function(){
          syncLayoutHeightVars();
          syncPinhkiTabPills();
          collapseSidebarInPortrait();
        }, 50);
        setTimeout(syncPinhkiTabPills, 250);
      })();
    "))
    ,
    tags$script(HTML("
      (function registerFlowJoHandlers(){
      if (!window.Shiny || !Shiny.addCustomMessageHandler) {
        setTimeout(registerFlowJoHandlers, 25);
        return;
      }
      if (window.FlowJoHandlersRegistered) return;
      window.FlowJoHandlersRegistered = true;
      Shiny.addCustomMessageHandler('overlay_show', function(payload){
        if (!payload || !payload.id) return;
        window.overlayShow(payload.id, payload.text || 'Working...', payload.tip || '', payload.progress, payload.progressLabel || '');
      });
      Shiny.addCustomMessageHandler('overlay_hide', function(payload){
        if (!payload || !payload.id) return;
        window.overlayHide(payload.id);
      });
      Shiny.addCustomMessageHandler('workspace_page_loading', function(payload){
        var timeoutMs = Number((payload && payload.timeout_ms) || 120000);
        window.FlowJoWorkspaceLoadingToken = (window.FlowJoWorkspaceLoadingToken || 0) + 1;
        window.FlowJoWorkspaceLoadingActive = true;
        var token = window.FlowJoWorkspaceLoadingToken;
        var start = Date.now();
        var done = false;
        var hasContent = function(id){
          var el = document.getElementById(id);
          if (!el) return false;
          if (el.querySelector('.workspace-tree, .gate-nav-empty, .workspace-details-scroll > *')) return true;
          return (el.textContent || '').trim().length > 0;
        };
        var keepVisible = function(){
          window.overlayShow(
            'global_overlay',
            'Opening Workspace...',
            'Loading samples, gates, and details.'
          );
        };
        var finish = function(){
          if (!done && token === window.FlowJoWorkspaceLoadingToken) {
            done = true;
            window.FlowJoWorkspaceLoadingActive = false;
            window.overlayHide('global_overlay');
          }
        };
        var tick = function(){
          if (done || token !== window.FlowJoWorkspaceLoadingToken) return;
          var overlay = document.getElementById('global_overlay');
          if (!overlay || getComputedStyle(overlay).display === 'none') keepVisible();
          var ready = hasContent('workspace-workspace_tree') && hasContent('workspace-workspace_details');
          if (ready || Date.now() - start > timeoutMs) {
            finish();
            return;
          }
          requestAnimationFrame(tick);
        };
        keepVisible();
        requestAnimationFrame(tick);
      });
      Shiny.addCustomMessageHandler('workspace_page_loading_cancel', function(payload){
        if (window.FlowJoWorkspaceLoadingActive) window.overlayHide('global_overlay');
        window.FlowJoWorkspaceLoadingToken = (window.FlowJoWorkspaceLoadingToken || 0) + 1;
        window.FlowJoWorkspaceLoadingActive = false;
      });
      document.addEventListener('click', function(event){
        var target = event.target && event.target.closest && event.target.closest(
          'a[data-value=\"workspace_tab\"], button[data-value=\"workspace_tab\"], a[data-bs-target=\"#workspace_tab\"], button[data-bs-target=\"#workspace_tab\"], a[href=\"#workspace_tab\"]'
        );
        if (!target || typeof window.overlayShow !== 'function') return;
        window.overlayShow(
          'global_overlay',
          'Opening Workspace...',
          'Loading samples, gates, and details.'
        );
      }, true);
      Shiny.addCustomMessageHandler('app_nav_select', function(payload){
        if (!payload || !payload.selected) return;
        var selected = String(payload.selected);
        if (window.Shiny && Shiny.setInputValue) {
          Shiny.setInputValue('app_nav_message_received', {
            selected: selected,
            href: window.location.href,
            nonce: Math.random()
          }, {priority: 'event'});
        }
        var target = document.querySelector(
          '.navbar a[data-value=\"' + selected + '\"], ' +
          '.navbar button[data-bs-target=\"#' + selected + '\"], ' +
          '.navbar a[href=\"#' + selected + '\"], ' +
          '[data-bs-toggle=\"tab\"][data-value=\"' + selected + '\"], ' +
          '[data-bs-toggle=\"pill\"][data-value=\"' + selected + '\"], ' +
          '[role=\"tab\"][data-value=\"' + selected + '\"]'
        );
        if (target && typeof target.click === 'function') target.click();
        requestAnimationFrame(function(){
          window.dispatchEvent(new Event('resize'));
          setTimeout(function(){
            if (!window.Shiny || !Shiny.setInputValue) return;
            var overlay = document.getElementById('global_overlay');
            var overlayStyle = overlay ? getComputedStyle(overlay) : null;
            var activePane = document.querySelector('.tab-pane.active');
            var workspaceTree = document.getElementById('workspace-workspace_tree');
            var workspaceDetails = document.getElementById('workspace-workspace_details');
            var sidebarFiles = document.getElementById('sidebar-fcs_registry_cards');
            var rectSummary = function(el) {
              if (!el) return null;
              var r = el.getBoundingClientRect();
              var s = getComputedStyle(el);
              return {
                id: el.id || '',
                cls: String(el.className || '').slice(0, 120),
                text: (el.textContent || '').trim().slice(0, 120),
                html: (el.innerHTML || '').length,
                x: Math.round(r.x),
                y: Math.round(r.y),
                w: Math.round(r.width),
                h: Math.round(r.height),
                display: s.display,
                visibility: s.visibility,
                opacity: s.opacity
              };
            };
            Shiny.setInputValue('app_nav_client_trace', {
              selected: selected,
              targetFound: !!target,
              targetText: target ? (target.textContent || '').trim().slice(0, 80) : '',
              activePane: rectSummary(activePane),
              workspaceTree: rectSummary(workspaceTree),
              workspaceDetails: rectSummary(workspaceDetails),
              sidebarFiles: rectSummary(sidebarFiles),
              overlay: overlay ? {
                display: overlayStyle.display,
                visibility: overlayStyle.visibility,
                opacity: overlayStyle.opacity,
                text: (overlay.textContent || '').trim().slice(0, 120)
              } : null,
              bodyText: (document.body.textContent || '').trim().slice(0, 180),
              nonce: Math.random()
            }, {priority: 'event'});
          }, 250);
        });
      });
      Shiny.addCustomMessageHandler('overlay_hide_when_output_ready', function(payload){
        if (!payload || !payload.id || !payload.output_id) return;
        var id = payload.id;
        var outId = payload.output_id;
        var readySelector = payload.ready_selector ? String(payload.ready_selector) : '';
        var readyToken = payload.ready_token ? String(payload.ready_token) : '';
        if (readyToken) {
          window.FlowJoCanvasReadyTokens = window.FlowJoCanvasReadyTokens || {};
          window.FlowJoCanvasReadyTokens[outId] = readyToken;
        }
        var timeoutMs = Number(payload.timeout_ms || 120000);
        var start = Date.now();
        var done = false;
        var hide = function(){ if (!done) { done = true; window.overlayHide(id); } };
        var tick = function(){
          if (done) return;
          var el = document.getElementById(outId);
          if (el && readyToken && el.getAttribute('data-flowjo-canvas-ready-token') === readyToken) {
            hide(); return;
          }
          if (el && readyToken && el.querySelector && el.querySelector('[data-flowjo-canvas-ready-token=\"' + readyToken + '\"]')) {
            hide(); return;
          }
          if (el && readySelector && el.querySelector && el.querySelector(readySelector)) {
            hide(); return;
          }
          if (el && !readySelector && el.querySelector && (el.querySelector('img,svg,canvas') || el.textContent.trim().length > 0)) {
            hide(); return;
          }
          if (Date.now() - start > timeoutMs) { hide(); return; }
          requestAnimationFrame(tick);
        };
        requestAnimationFrame(tick);
      });
      })();
    "))
  ),
  wan_lab_navbar(
    id = "main_nav",
    theme = my_theme,
    import_ui = import_page_ui("import"),
    workspace_ui = workspace_page_ui("workspace"),
    compensation_ui = compensation_page_ui("compensation"),
    gating_ui = gating_page_ui("gating"),
    stats_ui = statistics_page_ui("statistics"),
    layout_ui = layout_editor_page_ui("layout"),
    import_export_ui = import_export_page_ui("import_export"),
    feature_ui = feature_page_ui("feature"),
    umap_ui = umap_flowsom_page_ui("umap"),
    pipeline_ui = flowjo_pipeline_page_ui("pipeline"),
    right_sidebar_ui = right_sidebar_ui("sidebar"),
    APP_VERSION = APP_VERSION
  ),
  wan_lab_footer(APP_VERSION = APP_VERSION)
)

server <- function(input, output, session) {
  shared <- make_shared_store(session)
  loaded_settings <- load_local_settings()
  apply_local_settings(shared, loaded_settings$values)
  if (!is.null(loaded_settings$warning)) {
    session$onFlushed(function() showNotification(loaded_settings$warning, type = "warning", duration = 8), once = TRUE)
  }

  observeEvent(input$app_nav_client_trace, {
    x <- input$app_nav_client_trace
    fmt <- function(el) {
      if (is.null(el)) return("<missing>")
      sprintf(
        "id=%s class=%s rect=%sx%s@%s,%s display=%s visibility=%s opacity=%s html=%s text=%s",
        el$id %||% "",
        el$cls %||% "",
        el$w %||% "",
        el$h %||% "",
        el$x %||% "",
        el$y %||% "",
        el$display %||% "",
        el$visibility %||% "",
        el$opacity %||% "",
        el$html %||% "",
        el$text %||% ""
      )
    }
    ov <- x$overlay
    message("[app-nav-client] selected=", x$selected %||% "", " targetFound=", isTRUE(x$targetFound), " targetText=", x$targetText %||% "")
    message("[app-nav-client] activePane ", fmt(x$activePane))
    message("[app-nav-client] workspaceTree ", fmt(x$workspaceTree))
    message("[app-nav-client] workspaceDetails ", fmt(x$workspaceDetails))
    message("[app-nav-client] sidebarFiles ", fmt(x$sidebarFiles))
    message("[app-nav-client] overlay display=", ov$display %||% "<missing>", " visibility=", ov$visibility %||% "", " opacity=", ov$opacity %||% "", " text=", ov$text %||% "")
    message("[app-nav-client] bodyText=", x$bodyText %||% "")
  }, ignoreInit = TRUE)

  observeEvent(input$app_nav_message_received, {
    x <- input$app_nav_message_received
    message("[app-nav-client] message received selected=", x$selected %||% "", " href=", x$href %||% "")
  }, ignoreInit = TRUE)

  observeEvent(input$app_nav_bind_error, {
    x <- input$app_nav_bind_error
    message("[app-nav-client] bind error selected=", x$selected %||% "", " message=", x$message %||% "")
  }, ignoreInit = TRUE)

  observe({
    message("[app-nav-state] main_nav=", input$main_nav %||% "<null>")
  })

  observeEvent(input$main_nav, {
    tab <- input$main_nav %||% "<null>"
    if (!tab %in% c("workspace_tab", "gating_tab")) return()
    message(
      "[page-state] active=", tab,
      " samples=", length(shared$fcs$registry()),
      " table_rows=", nrow(shared$fcs$table()),
      " gates=", length(shared$gating$gates()),
      " groups=", length(shared$gating$groups()),
      " active=", shared$fcs$active_id() %||% "<none>"
    )
  }, ignoreInit = TRUE)

  observeEvent(input$main_nav, {
    if (identical(input$main_nav %||% NULL, "workspace_tab")) {
      session$sendCustomMessage("workspace_page_loading", list(timeout_ms = 120000L))
    } else {
      session$sendCustomMessage("workspace_page_loading_cancel", list())
    }
  })

  observeEvent(shared$ui$nav_route_request(), {
    req <- shared$ui$nav_route_request()
    tab <- req$tab %||% ""
    if (!nzchar(tab)) return()
    app_trace("root queued route=", tab, " current=", input$main_nav %||% "<null>", .prefix = "[app-nav-route]")
    tryCatch({
      app_trace("app_nav_select start route=", tab, .prefix = "[app-nav-route]")
      app_nav_select(session, tab)
      app_trace("app_nav_select done route=", tab, .prefix = "[app-nav-route]")
    }, error = function(e) app_trace_error("app-nav-route:app_nav_select", e))
    app_trace("route observer done selected=", tab, .prefix = "[app-nav-route]")
  }, ignoreInit = TRUE)

  right_sidebar_server("sidebar", shared)

  observeEvent(shared$fcs$auto_switch_to_umap(), {
    if (isTRUE(shared$fcs$auto_switch_to_umap())) {
      app_nav_select(session, "gating_tab")
      shared$fcs$clear_auto_switch_to_umap()
      showNotification("No UMAP or imported gates found. Switched to Gating.", type = "message")
    }
  }, ignoreInit = TRUE)

  observeEvent(input$nav_help, {
    showModal(modalDialog(
      title = "FlowJo Re-plotting Tool",
      easyClose = TRUE,
      p("Import FCS first, then create an analysis object for Feature Plots and Cluster runs."),
      p("Global settings and workspace save/load are in the right sidebar."),
      footer = modalButton("Close")
    ))
  })

  observeEvent(input$nav_dark_mode_toggle, {
    shinyjs::runjs("
      (function(){
        var b = document.body;
        if (b.classList.contains('app-dark-mode')) {
          b.classList.remove('app-dark-mode');
        } else {
          b.classList.add('app-dark-mode');
        }
      })();
    ")
  })

  observeEvent(input$nav_settings, {
    showModal(modalDialog(
      title = "Shared Plot Settings",
      easyClose = TRUE, size = "l",
      uiOutput("shared_controls_ui"),
      footer = modalButton("Close")
    ))
  })

  output$shared_controls_ui <- renderUI({
    tagList(
      h5("Global Plot Style"),
      radioButtons("sh_theme", "Theme", inline = TRUE,
                   choices = c("UMAP Arrows", "Minimal", "Classic", "Black & White", "Cowplot"),
                   selected = shared$params$plot_theme()
      ),
      sliderInput("sh_point_size", "Point Size", min = 0.1, max = 10, value = shared$params$point_size(), step = 0.1),
      sliderInput("sh_point_alpha","Transparency", min = 0.1, max = 1.0, value = shared$params$point_alpha(), step = 0.05),
      sliderInput("sh_point_shape","Shape", min = 0, max = 25, value = shared$params$point_shape(), step = 1),
      sliderInput("sh_point_stroke","Border Width", min = 0, max = 3, value = shared$params$point_stroke(), step = 0.1),
      radioButtons("sh_color_map", "Apply Color To", inline = TRUE,
                   choices = c("Fill (shapes 21-25)" = "fill", "Color (all shapes)" = "color"),
                   selected = shared$params$color_map()
      ),
      fluidRow(
        column(6, colourpicker::colourInput("sh_low_color",  "Low",  shared$params$low_color())),
        column(6, colourpicker::colourInput("sh_high_color", "High", shared$params$high_color()))
      ),
      checkboxInput("sh_use_mid_color", "Use Mid", shared$params$use_mid_color()),
      shinyjs::hidden(div(id = "sh_mid_wrap",
                          colourpicker::colourInput("sh_mid_color", "Mid", shared$params$mid_color())
      )),
      hr(),
      h5("Axis Labels"),
      textInput("sh_title", "Title", value = shared$params$title()),
      textInput("sh_xlab",  "X Label", value = shared$params$xlab()),
      textInput("sh_ylab",  "Y Label", value = shared$params$ylab()),
      sliderInput("sh_title_size", "Title Size", min = 8, max = 32, value = shared$params$title_size(), step = 1),
      sliderInput("sh_axis_title_size", "Axis Title Size", min = 8, max = 24, value = shared$params$axis_title_size(), step = 1),
      sliderInput("sh_axis_text_size", "Axis Text Size", min = 8, max = 20, value = shared$params$axis_text_size(), step = 1),
      hr(),
      h5("Performance"),
      checkboxInput("sh_plot_cache_enabled", "Enable plot cache", value = isTRUE(shared$params$plot_cache_enabled())),
      checkboxInput("sh_plot_preload_enabled", "Preload gating plots", value = isTRUE(shared$params$plot_preload_enabled())),
      checkboxInput("sh_plot_preload_after_load", "Prepare plots after loading", value = isTRUE(shared$params$plot_preload_after_load())),
      checkboxInput("sh_plot_event_limit_enabled", "Limit plotted events", value = isTRUE(shared$params$plot_event_limit_enabled())),
      numericInput("sh_plot_event_limit", "Max plotted events", value = shared$params$plot_event_limit(), min = 1000, step = 1000),
      selectInput(
        "sh_plot_cache_mode", "Cache mode",
        choices = c(
          "Rebuild when plot settings change" = "settings",
          "Keep full filtered data" = "full",
          "Keep sampled plotted data" = "sampled"
        ),
        selected = shared$params$plot_cache_mode()
      ),
      tags$small(class = "text-muted", "Default rebuild mode keeps cached plot data aligned with current plot settings."),
      numericInput("sh_plot_cache_max_contexts", "Max cached plot contexts", value = shared$params$plot_cache_max_contexts(), min = 1, step = 1),
      selectInput(
        "sh_plot_preload_scope", "Preload sample scope",
        choices = c("All loaded samples" = "all", "Grouped/synced samples only" = "grouped", "Current sample only" = "current"),
        selected = shared$params$plot_preload_scope()
      ),
      fluidRow(
        column(6, actionButton("sh_reset_performance", "Reset performance settings", class = "btn btn-outline-secondary btn-sm")),
        column(6, actionButton("sh_reset_plot", "Reset all plot settings", class = "btn btn-outline-secondary btn-sm"))
      )
    )
  })

  persist_local_settings <- function() {
    tryCatch(save_local_settings(shared), error = function(e) showNotification(conditionMessage(e), type = "warning", duration = 8))
  }

  observe({ shinyjs::toggle(id = "sh_mid_wrap", condition = isTRUE(input$sh_use_mid_color)) })
  observeEvent(input$sh_theme,          shared$params$plot_theme(input$sh_theme),          ignoreInit = TRUE)
  observeEvent(input$sh_point_size,     shared$params$point_size(input$sh_point_size),     ignoreInit = TRUE)
  observeEvent(input$sh_point_alpha,    shared$params$point_alpha(input$sh_point_alpha),   ignoreInit = TRUE)
  observeEvent(input$sh_point_shape,    shared$params$point_shape(input$sh_point_shape),   ignoreInit = TRUE)
  observeEvent(input$sh_point_stroke,   shared$params$point_stroke(input$sh_point_stroke), ignoreInit = TRUE)
  observeEvent(input$sh_color_map,      shared$params$color_map(input$sh_color_map),       ignoreInit = TRUE)
  observeEvent(input$sh_low_color,      shared$params$low_color(input$sh_low_color),       ignoreInit = TRUE)
  observeEvent(input$sh_high_color,     shared$params$high_color(input$sh_high_color),     ignoreInit = TRUE)
  observeEvent(input$sh_use_mid_color,  shared$params$use_mid_color(input$sh_use_mid_color), ignoreInit = TRUE)
  observeEvent(input$sh_mid_color,      shared$params$mid_color(input$sh_mid_color),       ignoreInit = TRUE)
  observeEvent(input$sh_title,          shared$params$title(input$sh_title),               ignoreInit = TRUE)
  observeEvent(input$sh_xlab,           shared$params$xlab(input$sh_xlab),                 ignoreInit = TRUE)
  observeEvent(input$sh_ylab,           shared$params$ylab(input$sh_ylab),                 ignoreInit = TRUE)
  observeEvent(input$sh_title_size,     shared$params$title_size(input$sh_title_size),     ignoreInit = TRUE)
  observeEvent(input$sh_axis_title_size,shared$params$axis_title_size(input$sh_axis_title_size), ignoreInit = TRUE)
  observeEvent(input$sh_axis_text_size, shared$params$axis_text_size(input$sh_axis_text_size),   ignoreInit = TRUE)
  observeEvent(input$sh_plot_cache_enabled, {
    shared$params$plot_cache_enabled(isTRUE(input$sh_plot_cache_enabled))
    if (isTRUE(shared$params$plot_preload_enabled()) && !isTRUE(input$sh_plot_cache_enabled)) {
      shared$params$plot_preload_enabled(FALSE)
      updateCheckboxInput(session, "sh_plot_preload_enabled", value = FALSE)
      showNotification("Preload requires the plot cache, so preloading was turned off.", type = "warning")
    }
    persist_local_settings()
  }, ignoreInit = TRUE)
  observeEvent(input$sh_plot_preload_enabled, {
    if (isTRUE(input$sh_plot_preload_enabled) && !isTRUE(shared$params$plot_cache_enabled())) {
      shared$params$plot_cache_enabled(TRUE)
      updateCheckboxInput(session, "sh_plot_cache_enabled", value = TRUE)
      showNotification("Plot cache was enabled because preload uses cached plot data.", type = "warning")
    }
    shared$params$plot_preload_enabled(isTRUE(input$sh_plot_preload_enabled))
    persist_local_settings()
  }, ignoreInit = TRUE)
  observeEvent(input$sh_plot_preload_after_load, {
    shared$params$plot_preload_after_load(isTRUE(input$sh_plot_preload_after_load))
    persist_local_settings()
  }, ignoreInit = TRUE)
  observeEvent(input$sh_plot_event_limit_enabled, { shared$params$plot_event_limit_enabled(isTRUE(input$sh_plot_event_limit_enabled)); persist_local_settings() }, ignoreInit = TRUE)
  observeEvent(input$sh_plot_event_limit, { shared$params$plot_event_limit(max(1000L, as.integer(input$sh_plot_event_limit %||% 75000L))); persist_local_settings() }, ignoreInit = TRUE)
  observeEvent(input$sh_plot_cache_mode, { shared$params$plot_cache_mode(input$sh_plot_cache_mode %||% "settings"); persist_local_settings() }, ignoreInit = TRUE)
  observeEvent(input$sh_plot_cache_max_contexts, { shared$params$plot_cache_max_contexts(max(1L, as.integer(input$sh_plot_cache_max_contexts %||% 50L))); persist_local_settings() }, ignoreInit = TRUE)
  observeEvent(input$sh_plot_preload_scope, { shared$params$plot_preload_scope(input$sh_plot_preload_scope %||% "current"); persist_local_settings() }, ignoreInit = TRUE)

  observeEvent(input$sh_reset_performance, {
    showModal(modalDialog(
      title = "Reset performance settings?",
      "This restores cache, preload, and plotted-event options to their defaults.",
      footer = tagList(modalButton("Cancel"), actionButton("confirm_reset_performance", "Reset", class = "btn btn-danger"))
    ))
  }, ignoreInit = TRUE)
  observeEvent(input$confirm_reset_performance, {
    reset_performance_settings(shared)
    persist_local_settings()
    removeModal()
    showNotification("Performance settings reset.", type = "message")
  }, ignoreInit = TRUE)
  observeEvent(input$sh_reset_plot, {
    showModal(modalDialog(
      title = "Reset all plot settings?",
      "This restores plot style and performance options to their defaults.",
      footer = tagList(modalButton("Cancel"), actionButton("confirm_reset_plot", "Reset", class = "btn btn-danger"))
    ))
  }, ignoreInit = TRUE)
  observeEvent(input$confirm_reset_plot, {
    reset_plot_settings(shared)
    persist_local_settings()
    removeModal()
    showNotification("Plot settings reset.", type = "message")
  }, ignoreInit = TRUE)

  import_page_server("import", shared)
  workspace_page_server("workspace", shared)
  compensation_page_server("compensation", shared)
  gating_page_server("gating", shared)
  statistics_page_server("statistics", shared)
  layout_editor_page_server("layout", shared)
  import_export_page_server("import_export", shared)
  feature_page_server("feature", shared)
  umap_flowsom_page_server("umap", shared)
  flowjo_pipeline_page_server("pipeline", shared)
}

shinyApp(ui, server)
