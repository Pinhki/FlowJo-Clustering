source("global.R")
APP_VERSION <- "0.1.0"
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
    tags$script(HTML("
      (function () {
        function syncLayoutHeightVars() {
          var nav = document.querySelector('.navbar');
          var footer = document.querySelector('.footer');
          var navH = nav ? Math.ceil(nav.getBoundingClientRect().height || 56) : 56;
          var footerH = footer ? Math.ceil(footer.getBoundingClientRect().height || 0) : 0;
          document.documentElement.style.setProperty('--nav-h', navH + 'px');
          document.documentElement.style.setProperty('--footer-h', footerH + 'px');
        }
        window.addEventListener('load', syncLayoutHeightVars);
        window.addEventListener('resize', syncLayoutHeightVars);
        document.addEventListener('shown.bs.collapse', syncLayoutHeightVars);
        setTimeout(syncLayoutHeightVars, 50);
      })();
    "))
    ,
    tags$script(HTML("
      Shiny.addCustomMessageHandler('overlay_show', function(payload){
        if (!payload || !payload.id) return;
        window.overlayShow(payload.id, payload.text || 'Working...', payload.tip || '');
      });
      Shiny.addCustomMessageHandler('overlay_hide', function(payload){
        if (!payload || !payload.id) return;
        window.overlayHide(payload.id);
      });
      Shiny.addCustomMessageHandler('overlay_hide_when_output_ready', function(payload){
        if (!payload || !payload.id || !payload.output_id) return;
        var id = payload.id;
        var outId = payload.output_id;
        var timeoutMs = Number(payload.timeout_ms || 120000);
        var start = Date.now();
        var done = false;
        var hide = function(){ if (!done) { done = true; window.overlayHide(id); } };
        var tick = function(){
          if (done) return;
          var el = document.getElementById(outId);
          if (el && el.querySelector && (el.querySelector('img,svg,canvas') || el.textContent.trim().length > 0)) {
            hide(); return;
          }
          if (Date.now() - start > timeoutMs) { hide(); return; }
          requestAnimationFrame(tick);
        };
        requestAnimationFrame(tick);
      });
    "))
  ),
  wan_lab_navbar(
    id = "main_nav",
    theme = my_theme,
    import_ui = import_page_ui("import"),
    feature_ui = feature_page_ui("feature"),
    flowsom_ui = flowsom_page_ui("flowsom"),
    umap_ui = umap_flowsom_page_ui("umap"),
    pipeline_ui = flowjo_pipeline_page_ui("pipeline"),
    right_sidebar_ui = right_sidebar_ui("sidebar"),
    APP_VERSION = APP_VERSION
  ),
  wan_lab_footer(APP_VERSION = APP_VERSION)
)

server <- function(input, output, session) {
  shared <- make_shared_store(session)

  sidebar_state <- right_sidebar_server("sidebar", shared)

  observeEvent(shared$fcs$auto_switch_to_umap(), {
    if (isTRUE(shared$fcs$auto_switch_to_umap())) {
      shiny::updateNavbarPage(session = session, inputId = "main_nav", selected = "umap_tab")
      shared$fcs$clear_auto_switch_to_umap()
      showNotification("No UMAP columns found in loaded FCS. Switched to UMAP + FlowSOM tab.", type = "message")
    }
  }, ignoreInit = TRUE)

  observeEvent(input$nav_help, {
    showModal(modalDialog(
      title = "FlowJo Re-plotting Tool",
      easyClose = TRUE,
      p("Import FCS first, then use Feature Plots, FlowSOM, or UMAP + FlowSOM tabs."),
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

  observeEvent(sidebar_state$open_settings(), {
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
      sliderInput("sh_axis_text_size", "Axis Text Size", min = 8, max = 20, value = shared$params$axis_text_size(), step = 1)
    )
  })

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

  import_page_server("import", shared)
  feature_page_server("feature", shared)
  flowsom_page_server("flowsom", shared)
  umap_flowsom_page_server("umap", shared)
  flowjo_pipeline_page_server("pipeline", shared)
}

shinyApp(ui, server)
