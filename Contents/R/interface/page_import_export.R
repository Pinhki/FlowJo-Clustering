import_export_page_ui <- function(id) {
  ns <- NS(id)
  div(class = "card-page-wrap",
    card_page_header("Import / Export", subtitle = "FCS, WSP, app workspace, and report exports."),
    fluidRow(
      column(4, card_page_card("Reference Inventory",
        textInput(ns("ref_path"), "Reference directory", value = "/Users/rongzhenyu/Library/CloudStorage/Box-Box/RY Result Box/Flow Data - Symphony/NEW 3AM exp/HLA-A2/RY NY83 Tet Test OtherTetramers in different tissues_001"),
        actionButton(ns("scan_ref"), "Scan Reference", class = "btn btn-secondary w-100"),
        div(style = "height:8px"),
        actionButton(ns("import_wsp"), "Import WSP Report", class = "btn btn-primary w-100")
      )),
      column(8,
        card_page_card("Analysis Bundle",
          div(class = "Pinhki-inline-note", tags$b("Analysis object import. "), "Load a saved analysis bundle without replacing the full app workspace."),
          div(class = "workspace-load-upload", fileInput(ns("load_analysis_bundle"), NULL, accept = ".zip", buttonLabel = "Load Analysis Bundle", multiple = FALSE))
        ),
        card_page_card("App Workspace",
          p("Save or load the full app workspace as an RDS file. Loading replaces the current in-memory workspace."),
          div(class = "d-flex gap-2",
            downloadButton(
              ns("save_workspace"),
              "Save Workspace",
              class = "btn btn-primary",
              onclick = "window.overlayShow && window.overlayShow('global_overlay','Saving workspace...','Large workspaces can take a moment to serialize.');"
            ),
            div(class = "workspace-load-upload", fileInput(ns("load_workspace"), NULL, accept = ".rds", buttonLabel = "Load Workspace", multiple = FALSE))
          ),
          tags$script(HTML(sprintf("
            document.addEventListener('change', function(event){
              var input = event.target;
              if (!input || input.type !== 'file' || input.id !== '%s') return;
              if (!input.files || input.files.length === 0) return;
              window.overlayShow && window.overlayShow('global_overlay','Loading workspace...','Reading selected workspace file.');
            }, true);
          ", ns("load_workspace"))))
        ),
        card_page_card("Fast App Workspace (QS2)",
          p("Fast compressed workspace save/load for large sessions. Use this when RDS is slow; loading still replaces the current in-memory workspace."),
          div(class = "d-flex gap-2",
            downloadButton(
              ns("save_workspace_qs"),
              "Save Workspace QS",
              class = "btn btn-primary",
              onclick = "window.overlayShow && window.overlayShow('global_overlay','Saving fast workspace...','Large workspaces can take a moment to serialize.');"
            ),
            div(class = "workspace-load-upload", fileInput(ns("load_workspace_qs"), NULL, accept = ".qs", buttonLabel = "Load Workspace QS", multiple = FALSE))
          ),
          tags$script(HTML(sprintf("
            document.addEventListener('change', function(event){
              var input = event.target;
              if (!input || input.type !== 'file' || input.id !== '%s') return;
              if (!input.files || input.files.length === 0) return;
              window.overlayShow && window.overlayShow('global_overlay','Loading fast workspace...','Reading selected workspace file.');
            }, true);
          ", ns("load_workspace_qs"))))
        ),
        card_page_card("Files & WSP Report",
          DT::dataTableOutput(ns("ref_files")),
          hr(),
          verbatimTextOutput(ns("wsp_report"))
        )
      )
    )
  )
}

import_export_page_server <- function(id, shared) {
  moduleServer(id, function(input, output, session) {
    inventory <- reactiveVal(data.frame(type = character(), path = character(), name = character()))
    pending_workspace <- reactiveVal(NULL)
    pending_workspace_qs <- reactiveVal(NULL)
    pending_analysis_bundle <- reactiveVal(NULL)

    require_qs2 <- function() {
      if (!requireNamespace("qs2", quietly = TRUE)) {
        stop("Package 'qs2' is required for fast workspace save/load.")
      }
    }

    observeEvent(input$scan_ref, {
      overlay_show("Scanning reference inventory...")
      on.exit(overlay_hide(), add = TRUE)
      inventory(flowjo_reference_inventory(input$ref_path))
    }, ignoreInit = FALSE)

    output$ref_files <- DT::renderDataTable({
      DT::datatable(inventory(), options = list(pageLength = 15, scrollX = TRUE), rownames = FALSE)
    })

    observeEvent(input$import_wsp, {
      overlay_show("Importing WSP report...")
      on.exit(overlay_hide(), add = TRUE)
      inv <- inventory()
      wsp <- inv$path[inv$type == "wsp"][1]
      req(!is.na(wsp), nzchar(wsp))
      shared$gating$wsp_report(flowjo_wsp_import_report(wsp, dirname(wsp), execute = FALSE))
    })

    output$save_workspace <- downloadHandler(
      filename = function() paste0("flowjo_workspace_", Sys.Date(), ".rds"),
      content = function(file) {
        overlay_show("Saving workspace...")
        on.exit(overlay_hide(), add = TRUE)
        saveRDS(shared$export_state(), file)
      }
    )

    output$save_workspace_qs <- downloadHandler(
      filename = function() paste0("flowjo_workspace_", Sys.Date(), ".qs"),
      content = function(file) {
        overlay_show("Saving fast workspace...")
        on.exit(overlay_hide(), add = TRUE)
        require_qs2()
        qs2::qs_save(shared$export_state(), file)
      }
    )

    has_workspace_state <- function() {
      length(shared$fcs$registry()) ||
        length(shared$gating$gates()) ||
        length(shared$gating$groups()) ||
        length(shared$populations$items())
    }

    load_workspace_file <- function(path) {
      overlay_show("Loading workspace...")
      on.exit(overlay_hide(), add = TRUE)
      obj <- readRDS(path)
      if (!is.list(obj)) stop("Workspace file is not a valid saved workspace.")
      shared$import_state(obj)
      shiny::updateNavbarPage(session = shared$session, inputId = "main_nav", selected = "workspace_tab")
      showNotification("Workspace loaded.", type = "message")
    }

    load_workspace_qs_file <- function(path) {
      overlay_show("Loading fast workspace...")
      on.exit(overlay_hide(), add = TRUE)
      require_qs2()
      obj <- qs2::qs_read(path)
      if (!is.list(obj)) stop("Workspace file is not a valid saved workspace.")
      shared$import_state(obj)
      shiny::updateNavbarPage(session = shared$session, inputId = "main_nav", selected = "workspace_tab")
      showNotification("Workspace loaded.", type = "message")
    }

    load_analysis_bundle_file <- function(path, name = NULL) {
      overlay_show("Loading analysis bundle...")
      on.exit(overlay_hide(), add = TRUE)
      obj <- shared$analysis$import_bundle(path, name = name)
      shiny::updateNavbarPage(session = shared$session, inputId = "main_nav", selected = "umap_tab")
      showNotification(sprintf("Analysis object loaded: %s", obj$name), type = "message")
    }

    observeEvent(input$load_analysis_bundle, {
      req(input$load_analysis_bundle$datapath)
      path <- input$load_analysis_bundle$datapath
      tryCatch(load_analysis_bundle_file(path), error = function(e) {
        overlay_hide()
        pending_analysis_bundle(path)
        showModal(modalDialog(
          title = "Rename Analysis Object",
          div(class = "Pinhki-inline-note", tags$b("Name already exists or cannot be used. "), conditionMessage(e)),
          textInput(ns("analysis_bundle_name"), "New object name", value = tools::file_path_sans_ext(input$load_analysis_bundle$name %||% "Imported analysis")),
          footer = tagList(modalButton("Cancel"), actionButton(ns("confirm_analysis_bundle_name"), "Import Bundle", class = "btn btn-primary")),
          easyClose = TRUE
        ))
      })
    }, ignoreInit = TRUE)

    observeEvent(input$confirm_analysis_bundle_name, {
      path <- pending_analysis_bundle()
      pending_analysis_bundle(NULL)
      removeModal()
      req(path)
      name <- trimws(input$analysis_bundle_name %||% "")
      if (!nzchar(name)) {
        showNotification("Object name is required.", type = "warning")
        return()
      }
      tryCatch(load_analysis_bundle_file(path, name = name), error = function(e) {
        overlay_hide()
        showNotification(conditionMessage(e), type = "error", duration = 10)
      })
    }, ignoreInit = TRUE)

    observeEvent(input$load_workspace, {
      req(input$load_workspace$datapath)
      path <- input$load_workspace$datapath
      if (isTRUE(has_workspace_state())) {
        pending_workspace(path)
        overlay_hide()
        showModal(modalDialog(
          title = "Replace current workspace?",
          "Loading this workspace will replace the current in-memory FCS files, gates, groups, populations, transforms, and analysis state.",
          footer = tagList(
            modalButton("Cancel"),
            actionButton(session$ns("confirm_load_workspace"), "Replace and Load", class = "btn-danger")
          )
        ))
      } else {
        tryCatch(load_workspace_file(path), error = function(e) {
          overlay_hide()
          showNotification(conditionMessage(e), type = "error")
        })
      }
    }, ignoreInit = TRUE)

    observeEvent(input$load_workspace_qs, {
      req(input$load_workspace_qs$datapath)
      path <- input$load_workspace_qs$datapath
      if (isTRUE(has_workspace_state())) {
        pending_workspace_qs(path)
        overlay_hide()
        showModal(modalDialog(
          title = "Replace current workspace?",
          "Loading this workspace will replace the current in-memory FCS files, gates, groups, populations, transforms, and analysis state.",
          footer = tagList(
            modalButton("Cancel"),
            actionButton(session$ns("confirm_load_workspace_qs"), "Replace and Load", class = "btn-danger")
          )
        ))
      } else {
        tryCatch(load_workspace_qs_file(path), error = function(e) {
          overlay_hide()
          showNotification(conditionMessage(e), type = "error")
        })
      }
    }, ignoreInit = TRUE)

    observeEvent(input$confirm_load_workspace, {
      path <- pending_workspace()
      pending_workspace(NULL)
      removeModal()
      req(path)
      tryCatch(load_workspace_file(path), error = function(e) {
        overlay_hide()
        showNotification(conditionMessage(e), type = "error")
      })
    }, ignoreInit = TRUE)

    observeEvent(input$confirm_load_workspace_qs, {
      path <- pending_workspace_qs()
      pending_workspace_qs(NULL)
      removeModal()
      req(path)
      tryCatch(load_workspace_qs_file(path), error = function(e) {
        overlay_hide()
        showNotification(conditionMessage(e), type = "error")
      })
    }, ignoreInit = TRUE)

    output$wsp_report <- renderPrint({
      r <- shared$gating$wsp_report()
      if (is.null(r)) return("No WSP report yet.")
      cat("WSP:", r$wsp_file, "\n")
      cat("FCS path:", r$fcs_path, "\n")
      if (!is.null(r$error)) cat("Error:", r$error, "\n")
      cat("Samples:", nrow(r$samples), "\n")
      cat("Populations:", length(r$populations), "\n")
      if (length(r$populations)) print(utils::head(r$populations, 30))
    })
  })
}
