import_export_page_ui <- function(id) {
  ns <- NS(id)
  div(class = "card-page-wrap wsp-import-page",
    tags$style(HTML(".wsp-import-page .col-sm-8 { min-width: 0; } .wsp-import-page .d-flex { flex-wrap: wrap; } .wsp-import-page .dataTables_wrapper, .wsp-import-page .shiny-html-output { max-width: 100%; min-width: 0; } .wsp-import-page table.dataTable { width: 100% !important; table-layout: fixed; } .wsp-import-page table.dataTable td, .wsp-import-page .Pinhki-inline-note { overflow-wrap: anywhere; } .wsp-import-page table.dataTable th { white-space: normal; } .wsp-import-page .wsp-import-ready, .wsp-import-page .wsp-import-complete, .wsp-import-page .wsp-import-complete:disabled { background-color: #22c55e !important; border-color: #34d278 !important; border-width: 2px !important; color: #ffffff !important; opacity: 1; box-shadow: 0 0 14px rgba(34, 197, 94, 0.28) !important; } .wsp-import-page .wsp-import-ready:hover, .wsp-import-page .wsp-import-ready:focus { background-color: #16a34a !important; border-color: #2dc06a !important; box-shadow: 0 0 16px rgba(34, 197, 94, 0.32) !important; } .wsp-import-page .wsp-import-ready:active { background-color: #15803d !important; border-color: #24ad5b !important; box-shadow: 0 0 10px rgba(34, 197, 94, 0.24) !important; }")),
    card_page_header("Import / Export", subtitle = "FCS, WSP, app workspace, and report exports."),
    fluidRow(
      column(4, card_page_card("WSP Import",
        shinyFiles::shinyFilesButton(
          ns("choose_wsp_file"),
          "Select WSP file",
          "Choose the FlowJo WSP workspace to import",
          multiple = FALSE,
          buttonType = "secondary",
          class = "w-100",
          viewtype = "detail"
        ),
        uiOutput(ns("selected_wsp_path")),
        div(style = "height:8px"),
        uiOutput(ns("wsp_import_control")),
        uiOutput(ns("wsp_missing_control")),
        div(style = "height:8px"),
        uiOutput(ns("wsp_export_control"))
      )),
      column(8,
        uiOutput(ns("wsp_result_card")),
        card_page_card("Analysis Bundle",
          div(class = "Pinhki-inline-note", tags$b("Analysis object import. "), "Load a saved analysis bundle without replacing the full app workspace."),
          div(class = "workspace-load-upload", fileInput(ns("load_analysis_bundle"), NULL, accept = ".zip", buttonLabel = "Load Analysis Bundle", multiple = FALSE))
        ),
        card_page_card("App Workspace",
          p("Save or load the full app workspace as an FXRS file (RDS serialization). Loading replaces the current in-memory workspace."),
          div(class = "d-flex gap-2",
            downloadButton(
              ns("save_workspace"),
              "Save Workspace FXRS",
              class = "btn btn-primary",
              onclick = "window.overlayShow && window.overlayShow('global_overlay','Saving workspace...','Large workspaces can take a moment to serialize.');"
            ),
            div(class = "workspace-load-upload", fileInput(ns("load_workspace"), NULL, accept = c(".fxrs", ".rds"), buttonLabel = "Load Workspace FXRS", multiple = FALSE))
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
        card_page_card("Fast App Workspace (FXRS2 / QS2)",
          p("Fast compressed workspace save/load using FXRS2 (QS2 serialization). Use this when FXRS is slow; loading still replaces the current in-memory workspace."),
          div(class = "d-flex gap-2",
            downloadButton(
              ns("save_workspace_qs"),
              "Save Workspace FXRS2",
              class = "btn btn-primary",
              onclick = "window.overlayShow && window.overlayShow('global_overlay','Saving fast workspace...','Large workspaces can take a moment to serialize.');"
            ),
            div(class = "workspace-load-upload", fileInput(ns("load_workspace_qs"), NULL, accept = c(".fxrs2", ".qs", ".qs2"), buttonLabel = "Load Workspace FXRS2", multiple = FALSE))
          ),
          tags$script(HTML(sprintf("
            document.addEventListener('change', function(event){
              var input = event.target;
              if (!input || input.type !== 'file' || input.id !== '%s') return;
              if (!input.files || input.files.length === 0) return;
              window.overlayShow && window.overlayShow('global_overlay','Loading fast workspace...','Reading selected workspace file.');
            }, true);
          ", ns("load_workspace_qs"))))
        )
      )
    )
  )
}

import_export_page_server <- function(id, shared) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    inventory <- reactiveVal(data.frame(
      type = character(), path = character(), name = character(), relative_path = character()
    ))
    selected_wsp <- reactiveVal("")
    importing_wsp <- reactiveVal(FALSE)
    import_error <- reactiveVal(NULL)
    resolved_files <- reactiveVal(character())
    missing_files <- reactive({
      inv <- inventory()
      inv[inv$type == "fcs" & !file.exists(inv$path), , drop = FALSE]
    })
    pending_workspace <- reactiveVal(NULL)
    pending_workspace_qs <- reactiveVal(NULL)
    pending_analysis_bundle <- reactiveVal(NULL)

    add_reference_root <- function(roots, label, path_value) {
      if (length(path_value) != 1L || is.na(path_value) || !nzchar(path_value) || !dir.exists(path_value)) return(roots)
      path_value <- normalizePath(path_value, winslash = "/", mustWork = FALSE)
      if (path_value %in% unname(roots)) return(roots)
      label <- make.unique(c(names(roots), label), sep = " ")[length(roots) + 1L]
      c(roots, setNames(path_value, label))
    }

    home_path <- normalizePath(path.expand("~"), winslash = "/", mustWork = FALSE)
    reference_roots <- c(Home = home_path)
    common_roots <- c(
      Desktop = file.path(home_path, "Desktop"),
      Documents = file.path(home_path, "Documents"),
      Downloads = file.path(home_path, "Downloads"),
      "Cloud Storage" = file.path(home_path, "Library", "CloudStorage"),
      "iCloud Drive" = file.path(home_path, "Library", "Mobile Documents", "com~apple~CloudDocs")
    )
    for (label in names(common_roots)) {
      reference_roots <- add_reference_root(reference_roots, label, common_roots[[label]])
    }
    cloud_storage <- common_roots[["Cloud Storage"]]
    if (dir.exists(cloud_storage)) {
      cloud_providers <- list.files(cloud_storage, full.names = TRUE, recursive = FALSE)
      cloud_providers <- cloud_providers[dir.exists(cloud_providers)]
      for (provider in cloud_providers) {
        reference_roots <- add_reference_root(
          reference_roots,
          paste0("Cloud: ", basename(provider)),
          provider
        )
      }
    }
    if (dir.exists("/Volumes")) reference_roots <- add_reference_root(reference_roots, "Volumes", "/Volumes")
    volume_paths <- tryCatch(shinyFiles::getVolumes()(), error = function(e) character())
    if (length(volume_paths)) {
      volume_labels <- names(volume_paths)
      if (is.null(volume_labels)) volume_labels <- basename(as.character(volume_paths))
      for (i in seq_along(volume_paths)) {
        reference_roots <- add_reference_root(
          reference_roots,
          paste0("Volume: ", volume_labels[[i]]),
          as.character(volume_paths[[i]])
        )
      }
    }
    shinyFiles::shinyFileChoose(
      input,
      "choose_wsp_file",
      roots = reference_roots,
      filetypes = c("wsp"),
      defaultRoot = "Home",
      session = session
    )

    shinyFiles::shinyFileChoose(input, "locate_fcs", roots = reference_roots,
      filetypes = c("fcs"), defaultRoot = "Home", session = session)

    show_missing_files <- function() {
      missing <- missing_files()
      if (!nrow(missing)) return(invisible(NULL))
      showModal(modalDialog(
        title = "Locate missing FCS files",
        p("These files are required to load the complete workspace. Choose a reference, then locate its original FCS file."),
        selectInput(ns("missing_reference"), "Missing file", choices = setNames(missing$path, missing$name)),
        shinyFiles::shinyFilesButton(ns("locate_fcs"), "Locate FCS file", "Choose the missing FCS file",
          multiple = FALSE, buttonType = "primary", viewtype = "detail"),
        p("The filename must match the selected reference to avoid replacing a sample with a different file."),
        footer = modalButton("Close"), easyClose = TRUE
      ))
    }
    observeEvent(input$resolve_missing, show_missing_files())
    observeEvent(input$locate_fcs, {
      req(!importing_wsp())
      if (!is.list(input$locate_fcs)) return()
      picked <- shinyFiles::parseFilePaths(reference_roots, input$locate_fcs)
      if (!nrow(picked)) return()
      replacement <- as.character(picked$datapath[[1]])
      reference <- input$missing_reference
      inv <- inventory()
      row <- which(inv$type == "fcs" & inv$path == reference & !file.exists(inv$path))
      if (length(row) != 1L) return()
      if (!file.exists(replacement) || tolower(basename(replacement)) != tolower(inv$name[[row]])) {
        showNotification("Choose an existing FCS file with the same filename as the selected reference.", type = "error", duration = 10)
        return()
      }
      valid <- tryCatch({flowCore::read.FCS(replacement, which.lines = 1, transformation = FALSE); TRUE},
        error = function(e) {showNotification(paste("Cannot read this FCS file:", conditionMessage(e)), type = "error", duration = 10); FALSE})
      if (!valid) return()
      mapping <- resolved_files()
      key <- names(mapping)[match(reference, unname(mapping))]
      if (length(key) != 1L || is.na(key)) return()
      mapping[[key]] <- normalizePath(replacement, winslash = "/", mustWork = TRUE)
      resolved_files(mapping)
      inv$path[[row]] <- mapping[[key]]
      inv$relative_path[[row]] <- replacement
      inventory(inv)
      removeModal()
      if (nrow(missing_files())) show_missing_files()
      else showNotification("All referenced files are available. You can now import the WSP.", type = "message")
    }, ignoreInit = TRUE)

    observeEvent(input$choose_wsp_file, {
      if (importing_wsp()) return()
      selected <- tryCatch(
        shinyFiles::parseFilePaths(reference_roots, input$choose_wsp_file),
        error = function(e) data.frame()
      )
      selected_path <- if (nrow(selected)) as.character(selected$datapath[[1]]) else ""
      if (nzchar(selected_path) && grepl("[.]wsp$", selected_path, ignore.case = TRUE)) {
        selected_wsp(selected_path)
        inventory(flowjo_wsp_reference_inventory(selected_path, dirname(selected_path)))
        refs <- unique(flowjo_wsp_dataset_paths(selected_path))
        inv <- inventory()
        resolved_files(setNames(inv$path[inv$type == "fcs"], refs))
        import_error(NULL)
        shared$gating$wsp_report(NULL)
        if (nrow(missing_files())) show_missing_files()
      }
    }, ignoreInit = TRUE)

    output$selected_wsp_path <- renderUI({
      path <- selected_wsp()
      if (!nzchar(path)) return(div(class = "Pinhki-inline-note", "No WSP file selected."))
      div(
        class = "Pinhki-inline-note",
        tags$b("Selected WSP: "), basename(path),
        tags$br(),
        tags$small(dirname(path))
      )
    })

    output$wsp_import_control <- renderUI({
      report <- shared$gating$wsp_report()
      successful <- !is.null(report) && is.null(report$error) && !is.null(report$gating_set)
      ready <- nzchar(selected_wsp()) && file.exists(selected_wsp()) &&
        length(resolved_files()) > 0L && !nrow(missing_files())
      label <- if (importing_wsp()) "Importing…" else if (successful) "WSP imported" else if (!is.null(import_error())) "Retry WSP import" else "Import WSP"
      disabled <- importing_wsp() || successful || !ready
      # Agent note: Success is a status indicator; keep its green visible even when disabled by theme styles.
      actionButton(ns("import_wsp"), label,
        class = paste("btn w-100", if (successful) "btn-success wsp-import-complete" else if (disabled) "btn-secondary" else "btn-success wsp-import-ready"),
        disabled = disabled)
    })
    output$wsp_missing_control <- renderUI({
      missing <- missing_files()
      if (!nrow(missing)) return(NULL)
      tagList(p(sprintf("%d FCS files missing. Import is unavailable until they are located.", nrow(missing))),
        actionButton(ns("resolve_missing"), "Locate missing files", class = "btn btn-secondary w-100"))
    })

    require_qs2 <- function() {
      if (!requireNamespace("qs2", quietly = TRUE)) {
        stop("Package 'qs2' is required for fast workspace save/load.")
      }
    }

    output$wsp_result_card <- renderUI({
      report <- shared$gating$wsp_report()
      if (!nzchar(selected_wsp()) && is.null(report)) return(NULL)
      card_page_card("Files & WSP Report",
        uiOutput(ns("wsp_report")),
        tags$details(open = "open",
          tags$summary("Check referenced FCS files"),
          p("Use this list to identify missing source files. Locations are relative to the WSP folder; this is not a population or analysis table."),
          DT::dataTableOutput(ns("ref_files"))
        )
      )
    })

    output$ref_files <- DT::renderDataTable({
      inv <- inventory()
      inv <- inv[inv$type == "fcs", , drop = FALSE]
      files <- data.frame(
        File = inv$relative_path,
        Status = ifelse(file.exists(inv$path), "Found", "Missing"),
        check.names = FALSE
      )
      DT::datatable(files, rownames = FALSE, selection = "none",
        options = list(pageLength = 10, lengthChange = FALSE, autoWidth = FALSE,
          columnDefs = list(list(width = "80%", targets = 0), list(width = "20%", targets = 1)),
          language = list(emptyTable = "No FCS references found in this WSP.")))
    })

    observeEvent(input$import_wsp, {
      wsp <- selected_wsp()
      if (!length(wsp) || !nzchar(wsp) || !file.exists(wsp)) {
        showNotification("Select a WSP file first.", type = "warning")
        return()
      }
      if (importing_wsp()) return()
      report_now <- shared$gating$wsp_report()
      if (!is.null(report_now) && is.null(report_now$error) && !is.null(report_now$gating_set)) return()
      if (!length(resolved_files()) || nrow(missing_files())) {
        showNotification("Locate all referenced FCS files before importing.", type = "warning")
        show_missing_files()
        return()
      }
      file_mapping <- resolved_files()
      importing_wsp(TRUE)
      import_error(NULL)
      overlay_show("Importing WSP report...", session = session)
      # Agent note: Defer CytoML parsing until after this flush so the import
      # overlay is visible before the WSP and FCS files are read.
      session$onFlushed(function() {
        later::later(function() {
          # Agent note: A session domain is not a reactive context. Deferred import
          # reads shared reactive state, so isolate the entire callback after flush.
          shiny::withReactiveDomain(session, shiny::isolate({
            tryCatch({
              # Agent note: WSP export serializes this GatingSet through save_gs(); execute=FALSE leaves the imported report without the data needed by that step.
              report <- flowjo_wsp_import_report(wsp, dirname(wsp), execute = TRUE, resolved_files = file_mapping)
              state <- flowjo_wsp_app_state(report)
              if (!is.null(state$error)) {
                import_error(state$error)
                report$error <- state$error
                shared$gating$wsp_report(report)
                showNotification(state$error, type = "error", duration = 10)
                return(invisible(NULL))
              }
              shared$fcs$add_many(state$entries)
              wsp_axis_settings <- list()
              for (entry in state$entries) {
                for (nm in names(entry$wsp_axis_transforms %||% list())) {
                  if (is.null(wsp_axis_settings[[nm]])) wsp_axis_settings[[nm]] <- entry$wsp_axis_transforms[[nm]]
                }
              }
              if (length(wsp_axis_settings)) {
                # Agent note: Keep exact WSP transforms in the shared workspace
                # slot used by gating, recomputation, and saved-state restore;
                # entries retain their own copy for sample-specific fallback.
                transform_settings <- shared$transforms$settings()
                transform_settings[names(wsp_axis_settings)] <- wsp_axis_settings
                shared$transforms$settings(transform_settings)
              }
              existing_gates <- shared$gating$gates()
              existing_gates[names(state$gates)] <- state$gates
              shared$gating$gates(existing_gates)
              existing_memberships <- shared$gating$memberships()
              for (sid in names(state$memberships)) {
                sample_memberships <- existing_memberships[[sid]] %||% list()
                sample_memberships[names(state$memberships[[sid]])] <- state$memberships[[sid]]
                existing_memberships[[sid]] <- sample_memberships
              }
              shared$gating$memberships(existing_memberships)
              existing_groups <- shared$gating$groups()
              existing_groups[names(state$groups)] <- state$groups
              shared$gating$groups(existing_groups)
              shared$gating$wsp_report(report)
              # The WSP adapter already supplies exact per-sample memberships.
              # Avoid eagerly recomputing every gate across every sample; the
              # active Gating page prepares only its current sample on demand.
              showNotification(sprintf("Imported %d WSP samples and %d FlowJo gates into the app.", length(state$entries), length(state$gates)), type = "message", duration = 8)
            }, error = function(e) {
              import_error(conditionMessage(e))
              message("[wsp-import] ", conditionMessage(e))
              showNotification(paste("WSP import failed:", conditionMessage(e)), type = "error", duration = 10)
            }, finally = {
              importing_wsp(FALSE)
              overlay_hide(session = session)
            })
            invisible(NULL)
          }))
        }, delay = 0)
      }, once = TRUE)
    })

    output$wsp_export_control <- renderUI({
      report <- shared$gating$wsp_report()
      if (is.null(report)) {
        return(div(class = "Pinhki-inline-note", "Import a WSP report before exporting."))
      }
      if (!is.null(report$error) || is.null(report$gating_set)) {
        return(div(class = "Pinhki-inline-note", "Export is unavailable because the WSP import did not produce a usable GatingSet."))
      }
      tagList(
        downloadButton(
          ns("export_wsp"),
          "Export WSP",
          class = "btn btn-secondary w-100",
          onclick = "window.overlayShow && window.overlayShow('global_overlay','Exporting WSP...','Writing a FlowJo workspace.');"
        ),
        div(class = "Pinhki-inline-note", "Requires a local gs-to-flowjo binary or a running Docker daemon.")
      )
    })

    output$export_wsp <- downloadHandler(
      filename = function() {
        report <- shared$gating$wsp_report()
        stem <- "flowjo_workspace"
        if (!is.null(report) && isTRUE(nzchar(report$wsp_file %||% ""))) {
          stem <- tools::file_path_sans_ext(basename(report$wsp_file))
        }
        paste0(stem, "_exported.wsp")
      },
      content = function(file) {
        overlay_show(
          "Exporting WSP...",
          tip = "Writing the imported gate hierarchy and sample data to a FlowJo workspace."
        )
        on.exit(overlay_hide(), add = TRUE)
        report <- shared$gating$wsp_report()
        if (is.null(report) || is.null(report$gating_set)) {
          stop("Import a WSP report before exporting.")
        }
        compensation_entries <- Filter(function(entry) !is.null(entry$wsp_sample_id), shared$fcs$registry())
        result <- flowjo_wsp_export_report(report$gating_set, file, compensation_entries = compensation_entries)
        if (!isTRUE(result$ok)) {
          msg <- result$error %||% "WSP export failed."
          if (grepl("docker|gs-to-flowjo|binary", msg, ignore.case = TRUE)) {
            msg <- paste0(msg, " Start Docker Desktop or install the gs-to-flowjo binary, then retry.")
          }
          stop(msg)
        }
        if (!file.exists(file) || is.na(file.info(file)$size) || file.info(file)$size <= 0) {
          stop("WSP export completed without creating a usable file.")
        }
      }
    )

    # Agent note: FXRS is the app-facing RDS workspace suffix; FXRS2 is the app-facing QS2 suffix. Keep these mappings aligned with the upload filters and serializer calls below.
    output$save_workspace <- downloadHandler(
      filename = function() paste0("flowjo_workspace_", Sys.Date(), ".fxrs"),
      content = function(file) {
        overlay_show("Saving workspace...")
        on.exit(overlay_hide(), add = TRUE)
        saveRDS(shared$export_state(), file)
      }
    )

    output$save_workspace_qs <- downloadHandler(
      filename = function() paste0("flowjo_workspace_", Sys.Date(), ".fxrs2"),
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

    notify_workspace_load_error <- function(e) {
      msg <- conditionMessage(e)
      message("[workspace-load] ", msg)
      showNotification(msg, type = "error", duration = 10)
    }

    validate_restored_workspace <- function() {
      reg <- shared$fcs$registry()
      active_id <- shared$fcs$active_id()
      if (!length(reg)) stop("Workspace loaded, but no FCS samples were restored.")
      if (is.null(active_id) || is.null(reg[[active_id]])) stop("Workspace loaded, but the active sample could not be restored.")
      if (is.null(shared$df$raw()) || !nrow(shared$df$raw())) stop("Workspace loaded, but the active sample event data is not usable.")
      invisible(TRUE)
    }

    complete_workspace_load <- function(source) {
      tryCatch({
        app_trace("complete start source=", source, .prefix = "[workspace-load]")
        validate_restored_workspace()
        app_trace(
          "restored samples=", length(shared$fcs$registry()),
          " table_rows=", nrow(shared$fcs$table()),
          " active=", shared$fcs$active_id() %||% "<none>",
          .prefix = "[workspace-load]"
        )
        app_trace("request nav route start", .prefix = "[workspace-load]")
        shared$ui$request_nav_route("workspace_tab")
        app_trace("request nav route done", .prefix = "[workspace-load]")
        if (
          isTRUE(shared$params$plot_cache_enabled()) &&
          isTRUE(shared$params$plot_preload_enabled()) &&
          isTRUE(shared$params$plot_preload_after_load())
        ) {
          app_trace("preload later schedule start", .prefix = "[workspace-load]")
          later::later(function() {
            tryCatch(shiny::withReactiveDomain(shared$session, {
              app_trace("preload later entered", .prefix = "[workspace-load]")
              shared$ui$request_plot_preload()
              app_trace("preload request done", .prefix = "[workspace-load]")
            }), error = function(e) app_trace_error("workspace-load:preload-later", e))
          }, delay = 1)
          app_trace("preload later schedule done", .prefix = "[workspace-load]")
        }
        app_trace("show loaded notification start", .prefix = "[workspace-load]")
        showNotification("Workspace loaded.", type = "message")
        app_trace("complete done", .prefix = "[workspace-load]")
      }, error = notify_workspace_load_error)
    }

    finish_workspace_load <- function(source) {
      # Agent note: Complete from the load observer itself; deferred flush callbacks were not reliable here because restore invalidations are already in-flight after import_state().
      message("[workspace-load] restore imported; completing workspace route")
      complete_workspace_load(source)
    }

    load_workspace_file <- function(path) {
      overlay_show(
        "Loading workspace...",
        tip = "Restoring samples, groups, saved gates, and app settings.",
        progress = "indeterminate",
        progress_label = "Reading saved RDS workspace"
      )
      on.exit(overlay_hide(), add = TRUE)
      obj <- readRDS(path)
      if (!is.list(obj)) stop("Workspace file is not a valid saved workspace.")
      shared$import_state(obj)
      message("[workspace-load] RDS import_state returned")
      finish_workspace_load("RDS observer")
    }

    load_workspace_qs_file <- function(path) {
      overlay_show(
        "Loading fast workspace...",
        tip = "Restoring samples, groups, saved gates, and app settings.",
        progress = "indeterminate",
        progress_label = "Reading saved QS workspace"
      )
      on.exit(overlay_hide(), add = TRUE)
      require_qs2()
      obj <- qs2::qs_read(path)
      if (!is.list(obj)) stop("Workspace file is not a valid saved workspace.")
      shared$import_state(obj)
      message("[workspace-load] QS import_state returned")
      finish_workspace_load("QS observer")
    }

    load_analysis_bundle_file <- function(path, name = NULL) {
      overlay_show("Loading analysis bundle...")
      on.exit(overlay_hide(), add = TRUE)
      obj <- shared$analysis$import_bundle(path, name = name)
      app_nav_select(shared$session, "umap_tab")
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
          notify_workspace_load_error(e)
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
          notify_workspace_load_error(e)
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
        notify_workspace_load_error(e)
      })
    }, ignoreInit = TRUE)

    observeEvent(input$confirm_load_workspace_qs, {
      path <- pending_workspace_qs()
      pending_workspace_qs(NULL)
      removeModal()
      req(path)
      tryCatch(load_workspace_qs_file(path), error = function(e) {
        overlay_hide()
        notify_workspace_load_error(e)
      })
    }, ignoreInit = TRUE)

    output$wsp_report <- renderUI({
      report <- shared$gating$wsp_report()
      if (!is.null(import_error())) return(tagList(tags$b("Import failed"), p(import_error()), p("Resolve the issue, then retry. No successful import has been confirmed.")))
      if (importing_wsp()) return(p("Importing samples and gates…"))
      if (is.null(report)) {
        if (nrow(missing_files())) return(p("WSP selected. Locate the missing FCS files before importing."))
        if (!length(resolved_files())) return(p("No FCS references found. This WSP cannot be imported as a complete sample workspace."))
        return(p("Ready to import. All referenced FCS files were found. Click Import WSP to load samples and gates."))
      }
      if (!is.null(report$error)) {
        return(tagList(tags$b("Import failed"), p(report$error)))
      }
      if (is.null(report$gating_set)) {
        return(p("Samples and gates have not been loaded. Import this WSP before continuing."))
      }
      # Agent note: Report import results, not global registry totals, which may include earlier imports.
      tagList(
        tags$b("WSP imported"),
        p(basename(report$wsp_file)),
        p(sprintf("%d samples loaded. %d population paths available.",
          nrow(report$samples), length(report$populations))),
        p("Continue in Workspace to review samples and gates, or Gating to inspect populations.")
      )
    })
  })
}
