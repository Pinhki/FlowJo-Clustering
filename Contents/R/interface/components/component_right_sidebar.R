right_sidebar_ui <- function(id) {
  ns <- NS(id)

  tagList(
    div(
      class = "right-sidebar-stack",
      div(
        class = "sidebar-section sidebar-data-section",
        h5("FCS Files", style = "margin-top: 0; font-size: 0.9rem; color: #666;"),
        div(
          class = "sidebar-file-actions",
          div(
            class = "sidebar-fcs-upload",
            fileInput(ns("fcs_file"), NULL, accept = ".fcs", buttonLabel = "Add FCS", multiple = FALSE, width = "100%")
          )
        ),
        div(class = "sidebar-data-scroll", uiOutput(ns("fcs_registry_cards"))),
        div(style = "height:6px"),
        actionButton(ns("btn_set_active"), "Set Active", icon = icon("check-circle"), class = "btn-outline-secondary btn-sm w-100"),
        div(style = "height:6px"),
        actionButton(ns("btn_rename"), "Rename", icon = icon("edit"), class = "btn-outline-secondary btn-sm w-100"),
        div(style = "height:6px"),
        actionButton(ns("btn_edit_notes"), "Edit Notes", icon = icon("sticky-note"), class = "btn-outline-secondary btn-sm w-100"),
        div(style = "height:6px"),
        actionButton(ns("btn_remove"), "Remove", icon = icon("trash"), class = "btn-outline-danger btn-sm w-100")
      ),
      div(
        class = "sidebar-bottom-actions",
        actionButton(ns("open_settings"), "Plot Settings", icon = icon("sliders"), class = "btn btn-sm w-100"),
        div(style = "height:6px"),
        downloadButton(ns("save_state"), "Save Workspace", class = "btn btn-sm w-100 workspace-btn"),
        div(style = "height:6px"),
        actionButton(ns("load_state_btn"), "Load Workspace", icon = icon("upload"), class = "btn btn-sm w-100 workspace-btn"),
        div(style = "display:none;", fileInput(ns("load_state"), NULL, accept = ".rds", buttonLabel = "Browse", multiple = FALSE))
      )
    )
  )
}

right_sidebar_server <- function(id, shared) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    selected_fcs_id <- reactiveVal(NULL)

    observeEvent(input$selected_fcs_id, {
      selected_fcs_id(as.character(input$selected_fcs_id %||% ""))
    }, ignoreInit = TRUE)

    selected_id <- reactive({
      sel <- selected_fcs_id()
      tbl <- shared$fcs$table()
      if (is.null(sel) || !nzchar(sel) || nrow(tbl) == 0 || !(sel %in% tbl$id)) return(NULL)
      sel
    })

    output$fcs_registry_cards <- renderUI({
      tbl <- shared$fcs$table()
      if (nrow(tbl) == 0) {
        return(div(class = "sidebar-file-empty", "No FCS files loaded."))
      }
      cur_sel <- selected_fcs_id()
      if (is.null(cur_sel) || !nzchar(cur_sel) || !(cur_sel %in% tbl$id)) {
        idx <- which(tbl$is_active)
        if (length(idx) == 0) idx <- 1
        next_sel <- tbl$id[[idx[1]]]
        selected_fcs_id(next_sel)
      }

      active_sel <- selected_id()
      cards <- lapply(seq_len(nrow(tbl)), function(i) {
        id_i <- tbl$id[[i]]
        nm <- tbl$name[[i]] %||% "unnamed"
        path <- tbl$path[[i]] %||% ""
        path_short <- basename(path)
        cells <- tbl$cells[[i]] %||% NA_integer_
        ch <- tbl$channels[[i]] %||% NA_integer_
        notes <- tbl$notes[[i]] %||% ""
        loaded <- tbl$loaded_at[[i]] %||% ""
        is_active <- isTRUE(tbl$is_active[[i]])
        is_selected <- !is.null(active_sel) && identical(active_sel, id_i)
        cls <- paste(
          c("sidebar-file-item", "sfi-seurat", if (is_active) "sfi-active" else NULL, if (is_selected) "sfi-selected" else NULL),
          collapse = " "
        )
        tags$div(
          class = cls,
          `data-fcs-id` = id_i,
          onclick = sprintf(
            "if(window.Shiny){Shiny.setInputValue('%s','%s',{priority:'event'});}",
            ns("selected_fcs_id"), id_i
          ),
          tags$div(class = "sfi-title", title = nm, tags$span(class = "sfi-title-text", nm)),
          tags$div(class = "sfi-meta1", sprintf("Cells: %s", format(as.integer(cells), big.mark = ","))),
          tags$div(class = "sfi-meta2", sprintf("Channels: %s", as.integer(ch))),
          tags$div(class = "sfi-meta2", sprintf("File: %s", path_short)),
          if (nzchar(loaded)) tags$div(class = "sfi-meta2", sprintf("Loaded: %s", loaded)) else NULL,
          if (nzchar(notes)) tags$div(class = "sfi-meta2", paste("Notes:", notes)) else NULL
        )
      })
      tags$div(class = "sidebar-file-list", cards)
    })

    observeEvent(input$fcs_file, {
      req(input$fcs_file)
      showNotification("Reading FCS...", type = "message")

      fcs <- flowCore::read.FCS(input$fcs_file$datapath, transformation = FALSE, truncate_max_range = FALSE)
      df  <- as.data.frame(flowCore::exprs(fcs))
      pd  <- flowCore::pData(flowCore::parameters(fcs))
      raw_names <- ifelse(!is.na(pd$desc) & pd$desc != "", pd$desc, pd$name)
      colnames(df) <- make.unique(raw_names, sep = "_")
      spill <- flowjo_extract_spillover(fcs)
      channel_map <- data.frame(
        channel = make.unique(pd$name, sep = "_"),
        marker = colnames(df),
        stringsAsFactors = FALSE
      )

      choices <- df %>% dplyr::select(where(is.numeric)) %>% colnames()
      validate(need(length(choices) >= 2, "FCS must contain at least two numeric columns."))

      suitable_for_facet <- sapply(colnames(df), function(col) length(unique(df[[col]])) <= 100)
      facet_cols <- names(suitable_for_facet[suitable_for_facet])
      entry <- list(
        id = paste0(
          "fcs_",
          format(Sys.time(), "%Y%m%d%H%M%OS3"),
          "_",
          sprintf("%04d", sample.int(9999, 1))
        ),
        name = sub("\\.fcs$", "", basename(input$fcs_file$name), ignore.case = TRUE),
        path = input$fcs_file$name,
        cells = nrow(df),
        channels = ncol(df),
        notes = "",
        loaded_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
        df = df,
        raw_exprs = df,
        spillover = spill,
        channel_map = channel_map,
        sample_meta = list(
          condition = NA_character_,
          batch = NA_character_,
          timepoint = NA_character_,
          subject = NA_character_
        ),
        numeric_cols = choices,
        facet_cols = facet_cols,
        axis_pairs = find_axis_pairs(choices)
      )
      shared$fcs$add(entry)
      showNotification("FCS added and set active.", type = "message")
    })

    observeEvent(input$btn_set_active, {
      id <- selected_id()
      req(id)
      shared$fcs$set_active(id)
      showNotification("Active FCS updated.", type = "message")
    })

    observeEvent(input$btn_remove, {
      id <- selected_id()
      req(id)
      shared$fcs$remove(id)
      showNotification("FCS removed.", type = "message")
    })

    observeEvent(input$btn_rename, {
      id <- selected_id()
      req(id)
      current <- shared$fcs$registry()[[id]]$name
      showModal(modalDialog(
        title = "Rename FCS",
        textInput(ns("rename_value"), "Name", value = current),
        footer = tagList(
          modalButton("Cancel"),
          actionButton(ns("confirm_rename"), "Save", class = "btn-primary")
        )
      ))
    })

    observeEvent(input$confirm_rename, {
      id <- selected_id()
      req(id, input$rename_value)
      nm <- trimws(input$rename_value)
      req(nchar(nm) > 0)
      shared$fcs$rename(id, nm)
      removeModal()
      showNotification("Name updated.", type = "message")
    })

    observeEvent(input$btn_edit_notes, {
      id <- selected_id()
      req(id)
      current <- shared$fcs$registry()[[id]]$notes %||% ""
      showModal(modalDialog(
        title = "Edit Notes",
        textAreaInput(ns("notes_value"), "Notes", value = current, rows = 4),
        footer = tagList(
          modalButton("Cancel"),
          actionButton(ns("confirm_notes"), "Save", class = "btn-primary")
        )
      ))
    })

    observeEvent(input$confirm_notes, {
      id <- selected_id()
      req(id)
      shared$fcs$set_notes(id, input$notes_value %||% "")
      removeModal()
      showNotification("Notes updated.", type = "message")
    })

    observeEvent(input$load_state_btn, {
      shinyjs::runjs(sprintf("
        (function(){
          var root = document.getElementById('%s');
          var inp = null;
          if (root && root.tagName && root.tagName.toLowerCase() === 'input' && root.type === 'file') {
            inp = root;
          } else if (root) {
            inp = root.querySelector('input[type=file]');
          }
          if (!inp) inp = document.querySelector('#%s input[type=file]');
          if (inp) inp.click();
        })();
      ", ns("load_state"), ns("load_state")))
    })

    output$save_state <- downloadHandler(
      filename = function() paste0("flowjo_state_", Sys.Date(), ".rds"),
      content  = function(file) saveRDS(shared$export_state(), file)
    )

    observeEvent(input$load_state, {
      req(input$load_state)
      shared$import_state(readRDS(input$load_state$datapath))
      showNotification("Workspace state loaded.", type = "message")
    })

    list(open_settings = reactive(input$open_settings))
  })
}
