right_sidebar_ui <- function(id) {
  ns <- NS(id)

  tagList(
    singleton(tags$script(HTML("
      (function(){
        if (window.__flowjoRightSidebarLowerInit) return;
        window.__flowjoRightSidebarLowerInit = true;
        var KEY = 'flowjo.sidebar.lowerPinned';
        function sections(){
          return Array.prototype.slice.call(document.querySelectorAll('.sidebar-lower-section'));
        }
        function applyPinned(){
          var pinned = sessionStorage.getItem(KEY) || '';
          sections().forEach(function(section){
            section.classList.toggle('is-pinned', section.dataset.sidebarSection === pinned);
          });
        }
        function updateFade(el){
          var top = el.scrollTop > 1;
          var bottom = el.scrollTop + el.clientHeight < el.scrollHeight - 1;
          var target = el.closest('.sidebar-lower-section.Pinhki-scroll-fade') || el;
          target.classList.toggle('is-scroll-top', top);
          target.classList.toggle('is-scroll-bottom', bottom);
        }
        function updateAllFades(){
          document.querySelectorAll('.sidebar-data-scroll.Pinhki-scroll-fade, .sidebar-lower-section .sidebar-group-scroll, .sidebar-lower-section .sidebar-pop-scroll').forEach(updateFade);
        }
        document.addEventListener('click', function(e){
          var btn = e.target.closest('.sidebar-lower-toggle');
          if (!btn) return;
          e.preventDefault();
          e.stopPropagation();
          var section = btn.closest('.sidebar-lower-section');
          if (!section) return;
          var name = section.dataset.sidebarSection || '';
          sessionStorage.setItem(KEY, sessionStorage.getItem(KEY) === name ? '' : name);
          applyPinned();
          updateAllFades();
        });
        document.addEventListener('scroll', function(e){
          if (e.target && e.target.matches && e.target.matches('.sidebar-data-scroll.Pinhki-scroll-fade, .sidebar-lower-section .sidebar-group-scroll, .sidebar-lower-section .sidebar-pop-scroll')) updateFade(e.target);
        }, true);
        document.addEventListener('dragover', function(e){
          var section = e.target.closest && e.target.closest('.sidebar-lower-section');
          if (section) section.classList.add('is-drag-active');
          updateAllFades();
        }, true);
        document.addEventListener('mouseover', function(e){
          if (e.target.closest && e.target.closest('.sidebar-lower-section')) setTimeout(updateAllFades, 170);
        }, true);
        document.addEventListener('focusin', function(e){
          if (e.target.closest && e.target.closest('.sidebar-lower-section')) setTimeout(updateAllFades, 170);
        }, true);
        document.addEventListener('transitionend', function(e){
          if (e.target && e.target.classList && e.target.classList.contains('sidebar-lower-section')) updateAllFades();
        }, true);
        document.addEventListener('dragleave', function(e){
          var section = e.target.closest && e.target.closest('.sidebar-lower-section');
          if (section && !section.contains(e.relatedTarget)) section.classList.remove('is-drag-active');
        }, true);
        document.addEventListener('drop', function(e){
          var section = e.target.closest && e.target.closest('.sidebar-lower-section');
          if (section) section.classList.remove('is-drag-active');
          setTimeout(updateAllFades, 0);
        }, true);
        document.addEventListener('shiny:value', function(){ setTimeout(function(){ applyPinned(); updateAllFades(); }, 0); });
        window.addEventListener('resize', updateAllFades);
        setTimeout(function(){ applyPinned(); updateAllFades(); }, 0);
      })();
    "))),
    div(
      class = "right-sidebar-stack",
      div(
        class = "sidebar-section sidebar-data-section",
        h5("FCS Files", style = "margin-top: 0; font-size: 0.9rem; color: #666;"),
        div(
          class = "sidebar-file-actions",
          div(
            class = "sidebar-fcs-upload",
            fileInput(ns("fcs_file"), NULL, accept = ".fcs", buttonLabel = "Add FCS", multiple = TRUE, width = "100%")
          )
        ),
        div(class = "sidebar-data-scroll Pinhki-scroll-fade", uiOutput(ns("fcs_registry_cards"))),
        div(
          class = "sidebar-button-row",
          actionButton(ns("btn_set_active"), "Active", icon = icon("check-circle"), class = "btn-outline-secondary btn-sm"),
          actionButton(ns("btn_rename"), "Rename", icon = icon("edit"), class = "btn-outline-secondary btn-sm"),
          actionButton(ns("btn_edit_notes"), "Notes", icon = icon("sticky-note"), class = "btn-outline-secondary btn-sm"),
          actionButton(ns("btn_remove"), "Remove", icon = icon("trash"), class = "btn-outline-danger btn-sm")
        )
      ),
      div(
        class = "sidebar-lower-stack",
        div(
          class = "sidebar-section sidebar-lower-section sidebar-group-section Pinhki-scroll-fade",
          `data-sidebar-section` = "groups",
          div(
            class = "sidebar-lower-header",
            h5("Groups", style = "margin-top: 0; font-size: 0.9rem; color: #666;"),
            actionButton(ns("toggle_groups"), "", icon = icon("chevron-up"), class = "sidebar-lower-toggle btn btn-link btn-sm", title = "Pin Groups")
          ),
          div(class = "sidebar-group-scroll", uiOutput(ns("group_cards"))),
          div(
            class = "sidebar-button-row",
            actionButton(ns("open_workspace"), "Workspace", icon = icon("folder-tree"), class = "btn-outline-secondary btn-sm")
          )
        ),
        div(
          class = "sidebar-section sidebar-lower-section sidebar-pop-section Pinhki-scroll-fade",
          `data-sidebar-section` = "basket",
          div(
            class = "sidebar-lower-header",
            h5("Population Basket", style = "margin-top: 0; font-size: 0.9rem; color: #666;"),
            actionButton(ns("toggle_basket"), "", icon = icon("chevron-up"), class = "sidebar-lower-toggle btn btn-link btn-sm", title = "Pin Population Basket")
          ),
          div(
            class = "sidebar-pop-scroll",
            ondragover = "event.preventDefault(); this.classList.add('drop-target');",
            ondragleave = "this.classList.remove('drop-target');",
            ondrop = sprintf("event.preventDefault(); this.classList.remove('drop-target'); if(window.Shiny){Shiny.setInputValue('%s',{keys:event.dataTransfer.getData('application/x-flowjo-workspace-rows'),nonce:Math.random()},{priority:'event'});}", ns("population_basket_drop")),
            uiOutput(ns("population_basket"))
          ),
          div(
            class = "sidebar-button-row",
            actionButton(ns("edit_population"), "Edit", icon = icon("pen"), class = "btn-outline-secondary btn-sm"),
            actionButton(ns("remove_population"), "Remove", icon = icon("trash"), class = "btn-outline-danger btn-sm")
          )
        ),
        div(
          class = "sidebar-bottom-actions",
          div(
            class = "sidebar-button-row",
            actionButton(ns("open_settings"), "Settings", icon = icon("sliders"), class = "btn btn-sm"),
            actionButton(ns("open_import_export"), "Import / Export", icon = icon("file-export"), class = "btn btn-sm workspace-btn")
          )
        )
      )
    )
  )
}

right_sidebar_server <- function(id, shared) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    selected_fcs_id <- reactiveVal(NULL)
    selected_group_id <- reactiveVal(NULL)
    selected_population_id <- reactiveVal(NULL)

    switch_main_tab <- function(value) {
      shinyjs::runjs(sprintf("document.querySelector('a[data-value=\"%s\"], button[data-value=\"%s\"]').click();", value, value))
    }

    observeEvent(input$selected_fcs_id, {
      selected_fcs_id(as.character(input$selected_fcs_id %||% ""))
    }, ignoreInit = TRUE)

    observeEvent(input$selected_group_id, {
      selected_group_id(as.character(input$selected_group_id %||% ""))
    }, ignoreInit = TRUE)

    observeEvent(input$selected_population_id, {
      selected_population_id(as.character(input$selected_population_id %||% ""))
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
          c("sidebar-file-item", "sfi-fcs", if (is_active) "sfi-active" else NULL, if (is_selected) "sfi-selected" else NULL),
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

    output$group_cards <- renderUI({
      groups <- shared$gating$groups()
      if (!length(groups)) return(div(class = "sidebar-file-empty", "No groups created."))
      cur_sel <- selected_group_id()
      tags$div(class = "sidebar-file-list", lapply(groups, function(g) {
        gid <- g$id %||% ""
        is_selected <- nzchar(cur_sel %||% "") && identical(cur_sel, gid)
        tags$div(
          class = paste(c("sidebar-file-item", "sfi-group", if (is_selected) c("sfi-selected", "sfi-active") else NULL), collapse = " "),
          `data-active` = if (is_selected) "true" else "false",
          `data-group-id` = gid,
          onclick = sprintf(
            "if(window.Shiny){Shiny.setInputValue('%s','%s',{priority:'event'});}",
            ns("selected_group_id"), gid
          ),
          style = sprintf("--group-color:%s;", g$color %||% "#2563EB"),
          tags$div(class = "sfi-title", title = g$name %||% g$id, tags$span(class = "sfi-title-text", g$name %||% g$id)),
          tags$div(class = "sfi-meta1", sprintf("%d sample%s", length(g$sample_ids %||% character()), if (length(g$sample_ids %||% character()) == 1L) "" else "s")),
          tags$div(class = "sfi-meta2", if (isTRUE(g$sync_enabled)) "Sync gates: on" else "Sync gates: off")
        )
      }))
    })

    output$population_basket <- renderUI({
      tbl <- shared$populations$table()
      if (!nrow(tbl)) return(div(class = "sidebar-file-empty", "No gated populations added."))
      sel <- selected_population_id()
      tags$div(class = "sidebar-pop-list", lapply(seq_len(nrow(tbl)), function(i) {
        id_i <- tbl$id[[i]]
        cls <- paste("sidebar-pop-item", if (identical(sel, id_i)) "spi-selected" else "", if (!isTRUE(tbl$include[[i]])) "spi-muted" else "")
        tags$div(
          class = cls,
          onclick = sprintf("if(window.Shiny){Shiny.setInputValue('%s','%s',{priority:'event'});}", ns("selected_population_id"), id_i),
          tags$div(class = "spi-title", tbl$label[[i]]),
          tags$div(class = "spi-meta", sprintf("%s | %s", tbl$population_set[[i]], tbl$sample_name[[i]])),
          tags$div(class = "spi-meta", sprintf("%s events | %.1f%% parent | %s", format(tbl$events[[i]], big.mark = ","), tbl$percent_parent[[i]], tbl$sync_status[[i]]))
        )
      }))
    })

    observeEvent(input$edit_population, {
      id <- selected_population_id(); req(id)
      items <- shared$populations$items(); item <- items[[id]]; req(item)
      showModal(modalDialog(
        title = "Edit population",
        textInput(ns("pop_label"), "Label", value = item$label %||% ""),
        textInput(ns("pop_set"), "Population set", value = item$set_name %||% "Population Set"),
        numericInput(ns("pop_volume"), "Volume (uL, annotation only)", value = item$volume_uL %||% NA_real_, min = 0),
        checkboxInput(ns("pop_include"), "Include in concat/export", value = isTRUE(item$include)),
        footer = tagList(modalButton("Cancel"), actionButton(ns("save_population"), "Save", class = "btn-primary")),
        easyClose = TRUE
      ))
    }, ignoreInit = TRUE)

    observeEvent(input$save_population, {
      id <- selected_population_id(); req(id)
      shared$populations$update(id, include = input$pop_include, label = trimws(input$pop_label %||% ""), set_name = trimws(input$pop_set %||% "Population Set"), volume_uL = input$pop_volume)
      removeModal()
    }, ignoreInit = TRUE)

    observeEvent(input$remove_population, {
      id <- selected_population_id(); req(id)
      shared$populations$remove(id); selected_population_id(NULL)
    }, ignoreInit = TRUE)

    observeEvent(input$population_basket_drop, {
      keys <- unique(trimws(unlist(strsplit(input$population_basket_drop$keys %||% "", ",", fixed = TRUE), use.names = FALSE)))
      added <- 0L
      for (key in keys[nzchar(keys)]) {
        parts <- strsplit(key, "::", fixed = TRUE)[[1]]
        if (length(parts) >= 3L && identical(parts[[1]], "gate") && nzchar(parts[[2]]) && parts[[2]] != "group" && nzchar(parts[[3]])) {
          added <- added + shared$populations$add(parts[[2]], parts[[3]], set_name = "Population Set")
        }
      }
      showNotification(sprintf("Added %d population%s to the basket.", added, if (added == 1L) "" else "s"), type = if (added) "message" else "warning")
    }, ignoreInit = TRUE)

    observeEvent(input$fcs_file, {
      req(input$fcs_file)
      files <- input$fcs_file
      land_on_gating <- FALSE
      overlay_show(
        text = if (nrow(files) == 1) "Reading FCS file..." else "Reading FCS files...",
        tip = "Large cytometry files can take a moment to import."
      )
      on.exit(overlay_hide(), add = TRUE)
      showNotification(sprintf("Reading %s FCS file%s...", nrow(files), if (nrow(files) == 1) "" else "s"), type = "message")

      entries <- vector("list", nrow(files))
      for (i in seq_len(nrow(files))) {
        fcs <- flowCore::read.FCS(files$datapath[[i]], transformation = FALSE, truncate_max_range = FALSE)
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
        range_cols <- intersect(c("range", "$PnR", "maxRange"), colnames(pd))
        range_col <- if (length(range_cols)) range_cols[[1]] else NA_character_
        channel_ranges <- data.frame(
          channel = channel_map$channel,
          marker = channel_map$marker,
          range = suppressWarnings(as.numeric(if (!is.na(range_col)) pd[[range_col]] else NA_real_)),
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
          name = sub("\\.fcs$", "", basename(files$name[[i]]), ignore.case = TRUE),
          path = files$name[[i]],
          cells = nrow(df),
          channels = ncol(df),
          notes = "",
          loaded_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
          df = df,
          raw_exprs = df,
          spillover = spill,
          channel_map = channel_map,
          channel_ranges = channel_ranges,
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
        entries[[i]] <- entry
        if (!length(shared$gating$gates()) && is.null(shared$gating$wsp_report()) &&
            !any(grepl("UMAP", c(entry$numeric_cols, colnames(entry$df)), ignore.case = TRUE))) {
          land_on_gating <- TRUE
        }
      }
      shared$fcs$add_many(entries)
      if (land_on_gating) switch_main_tab("gating_tab")
      showNotification(sprintf("%s FCS file%s added. Last file set active.", nrow(files), if (nrow(files) == 1) "" else "s"), type = "message")
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

    observeEvent(input$open_import_export, {
      switch_main_tab("import_export_tab")
    }, ignoreInit = TRUE)

    observeEvent(input$open_workspace, {
      switch_main_tab("workspace_tab")
    }, ignoreInit = TRUE)

    list(open_settings = reactive(input$open_settings))
  })
}
