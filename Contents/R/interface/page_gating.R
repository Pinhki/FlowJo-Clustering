gating_page_ui <- function(id) {
  ns <- NS(id)
  div(class = "card-page-wrap gating-workspace",
    singleton(tags$script(src = "gating_canvas.js")),
    card_page_header("Gating", subtitle = "Manual gate drawing, population tree, and sample navigation."),
    fluidRow(
      column(3,
        card_page_card("Gate Tools",
          uiOutput(ns("sample_picker")),
          uiOutput(ns("active_parent")),
          uiOutput(ns("axis_controls")),
          selectInput(
            ns("display_mode"), "Display",
            choices = c(
              "Pseudocolor" = "pseudocolor",
              "Dot plot" = "dot",
              "Contour" = "contour",
              "Color by gate" = "gate",
              "Color by marker" = "marker"
            ),
            selected = "pseudocolor"
          ),
          uiOutput(ns("marker_color_picker")),
          textInput(ns("gate_name"), "Population name", value = "New gate"),
          selectInput(ns("quadrant_name"), "Quadrant", choices = c("upper_right", "upper_left", "lower_left", "lower_right")),
          actionButton(ns("clear_polygon"), "Clear Polygon Points", class = "btn btn-outline-secondary btn-sm w-100"),
            div(style = "height:8px"),
            uiOutput(ns("save_gate_button")),
            div(style = "height:8px"),
            actionButton(ns("delete_gate"), "Delete Selected Gate", class = "btn btn-outline-danger w-100"),
            div(style = "height:8px"),
            uiOutput(ns("sync_edit_controls")),
          div(style = "height:8px"),
          actionButton(ns("replace_gate"), "Replace Selected Gate", class = "btn btn-outline-secondary w-100"),
          div(style = "height:8px"),
          actionButton(ns("use_root"), "Draw at Root Level", class = "btn btn-outline-secondary w-100"),
          div(style = "height:8px"),
          actionButton(ns("add_active_to_basket"), "Add Gate to Basket", class = "btn btn-success w-100")
        )
      ),
      column(9,
        card_page_card("Preview",
          div(
            id = ns("preview_card"),
            class = "gate-preview-card",
            tabindex = "0",
            div(class = "gate-preview-head",
              div(class = "gate-preview-path-slot", uiOutput(ns("gate_path_toolbar"))),
              div(class = "gate-preview-tool-slot", uiOutput(ns("gate_toolbar")))
            ),
            uiOutput(ns("preview_gate_nav")),
            uiOutput(ns("preview_sample_nav")),
            uiOutput(ns("preview_plots"))
          ),
          div(class = "gate-node-meta", "Canvas preview: click a plot to activate it, use arrows or keyboard arrows to move through gates and samples, double-click a saved gate to enter its population.")
        ),
        card_page_card("Population Counts", DT::dataTableOutput(ns("count_table")))
      )
    ),
    uiOutput(ns("gate_preview_windows"))
  )
}

gating_page_server <- function(id, shared) {
  moduleServer(id, function(input, output, session) {
    active_entry <- reactive({
      sid <- input$sample_id %||% shared$fcs$active_id()
      reg <- shared$fcs$registry()
      if (is.null(sid) || is.null(reg[[sid]])) return(NULL)
      reg[[sid]]
    })
    navigator_selected_gates <- reactiveVal(character())
    gate_preview_windows <- reactiveVal(list())
    pending_gate_drop <- reactiveVal(NULL)
    canvas_selected_gate <- reactiveVal(NULL)
    active_plot <- reactiveVal("left")
    plot_axes <- reactiveValues(left = list(x = NULL, y = NULL), right = list(x = NULL, y = NULL))
    displayed_plot_axes <- reactiveValues(left = list(x = NULL, y = NULL), right = list(x = NULL, y = NULL))
    view_axes <- reactiveVal(list())
    skip_next_gate_payload <- reactiveVal(FALSE)
    active_stats_cache <- new.env(parent = emptyenv())
    label_pos_cache <- new.env(parent = emptyenv())

    active_sample_id <- reactive({
      e <- active_entry()
      if (is.null(e)) NULL else e$id
    })

    default_axis_pair <- function(e) {
      axes <- flowjo_gate_axes(e)
      axis_names <- names(axes)
      x_sel <- axes[[1]]
      y_sel <- axes[[min(2, length(axes))]]
      fsc_idx <- match(TRUE, grepl("^FSC-A$", axis_names, ignore.case = TRUE))
      ssc_idx <- match(TRUE, grepl("^SSC-A$", axis_names, ignore.case = TRUE))
      if (!is.na(fsc_idx)) x_sel <- axes[[fsc_idx]]
      if (!is.na(ssc_idx)) y_sel <- axes[[ssc_idx]]
      list(x = x_sel, y = y_sel)
    }

    plot_focus_gate_id <- function(plot = NULL, gates = NULL) {
      if (is.null(plot)) plot <- isolate(active_plot())
      if (is.null(gates)) gates <- isolate(shared$gating$gates())
      active_id <- preview_focus_gate_id(gates)
      if (identical(plot, "left")) flowjo_gate_parent_id(gates, active_id) else active_id
    }

    view_axis_key <- function(plot, focus_id = NULL) {
      paste(active_sample_id() %||% "sample", plot %||% "plot", focus_id %||% "root", sep = "::")
    }

    reset_right_axis_override <- function(gid) {
      if (is.null(gid)) return()
      axes <- view_axes()
      axes[[view_axis_key("right", gid)]] <- NULL
      view_axes(axes)
      plot_axes[["right"]] <- NULL
    }

    gate_descendant_ids <- function(gates, root_id) {
      out <- character()
      visit <- function(id) {
        kids <- flowjo_gate_children(gates, id %||% "root")
        for (kid in kids) {
          out <<- c(out, kid)
          visit(kid)
        }
      }
      visit(root_id)
      out
    }

    first_applicable_gate_id <- function(gates, ids, entry) {
      ids <- ids[vapply(ids, function(id) isTRUE(flowjo_gate_applicability(entry, gates[[id]], gates)$ok), logical(1))]
      if (length(ids)) ids[[1]] else NULL
    }

    right_axis_source_gate_id <- function(gates, root_id, entry) {
      # Agent note: Right preview shows the opened population, but its axes must come from the next immediate child gate when available.
      kids <- flowjo_gate_children(gates, root_id %||% "root")
      first_applicable_gate_id(gates, kids, entry) %||%
        first_applicable_gate_id(gates, gate_descendant_ids(gates, root_id), entry)
    }

    plot_axis_pair <- function(plot = NULL, focus_gate_id = NULL) {
      if (is.null(plot)) plot <- isolate(active_plot())
      e <- active_entry()
      req(e)
      gates <- shared$gating$gates()
      gid <- focus_gate_id %||% plot_focus_gate_id(plot, gates)
      gate <- if (!is.null(gid) && !identical(gid, "root")) gates[[gid]] else NULL
      pair <- view_axes()[[view_axis_key(plot, gid)]]
      if (is.null(pair) && identical(plot, "right") && !is.null(gate)) {
        axis_gid <- right_axis_source_gate_id(gates, gid, e) %||% gid
        axis_gate <- gates[[axis_gid]]
        if (!is.null(axis_gate)) pair <- list(x = axis_gate$x_channel, y = axis_gate$y_channel)
      }
      pair <- pair %||% plot_axes[[plot]] %||% if (!is.null(gate)) list(x = gate$x_channel, y = gate$y_channel) else list()
      if ((is.null(pair$x) || is.null(pair$y)) && is.null(gate)) {
        seed_id <- first_applicable_gate_id(gates, names(gates), e)
        seed_gate <- if (!is.null(seed_id)) gates[[seed_id]] else NULL
        if (!is.null(seed_gate)) pair <- list(x = seed_gate$x_channel, y = seed_gate$y_channel)
      }
      defaults <- default_axis_pair(e)
      list(x = pair$x %||% defaults$x, y = pair$y %||% defaults$y)
    }

    gate_path <- function(gates, gate_id) {
      ids <- character()
      cur <- gate_id
      while (!is.null(cur) && !identical(cur, "root") && !is.null(gates[[cur]])) {
        ids <- c(cur, ids)
        cur <- gates[[cur]]$parent_id %||% "root"
      }
      ids
    }

    gate_path_label <- function(gates, gate_id) {
      ids <- gate_path(gates, gate_id)
      paste(c("Root", vapply(ids, function(id) gates[[id]]$name %||% id, character(1))), collapse = " / ")
    }

    first_gate_id <- function(gates = NULL) {
      if (is.null(gates)) gates <- shared$gating$gates()
      ids <- names(gates)
      if (length(ids)) ids[[1]] else NULL
    }

    clean_gate_marker <- function(entry, channel) {
      marker <- flowjo_channel_to_marker(entry$channel_map, channel) %||% channel
      marker <- trimws(marker %||% channel)
      if (!nzchar(marker) || identical(marker, channel)) marker <- channel
      sub("-(A|H|W)$", "", marker, ignore.case = TRUE)
    }

    axis_midpoint <- function(entry, channel) {
      col <- flowjo_channel_to_marker(entry$channel_map, channel)
      scales <- shared$transforms$scales()
      rng <- scales[[channel]] %||% scales[[col]]
      if (is.null(rng) || length(rng) != 2 || !all(is.finite(rng))) {
        if (is.null(col) || !(col %in% colnames(entry$df))) return(0)
        vals <- entry$df[[col]]
        rng <- range(vals[is.finite(vals)], na.rm = TRUE)
      }
      if (length(rng) != 2 || !all(is.finite(rng))) return(0)
      mean(rng)
    }

    gate_center <- function(gate_type, geom, axis) {
      if (identical(gate_type, "polygon")) {
        vs <- geom$vertices
        vals <- if (is.data.frame(vs)) vs[[axis]] else suppressWarnings(as.numeric(vapply(vs %||% list(), function(v) v[[axis]] %||% NA_real_, numeric(1))))
        return(mean(vals[is.finite(vals)]))
      }
      if (axis == "x") return(mean(c(geom$xmin %||% geom$x, geom$xmax %||% geom$x), na.rm = TRUE))
      mean(c(geom$ymin %||% geom$y, geom$ymax %||% geom$y), na.rm = TRUE)
    }

    gate_axis_sign <- function(entry, channel, gate_type, geom, axis) {
      if (gate_type %in% c("quadrant", "curved_quadrant")) {
        quad <- geom$quadrant %||% "upper_right"
        if (axis == "x") return(if (grepl("right", quad)) "+" else "-")
        return(if (grepl("upper", quad)) "+" else "-")
      }
      center <- gate_center(gate_type, geom, axis)
      if (!is.finite(center)) return("+")
      if (center >= axis_midpoint(entry, channel)) "+" else "-"
    }

    smart_gate_name <- function(gates, parent_id = "root", geom = NULL, gate_type = NULL, channels = NULL) {
      e <- active_entry()
      if (is.null(e) || is.null(geom)) return("New gate")
      gate_type <- gate_type %||% input$gate_canvas_draft$type %||% input$gate_type %||% "rectangle"
      channels <- channels %||% current_draft_channels()
      x <- channels$x; y <- channels$y
      if (is.null(x)) return("New gate")
      x_name <- clean_gate_marker(e, x)
      y_name <- if (!is.null(y)) clean_gate_marker(e, y) else NULL
      x_raw <- paste(x, x_name)
      y_raw <- paste(y %||% "", y_name %||% "")
      x_sign <- gate_axis_sign(e, x, gate_type, geom, "x")
      y_sign <- if (!is.null(y)) gate_axis_sign(e, y, gate_type, geom, "y") else NULL
      is_fsc <- function(v) grepl("\\bFSC", v, ignore.case = TRUE)
      is_ssc <- function(v) grepl("\\bSSC", v, ignore.case = TRUE)
      is_fsc_area <- function(v) is_fsc(v) && grepl("FSC-A", v, ignore.case = TRUE)
      is_fsc_shape <- function(v) is_fsc(v) && grepl("FSC-(H|W)", v, ignore.case = TRUE)
      is_viability <- function(v) grepl("live|dead|l/d|viability|zombie|dapi|\\bpi\\b|7-aad|fixable|aqua", v, ignore.case = TRUE)
      if (!is.null(y) && ((is_fsc(x_raw) && is_ssc(y_raw)) || (is_ssc(x_raw) && is_fsc(y_raw)))) {
        return("Non-debris")
      }
      if (!is.null(y) && ((is_fsc_area(x_raw) && is_fsc_shape(y_raw)) || (is_fsc_area(y_raw) && is_fsc_shape(x_raw)))) return("Singlets")
      if (is_viability(x_raw) && identical(x_sign, "-")) return("Live cells")
      if (!is.null(y) && is_viability(y_raw) && identical(y_sign, "-")) return("Live cells")
      paste(c(paste0(x_name, x_sign), if (!is.null(y_name) && !identical(gate_type, "range")) paste0(y_name, y_sign)), collapse = " ")
    }

    duplicate_gate_name <- function(gates, parent_id, nm, exclude_id = NULL) {
      siblings <- flowjo_gate_children(gates, parent_id %||% "root")
      siblings <- setdiff(siblings, exclude_id %||% character())
      any(vapply(siblings, function(id) identical(gates[[id]]$name %||% "", nm), logical(1)))
    }

    next_gate_name <- function(gates, parent_id = "root", geom = NULL, gate_type = NULL, channels = NULL) {
      nm <- smart_gate_name(gates, parent_id, geom, gate_type, channels)
      if (nzchar(nm)) nm else "New gate"
    }

    clear_gate_canvas_drafts <- function() {
      session$sendCustomMessage("flowjo-gating-clear-draft", list(id = session$ns("canvas_wrap_left")))
      session$sendCustomMessage("flowjo-gating-clear-draft", list(id = session$ns("canvas_wrap_right")))
    }

    gate_stat_row <- function(sample_id, gate_id) {
      if (!identical(sample_id, active_sample_id())) return(NULL)
      stats <- active_sample_stats()
      if (!nrow(stats)) return(NULL)
      row <- stats[stats$sample_id == sample_id & stats$gate_id == gate_id, , drop = FALSE]
      if (!nrow(row)) NULL else row[1, , drop = FALSE]
    }

    gate_status <- function(sample_id, gate_id) {
      reg <- shared$fcs$registry()
      gates <- shared$gating$gates()
      app <- flowjo_gate_applicability(reg[[sample_id]], gates[[gate_id]], gates)
      app$status
    }

    gate_available <- function(sample_id, gate_id) {
      reg <- shared$fcs$registry()
      gates <- shared$gating$gates()
      isTRUE(flowjo_gate_applicability(reg[[sample_id]], gates[[gate_id]], gates)$ok)
    }

    gate_stats_key <- function(e, gates) {
      gate_bits <- vapply(gates, function(gate) {
        paste(
          gate$id %||% "",
          gate$name %||% "",
          gate$type %||% "",
          gate$parent_id %||% "",
          gate$x_channel %||% "",
          gate$y_channel %||% "",
          gate$applied_sample_id %||% "",
          gate$sync_group_id %||% "",
          gate$group_id %||% "",
          gate$group_state %||% "",
          gate$color %||% "",
          gate$group_color %||% "",
          paste(unlist(gate$geometry %||% list(), use.names = FALSE), collapse = ","),
          sep = "|"
        )
      }, character(1))
      paste(c(e$id %||% "", gate_bits), collapse = "\n")
    }

    active_sample_gate_stats <- function() {
      e <- active_entry()
      gates <- shared$gating$gates()
      if (is.null(e) || !length(gates)) return(data.frame())
      key <- gate_stats_key(e, gates)
      if (identical(active_stats_cache$key %||% NULL, key)) return(active_stats_cache$value)
      gate_df <- e$df
      memberships <- list()
      rows <- list()
      visible <- list()
      collect <- function(parent_id = "root", depth = 0L) {
        for (gid in flowjo_gate_children(gates, parent_id)) {
          gate <- gates[[gid]]
          app <- flowjo_gate_applicability(e, gate, gates)
          if (!isTRUE(app$ok)) next
          visible[[length(visible) + 1L]] <<- list(id = gid, depth = depth, status = app$status)
          collect(gid, depth + 1L)
        }
      }
      collect()
      for (row in visible) {
        gid <- row$id
        gate <- gates[[gid]]
        parent <- flowjo_gate_parent_membership(gates, memberships, gate)
        keep <- flowjo_gate_membership(gate_df, e$channel_map, gate, parent = parent)
        memberships[[gid]] <- keep
        st <- flowjo_gate_stats(gate_df, keep, parent)
        rows[[length(rows) + 1L]] <- data.frame(
          sample_id = e$id,
          sample_name = e$name %||% e$id,
          gate_id = gid,
          gate_name = gate$name,
          gate_type = gate$type,
          gate_depth = row$depth,
          gate_x = gate$x_channel %||% "",
          gate_y = gate$y_channel %||% "",
          gate_status = row$status,
          gate_color = flowjo_gate_color(gate, match(gid, names(gates))),
          st,
          stringsAsFactors = FALSE
        )
      }
      out <- if (length(rows)) do.call(rbind, rows) else data.frame()
      active_stats_cache$key <- key
      active_stats_cache$value <- out
      out
    }

    active_sample_stats <- reactive(active_sample_gate_stats())

    gate_subtree_ids <- function(gates, gate_ids) {
      out <- character()
      visit <- function(id) {
        if (is.null(gates[[id]]) || id %in% out) return()
        out <<- c(out, id)
        for (kid in flowjo_gate_children(gates, id)) visit(kid)
      }
      for (id in gate_ids) visit(id)
      out
    }

    mark_gate_modified_or_sync <- function(gates, gate_id) {
      gate <- gates[[gate_id]]
      if (is.null(gate)) return(gates)
      if (!is.null(gate$sync_group_id)) {
        # Agent note: Synced gate edits stay synced by default; only Unsync this sample may break one sample copy away from the group.
        gates[[gate_id]]$group_id <- gate$group_id %||% gate$sync_group_id
        gates[[gate_id]]$group_state <- "group-owned"
        return(sync_gate_peers(gates, gate_id))
      }
      if (identical(gate$group_state %||% NULL, "group-owned") || !is.null(gate$group_id)) {
        gates[[gate_id]]$group_state <- "sample-modified"
      }
      gates
    }

    unsync_sample_gate <- function(gate_id) {
      gates <- shared$gating$gates()
      gate <- gates[[gate_id]]
      if (is.null(gate) || is.null(gate$sync_group_id)) return(FALSE)
      gates[[gate_id]]$group_id <- gate$group_id %||% gate$sync_group_id
      gates[[gate_id]]$sync_group_id <- NULL
      gates[[gate_id]]$sync_sample_ids <- NULL
      gates[[gate_id]]$group_state <- "sample-modified"
      gates[[gate_id]]$applied_sample_id <- gate$applied_sample_id %||% active_sample_id()
      shared$gating$gates(gates)
      TRUE
    }

    copy_gate_to_target <- function(gate_id, sample_id = NULL, group_id = NULL, parent_id = NULL, overwrite_id = NULL, sync = FALSE) {
      gates <- shared$gating$gates()
      src <- gates[[gate_id]]
      if (is.null(src)) return(NULL)
      groups <- shared$gating$groups()
      group <- groups[[group_id]]
      sync <- isTRUE(sync) && !is.null(group_id)
      cp <- flowjo_copy_gate(
        src,
        parent_id = parent_id %||% src$parent_id,
        sample_id = sample_id,
        group_id = group_id,
        sync = sync,
        sync_sample_ids = group$sample_ids %||% character(),
        group_color = group$color %||% NULL
      )
      if (!is.null(overwrite_id) && !is.null(gates[[overwrite_id]])) {
        cp$id <- overwrite_id
        cp$created_at <- gates[[overwrite_id]]$created_at %||% cp$created_at
      }
      gates[[cp$id]] <- cp
      if (isTRUE(sync)) gates <- sync_gate_peers(gates, cp$id)
      shared$gating$gates(gates)
      shared$gating$active_gate_id(cp$id)
      cp$id
    }

    sync_gate_peers <- function(gates, gate_id) {
      gate <- gates[[gate_id]]
      if (is.null(gate) || is.null(gate$sync_group_id)) return(gates)
      key <- flowjo_gate_lineage_id(gate)
      peers <- names(gates)[vapply(gates, function(g) {
        identical(g$sync_group_id, gate$sync_group_id) && identical(flowjo_gate_lineage_id(g), key)
      }, logical(1))]
      for (pid in peers) {
        gates[[pid]]$name <- gate$name
        gates[[pid]]$color <- gate$color
        gates[[pid]]$type <- gate$type
        gates[[pid]]$x_channel <- gate$x_channel
        gates[[pid]]$y_channel <- gate$y_channel
        gates[[pid]]$geometry <- gate$geometry
        gates[[pid]]$label_pos <- gate$label_pos
        gates[[pid]]$group_state <- "group-owned"
      }
      gates
    }

    apply_gate_ids_to_sample <- function(gate_ids, sample_id, parent_id = NULL) {
      gates <- shared$gating$gates()
      made <- character()
      for (gid in gate_ids) {
        src <- gates[[gid]]
        if (is.null(src)) next
        hits <- flowjo_gate_same_path(gates, src$name, parent_id %||% src$parent_id, sample_id)
        if (length(hits)) {
          pending_gate_drop(list(type = "sample", gate_ids = gate_ids, conflict_gate_id = gid, sample_id = sample_id, parent_id = parent_id, overwrite_id = hits[[1]]))
          showModal(modalDialog(
            title = "Overwrite gate?",
            sprintf("Replace existing gate '%s' on this target?", src$name),
            footer = tagList(modalButton("Cancel"), actionButton(session$ns("confirm_gate_drop"), "Overwrite", class = "btn btn-primary")),
            easyClose = TRUE
          ))
          return(invisible(made))
        }
        made <- c(made, copy_gate_to_target(gid, sample_id = sample_id, parent_id = parent_id))
      }
      invisible(made)
    }

    apply_gate_ids_to_group <- function(gate_ids, group_id, mode = c("copy", "sync"), confirm = TRUE) {
      mode <- match.arg(mode)
      groups <- shared$gating$groups()
      if (is.null(groups[[group_id]])) return(invisible(character()))
      sample_ids <- groups[[group_id]]$sample_ids %||% character()
      gates <- shared$gating$gates()
      gate_ids <- gate_subtree_ids(gates, gate_ids)
      conflicts <- list()
      for (sid in sample_ids) {
        for (gid in gate_ids) {
          src <- gates[[gid]]
          if (is.null(src)) next
          hits <- flowjo_gate_same_path(gates, src$name, src$parent_id, sid)
          if (length(hits)) conflicts[[paste(sid, gid, sep = "::")]] <- list(sample_id = sid, gate_id = gid, overwrite_id = hits[[1]])
        }
      }
      if (isTRUE(confirm) && length(conflicts)) {
        pending_gate_drop(list(type = "group", gate_ids = gate_ids, group_id = group_id, mode = mode, conflicts = conflicts))
        showModal(modalDialog(
          title = "Overwrite gates?",
          sprintf("Replace %d same-name gate(s) in this group?", length(conflicts)),
          footer = tagList(modalButton("Cancel"), actionButton(session$ns("confirm_gate_drop"), "Overwrite", class = "btn btn-primary")),
          easyClose = TRUE
        ))
        return(invisible(character()))
      }
      made <- character()
      for (sid in sample_ids) {
        id_map <- list()
        for (gid in gate_ids) {
          src <- gates[[gid]]
          if (is.null(src)) next
          key <- paste(sid, gid, sep = "::")
          parent_id <- src$parent_id %||% "root"
          if (!is.null(id_map[[parent_id]])) parent_id <- id_map[[parent_id]]
          new_id <- copy_gate_to_target(
            gid,
            sample_id = sid,
            group_id = group_id,
            parent_id = parent_id,
            overwrite_id = conflicts[[key]]$overwrite_id %||% NULL,
            sync = identical(mode, "sync")
          )
          if (!is.null(new_id)) {
            id_map[[gid]] <- new_id
            made <- c(made, new_id)
          }
        }
      }
      invisible(made)
    }

    output$sample_picker <- renderUI({
      tbl <- shared$fcs$table()
      if (!nrow(tbl)) return(helpText("Add FCS files from the right sidebar first."))
      selectInput(session$ns("sample_id"), "Sample", choices = stats::setNames(tbl$id, tbl$name), selected = shared$fcs$active_id())
    })

    output$active_parent <- renderUI({
      gates <- shared$gating$gates()
      gid <- shared$gating$active_gate_id()
      name <- if (!is.null(gid) && !is.null(gates[[gid]])) gates[[gid]]$name else "Root"
      div(class = "gate-active-parent",
        span("Viewing population / next gate parent"),
        strong(name),
        div("Click a Gate Tree row to switch; Save Gate creates a child here.")
      )
    })

    output$sync_edit_controls <- renderUI({
      gates <- shared$gating$gates()
      gid <- canvas_selected_gate() %||% shared$gating$active_gate_id()
      if (is.null(gid) || !nzchar(gid %||% "")) return(NULL)
      gate <- gates[[gid]]
      if (is.null(gate)) return(NULL)
      state <- flowjo_gate_group_state(gate)
      groups <- shared$gating$groups()
      group_id <- gate$group_id %||% gate$sync_group_id
      group <- if (!is.null(group_id)) groups[[group_id]] else NULL
      if (identical(state, "synced")) {
        return(div(class = "gate-sync-controls",
          div(class = "gate-state-badge synced", "Synced group gate"),
          div(class = "gate-node-meta", sprintf("Managed in Workspace%s. Edits here update synced copies.", if (!is.null(group)) paste0(" · ", group$name %||% group$id) else "")),
          actionButton(session$ns("unsync_sample_gate"), "Unsync this sample", class = "btn btn-sm btn-outline-secondary")
        ))
      }
      if (identical(state, "sample-modified")) {
        return(div(class = "gate-sync-controls",
          div(class = "gate-state-badge sample-modified", "Sample modified"),
          div(class = "gate-node-meta", sprintf("This gate no longer follows%s. Use Workspace to reset or sync.", if (!is.null(group)) paste0(" ", group$name %||% group$id) else " the group"))
        ))
      }
      NULL
    })

    output$save_gate_button <- renderUI({
      gates <- shared$gating$gates()
      gid <- canvas_selected_gate() %||% shared$gating$active_gate_id()
      label <- if (!is.null(gid) && !is.null(gates[[gid]])) "Update Gate" else "Save Gate"
      actionButton(session$ns("save_gate"), label, class = "btn btn-primary w-100")
    })

    output$axis_controls <- renderUI({
      e <- active_entry()
      if (is.null(e)) return(NULL)
      axes <- flowjo_gate_axes(e)
      validate(need(length(axes) >= 2, "FCS must contain at least two numeric channels."))
      pair <- plot_axis_pair(active_plot())
      axis_row <- function(axis, selected) {
        label <- sprintf("%s axis", toupper(axis))
        div(
          class = "gate-axis-control",
          tags$label(class = "control-label", label),
          div(
            class = "gate-axis-control-row",
            selectInput(session$ns(paste0(axis, "_channel")), NULL, choices = axes, selected = selected),
            actionButton(
              session$ns(paste0("adjust_", axis, "_axis")),
              label = icon("sliders"),
              title = sprintf("Adjust %s Axis", toupper(axis)),
              class = "btn btn-outline-secondary btn-sm gate-axis-adjust"
            )
          )
        )
      }
      tagList(
        axis_row("y", pair$y),
        axis_row("x", pair$x)
      )
    })

    observeEvent(input$x_channel, {
      plot <- active_plot()
      pair <- plot_axes[[plot]] %||% plot_axis_pair(plot)
      pair$x <- input$x_channel
      plot_axes[[plot]] <- pair
      axes <- view_axes()
      axes[[view_axis_key(plot, plot_focus_gate_id(plot))]] <- pair
      view_axes(axes)
    }, ignoreInit = TRUE)

    observeEvent(input$y_channel, {
      plot <- active_plot()
      pair <- plot_axes[[plot]] %||% plot_axis_pair(plot)
      pair$y <- input$y_channel
      plot_axes[[plot]] <- pair
      axes <- view_axes()
      axes[[view_axis_key(plot, plot_focus_gate_id(plot))]] <- pair
      view_axes(axes)
    }, ignoreInit = TRUE)

    axis_modal_axis <- reactiveVal(NULL)
    axis_modal_plot <- reactiveVal(NULL)
    axis_range_updating <- reactiveVal(FALSE)
    axis_range_touched <- reactiveVal(FALSE)

    axis_setting_ui <- function(prefix, setting, default_type = "none", raw_range = c(0, 1), slider_range = raw_range) {
      type <- setting$type %||% default_type
      if (!type %in% c("none", "log1p", "biex")) type <- "none"
      div(class = "gate-axis-panel",
        plotOutput(session$ns("axis_hist"), height = "460px"),
        div(class = "gate-node-meta", "Preview uses the current modal settings. X labels show raw intensity values."),
        sliderInput(
          session$ns(paste0(prefix, "_range")),
          "Display range",
          min = slider_range[[1]],
          max = slider_range[[2]],
          value = raw_range,
          step = signif(diff(slider_range) / 1000, 2),
          sep = "",
          ticks = TRUE
        ),
        actionButton(session$ns(paste0(prefix, "_reset_auto_range")), "Reset to auto", class = "btn btn-sm btn-outline-secondary"),
        div(
          class = "gate-axis-scale-row",
          radioButtons(
            session$ns(paste0(prefix, "_scale_type")),
            "Scale",
            choices = c("Linear" = "none", "Log1p" = "log1p", "Biex/logicle" = "biex"),
            selected = type,
            inline = TRUE
          )
        ),
        conditionalPanel(
          sprintf("input['%s'] == 'biex'", session$ns(paste0(prefix, "_scale_type"))),
          fluidRow(
            column(4, sliderInput(session$ns(paste0(prefix, "_axis_w")), "Width (w)", value = setting$w %||% 1.25, min = 0, max = 2, step = 0.05, ticks = TRUE)),
            column(4, sliderInput(session$ns(paste0(prefix, "_axis_p")), "Positive decades (m)", value = setting$p %||% 5, min = 1, max = 6, step = 0.1, ticks = TRUE)),
            column(4, sliderInput(session$ns(paste0(prefix, "_axis_n")), "Negative decades (a)", value = setting$n %||% 0, min = 0, max = 2, step = 0.1, ticks = TRUE))
          )
        )
      )
    }

    open_axis_modal <- function(axis) {
      e <- active_entry()
      plot <- active_plot()
      if (identical(plot, "left")) {
        gates <- shared$gating$gates()
        if (!is.null(preview_focus_gate_id(gates))) {
          plot <- "right"
          active_plot("right")
        }
      }
      pair <- plot_axis_pair(plot)
      req(e, pair$x, pair$y)
      axis_modal_axis(axis)
      axis_modal_plot(plot)
      axis_range_touched(FALSE)
      settings <- shared$transforms$settings()
      channel <- if (identical(axis, "x")) pair$x else pair$y
      col <- flowjo_channel_to_marker(e$channel_map, channel)
      if (is.null(col) || !nzchar(col)) {
        showNotification(sprintf("Channel %s is not available in this sample.", channel), type = "warning")
        return(invisible(NULL))
      }
      default_type <- flowjo_default_axis_scale_type(channel)
      current_setting <- settings[[channel]] %||% settings[[col]] %||%
        flowjo_axis_scale_setting(default_type, t = flowjo_channel_range(e, channel, col)) %||%
        list(type = default_type)
      rng <- modal_axis_display_range(e, channel, col, current_setting, use_manual = TRUE)
      showModal(modalDialog(
        title = sprintf("Adjust %s Axis: %s", toupper(axis), flowjo_channel_label(e$channel_map, channel)),
        size = "l",
        div(class = "gate-axis-modal", axis_setting_ui("axis", current_setting, default_type = default_type, raw_range = rng$raw_range, slider_range = rng$slider_range)),
        footer = tagList(
          modalButton("Cancel"),
          actionButton(session$ns("apply_axis"), "Apply", class = "btn btn-primary")
        )
      ))
    }

    observeEvent(input$adjust_x_axis, open_axis_modal("x"), ignoreInit = TRUE)
    observeEvent(input$adjust_y_axis, open_axis_modal("y"), ignoreInit = TRUE)

    modal_axis_setting <- function(e, channel, col) {
      flowjo_axis_scale_setting(
        input$axis_scale_type %||% "none",
        input$axis_axis_w %||% 1.25,
        input$axis_axis_p %||% 5,
        input$axis_axis_n %||% 0,
        t = flowjo_channel_range(e, channel, col)
      )
    }

    modal_axis_display_range <- function(e, channel, col, setting, use_manual = FALSE) {
      raw_vals <- axis_population_raw_values(e, channel, axis_modal_plot() %||% active_plot())
      raw_vals <- raw_vals[is.finite(raw_vals)]
      if (!length(raw_vals)) return(list(raw_range = c(0, 1), slider_range = c(-0.5, 1.5)))
      raw_range <- NULL
      if (isTRUE(use_manual)) {
        scales <- shared$transforms$scales()
        current_scale <- scales[[channel]] %||% scales[[col]]
        if (!is.null(current_scale) && length(current_scale) == 2 && all(is.finite(current_scale))) {
          # Agent note: scales() stores raw manual slider values; auto_scales() stores transformed canvas ranges and must not be displayed as raw histogram readings.
          raw_range <- sort(current_scale)
        }
      }
      if (is.null(raw_range) || length(raw_range) != 2 || !all(is.finite(raw_range)) || raw_range[[1]] == raw_range[[2]]) {
        vals <- flowjo_transform_column(raw_vals, setting)
        vals <- vals[is.finite(vals)]
        rng <- canvas_auto_range(vals, channel, NULL, raw_vals, setting)
        raw_range <- sort(axis_hist_raw_values(rng, setting))
      }
      if (!all(is.finite(raw_range)) || raw_range[[1]] == raw_range[[2]]) raw_range <- range(raw_vals, na.rm = TRUE)
      if (!all(is.finite(raw_range)) || raw_range[[1]] == raw_range[[2]]) raw_range <- c(0, 1)
      if (grepl("^(FSC|SSC)", channel %||% "", ignore.case = TRUE)) raw_range[[1]] <- 0
      pad <- max(diff(raw_range), 1)
      list(raw_range = raw_range, slider_range = raw_range + c(-pad / 2, pad / 2))
    }

    observeEvent(list(input$axis_scale_type, input$axis_axis_w, input$axis_axis_p, input$axis_axis_n), {
      if (isTRUE(axis_range_updating())) return()
      axis <- axis_modal_axis()
      e <- active_entry()
      pair <- plot_axis_pair(axis_modal_plot() %||% active_plot())
      req(axis, e, pair$x, pair$y)
      channel <- if (identical(axis, "x")) pair$x else pair$y
      col <- flowjo_channel_to_marker(e$channel_map, channel)
      req(col)
      setting <- modal_axis_setting(e, channel, col)
      rng <- modal_axis_display_range(e, channel, col, setting, use_manual = FALSE)
      current_range <- sort(suppressWarnings(as.numeric(input$axis_range)))
      use_current <- isTRUE(axis_range_touched()) &&
        length(current_range) == 2 && all(is.finite(current_range)) && current_range[[2]] > current_range[[1]]
      value <- if (use_current) current_range else rng$raw_range
      slider_range <- if (use_current) range(c(rng$slider_range, value)) else rng$slider_range
      axis_range_updating(TRUE)
      on.exit(axis_range_updating(FALSE), add = TRUE)
      updateSliderInput(
        session, "axis_range",
        min = slider_range[[1]],
        max = slider_range[[2]],
        value = value,
        step = signif(diff(slider_range) / 1000, 2)
      )
    }, ignoreInit = TRUE)

    observeEvent(input$axis_range, {
      if (!isTRUE(axis_range_updating())) axis_range_touched(TRUE)
    }, ignoreInit = TRUE)

    observeEvent(input$axis_reset_auto_range, {
      axis <- axis_modal_axis()
      e <- active_entry()
      pair <- plot_axis_pair(axis_modal_plot() %||% active_plot())
      req(axis, e, pair$x, pair$y)
      channel <- if (identical(axis, "x")) pair$x else pair$y
      col <- flowjo_channel_to_marker(e$channel_map, channel)
      req(col)
      setting <- modal_axis_setting(e, channel, col)
      rng <- modal_axis_display_range(e, channel, col, setting, use_manual = FALSE)
      axis_range_updating(TRUE)
      axis_range_touched(FALSE)
      on.exit(axis_range_updating(FALSE), add = TRUE)
      updateSliderInput(
        session, "axis_range",
        min = rng$slider_range[[1]],
        max = rng$slider_range[[2]],
        value = rng$raw_range,
        step = signif(diff(rng$slider_range) / 1000, 2)
      )
    }, ignoreInit = TRUE)

    observeEvent(input$apply_axis, {
      e <- active_entry()
      pair <- plot_axis_pair(axis_modal_plot() %||% active_plot())
      req(e, pair$x, pair$y)
      axis <- axis_modal_axis()
      req(axis)
      settings <- shared$transforms$settings()
      channel <- if (identical(axis, "x")) pair$x else pair$y
      col <- flowjo_channel_to_marker(e$channel_map, channel)
      req(col)
      setting <- modal_axis_setting(e, channel, col)
      if (is.null(setting)) {
        settings[[channel]] <- NULL
        settings[[col]] <- NULL
      } else {
        # Agent note: RDS restore and canvas code may look up transforms by raw channel or mapped data column; keep both keys in sync.
        settings[[channel]] <- setting
        settings[[col]] <- setting
      }
      shared$transforms$settings(settings)
      scales <- shared$transforms$scales()
      raw_range <- sort(suppressWarnings(as.numeric(input$axis_range)))
      if (length(raw_range) == 2 && all(is.finite(raw_range)) && raw_range[[2]] > raw_range[[1]]) {
        scales[[channel]] <- raw_range
        scales[[col]] <- raw_range
      } else {
        scales[[channel]] <- NULL
        scales[[col]] <- NULL
      }
      shared$transforms$scales(scales)
      removeModal()
    }, ignoreInit = TRUE)

    output$marker_color_picker <- renderUI({
      e <- active_entry()
      if (is.null(e) || !identical(input$display_mode %||% "dot", "marker")) return(NULL)
      axes <- flowjo_gate_axes(e)
      selectInput(session$ns("marker_color"), "Marker color", choices = axes, selected = input$x_channel %||% axes[[1]])
    })

    output$gate_toolbar <- renderUI({
      tool_names <- list(
        tags$span(icon("square"), tags$span(class = "visually-hidden", "Rectangle")),
        tags$span(icon("arrows-left-right"), tags$span(class = "visually-hidden", "Range")),
        tags$span(icon("draw-polygon"), tags$span(class = "visually-hidden", "Polygon")),
        tags$span(icon("circle"), tags$span(class = "visually-hidden", "Ellipse")),
        tags$span(icon("crosshairs"), tags$span(class = "visually-hidden", "Quadrant")),
        tags$span(icon("wave-square"), tags$span(class = "visually-hidden", "Curved quadrant"))
      )
      tool_values <- c("rectangle", "range", "polygon", "ellipse", "quadrant", "curved_quadrant")
      tags$div(
        class = "gate-toolbar plot-toolbox Pinhki-plot-toolbox app-tab-pill-track",
        radioButtons(
          session$ns("gate_type"), NULL,
          choiceNames = tool_names,
          choiceValues = tool_values,
          selected = "rectangle",
          inline = TRUE
        )
      )
    })

    plot_title <- function(plot) {
      e <- active_entry()
      if (is.null(e)) return("")
      gates <- shared$gating$gates()
      if (identical(plot, "right") && is.null(preview_focus_gate_id(gates))) return("New gate")
      gid <- plot_focus_gate_id(plot, gates)
      sample_name <- e$name %||% e$id %||% ""
      paste(sample_name, gate_path_label(gates, gid), sep = " | ")
    }

    plot_axis_chip <- function(plot) {
      gates <- shared$gating$gates()
      gid <- plot_focus_gate_id(plot, gates)
      gate <- if (!is.null(gid) && !identical(gid, "root")) gates[[gid]] else NULL
      if (is.null(gate)) return(NULL)
      pair <- plot_axis_pair(plot, gid)
      if (identical(pair$x, gate$x_channel) && identical(pair$y, gate$y_channel)) return(NULL)
      span(class = "gate-axis-chip", sprintf("Gate: %s / %s", gate$x_channel, gate$y_channel %||% ""))
    }

    output$plot_title_left <- renderUI(div(class = "gate-plot-title", plot_title("left"), plot_axis_chip("left")))
    output$plot_title_right <- renderUI(div(class = "gate-plot-title", plot_title("right"), plot_axis_chip("right")))

    preview_arrow <- function(id, icon_name, title) {
      actionButton(session$ns(id), icon(icon_name), label = NULL, title = title, class = "btn btn-light btn-sm gate-preview-arrow")
    }

    output$preview_gate_nav <- renderUI({
      gates <- shared$gating$gates()
      gid <- shared$gating$active_gate_id()
      parent_id <- flowjo_gate_parent_id(gates, gid)
      child_ids <- flowjo_gate_children(gates, gid %||% "root")
      if (is.null(parent_id) && !length(child_ids)) return(NULL)
      div(class = "gate-preview-nav gate-preview-nav-horizontal",
        if (!is.null(parent_id)) preview_arrow("preview_prev_gate", "chevron-left", "Previous gate"),
        if (length(child_ids)) preview_arrow("preview_next_gate", "chevron-right", "Next child gate")
      )
    })

    output$preview_sample_nav <- renderUI({
      gates <- shared$gating$gates()
      gid <- shared$gating$active_gate_id()
      sid <- active_sample_id()
      prev_sid <- flowjo_prev_next_sample(shared$fcs$registry(), gates, gid, sid, -1L)
      next_sid <- flowjo_prev_next_sample(shared$fcs$registry(), gates, gid, sid, 1L)
      if (is.null(prev_sid) && is.null(next_sid)) return(NULL)
      div(class = "gate-preview-nav gate-preview-nav-vertical",
        if (!is.null(prev_sid)) preview_arrow("preview_prev_sample", "chevron-up", "Previous sample"),
        if (!is.null(next_sid)) preview_arrow("preview_next_sample", "chevron-down", "Next sample")
      )
    })

    output$preview_plots <- renderUI({
      gates <- shared$gating$gates()
      active_id <- preview_focus_gate_id(gates) %||% first_gate_id(gates)
      has_active <- !is.null(active_id)
      siblings <- character()
      sibling_index <- NA_integer_
      if (!is.null(active_id) && !is.null(gates[[active_id]])) {
        siblings <- flowjo_gate_children(gates, gates[[active_id]]$parent_id %||% "root")
        sibling_index <- match(active_id, siblings)
      }
      plot_box <- function(id, plot, active, blank = FALSE) {
        div(
          id = session$ns(id),
          class = paste("gate-canvas-wrap gate-history-plot", if (isTRUE(active)) "active" else "", if (isTRUE(blank)) "blank" else ""),
          `data-plot` = plot,
          uiOutput(session$ns(paste0("plot_title_", plot))),
          if (identical(plot, "right") && !isTRUE(blank) && length(siblings) > 1L) div(class = "gate-preview-nav gate-preview-nav-sibling",
            if (!is.na(sibling_index) && sibling_index > 1L) actionButton(session$ns("preview_prev_sibling"), tagList(icon("chevron-left"), icon("layer-group")), label = NULL, title = "Previous sibling gate", class = "btn btn-light btn-sm gate-preview-arrow"),
            if (!is.na(sibling_index) && sibling_index < length(siblings)) actionButton(session$ns("preview_next_sibling"), tagList(icon("layer-group"), icon("chevron-right")), label = NULL, title = "Next sibling gate", class = "btn btn-light btn-sm gate-preview-arrow")
          ),
          if (!isTRUE(blank) && isTRUE(active)) tags$div(class = "gate-plot-badge", "Editing"),
          if (isTRUE(blank)) div(class = "gate-blank-note", "Draw a gate on the left, then open it to view the gated population here."),
          tags$canvas(class = "gate-canvas gate-canvas-points"),
          tags$canvas(class = "gate-canvas gate-canvas-overlay")
        )
      }
      div(
        class = "gate-preview-duo",
        plot_box("canvas_wrap_left", "left", !has_active || identical(active_plot(), "left")),
        plot_box("canvas_wrap_right", "right", has_active && identical(active_plot(), "right"), blank = !has_active),
        tags$script(sprintf(
          "setTimeout(function(){ if (window.Shiny) Shiny.setInputValue('%s', Date.now(), {priority:'event'}); }, 0);",
          session$ns("gate_canvas_request")
        ))
      )
    })

    output$gate_path_toolbar <- renderUI({
      gates <- shared$gating$gates()
      gid <- shared$gating$active_gate_id() %||% canvas_selected_gate()
      ids <- gate_path(gates, gid)
      labels <- c(Root = "Root", vapply(ids, function(id) gates[[id]]$name %||% id, character(1)))
      values <- c(Root = "root", ids)
      div(class = "gate-path-toolbar",
        lapply(seq_along(values), function(i) {
          tagList(
            if (i > 1) span(class = "gate-path-sep", "/"),
            tags$button(
              type = "button",
              class = paste("gate-path-chip", if (identical(values[[i]], gid %||% "root")) "active" else ""),
              onclick = sprintf("Shiny.setInputValue('%s','%s',{priority:'event'});", session$ns("gate_path_select"), values[[i]]),
              labels[[i]]
            )
          )
        })
      )
    })

    observe({
      gates <- shared$gating$gates()
      if (!length(gates)) return()
      changed <- FALSE
      for (i in seq_along(gates)) {
        id <- paste0("gate_color_", gates[[i]]$id)
        col <- toupper(input[[id]] %||% "")
        if (grepl("^#[0-9A-F]{6}$", col) && !identical(toupper(gates[[i]]$color %||% ""), col)) {
          gates[[i]]$color <- col
          gates <- mark_gate_modified_or_sync(gates, gates[[i]]$id)
          changed <- TRUE
        }
      }
      if (changed) shared$gating$gates(gates)
    })

    polygon_points <- reactiveVal(data.frame(x = numeric(), y = numeric()))

    canvas_points_df <- function(pts) {
      if (is.null(pts) || !length(pts)) return(data.frame(x = numeric(), y = numeric()))
      if (is.data.frame(pts)) return(data.frame(x = as.numeric(pts$x), y = as.numeric(pts$y)))
      if (is.list(pts) && !is.null(pts$x) && !is.null(pts$y)) return(data.frame(x = as.numeric(pts$x), y = as.numeric(pts$y)))
      rows <- lapply(pts, function(p) {
        if (is.list(p) && !is.null(p$x) && !is.null(p$y)) return(c(as.numeric(p$x), as.numeric(p$y)))
        p <- unlist(p, use.names = FALSE)
        if (length(p) < 2) return(c(NA_real_, NA_real_))
        c(as.numeric(p[[1]]), as.numeric(p[[2]]))
      })
      out <- as.data.frame(do.call(rbind, rows))
      names(out) <- c("x", "y")
      out[is.finite(out$x) & is.finite(out$y), , drop = FALSE]
    }

    normalize_canvas_geometry <- function(geometry) {
      if (is.null(geometry$vertices)) return(geometry)
      vs <- geometry$vertices
      if (is.data.frame(vs)) return(geometry)
      geometry$vertices <- canvas_points_df(vs)
      geometry
    }

    active_parent_keep <- function(df, channel_map, gates, active_id) {
      if (is.null(active_id) || is.null(gates[[active_id]])) return(NULL)
      memberships <- list()
      for (gate in gates) {
        parent <- flowjo_gate_parent_membership(gates, memberships, gate)
        memberships[[gate$id]] <- flowjo_gate_membership(df, channel_map, gate, parent = parent)
        if (identical(gate$id, active_id)) return(memberships[[gate$id]])
      }
      NULL
    }

    parent_keep_for_gate <- function(df, channel_map, gates, gate_id) {
      gate <- gates[[gate_id]]
      if (is.null(gate) || is.null(gate$parent_id) || identical(gate$parent_id, "root")) return(NULL)
      memberships <- list()
      for (g in gates) {
        parent <- flowjo_gate_parent_membership(gates, memberships, g)
        memberships[[g$id]] <- flowjo_gate_membership(df, channel_map, g, parent = parent)
        if (identical(g$id, gate$parent_id)) return(memberships[[g$id]])
      }
      NULL
    }

    observeEvent(input$gate_canvas_polygon, {
      pts <- input$gate_canvas_polygon
      if (!length(pts)) return()
      polygon_points(canvas_points_df(pts))
    }, ignoreInit = TRUE)
    observeEvent(input$clear_polygon, polygon_points(data.frame(x = numeric(), y = numeric())))

    effective_axis_setting <- function(channel, col, entry = NULL) {
      settings <- shared$transforms$settings()
      saved <- settings[[channel]] %||% settings[[col]]
      if (!is.null(saved)) {
        if (identical(saved$type %||% NULL, "biex") && is.null(saved$t)) saved$t <- flowjo_channel_range(entry, channel, col) %||% 262144
        return(saved)
      }
      flowjo_axis_scale_setting(flowjo_default_axis_scale_type(channel), t = flowjo_channel_range(entry, channel, col))
    }

    axis_setting_for_channel <- function(entry, channel) {
      if (is.null(entry) || is.null(channel) || !nzchar(channel)) return(NULL)
      col <- flowjo_channel_to_marker(entry$channel_map, channel)
      if (is.null(col) || !nzchar(col)) return(NULL)
      effective_axis_setting(channel, col, entry)
    }

    convert_gate_geometry_scale <- function(geom, type, x_setting, y_setting, inverse = FALSE) {
      if (is.null(geom)) return(NULL)
      f <- if (isTRUE(inverse)) flowjo_inverse_transform_column else flowjo_transform_column
      cv <- function(v, setting) {
        out <- suppressWarnings(as.numeric(v))
        ok <- is.finite(out)
        out[ok] <- f(out[ok], setting)
        out
      }
      out <- geom
      if (identical(type, "polygon")) {
        vs <- out$vertices
        if (is.data.frame(vs)) {
          if ("x" %in% names(vs)) vs$x <- cv(vs$x, x_setting)
          if ("y" %in% names(vs)) vs$y <- cv(vs$y, y_setting)
          out$vertices <- vs
        } else if (is.list(vs) && length(vs)) {
          out$vertices <- lapply(vs, function(pt) {
            if (!is.null(pt$x)) pt$x <- cv(pt$x, x_setting)
            if (!is.null(pt$y)) pt$y <- cv(pt$y, y_setting)
            pt
          })
        }
        return(out)
      }
      for (nm in intersect(c("xmin", "xmax", "x"), names(out))) out[[nm]] <- cv(out[[nm]], x_setting)
      for (nm in intersect(c("ymin", "ymax", "y"), names(out))) out[[nm]] <- cv(out[[nm]], y_setting)
      if (!is.null(out$xmin) && !is.null(out$xmax)) {
        xs <- sort(c(out$xmin, out$xmax))
        out$xmin <- xs[[1]]; out$xmax <- xs[[2]]
      }
      if (!is.null(out$ymin) && !is.null(out$ymax)) {
        ys <- sort(c(out$ymin, out$ymax))
        out$ymin <- ys[[1]]; out$ymax <- ys[[2]]
      }
      out
    }

    canvas_geometry_to_raw <- function(geom, type, channels) {
      e <- active_entry()
      if (is.null(e)) return(geom)
      # Agent note: Gate geometry is stored in raw FCS coordinates; only canvas input/output is transformed for the current display scale.
      convert_gate_geometry_scale(
        normalize_canvas_geometry(geom),
        type,
        axis_setting_for_channel(e, channels$x),
        axis_setting_for_channel(e, channels$y),
        inverse = TRUE
      )
    }

    raw_gate_for_canvas <- function(gate, x_channel, y_channel, entry) {
      display_gate <- flowjo_gate_for_plot_axes(gate, x_channel, y_channel)
      display_gate$geometry <- convert_gate_geometry_scale(
        display_gate$geometry,
        display_gate$type,
        axis_setting_for_channel(entry, x_channel),
        axis_setting_for_channel(entry, y_channel),
        inverse = FALSE
      )
      display_gate
    }

    gate_axis_values <- function(gates, axis = "x") {
      vals <- unlist(lapply(gates, function(gate) {
        geom <- gate$geometry %||% list()
        if (identical(gate$type, "polygon")) return(geom$vertices[[axis]] %||% numeric())
        if (axis == "x") return(c(geom$xmin, geom$xmax, geom$x))
        c(geom$ymin, geom$ymax, geom$y)
      }), use.names = FALSE)
      vals[is.finite(vals)]
    }

    gate_default_label_pos <- function(gate) {
      geom <- gate$geometry %||% list()
      if (identical(gate$type, "polygon")) {
        vs <- geom$vertices
        if (is.data.frame(vs) && nrow(vs)) return(list(x = mean(vs$x, na.rm = TRUE), y = mean(vs$y, na.rm = TRUE)))
        if (is.list(vs) && length(vs)) {
          xs <- suppressWarnings(as.numeric(vapply(vs, function(v) v$x %||% NA_real_, numeric(1))))
          ys <- suppressWarnings(as.numeric(vapply(vs, function(v) v$y %||% NA_real_, numeric(1))))
          if (any(is.finite(xs)) && any(is.finite(ys))) return(list(x = mean(xs, na.rm = TRUE), y = mean(ys, na.rm = TRUE)))
        }
      }
      if (identical(gate$type, "quadrant") || identical(gate$type, "curved_quadrant")) {
        return(list(x = (geom$x %||% 0) * 1.02, y = (geom$y %||% 0) * 1.02))
      }
      list(x = geom$xmin %||% geom$x %||% 0, y = geom$ymax %||% geom$y %||% geom$ymin %||% 0)
    }

    gate_canvas_label <- function(gate, stats) {
      st <- stats[stats$gate_id == gate$id, , drop = FALSE]
      pct <- if (nrow(st) && is.finite(st$percent_parent[[1]])) sprintf("%.1f%% parent", st$percent_parent[[1]]) else NULL
      saved_labels <- isolate(shared$gating$label_positions())
      pos <- label_pos_cache[[gate$id %||% ""]] %||% saved_labels[[gate$id %||% ""]] %||% gate$label_pos %||% gate_default_label_pos(gate)
      list(text = paste(c(gate$name %||% gate$id, pct), collapse = "\n"), x = pos$x %||% 0, y = pos$y %||% 0)
    }

    gate_canvas_label_for_axes <- function(gate, stats, entry, x_channel, y_channel) {
      label <- gate_canvas_label(gate, stats)
      label$x <- flowjo_transform_column(label$x, axis_setting_for_channel(entry, x_channel))
      label$y <- flowjo_transform_column(label$y, axis_setting_for_channel(entry, y_channel))
      label
    }

    canvas_auto_range <- function(vals, channel, scale_override = NULL, raw_vals = NULL, setting = NULL, gate_vals = numeric()) {
      if (!is.null(scale_override)) return(scale_override)
      vals <- vals[is.finite(vals)]
      gate_vals <- gate_vals[is.finite(gate_vals)]
      if (!length(vals) && !length(gate_vals)) return(c(0, 1))
      rng <- if (length(vals)) range(vals) else range(gate_vals)
      if (length(vals)) {
        qs <- stats::quantile(vals, probs = c(0.001, 0.999), na.rm = TRUE, names = FALSE, type = 8)
        if (all(is.finite(qs)) && qs[[2]] > qs[[1]]) rng <- qs
      }
      raw_vals <- raw_vals[is.finite(raw_vals)]
      if (grepl("^(FSC|SSC)", channel %||% "", ignore.case = TRUE)) {
        zero_val <- flowjo_transform_column(0, setting)
        if (is.finite(zero_val) && zero_val < rng[[2]]) rng[[1]] <- zero_val
      }
      if (length(gate_vals)) rng <- range(c(rng, gate_vals))
      pad <- diff(rng) * 0.04
      if (!is.finite(pad) || pad <= 0) pad <- 1
      rng + c(-pad, pad)
    }

    channel_canvas_range <- function(channel, col, next_rng, setting = NULL) {
      if (is.null(col) || length(next_rng) != 2 || !all(is.finite(next_rng))) return(next_rng)
      scales <- shared$transforms$scales()
      manual_rng <- scales[[channel]] %||% scales[[col]]
      if (!is.null(manual_rng) && length(manual_rng) == 2 && all(is.finite(manual_rng))) {
        return(sort(flowjo_transform_column(manual_rng, setting)))
      }
      auto_scales <- shared$transforms$auto_scales()
      auto_rng <- auto_scales[[col]]
      if (!is.null(auto_rng) && length(auto_rng) == 2 && all(is.finite(auto_rng))) {
        next_rng <- c(min(auto_rng[[1]], next_rng[[1]]), max(auto_rng[[2]], next_rng[[2]]))
        if (!any(abs(auto_rng - next_rng) > 1e-8)) return(auto_rng)
      }
      # Agent note: auto ranges can expand to keep data/gates visible, but only manual scales may intentionally shrink.
      auto_scales[[col]] <- next_rng
      shared$transforms$auto_scales(auto_scales)
      next_rng
    }

    persist_gate_autoscale <- function(gate, gates) {
      # Agent note: autoscale persists in auto_scales only; manual axis edits in scales() must always win.
      invisible(FALSE)
    }

    effective_transform_settings <- function(entry, x_channel, y_channel, gates = list()) {
      settings <- shared$transforms$settings()
      gate_channels <- unlist(lapply(gates, function(g) c(g$x_channel, g$y_channel)), use.names = FALSE)
      for (channel in unique(c(x_channel, y_channel, gate_channels))) {
        if (is.null(channel) || !nzchar(channel)) next
        col <- flowjo_channel_to_marker(entry$channel_map, channel)
        if (is.null(col) || !nzchar(col)) next
        if (!is.null(settings[[channel]]) || !is.null(settings[[col]])) next
        setting <- effective_axis_setting(channel, col, entry)
        if (!is.null(setting)) {
          settings[[channel]] <- setting
          settings[[col]] <- setting
        }
      }
      settings
    }

    axis_population_raw_values <- function(entry, channel, plot = NULL) {
      if (is.null(plot)) plot <- isolate(active_plot())
      col <- flowjo_channel_to_marker(entry$channel_map, channel)
      if (is.null(col) || !(col %in% colnames(entry$df))) return(numeric())
      gates <- shared$gating$gates()
      pair <- plot_axis_pair(plot)
      x_col <- flowjo_channel_to_marker(entry$channel_map, pair$x)
      y_col <- flowjo_channel_to_marker(entry$channel_map, pair$y)
      if (is.null(x_col) || is.null(y_col) || !(x_col %in% colnames(entry$df)) || !(y_col %in% colnames(entry$df))) return(numeric())
      settings <- effective_transform_settings(entry, pair$x, pair$y, gates)
      df <- flowjo_transform_data(entry$df, settings)
      keep <- is.finite(df[[col]]) & is.finite(df[[x_col]]) & is.finite(df[[y_col]])
      parent <- active_parent_keep(entry$df, entry$channel_map, gates, plot_focus_gate_id(plot, gates))
      if (!is.null(parent)) keep <- keep & parent
      entry$df[[col]][keep]
    }

    preview_full_data <- reactive({
      e <- active_entry()
      req(e, input$x_channel, input$y_channel)
      x_col <- flowjo_channel_to_marker(e$channel_map, input$x_channel)
      y_col <- flowjo_channel_to_marker(e$channel_map, input$y_channel)
      req(x_col, y_col)
      gates <- shared$gating$gates()
      df <- flowjo_transform_data(e$df, effective_transform_settings(e, input$x_channel, input$y_channel, gates))
      keep <- is.finite(df[[x_col]]) & is.finite(df[[y_col]])
      parent <- active_parent_keep(e$df, e$channel_map, gates, shared$gating$active_gate_id())
      if (!is.null(parent)) keep <- keep & parent
      df <- df[keep, , drop = FALSE]
      list(df = df, x_col = x_col, y_col = y_col, entry = e)
    })

    preview_data <- reactive({
      pd <- preview_full_data()
      df <- pd$df
      if (nrow(df) > 150000) df <- df[sample.int(nrow(df), 150000), , drop = FALSE]
      pd$df <- df
      pd
    })

    current_geometry <- reactive({
      draft <- input$gate_canvas_draft
      type <- draft$type %||% input$gate_type %||% "rectangle"
      if (identical(type, "polygon")) {
        pts <- polygon_points()
        if (nrow(pts) < 3 && isTRUE(draft$closed)) pts <- canvas_points_df(draft$geometry$vertices)
        if (nrow(pts) < 3) return(NULL)
        return(canvas_geometry_to_raw(list(vertices = pts), type, current_draft_channels()))
      }
      if (!is.null(draft) && identical(draft$type %||% type, type) && !is.null(draft$geometry)) {
        return(canvas_geometry_to_raw(draft$geometry, type, current_draft_channels()))
      }
      if (type %in% c("rectangle", "range", "ellipse")) {
        b <- input$gate_brush
        if (is.null(b)) return(NULL)
        geom <- list(xmin = min(b$xmin, b$xmax), xmax = max(b$xmin, b$xmax), ymin = min(b$ymin, b$ymax), ymax = max(b$ymin, b$ymax))
        return(canvas_geometry_to_raw(geom, type, current_draft_channels()))
      }
      if (type %in% c("quadrant", "curved_quadrant")) {
        p <- input$gate_click
        if (is.null(p)) return(NULL)
        geom <- list(x = p$x, y = p$y, quadrant = input$quadrant_name %||% "upper_right", curve = if (identical(type, "curved_quadrant")) 0.25 else 0)
        return(canvas_geometry_to_raw(geom, type, current_draft_channels()))
      }
      NULL
    })

    current_draft_channels <- function() {
      draft <- input$gate_canvas_draft
      ch <- draft$channels %||% list()
      if (!is.null(ch$x) && !is.null(ch$y)) return(ch)
      gid <- canvas_selected_gate() %||% shared$gating$active_gate_id()
      gate <- shared$gating$gates()[[gid %||% ""]]
      if (!is.null(gate)) return(list(x = gate$x_channel, y = gate$y_channel))
      plot_axis_pair(draft$plot %||% active_plot())
    }

    current_draft_parent_id <- function() {
      draft <- input$gate_canvas_draft
      draft$parentId %||% plot_focus_gate_id(draft$plot %||% active_plot()) %||% "root"
    }

    axis_hist_raw_values <- function(at, setting) {
      if (is.null(setting) || is.null(setting$type)) return(at)
      if (identical(setting$type, "log1p")) return(expm1(at))
      if (identical(setting$type, "biex")) {
        tf <- tryCatch(flowCore::logicleTransform(w = setting$w, t = setting$t %||% 262144, m = setting$p, a = setting$n), error = function(e) NULL)
        if (!is.null(tf)) {
          inv <- flowCore::inverseLogicleTransform(trans = tf)
          return(tryCatch(inv(at), error = function(e) at))
        }
      }
      at
    }

    axis_hist_tick_text <- function(x) {
      vapply(x, function(val) {
        if (!is.finite(val)) return("''")
        if (abs(val) < 1e-9) return("0")
        av <- abs(val)
        if (av >= 1000) {
          pow <- floor(log10(av))
          coef <- signif(val / (10 ^ pow), 2)
          if (abs(abs(coef) - 1) < 0.05) return(if (coef < 0) sprintf("-10^%d", pow) else sprintf("10^%d", pow))
          return(sprintf("%s %%*%% 10^%d", format(coef, scientific = FALSE, trim = TRUE), pow))
        }
        format(signif(val, 3), scientific = FALSE, trim = TRUE)
      }, character(1))
    }

    axis_hist_ticks <- function(raw_vals, plot_vals, setting) {
      plot_vals <- plot_vals[is.finite(plot_vals)]
      if (!length(plot_vals)) return(list(at = numeric(), raw = numeric(), minor = numeric()))
      if (is.null(setting) || is.null(setting$type)) {
        major <- pretty(plot_vals, n = 5)
        minor <- pretty(plot_vals, n = 20)
        return(list(at = major, raw = major, minor = setdiff(minor, major)))
      }
      raw_vals <- raw_vals[is.finite(raw_vals)]
      if (!length(raw_vals)) {
        at <- pretty(plot_vals, n = 5)
        return(list(at = at, raw = axis_hist_raw_values(at, setting), minor = pretty(plot_vals, n = 20)))
      }
      pos <- raw_vals[raw_vals > 0]
      raw_ticks <- numeric()
      if (length(pos)) {
        lo <- if (identical(setting$type %||% NULL, "biex") && max(pos) >= 10000) 3 else floor(log10(min(pos)))
        raw_ticks <- 10 ^ seq(lo, ceiling(log10(max(pos))))
      }
      if (identical(setting$type %||% NULL, "biex") && any(raw_vals >= 10000, na.rm = TRUE)) raw_ticks <- c(0, raw_ticks)
      if (any(raw_vals <= 0)) raw_ticks <- c(pretty(raw_vals[raw_vals <= 0], n = 3), raw_ticks)
      keep_zero <- identical(setting$type %||% NULL, "biex") && any(raw_vals >= 10000, na.rm = TRUE)
      raw_ticks <- sort(unique(raw_ticks[(raw_ticks >= min(raw_vals) | (keep_zero & raw_ticks == 0)) & raw_ticks <= max(raw_vals)]))
      raw_minor <- numeric()
      if (length(pos)) {
        decades <- seq(floor(log10(max(min(pos), .Machine$double.eps))), ceiling(log10(max(pos))))
        raw_minor <- sort(unique(as.vector(outer(2:9, 10 ^ decades))))
      }
      at <- flowjo_transform_column(raw_ticks, setting)
      minor <- flowjo_transform_column(raw_minor, setting)
      keep <- is.finite(at) & at >= min(plot_vals) & at <= max(plot_vals)
      if (sum(keep) < 3) {
        at <- pretty(plot_vals, n = 5)
        return(list(at = at, raw = axis_hist_raw_values(at, setting), minor = pretty(plot_vals, n = 20)))
      }
      minor <- minor[is.finite(minor) & minor >= min(plot_vals) & minor <= max(plot_vals)]
      at <- at[keep]; raw_ticks <- raw_ticks[keep]
      if (length(at) > 6) {
        idx <- unique(round(seq(1, length(at), length.out = 6)))
        at <- at[idx]; raw_ticks <- raw_ticks[idx]
      }
      list(at = at, raw = raw_ticks, minor = setdiff(minor, at))
    }

    canvas_axis_ticks <- function(raw_vals, plot_vals, setting) {
      ticks <- axis_hist_ticks(raw_vals, plot_vals, setting)
      list(
        major = unname(ticks$at),
        labels = unname(flowjo_axis_labeler(setting)(ticks$at)),
        minor = unname(ticks$minor)
      )
    }

    output$axis_hist <- renderPlot({
      axis <- axis_modal_axis()
      e <- active_entry()
      pair <- plot_axis_pair(axis_modal_plot() %||% active_plot())
      req(axis, e, pair$x, pair$y)
      channel <- if (identical(axis, "x")) pair$x else pair$y
      col <- flowjo_channel_to_marker(e$channel_map, channel)
      req(col)
      raw_vals <- axis_population_raw_values(e, channel, axis_modal_plot() %||% active_plot())
      setting <- flowjo_axis_scale_setting(
        input$axis_scale_type %||% "none",
        input$axis_axis_w %||% 1.25,
        input$axis_axis_p %||% 5,
        input$axis_axis_n %||% 0,
        t = flowjo_channel_range(e, channel, col)
      )
      vals <- flowjo_transform_column(raw_vals, setting)
      vals <- vals[is.finite(vals)]
      validate(need(length(unique(vals)) > 1, "No finite events for this axis."))
      display_raw <- sort(suppressWarnings(as.numeric(input$axis_range)))
      xlim <- if (length(display_raw) == 2 && all(is.finite(display_raw)) && display_raw[[2]] > display_raw[[1]]) flowjo_transform_column(display_raw, setting) else range(vals)
      xlim <- sort(xlim[is.finite(xlim)])
      if (length(xlim) != 2 || xlim[[1]] == xlim[[2]]) xlim <- range(vals)
      d <- density(vals, n = 4096, adjust = 0.35, na.rm = TRUE)
      tick_raw_vals <- raw_vals
      tick_vals <- vals
      if (length(display_raw) == 2 && all(is.finite(display_raw)) && display_raw[[2]] > display_raw[[1]]) {
        raw_keep <- raw_vals >= display_raw[[1]] & raw_vals <= display_raw[[2]]
        val_keep <- vals >= xlim[[1]] & vals <= xlim[[2]]
        tick_raw_vals <- raw_vals[raw_keep]
        tick_vals <- vals[val_keep]
      }
      if (length(tick_vals) < 2) tick_vals <- xlim
      ticks <- axis_hist_ticks(tick_raw_vals, tick_vals, setting)
      y_ticks <- pretty(c(0, d$y), n = 5)
      op <- par(mar = c(2.7, 4.2, 0.2, 0.3), mgp = c(1.35, 0.58, 0), tcl = -0.18, cex.axis = 0.68, las = 1)
      on.exit(par(op), add = TRUE)
      plot(d$x, d$y, type = "n", main = "", xlab = "", ylab = "", axes = FALSE, xlim = xlim)
      polygon(c(d$x[[1]], d$x, d$x[[length(d$x)]]), c(0, d$y, 0), col = "#526070", border = NA)
      lines(d$x, d$y, col = "#3f4b5a", lwd = 1.2)
      axis(1, at = ticks$minor, labels = FALSE, tcl = -0.1)
      axis(1, at = ticks$at, labels = parse(text = axis_hist_tick_text(ticks$raw)))
      axis(2, at = y_ticks, labels = format(signif(y_ticks, 2), scientific = FALSE, trim = TRUE))
      box(bty = "l", lwd = 1.2)
      mtext("Density", side = 2, line = 1, cex = 0.78, las = 0)
    }, res = 96)

    save_gate_with_name <- function(nm, geom) {
      e <- active_entry()
      req(e, geom, input$x_channel)
      nm <- trimws(nm %||% "")
      req(nzchar(nm))
      gate_type <- input$gate_canvas_draft$type %||% input$gate_type
      channels <- current_draft_channels()
      parent_id <- current_draft_parent_id()
      if (duplicate_gate_name(shared$gating$gates(), parent_id, nm)) {
        showNotification(sprintf("A sibling gate named '%s' already exists. Choose a unique population name.", nm), type = "warning")
        return(NULL)
      }
      gate <- flowjo_make_gate(
        name = nm,
        type = gate_type,
        parent_id = parent_id,
        x_channel = channels$x,
        y_channel = channels$y,
        geometry = geom,
        sample_id = e$id,
        color = flowjo_gate_palette(length(shared$gating$gates()) + 1L)[[length(shared$gating$gates()) + 1L]]
      )
      shared$gating$add_gate(gate)
      persist_gate_autoscale(gate, shared$gating$gates())
      shared$gating$active_gate_id(gate$id)
      canvas_selected_gate(gate$id)
      active_plot("right")
      polygon_points(data.frame(x = numeric(), y = numeric()))
      clear_gate_canvas_drafts()
      showNotification(sprintf("Saved gate '%s'.", nm), type = "message")
      gate
    }

    update_selected_gate <- function() {
      e <- active_entry()
      gid <- canvas_selected_gate() %||% shared$gating$active_gate_id()
      gates <- shared$gating$gates()
      req(e, gid, gates[[gid]])
      draft_geom <- current_geometry()
      geom <- draft_geom %||% gates[[gid]]$geometry
      channels <- if (!is.null(draft_geom)) current_draft_channels() else list(x = gates[[gid]]$x_channel, y = gates[[gid]]$y_channel)
      req(geom, channels$x, channels$y)
      nm <- trimws(input$gate_name %||% gates[[gid]]$name)
      if (duplicate_gate_name(gates, gates[[gid]]$parent_id %||% "root", nm, exclude_id = gid)) {
        showNotification(sprintf("A sibling gate named '%s' already exists. Choose a unique population name.", nm), type = "warning")
        return(FALSE)
      }
      gates[[gid]]$name <- nm
      gates[[gid]]$type <- input$gate_type
      gates[[gid]]$x_channel <- channels$x
      gates[[gid]]$y_channel <- channels$y
      gates[[gid]]$geometry <- geom
      gates[[gid]]$source_sample_id <- e$id
      gates <- mark_gate_modified_or_sync(gates, gid)
      if (is.null(gates[[gid]]$sync_group_id)) gates[[gid]]$applied_sample_id <- e$id
      persist_gate_autoscale(gates[[gid]], gates)
      shared$gating$gates(gates)
      polygon_points(data.frame(x = numeric(), y = numeric()))
      clear_gate_canvas_drafts()
      showNotification(sprintf("Updated gate '%s'.", nm), type = "message")
      TRUE
    }

    observeEvent(input$save_gate, {
      gid <- canvas_selected_gate() %||% shared$gating$active_gate_id()
      if (!is.null(gid) && !is.null(shared$gating$gates()[[gid]])) update_selected_gate() else save_gate_with_name(input$gate_name, current_geometry())
    })

    observeEvent(input$gate_canvas_draft, {
      if (!isTRUE(input$gate_canvas_draft$closed)) return()
      gate_label <- if (identical(input$gate_canvas_draft$type %||% input$gate_type, "rectangle")) "rectangle" else "polygon"
      geom <- current_geometry()
      showModal(modalDialog(
        title = sprintf("Name %s gate", gate_label),
        textInput(session$ns("polygon_gate_name"), "Population name", value = next_gate_name(shared$gating$gates(), current_draft_parent_id(), geom, input$gate_canvas_draft$type %||% input$gate_type, current_draft_channels())),
        footer = tagList(
          actionButton(session$ns("cancel_polygon_gate"), "Cancel", class = "btn btn-outline-secondary"),
          actionButton(session$ns("save_polygon_gate"), "Save Gate", class = "btn btn-primary")
        ),
        easyClose = FALSE
      ))
    }, ignoreInit = TRUE)

    observeEvent(input$cancel_polygon_gate, {
      polygon_points(data.frame(x = numeric(), y = numeric()))
      clear_gate_canvas_drafts()
      removeModal()
    }, ignoreInit = TRUE)

    observeEvent(input$save_polygon_gate, {
      if (!is.null(save_gate_with_name(input$polygon_gate_name, current_geometry()))) removeModal()
    })

    observeEvent(input$replace_gate, {
      update_selected_gate()
    })

    observeEvent(input$delete_gate, {
      gid <- canvas_selected_gate() %||% shared$gating$active_gate_id()
      gates <- shared$gating$gates()
      req(gid, gates[[gid]])
      ids <- gate_subtree_ids(gates, gid)
      showModal(modalDialog(
        title = "Delete selected gate?",
        sprintf("Delete '%s'%s?", gates[[gid]]$name %||% gid, if (length(ids) > 1L) sprintf(" and %d child gates", length(ids) - 1L) else ""),
        footer = tagList(
          modalButton("Cancel"),
          actionButton(session$ns("confirm_delete_gate"), "Delete", class = "btn-danger")
        )
      ))
    })

    observeEvent(input$confirm_delete_gate, {
      gid <- canvas_selected_gate() %||% shared$gating$active_gate_id()
      gates <- shared$gating$gates()
      req(gid, gates[[gid]])
      ids <- gate_subtree_ids(gates, gid)
      for (id in rev(ids)) shared$gating$remove_gate(id)
      canvas_selected_gate(NULL)
      active_plot("left")
      polygon_points(data.frame(x = numeric(), y = numeric()))
      clear_gate_canvas_drafts()
      removeModal()
      showNotification(sprintf("Deleted %d gate%s.", length(ids), if (length(ids) == 1L) "" else "s"), type = "message")
    }, ignoreInit = TRUE)

    observeEvent(input$gate_canvas_geometry, {
      msg <- input$gate_canvas_geometry
      gid <- msg$id %||% canvas_selected_gate() %||% shared$gating$active_gate_id()
      gates <- shared$gating$gates()
      req(gid, gates[[gid]], msg$geometry)
      channels <- list(x = gates[[gid]]$x_channel, y = gates[[gid]]$y_channel)
      gates[[gid]]$geometry <- canvas_geometry_to_raw(msg$geometry, gates[[gid]]$type, channels)
      gates <- mark_gate_modified_or_sync(gates, gid)
      persist_gate_autoscale(gates[[gid]], gates)
      shared$gating$gates(gates)
    }, ignoreInit = TRUE)

    observeEvent(input$unsync_sample_gate, {
      gid <- canvas_selected_gate() %||% shared$gating$active_gate_id()
      if (isTRUE(unsync_sample_gate(gid))) showNotification("This gate is now sample-local.", type = "message")
    }, ignoreInit = TRUE)

    observeEvent(input$gate_canvas_label, {
      msg <- input$gate_canvas_label
      gid <- msg$id %||% canvas_selected_gate() %||% shared$gating$active_gate_id()
      gates <- shared$gating$gates()
      pos <- msg$label_pos %||% list()
      x <- suppressWarnings(as.numeric(pos$x))
      y <- suppressWarnings(as.numeric(pos$y))
      req(gid, gates[[gid]], is.finite(x), is.finite(y))
      e <- active_entry()
      req(e)
      x <- flowjo_inverse_transform_column(x, axis_setting_for_channel(e, gates[[gid]]$x_channel))
      y <- flowjo_inverse_transform_column(y, axis_setting_for_channel(e, gates[[gid]]$y_channel))
      label_pos <- list(x = x, y = y)
      label_pos_cache[[gid]] <- label_pos
      labels <- shared$gating$label_positions()
      labels[[gid]] <- label_pos
      shared$gating$label_positions(labels)
    }, ignoreInit = TRUE)

    observeEvent(input$use_root, {
      shared$gating$active_gate_id(NULL)
      canvas_selected_gate(NULL)
      active_plot("left")
      updateTextInput(session, "gate_name", value = next_gate_name(shared$gating$gates(), "root", current_geometry()))
      polygon_points(data.frame(x = numeric(), y = numeric()))
      clear_gate_canvas_drafts()
    }, ignoreInit = TRUE)

    add_gates_to_basket <- function(gate_ids) {
      reg <- shared$fcs$registry(); gates <- shared$gating$gates()
      gate_ids <- intersect(gate_ids %||% character(), names(gates))
      if (!length(gate_ids)) return(showNotification("No gate selected.", type = "warning"))
      sid <- active_sample_id()
      if (is.null(sid) || is.null(reg[[sid]])) return(showNotification("No active sample.", type = "warning"))
      added <- 0L
      for (gid in gate_ids) {
        g <- gates[[gid]]
        if (isTRUE(flowjo_gate_applicability(reg[[sid]], g, gates)$ok)) {
          added <- added + shared$populations$add(sid, gid, set_name = "Population Set")
        }
      }
      showNotification(sprintf("Added %d current-sample population row%s.", added, if (added == 1L) "" else "s"), type = if (added) "message" else "warning")
    }

    observeEvent(input$add_active_to_basket, add_gates_to_basket(canvas_selected_gate() %||% shared$gating$active_gate_id()), ignoreInit = TRUE)

    preview_data_for_gate <- function(gate_id, plot_name = NULL, axis_gate_id = NULL) {
      if (is.null(plot_name)) plot_name <- isolate(active_plot())
      e <- active_entry()
      gates <- shared$gating$gates()
      axis_gate <- if (!is.null(axis_gate_id) && !identical(axis_gate_id, "root")) gates[[axis_gate_id]] else NULL
      pair <- if (!is.null(axis_gate)) list(x = axis_gate$x_channel, y = axis_gate$y_channel) else plot_axis_pair(plot_name)
      req(e, pair$x, pair$y)
      x_col <- flowjo_channel_to_marker(e$channel_map, pair$x)
      y_col <- flowjo_channel_to_marker(e$channel_map, pair$y)
      df <- flowjo_transform_data(e$df, effective_transform_settings(e, pair$x, pair$y, gates))
      keep <- is.finite(df[[x_col]]) & is.finite(df[[y_col]])
      parent <- active_parent_keep(e$df, e$channel_map, gates, gate_id)
      if (!is.null(parent)) keep <- keep & parent
      df <- df[keep, , drop = FALSE]
      if (nrow(df) > 150000) df <- df[sample.int(nrow(df), 150000), , drop = FALSE]
      list(df = df, x_col = x_col, y_col = y_col, entry = e, x_channel = pair$x, y_channel = pair$y)
    }

    canvas_payload <- function(wrap_id, plot_name, focus_gate_id, editable = FALSE, selected_gate_id = NULL, axis_gate_id = NULL) {
      mode <- input$display_mode %||% "dot"
      pd <- preview_data_for_gate(focus_gate_id, plot_name, axis_gate_id)
      if (plot_name %in% c("left", "right")) {
        displayed_plot_axes[[plot_name]] <- list(x = pd$x_channel, y = pd$y_channel)
      }
      gates <- shared$gating$gates()
      # Agent note: Canvas payload must be sample-scoped; synced copies from other samples share axes/parents and otherwise stack labels on one plot.
      plot_gates <- gates[vapply(gates, function(g) isTRUE(flowjo_gate_applicability(pd$entry, g, gates)$ok), logical(1))]
      stats <- active_sample_stats()
      plot_gate_list <- function() {
        parent_id <- focus_gate_id %||% "root"
        # Agent note: Right preview overlays are subtree-scoped so nested gates stay visible while editing the current child population.
        context_ids <- if (identical(plot_name, "right")) gate_descendant_ids(plot_gates, parent_id) else flowjo_gate_children(plot_gates, parent_id)
        out <- lapply(seq_along(plot_gates), function(i) {
          g <- plot_gates[[i]]
          if (!g$id %in% context_ids) return(NULL)
          if (!flowjo_gate_channel_match(g, pd$x_channel, pd$y_channel)) return(NULL)
          display_gate <- raw_gate_for_canvas(g, pd$x_channel, pd$y_channel, pd$entry)
          list(id = g$id, name = g$name, type = display_gate$type, geometry = display_gate$geometry, color = flowjo_gate_color(g, i), label = gate_canvas_label_for_axes(g, stats, pd$entry, pd$x_channel, pd$y_channel))
        })
        out[!vapply(out, is.null, logical(1))]
      }
      if (!nrow(pd$df)) {
        gate_list <- plot_gate_list()
        return(list(
          id = session$ns(wrap_id),
          ns = session$ns(""),
          input = list(
            geometry = session$ns("gate_canvas_geometry"),
            polygon = session$ns("gate_canvas_polygon"),
            draft = session$ns("gate_canvas_draft"),
            select = session$ns("gate_canvas_select"),
            drill = session$ns("gate_canvas_drill"),
            activate = session$ns("gate_canvas_activate"),
            label = session$ns("gate_canvas_label")
          ),
          readOnly = !isTRUE(editable),
          plot = plot_name,
          mode = mode,
          tool = input$gate_type %||% "rectangle",
          selected = selected_gate_id,
          parentId = focus_gate_id %||% "root",
          channels = list(x = pd$x_channel, y = pd$y_channel),
          x = numeric(),
          y = numeric(),
          marker = numeric(),
          group = character(),
          groupColors = list(Ungated = "#B8BEC7"),
          gates = gate_list,
          polygon = if (nrow(polygon_points())) split(polygon_points(), seq_len(nrow(polygon_points()))) else list(),
          ranges = list(x = c(0, 1), y = c(0, 1)),
          ticks = list(x = list(major = numeric(), labels = character(), minor = numeric()), y = list(major = numeric(), labels = character(), minor = numeric())),
          labels = list(x = flowjo_channel_label(pd$entry$channel_map, pd$x_channel), y = flowjo_channel_label(pd$entry$channel_map, pd$y_channel))
        ))
      }
      marker_col <- flowjo_channel_to_marker(pd$entry$channel_map, input$marker_color %||% pd$x_channel)
      marker_vals <- if (identical(mode, "marker") && marker_col %in% colnames(pd$df)) pd$df[[marker_col]] else rep(NA_real_, nrow(pd$df))
      groups <- if (identical(mode, "gate") && length(plot_gates)) as.character(flowjo_gate_plot_groups(pd$df, pd$entry$channel_map, plot_gates, pd$x_channel, pd$y_channel)) else rep("Ungated", nrow(pd$df))
      group_cols <- c(Ungated = "#B8BEC7")
      if (length(plot_gates)) group_cols <- c(group_cols, stats::setNames(vapply(seq_along(plot_gates), function(i) flowjo_gate_color(plot_gates[[i]], i), character(1)), vapply(plot_gates, `[[`, character(1), "name")))
      gate_list <- plot_gate_list()
      x_setting <- effective_axis_setting(pd$x_channel, pd$x_col, pd$entry)
      y_setting <- effective_axis_setting(pd$y_channel, pd$y_col, pd$entry)
      x_raw_col <- flowjo_channel_to_marker(pd$entry$channel_map, pd$x_channel)
      y_raw_col <- flowjo_channel_to_marker(pd$entry$channel_map, pd$y_channel)
      full_x <- flowjo_transform_column(pd$entry$df[[x_raw_col]], x_setting)
      full_y <- flowjo_transform_column(pd$entry$df[[y_raw_col]], y_setting)
      x_rng <- canvas_auto_range(pd$df[[pd$x_col]], pd$x_channel, NULL, numeric(), x_setting, gate_axis_values(gate_list, "x"))
      y_rng <- canvas_auto_range(pd$df[[pd$y_col]], pd$y_channel, NULL, numeric(), y_setting, gate_axis_values(gate_list, "y"))
      x_rng <- channel_canvas_range(pd$x_channel, pd$x_col, x_rng, x_setting)
      y_rng <- channel_canvas_range(pd$y_channel, pd$y_col, y_rng, y_setting)
      x_ticks <- canvas_axis_ticks(pd$entry$df[[x_raw_col]], full_x, x_setting)
      y_ticks <- canvas_axis_ticks(pd$entry$df[[y_raw_col]], full_y, y_setting)
      if (!all(is.finite(x_rng)) || x_rng[1] == x_rng[2]) x_rng <- c(0, 1)
      if (!all(is.finite(y_rng)) || y_rng[1] == y_rng[2]) y_rng <- c(0, 1)
      list(
        id = session$ns(wrap_id),
        ns = session$ns(""),
        input = list(
          geometry = session$ns("gate_canvas_geometry"),
          polygon = session$ns("gate_canvas_polygon"),
          draft = session$ns("gate_canvas_draft"),
          select = session$ns("gate_canvas_select"),
          drill = session$ns("gate_canvas_drill"),
          activate = session$ns("gate_canvas_activate"),
          label = session$ns("gate_canvas_label")
        ),
        readOnly = !isTRUE(editable),
        plot = plot_name,
        mode = mode,
        tool = input$gate_type %||% "rectangle",
        selected = selected_gate_id,
        parentId = focus_gate_id %||% "root",
        channels = list(x = pd$x_channel, y = pd$y_channel),
        x = unname(pd$df[[pd$x_col]]),
        y = unname(pd$df[[pd$y_col]]),
        marker = unname(marker_vals),
        group = groups,
        groupColors = as.list(group_cols),
        gates = gate_list,
        polygon = if (nrow(polygon_points())) split(polygon_points(), seq_len(nrow(polygon_points()))) else list(),
        ranges = list(x = unname(x_rng), y = unname(y_rng)),
        ticks = list(x = x_ticks, y = y_ticks),
        labels = list(x = flowjo_channel_label(pd$entry$channel_map, pd$x_channel), y = flowjo_channel_label(pd$entry$channel_map, pd$y_channel))
      )
    }

    preview_focus_gate_id <- function(gates = NULL) {
      if (is.null(gates)) gates <- shared$gating$gates()
      gid <- shared$gating$active_gate_id() %||% canvas_selected_gate()
      if (!is.null(gid) && !is.null(gates[[gid]])) gid else NULL
    }

    ensure_active_preview_gate <- function(gates = NULL) {
      if (is.null(gates)) gates <- shared$gating$gates()
      active_id <- preview_focus_gate_id(gates)
      if (!is.null(active_id)) return(active_id)
      gid <- first_gate_id(gates)
      if (is.null(gid)) return(NULL)
      # Agent note: Saved-gate landing must open a real population; otherwise the right preview can stay blank/root until a second user action.
      reset_right_axis_override(gid)
      shared$gating$active_gate_id(gid)
      canvas_selected_gate(gid)
      active_plot("right")
      gid
    }

    active_plot_message <- function(active_plot_value = isolate(active_plot())) {
      list(
        name = "flowjo-gating-active-plot",
        payload = list(id = session$ns("preview_card"), active = active_plot_value)
      )
    }

    preview_payload_messages <- function(gates, active_id, active_plot_value = isolate(active_plot())) {
      left_id <- flowjo_gate_parent_id(gates, active_id)
      left_axis_gate_id <- if (identical(active_plot_value, "left")) NULL else active_id
      right_axis_gate_id <- right_axis_source_gate_id(gates, active_id, active_entry()) %||% active_id
      selected_id <- isolate(canvas_selected_gate()) %||% active_id
      left_selected_id <- selected_id
      msgs <- list(list(
        name = "flowjo-gating-payload",
        # Agent note: Parent context keeps sibling gates visible; only the selected/current gate should receive the active overlay.
        payload = canvas_payload("canvas_wrap_left", "left", left_id, identical(active_plot_value, "left"), left_selected_id, left_axis_gate_id)
      ))
      if (!is.null(active_id)) {
        msgs <- c(msgs, list(list(
          name = "flowjo-gating-payload",
          payload = canvas_payload("canvas_wrap_right", "right", active_id, identical(active_plot_value, "right"), selected_id, right_axis_gate_id)
        )))
      }
      c(msgs, list(active_plot_message(active_plot_value)))
    }

    send_preview_messages <- function(msgs) {
      for (msg in msgs) session$sendCustomMessage(msg$name, msg$payload)
    }

    observe({
      gates <- shared$gating$gates()
      if (isTRUE(skip_next_gate_payload())) {
        skip_next_gate_payload(FALSE)
        return()
      }
      active_id <- ensure_active_preview_gate(gates)
      if (!is.null(active_id) && is.null(gates[[active_id]])) {
        shared$gating$active_gate_id(NULL)
        active_plot("right")
        return()
      }
      msgs <- preview_payload_messages(gates, active_id)
      session$onFlushed(function() send_preview_messages(msgs), once = TRUE)
    })

    observeEvent(active_plot(), {
      msg <- active_plot_message(active_plot())
      session$sendCustomMessage(msg$name, msg$payload)
    }, ignoreInit = TRUE)

    observeEvent(input$gate_canvas_request, {
      gates <- shared$gating$gates()
      active_id <- ensure_active_preview_gate(gates)
      if (!is.null(active_id) && is.null(gates[[active_id]])) {
        shared$gating$active_gate_id(NULL)
        active_plot("right")
        return()
      }
      msgs <- preview_payload_messages(gates, active_id)
      session$onFlushed(function() send_preview_messages(msgs), once = TRUE)
    }, ignoreInit = TRUE)

    observeEvent(input$gate_canvas_activate, {
      plot <- input$gate_canvas_activate$plot %||% "right"
      if (plot %in% c("left", "right")) {
        current_plot <- active_plot()
        if (!identical(plot, current_plot)) {
          current_pair <- plot_axis_pair(current_plot)
          if (!is.null(current_pair$x) || !is.null(current_pair$y)) plot_axes[[current_plot]] <- current_pair
          displayed_pair <- displayed_plot_axes[[plot]]
          if (!is.null(displayed_pair$x) || !is.null(displayed_pair$y)) {
            existing_pair <- plot_axes[[plot]]
            if (!identical(existing_pair, displayed_pair)) plot_axes[[plot]] <- displayed_pair
            axes <- view_axes()
            axis_key <- view_axis_key(plot, plot_focus_gate_id(plot))
            if (!identical(axes[[axis_key]], displayed_pair)) {
              axes[[axis_key]] <- displayed_pair
              view_axes(axes)
            }
          }
          active_plot(plot)
        }
        parent_id <- plot_focus_gate_id(plot) %||% "root"
        gid <- canvas_selected_gate() %||% shared$gating$active_gate_id()
        if (!is.null(gid) && !is.null(shared$gating$gates()[[gid]])) {
          sync_gate_config(gid)
        } else {
          next_name <- next_gate_name(shared$gating$gates(), parent_id)
          if (!identical(input$gate_name %||% "", next_name)) updateTextInput(session, "gate_name", value = next_name)
        }
      }
    }, ignoreInit = TRUE)

    observeEvent(input$gate_canvas_select, {
      selected_id <- input$gate_canvas_select %||% ""
      if (nzchar(selected_id)) {
        canvas_selected_gate(selected_id)
        session$sendCustomMessage("flowjo-gating-select", list(active = active_plot(), selected = selected_id))
      }
    }, ignoreInit = TRUE)

    observeEvent(input$gate_path_select, {
      val <- input$gate_path_select %||% "root"
      gid <- if (identical(val, "root")) NULL else val
      gates <- shared$gating$gates()
      if (!is.null(gid) && is.null(gates[[gid]])) return()
      reset_right_axis_override(gid)
      canvas_selected_gate(gid)
      shared$gating$active_gate_id(gid)
      active_plot("right")
      sync_gate_config(gid)
      polygon_points(data.frame(x = numeric(), y = numeric()))
      clear_gate_canvas_drafts()
    }, ignoreInit = TRUE)

    observeEvent(input$gate_canvas_drill, {
      gid <- input$gate_canvas_drill %||% ""
      gates <- shared$gating$gates()
      req(nzchar(gid), gates[[gid]])
      reset_right_axis_override(gid)
      canvas_selected_gate(gid)
      shared$gating$active_gate_id(gid)
      active_plot("right")
      polygon_points(data.frame(x = numeric(), y = numeric()))
      clear_gate_canvas_drafts()
      msgs <- preview_payload_messages(gates, gid, "right")
      session$onFlushed(function() send_preview_messages(msgs), once = TRUE)
    }, ignoreInit = TRUE)

    preview_nav <- function(dir) {
      gates <- shared$gating$gates()
      gid <- shared$gating$active_gate_id()
      sid <- active_sample_id()
      if (identical(dir, "left")) {
        pid <- flowjo_gate_parent_id(gates, gid)
        if (!is.null(pid)) {
          next_gid <- if (identical(pid, "root")) NULL else pid
          reset_right_axis_override(next_gid)
          shared$gating$active_gate_id(next_gid)
          active_plot("right")
        }
      } else if (identical(dir, "right")) {
        kids <- flowjo_gate_children(gates, gid %||% "root")
        if (length(kids)) {
          reset_right_axis_override(kids[[1]])
          shared$gating$active_gate_id(kids[[1]])
          active_plot("right")
        }
      } else if (identical(dir, "up")) {
        prev_sid <- flowjo_prev_next_sample(shared$fcs$registry(), gates, gid, sid, -1L)
        if (!is.null(prev_sid)) {
          peer_id <- flowjo_gate_peer_id(gates, gid, prev_sid)
          shared$fcs$set_active(prev_sid)
          updateSelectInput(session, "sample_id", selected = prev_sid)
          if (!is.null(peer_id)) {
            reset_right_axis_override(peer_id)
            shared$gating$active_gate_id(peer_id)
          }
        }
      } else if (identical(dir, "down")) {
        next_sid <- flowjo_prev_next_sample(shared$fcs$registry(), gates, gid, sid, 1L)
        if (!is.null(next_sid)) {
          peer_id <- flowjo_gate_peer_id(gates, gid, next_sid)
          shared$fcs$set_active(next_sid)
          updateSelectInput(session, "sample_id", selected = next_sid)
          if (!is.null(peer_id)) {
            reset_right_axis_override(peer_id)
            shared$gating$active_gate_id(peer_id)
          }
        }
      }
    }

    preview_sibling_nav <- function(direction = 1L) {
      gates <- shared$gating$gates()
      gid <- shared$gating$active_gate_id()
      if (is.null(gid) || is.null(gates[[gid]])) return()
      parent_id <- gates[[gid]]$parent_id %||% "root"
      siblings <- flowjo_gate_children(gates, parent_id)
      if (length(siblings) < 2L) return()
      idx <- match(gid, siblings)
      if (is.na(idx)) return()
      next_idx <- ((idx - 1L + direction) %% length(siblings)) + 1L
      reset_right_axis_override(siblings[[next_idx]])
      shared$gating$active_gate_id(siblings[[next_idx]])
      canvas_selected_gate(siblings[[next_idx]])
      active_plot("right")
    }

    observeEvent(input$preview_prev_gate, preview_nav("left"), ignoreInit = TRUE)
    observeEvent(input$preview_next_gate, preview_nav("right"), ignoreInit = TRUE)
    observeEvent(input$preview_prev_sibling, preview_sibling_nav(-1L), ignoreInit = TRUE)
    observeEvent(input$preview_next_sibling, preview_sibling_nav(1L), ignoreInit = TRUE)
    observeEvent(input$preview_prev_sample, preview_nav("up"), ignoreInit = TRUE)
    observeEvent(input$preview_next_sample, preview_nav("down"), ignoreInit = TRUE)
    observeEvent(input$preview_key_nav, preview_nav(input$preview_key_nav$dir), ignoreInit = TRUE)

    output$nav_toolbar <- renderUI({
      gates <- shared$gating$gates()
      gid <- shared$gating$active_gate_id()
      sid <- active_sample_id()
      reg <- shared$fcs$registry()
      parent_id <- flowjo_gate_parent_id(gates, gid)
      child_ids <- if (is.null(gid)) flowjo_gate_children(gates, "root") else flowjo_gate_children(gates, gid)
      child_id <- if (length(child_ids)) child_ids[[1]] else NULL
      prev_sid <- flowjo_prev_next_sample(reg, gates, gid, sid, -1L)
      next_sid <- flowjo_prev_next_sample(reg, gates, gid, sid, 1L)
      btn <- function(id, icon_name, title, enabled) {
        actionButton(session$ns(id), icon(icon_name), title = title,
          class = paste("btn btn-outline-secondary btn-sm gate-nav-icon", if (isTRUE(enabled)) "" else "disabled"))
      }
      div(class = "gate-nav-toolbar",
        btn("nav_prev_sample", "chevron-left", "Previous matching sample", !is.null(prev_sid)),
        btn("nav_next_sample", "chevron-right", "Next matching sample", !is.null(next_sid)),
        btn("nav_parent_gate", "chevron-up", "Parent gate", !is.null(parent_id) && !identical(parent_id, "root")),
        btn("nav_child_gate", "chevron-down", "First child gate", !is.null(child_id)),
        actionButton(session$ns("nav_add_basket"), icon("plus"), title = "Add selected gates to population basket", class = "btn btn-success btn-sm gate-nav-icon"),
        span(class = "gate-nav-toolbar-label", sprintf("%s / %s",
          shared$fcs$registry()[[sid]]$name %||% "No sample",
          gates[[gid]]$name %||% "Root"))
      )
    })

    observeEvent(input$nav_add_basket, {
      ids <- navigator_selected_gates()
      if (!length(ids)) ids <- shared$gating$active_gate_id()
      add_gates_to_basket(ids)
    }, ignoreInit = TRUE)

    observeEvent(input$nav_prev_sample, {
      gates <- shared$gating$gates()
      gid <- shared$gating$active_gate_id()
      sid <- flowjo_prev_next_sample(shared$fcs$registry(), gates, gid, active_sample_id(), -1L)
      if (!is.null(sid)) {
        peer_id <- flowjo_gate_peer_id(gates, gid, sid)
        shared$fcs$set_active(sid)
        updateSelectInput(session, "sample_id", selected = sid)
        if (!is.null(peer_id)) {
          reset_right_axis_override(peer_id)
          shared$gating$active_gate_id(peer_id)
        }
      }
    }, ignoreInit = TRUE)
    observeEvent(input$nav_next_sample, {
      gates <- shared$gating$gates()
      gid <- shared$gating$active_gate_id()
      sid <- flowjo_prev_next_sample(shared$fcs$registry(), gates, gid, active_sample_id(), 1L)
      if (!is.null(sid)) {
        peer_id <- flowjo_gate_peer_id(gates, gid, sid)
        shared$fcs$set_active(sid)
        updateSelectInput(session, "sample_id", selected = sid)
        if (!is.null(peer_id)) {
          reset_right_axis_override(peer_id)
          shared$gating$active_gate_id(peer_id)
        }
      }
    }, ignoreInit = TRUE)
    observeEvent(input$nav_parent_gate, {
      pid <- flowjo_gate_parent_id(shared$gating$gates(), shared$gating$active_gate_id())
      if (!is.null(pid)) {
        next_gid <- if (identical(pid, "root")) NULL else pid
        reset_right_axis_override(next_gid)
        shared$gating$active_gate_id(next_gid)
      }
    }, ignoreInit = TRUE)
    observeEvent(input$nav_child_gate, {
      gid <- shared$gating$active_gate_id()
      kids <- flowjo_gate_children(shared$gating$gates(), gid %||% "root")
      if (length(kids)) {
        reset_right_axis_override(kids[[1]])
        shared$gating$active_gate_id(kids[[1]])
      }
    }, ignoreInit = TRUE)

    output$navigator_samples <- renderUI({
      reg <- shared$fcs$registry()
      groups <- shared$gating$groups()
      if (!length(reg)) return(div(class = "gate-nav-empty", "No samples loaded."))
      sample_row <- function(id, grouped = FALSE) {
        e <- reg[[id]]
        cls <- paste("gate-nav-row gate-nav-sample", if (identical(id, active_sample_id())) "active" else "", if (grouped) "grouped" else "")
        div(
          class = cls,
          `data-sample-id` = id,
          ondragover = "event.preventDefault(); this.classList.add('drop-target');",
          ondragleave = "this.classList.remove('drop-target');",
          ondrop = sprintf("event.preventDefault(); this.classList.remove('drop-target'); Shiny.setInputValue('%s',{gate:event.dataTransfer.getData('text/plain'),targetType:'sample',targetId:'%s',nonce:Math.random()},{priority:'event'});", session$ns("gate_navigator_drop"), id),
          onclick = sprintf("Shiny.setInputValue('%s','%s',{priority:'event'});", session$ns("navigator_sample_select"), id),
          span(class = "gate-nav-name", e$name %||% id),
          span(class = "gate-nav-meta", sprintf("%s events", format(e$cells %||% nrow(e$df), big.mark = ",")))
        )
      }
      group_blocks <- lapply(names(groups), function(gid) {
        g <- groups[[gid]]
        div(class = "gate-nav-group",
          div(
            class = "gate-nav-row gate-nav-group-head",
            `data-group-id` = gid,
            span(class = "gate-nav-name", icon("folder"), " ", g$name %||% gid),
            span(class = "gate-nav-meta", sprintf("%d samples", length(g$sample_ids %||% character())))
          ),
          lapply(g$sample_ids %||% character(), function(id) if (!is.null(reg[[id]])) sample_row(id, TRUE))
        )
      })
      grouped_ids <- unique(unlist(lapply(groups, function(g) g$sample_ids %||% character()), use.names = FALSE))
      ungrouped <- setdiff(names(reg), grouped_ids)
      div(class = "gate-nav-section",
        group_blocks,
        if (length(ungrouped)) div(class = "gate-nav-ungrouped", div(class = "gate-nav-section-title", "Samples"), lapply(ungrouped, sample_row)) else NULL
      )
    })

    observeEvent(input$navigator_sample_select, {
      sid <- input$navigator_sample_select
      if (nzchar(sid %||% "")) {
        shared$fcs$set_active(sid)
        updateSelectInput(session, "sample_id", selected = sid)
      }
    }, ignoreInit = TRUE)

    flatten_gate_tree <- function(gates, parent_id = "root", depth = 0L) {
      ids <- flowjo_gate_children(gates, parent_id)
      out <- list()
      for (id in ids) {
        out[[length(out) + 1L]] <- list(id = id, depth = depth)
        out <- c(out, flatten_gate_tree(gates, id, depth + 1L))
      }
      out
    }

    output$navigator_gates <- renderUI({
      gates <- shared$gating$gates()
      sid <- active_sample_id()
      if (is.null(sid)) return(div(class = "gate-nav-empty", "No active sample."))
      selected <- navigator_selected_gates()
      row_for <- function(id, depth = 0L) {
        g <- gates[[id]]
        st <- gate_stat_row(sid, id)
        status <- gate_status(sid, id)
        avail <- gate_available(sid, id)
        cls <- paste("gate-nav-row gate-nav-gate",
          if (identical(id, shared$gating$active_gate_id())) "active" else "",
          if (id %in% selected) "selected" else "",
          if (!avail) "disabled" else "",
          if (identical(status, "synced")) "synced" else "")
        div(
          class = cls,
          draggable = if (avail) "true" else "false",
          `data-gate-id` = id,
          ondragstart = if (avail) sprintf("event.dataTransfer.setData('text/plain','%s');", id) else NULL,
          ondragover = if (avail) "event.preventDefault(); this.classList.add('drop-target');" else NULL,
          ondragleave = "this.classList.remove('drop-target');",
          ondrop = if (avail) sprintf("event.preventDefault(); this.classList.remove('drop-target'); Shiny.setInputValue('%s',{gate:event.dataTransfer.getData('text/plain'),targetType:'gate',targetId:'%s',nonce:Math.random()},{priority:'event'});", session$ns("gate_navigator_drop"), id) else NULL,
          onclick = if (avail) sprintf("Shiny.setInputValue('%s',{id:'%s',meta:event.metaKey||event.ctrlKey,shift:event.shiftKey,nonce:Math.random()},{priority:'event'});", session$ns("gate_navigator_select"), id) else NULL,
          span(class = "gate-tree-name", style = sprintf("padding-left:%dpx", depth * 16L), icon(if (length(flowjo_gate_children(gates, id))) "caret-right" else "circle"), " ", g$name),
          span(class = "gate-tree-type", g$type),
          span(class = "gate-tree-count", if (is.null(st)) "--" else format(st$cells, big.mark = ",")),
          span(class = "gate-tree-percent", if (is.null(st)) "--" else sprintf("%.1f%%", st$percent_parent)),
          span(class = "gate-tree-status", status),
          span(class = "gate-tree-swatch", style = sprintf("background:%s", flowjo_gate_color(g, match(id, names(gates))))),
          actionButton(session$ns(paste0("open_gate_preview_", id)), icon("up-right-from-square"), label = NULL, title = "Open preview", class = "btn btn-link btn-xs gate-preview-open")
        )
      }
      rows <- flatten_gate_tree(gates)
      div(class = "gate-nav-table",
        div(class = "gate-nav-row gate-nav-header",
          span("Gate"), span("Type"), span("Count"), span("% Parent"), span("Status"), span(""), span("")
        ),
        div(class = paste("gate-nav-row gate-nav-root", if (is.null(shared$gating$active_gate_id())) "active" else ""),
          onclick = sprintf("Shiny.setInputValue('%s','root',{priority:'event'});", session$ns("select_gate")),
          span(class = "gate-tree-name", icon("home"), " Root"), span("--"), span("--"), span("--"), span("available"), span(""), span("")
        ),
        if (length(rows)) lapply(rows, function(x) row_for(x$id, x$depth)) else div(class = "gate-nav-empty", "No gates yet.")
      )
    })

    observeEvent(input$gate_navigator_select, {
      msg <- input$gate_navigator_select
      id <- msg$id
      if (!nzchar(id %||% "")) return()
      cur <- navigator_selected_gates()
      if (isTRUE(msg$meta)) {
        cur <- if (id %in% cur) setdiff(cur, id) else c(cur, id)
      } else if (isTRUE(msg$shift) && length(cur)) {
        ids <- names(shared$gating$gates())
        rng <- range(match(c(cur[[length(cur)]], id), ids), na.rm = TRUE)
        cur <- unique(c(cur, ids[seq(rng[[1]], rng[[2]])]))
      } else {
        cur <- id
      }
      navigator_selected_gates(cur)
      shared$gating$active_gate_id(id)
    }, ignoreInit = TRUE)

    observeEvent(input$gate_navigator_drop, {
      msg <- input$gate_navigator_drop
      dragged <- unlist(msg$gate, use.names = FALSE)
      dragged <- unique(trimws(unlist(strsplit(paste(dragged, collapse = ","), ",", fixed = TRUE), use.names = FALSE)))
      dragged <- dragged[nzchar(dragged)]
      if (!length(dragged)) return()
      gate_ids <- navigator_selected_gates()
      if (!any(dragged %in% gate_ids)) gate_ids <- dragged
      target_type <- msg$targetType
      target_id <- msg$targetId
      if (identical(target_type, "sample")) {
        apply_gate_ids_to_sample(gate_ids, target_id)
      } else if (identical(target_type, "gate")) {
        for (gid in gate_ids) apply_gate_ids_to_sample(gid, active_sample_id(), parent_id = target_id)
      } else if (identical(target_type, "group") && (msg$mode %||% "") %in% c("copy", "sync")) {
        apply_gate_ids_to_group(gate_ids, target_id, mode = msg$mode, confirm = TRUE)
      }
    }, ignoreInit = TRUE)

    observeEvent(input$confirm_gate_drop, {
      drop <- pending_gate_drop()
      if (is.null(drop)) return()
      if (identical(drop$type, "group")) {
        apply_gate_ids_to_group(drop$gate_ids, drop$group_id, mode = drop$mode %||% "copy", confirm = FALSE)
      } else {
        for (gid in drop$gate_ids) {
          copy_gate_to_target(gid, sample_id = drop$sample_id, parent_id = drop$parent_id, overwrite_id = if (identical(gid, drop$conflict_gate_id)) drop$overwrite_id else NULL)
        }
      }
      pending_gate_drop(NULL)
      removeModal()
    }, ignoreInit = TRUE)

    gate_preview_payload <- function(win_id, gate_id, wrap_id) {
      reg <- shared$fcs$registry()
      sid <- active_sample_id()
      e <- reg[[sid]]
      gates <- shared$gating$gates()
      gate <- gates[[gate_id]]
      app <- flowjo_gate_applicability(e, gate, gates)
      if (!isTRUE(app$ok)) return(NULL)
      x_col <- flowjo_channel_to_marker(e$channel_map, gate$x_channel)
      y_col <- flowjo_channel_to_marker(e$channel_map, gate$y_channel)
      df <- flowjo_transform_data(e$df, effective_transform_settings(e, gate$x_channel, gate$y_channel, gates))
      keep <- is.finite(df[[x_col]]) & is.finite(df[[y_col]])
      df <- df[keep, , drop = FALSE]
      parent <- parent_keep_for_gate(df, e$channel_map, gates, gate_id)
      if (!is.null(parent)) df <- df[parent, , drop = FALSE]
      if (nrow(df) > 50000) df <- df[sample.int(nrow(df), 50000), , drop = FALSE]
      x_rng <- range(df[[x_col]], na.rm = TRUE)
      y_rng <- range(df[[y_col]], na.rm = TRUE)
      if (!all(is.finite(x_rng)) || x_rng[[1]] == x_rng[[2]]) x_rng <- c(0, 1)
      if (!all(is.finite(y_rng)) || y_rng[[1]] == y_rng[[2]]) y_rng <- c(0, 1)
      list(
        id = wrap_id,
        input = list(),
        mode = gate_preview_windows()[[win_id]]$mode %||% "pseudocolor",
        tool = "select",
        selected = gate_id,
        x = unname(df[[x_col]]),
        y = unname(df[[y_col]]),
        marker = rep(NA_real_, nrow(df)),
        group = rep("Ungated", nrow(df)),
        groupColors = list(Ungated = "#B8BEC7"),
        gates = list(list(id = gate$id, name = gate$name, type = gate$type, geometry = gate$geometry, color = flowjo_gate_color(gate, match(gate_id, names(gates))))),
        polygon = list(),
        ranges = list(x = unname(x_rng), y = unname(y_rng)),
        labels = list(x = flowjo_channel_label(e$channel_map, gate$x_channel), y = flowjo_channel_label(e$channel_map, gate$y_channel))
      )
    }

    observe({
      gates <- shared$gating$gates()
      for (gid in names(gates)) {
        id <- paste0("open_gate_preview_", gid)
        if (isTruthy(input[[id]])) {
          wins <- gate_preview_windows()
          win_id <- paste0("preview_", gid)
          if (is.null(wins[[win_id]])) wins[[win_id]] <- list(id = win_id, gate_id = gid, mode = input$display_mode %||% "pseudocolor")
          gate_preview_windows(wins)
        }
      }
    })

    observeEvent(input$close_gate_preview, {
      id <- input$close_gate_preview
      wins <- gate_preview_windows()
      wins[[id]] <- NULL
      gate_preview_windows(wins)
    }, ignoreInit = TRUE)

    observeEvent(input$gate_preview_settings, {
      id <- input$gate_preview_settings
      wins <- gate_preview_windows()
      win <- wins[[id]]
      req(win)
      showModal(modalDialog(
        title = "Preview settings",
        selectInput(session$ns("preview_mode"), "Display", c("Pseudocolor" = "pseudocolor", "Dot plot" = "dot", "Contour" = "contour", "Color by gate" = "gate"), selected = win$mode %||% "pseudocolor"),
        footer = tagList(modalButton("Cancel"), actionButton(session$ns("apply_preview_settings"), "Apply", class = "btn btn-primary")),
        easyClose = TRUE
      ))
      wins[[id]]$editing <- TRUE
      gate_preview_windows(wins)
    }, ignoreInit = TRUE)

    observeEvent(input$apply_preview_settings, {
      wins <- gate_preview_windows()
      ids <- names(wins)[vapply(wins, function(w) isTRUE(w$editing), logical(1))]
      if (length(ids)) {
        wins[[ids[[1]]]]$mode <- input$preview_mode %||% "pseudocolor"
        wins[[ids[[1]]]]$editing <- NULL
        gate_preview_windows(wins)
      }
      removeModal()
    }, ignoreInit = TRUE)

    output$gate_preview_windows <- renderUI({
      wins <- gate_preview_windows()
      if (!length(wins)) return(NULL)
      reg <- shared$fcs$registry()
      sid <- active_sample_id()
      gates <- shared$gating$gates()
      tagList(lapply(seq_along(wins), function(i) {
        win <- wins[[i]]
        gate <- gates[[win$gate_id]]
        e <- reg[[sid]]
        wrap_id <- session$ns(paste0(win$id, "_canvas"))
        app <- flowjo_gate_applicability(e, gate, gates)
        st <- gate_stat_row(sid, win$gate_id)
        div(
          class = paste("gate-preview-window", if (!isTRUE(app$ok)) "unavailable" else ""),
          style = sprintf("top:%dpx; left:%dpx;", 96 + (i - 1L) * 28L, 140 + (i - 1L) * 32L),
          div(class = "gate-preview-title",
            span(gate$name %||% win$gate_id),
            div(class = "gate-preview-actions",
              tags$button(type = "button", class = "btn btn-link btn-xs", onclick = sprintf("Shiny.setInputValue('%s','%s',{priority:'event'});", session$ns("gate_preview_settings"), win$id), icon("gear")),
              tags$button(type = "button", class = "btn btn-link btn-xs", onclick = sprintf("Shiny.setInputValue('%s','%s',{priority:'event'});", session$ns("close_gate_preview"), win$id), icon("xmark"))
            )
          ),
          div(class = "gate-preview-meta",
            sprintf("%s | %s | %s | %s",
              e$name %||% sid,
              if (is.null(st)) "-- events" else paste(format(st$cells, big.mark = ","), "events"),
              if (is.null(st)) "--" else sprintf("%.1f%% parent", st$percent_parent),
              app$status)
          ),
          if (isTRUE(app$ok)) div(id = wrap_id, class = "gate-preview-canvas-wrap",
            tags$canvas(class = "gate-canvas gate-canvas-points"),
            tags$canvas(class = "gate-canvas gate-canvas-overlay")
          ) else div(class = "gate-preview-unavailable", "Gate not available for this sample")
        )
      }))
    })

    observe({
      wins <- gate_preview_windows()
      payloads <- lapply(wins, function(win) {
        gate_preview_payload(win$id, win$gate_id, session$ns(paste0(win$id, "_canvas")))
      })
      session$onFlushed(function() {
        for (payload in payloads) {
          if (!is.null(payload)) session$sendCustomMessage("flowjo-gating-payload", payload)
        }
      }, once = TRUE)
    })

    output$gate_tree <- renderUI({
      gates <- shared$gating$gates()
      groups <- shared$gating$groups()
      if (!length(gates)) return(helpText("No gates yet. Draw a gate and save it."))
      rows <- flatten_gate_tree(gates)
      tags$div(class = "gate-tree gate-tree-flat", lapply(rows, function(row) {
        g <- gates[[row$id]]
        i <- match(row$id, names(gates))
        state <- flowjo_gate_group_state(g)
        group_id <- g$group_id %||% g$sync_group_id
        group <- if (!is.null(group_id)) groups[[group_id]] else NULL
        state_label <- switch(state,
          synced = "synced group",
          `group-owned` = "group copy",
          `sample-modified` = "sample modified",
          sample = "sample local",
          ""
        )
        state_short <- switch(state,
          synced = "sync",
          `group-owned` = "copy",
          `sample-modified` = "mod",
          sample = "local",
          ""
        )
        group_label <- NULL
        if (!is.null(group)) {
          group_label <- sprintf("%s (%d samples)", group$name %||% group$id, length(group$sample_ids %||% character()))
        }
        div(
          class = paste("gate-tree-row", state, if (identical(g$id, shared$gating$active_gate_id())) "active" else ""),
          style = sprintf("--group-color:%s", group$color %||% g$group_color %||% flowjo_gate_color(g, i)),
          title = paste(na.omit(c(g$name, sprintf("%s / %s", g$x_channel, g$y_channel %||% ""), state_label, group_label)), collapse = " · "),
          onclick = sprintf("Shiny.setInputValue('%s','%s',{priority:'event'});", session$ns("select_gate"), g$id),
          div(
            class = "gate-tree-main",
            style = sprintf("padding-left:%dpx", row$depth * 18L),
            span(class = "gate-tree-title", g$name),
            span(class = "gate-tree-type", toupper(substr(g$type %||% "", 1L, 3L))),
            span(class = "gate-node-meta", sprintf("%s/%s", g$x_channel, g$y_channel %||% "")),
            if (nzchar(state_short)) span(class = paste("gate-state-text", state), state_short)
          ),
          div(
            class = "gate-tree-color",
            onclick = "event.stopPropagation();",
            colourpicker::colourInput(
              session$ns(paste0("gate_color_", g$id)),
              label = NULL,
              value = flowjo_gate_color(g, i),
              showColour = "background",
              palette = "square",
              allowTransparent = FALSE
            )
          )
        )
      }))
    })

    observeEvent(input$select_gate, {
      val <- input$select_gate
      gid <- if (identical(val, "root")) NULL else val
      reset_right_axis_override(gid)
      shared$gating$active_gate_id(gid)
      canvas_selected_gate(gid)
      active_plot("right")
    }, ignoreInit = TRUE)

    sync_gate_config <- function(gid, sync_axes = FALSE) {
      gates <- shared$gating$gates()
      gate <- if (is.null(gid)) NULL else gates[[gid]]
      if (is.null(gate)) {
        updateTextInput(session, "gate_name", value = next_gate_name(gates, "root", current_geometry()))
        return()
      }
      updateTextInput(session, "gate_name", value = gate$name)
      updateRadioButtons(session, "gate_type", selected = gate$type %||% "rectangle")
      if (isTRUE(sync_axes)) {
        # Agent note: Selection/name sync must not overwrite active drawing axes; only explicit axis sync may mutate plot axes.
        plot_axes[["right"]] <- list(x = gate$x_channel, y = gate$y_channel)
        axes <- view_axes()
        axes[[view_axis_key("right", gid)]] <- list(x = gate$x_channel, y = gate$y_channel)
        view_axes(axes)
        session$onFlushed(function() {
          updateSelectInput(session, "x_channel", selected = gate$x_channel)
          updateSelectInput(session, "y_channel", selected = gate$y_channel)
        }, once = TRUE)
      }
    }

    observeEvent(shared$gating$active_gate_id(), {
      sync_gate_config(shared$gating$active_gate_id())
    }, ignoreInit = TRUE)

    observeEvent(canvas_selected_gate(), {
      sync_gate_config(canvas_selected_gate())
    }, ignoreInit = TRUE)

    output$count_table <- DT::renderDataTable({
      stats <- active_sample_stats()
      if (!nrow(stats)) {
        e <- active_entry()
        req(e)
        gates <- shared$gating$gates()
        gid <- canvas_selected_gate() %||% shared$gating$active_gate_id()
        if (is.null(gid) || is.null(gates[[gid]])) gid <- if (length(gates)) names(gates)[[1]] else NULL
        gate <- if (is.null(gid)) NULL else gates[[gid]]
        if (is.null(gate)) {
          geom <- current_geometry()
          channels <- current_draft_channels()
          req(geom, channels$x, channels$y)
          gate <- flowjo_make_gate(
            name = trimws(input$gate_name %||% "Current gate"),
            type = input$gate_canvas_draft$type %||% input$gate_type,
            parent_id = current_draft_parent_id(),
            x_channel = channels$x,
            y_channel = channels$y,
            geometry = geom,
            sample_id = e$id
          )
          gid <- "draft"
        }
        gate_df <- e$df
        parent <- active_parent_keep(gate_df, e$channel_map, gates, gate$parent_id)
        keep <- flowjo_gate_membership(gate_df, e$channel_map, gate, parent = parent)
        st <- flowjo_gate_stats(gate_df, keep, parent)
        stats <- data.frame(
          sample_id = e$id,
          sample_name = e$name %||% e$id,
          gate_id = gid,
          gate_name = gate$name,
          gate_type = gate$type,
          gate_depth = 0L,
          gate_x = gate$x_channel %||% "",
          gate_y = gate$y_channel %||% "",
          gate_status = "draft",
          gate_color = flowjo_gate_color(gate, match(gid, names(gates))),
          st,
          stringsAsFactors = FALSE
        )
      }
      selected_gate <- canvas_selected_gate() %||% shared$gating$active_gate_id() %||% ""
      gate_label <- function(i) {
        nm <- htmltools::htmlEscape(stats$gate_name[[i]] %||% stats$gate_id[[i]])
        axes_vals <- c(stats$gate_x[[i]] %||% "", stats$gate_y[[i]] %||% "")
        axes_vals <- axes_vals[nzchar(axes_vals)]
        axes <- htmltools::htmlEscape(paste(axes_vals, collapse = "/"))
        status <- switch(stats$gate_status[[i]] %||% "",
          synced = "sync",
          draft = "draft",
          available = "",
          stats$gate_status[[i]] %||% ""
        )
        gid <- stats$gate_id[[i]]
        color <- stats$gate_color[[i]] %||% "#2563EB"
        input_id <- session$ns(paste0("gate_color_", gid))
        sprintf(
          "<div class='gate-count-tree-cell' style='--gate-depth:%d;--gate-color:%s' title='%s'><input id='%s' class='gate-count-color-input' type='color' value='%s' onchange=\"Shiny.setInputValue(this.id,this.value,{priority:'event'});\" %s><span class='gate-count-tree-name'>%s</span><span class='gate-count-tree-axes'>%s</span>%s</div>",
          as.integer(stats$gate_depth[[i]] %||% 0L),
          htmltools::htmlEscape(color),
          htmltools::htmlEscape(paste(na.omit(c(stats$gate_name[[i]], axes, status)), collapse = " · ")),
          htmltools::htmlEscape(input_id),
          htmltools::htmlEscape(color),
          if (identical(gid, "draft")) "disabled" else "",
          nm,
          axes,
          if (nzchar(status)) sprintf("<span class='gate-count-tree-status'>%s</span>", htmltools::htmlEscape(status)) else ""
        )
      }
      counts <- data.frame(
        Sample = stats$sample_name %||% stats$sample_id,
        Gate = vapply(seq_len(nrow(stats)), gate_label, character(1)),
        Type = stats$gate_type,
        Cells = stats$cells,
        Parent = stats$parent_cells,
        Percent = round(stats$percent_parent, 2),
        gate_id = stats$gate_id,
        sample_id = stats$sample_id,
        selected = ifelse(nzchar(selected_gate) & stats$gate_id == selected_gate, "true", "false"),
        check.names = FALSE
      )
      DT::datatable(
        counts,
        class = "stripe hover compact gate-count-table",
        options = list(
          pageLength = 8,
          lengthMenu = c(8, 15, 25, 50),
          autoWidth = FALSE,
          columnDefs = list(
            list(targets = c(3, 4, 5), className = "dt-body-right dt-head-right"),
            list(targets = c(6, 7, 8), visible = FALSE, searchable = FALSE)
          )
        ),
        callback = DT::JS(sprintf("
          table.on('draw', function() {
            table.rows().every(function() {
              var data = this.data();
              var isSelected = !!data && (data[8] === true || ['true', 'TRUE', '1', 'yes', 'Yes'].indexOf(String(data[8])) >= 0);
              $(this.node()).toggleClass('gate-count-selected selected active', isSelected);
              $('td', this.node()).css({
                backgroundColor: isSelected ? 'rgba(24, 119, 242, 0.14)' : '',
                boxShadow: isSelected ? 'inset 3px 0 0 rgba(24, 119, 242, 0.75)' : ''
              });
            });
          });
          table.on('click', 'tbody input.gate-count-color-input', function(event) {
            event.stopPropagation();
          });
          table.on('dblclick', 'tbody tr', function() {
            var data = table.row(this).data();
            if (!data) return;
            Shiny.setInputValue('%s', {gate_id: data[6], sample_id: data[7], nonce: Math.random()}, {priority: 'event'});
          });
          table.draw(false);
        ", session$ns("count_table_open"))),
        escape = FALSE,
        rownames = FALSE
      )
    })

    observeEvent(input$count_table_open, {
      msg <- input$count_table_open
      gid <- msg$gate_id %||% ""
      sid <- msg$sample_id %||% ""
      gates <- shared$gating$gates()
      reg <- shared$fcs$registry()
      if (!nzchar(gid) || identical(gid, "draft") || is.null(gates[[gid]])) return()
      if (nzchar(sid) && !is.null(reg[[sid]])) {
        shared$fcs$set_active(sid)
        updateSelectInput(session, "sample_id", selected = sid)
      }
      shared$gating$active_gate_id(gid)
      canvas_selected_gate(NULL)
      reset_right_axis_override(gid)
      active_plot("right")
    }, ignoreInit = TRUE)
  })
}
