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
        card_page_card(tagList(span("Preview"), div(class = "gate-preview-tool-slot", uiOutput(ns("gate_toolbar")))),
          div(
            id = ns("preview_card"),
            class = "gate-preview-card",
            tabindex = "0",
            div(class = "gate-preview-head",
              div(class = "gate-preview-path-slot", uiOutput(ns("gate_path_toolbar")))
            ),
            uiOutput(ns("preview_gate_nav")),
            uiOutput(ns("preview_sample_nav")),
            uiOutput(ns("preview_plots"))
          ),
          div(class = "gate-node-meta gate-preview-note", "Canvas preview: click a plot to activate it, use arrows or keyboard arrows to move through gates and samples, double-click a saved gate to enter its population.")
        ),
        card_page_card(tagList(span("Population Counts"), div(class = "gate-count-header-tools")),
          DT::dataTableOutput(ns("count_table")), class = "gate-count-card")
      )
    ),
    uiOutput(ns("gate_preview_windows"))
  )
}

gating_page_server <- function(id, shared) {
  moduleServer(id, function(input, output, session) {
    active_sample_override <- reactiveVal(NULL)
    active_entry <- reactive({
      # Agent note: Workspace navigation updates shared sample + gate atomically; a stale picker input must not reject that gate during the first Gating render.
      sid <- active_sample_override() %||% shared$fcs$active_id() %||% input$sample_id
      reg <- shared$fcs$registry()
      if (is.null(sid) || is.null(reg[[sid]])) return(NULL)
      reg[[sid]]
    })

    observeEvent(input$sample_id, {
      sid <- input$sample_id
      if (!is.null(shared$fcs$registry()[[sid]])) shared$fcs$set_active(sid)
      if (identical(active_sample_override(), sid)) active_sample_override(NULL)
    }, ignoreInit = TRUE)
    navigator_selected_gates <- reactiveVal(character())
    gate_preview_windows <- reactiveVal(list())
    pending_gate_drop <- reactiveVal(NULL)
    canvas_selected_gate <- reactiveVal(NULL)
    active_plot <- reactiveVal("left")
    plot_display_modes <- reactiveValues(left = "pseudocolor", right = "pseudocolor")
    right_axis_template_gate_id <- reactiveVal(NULL)
    plot_axes <- reactiveValues(left = list(x = NULL, y = NULL), right = list(x = NULL, y = NULL))
    displayed_plot_axes <- reactiveValues(left = list(x = NULL, y = NULL), right = list(x = NULL, y = NULL))
    view_axes <- reactiveVal(list())
    skip_next_gate_payload <- reactiveVal(FALSE)
    collapsed_count_gates <- reactiveVal(character())
    count_color_target <- reactiveVal(NULL)
    count_color_picker <- motion_color_picker_server("count_gate_color_picker")

    # Agent note: Population Counts keeps the full gate tree in shared state;
    # this local set only controls which descendants are visible in the table.
    observeEvent(input$count_gate_toggle, {
      msg <- input$count_gate_toggle
      gid <- if (is.list(msg)) as.character(msg$gate_id %||% "") else as.character(msg %||% "")
      if (!length(gid) || !nzchar(gid[[1]])) return()
      gid <- gid[[1]]
      collapsed <- collapsed_count_gates()
      collapsed_count_gates(if (gid %in% collapsed) setdiff(collapsed, gid) else unique(c(collapsed, gid)))
    }, ignoreInit = TRUE)

    observeEvent(input$count_gate_color_open, {
      msg <- input$count_gate_color_open
      gid <- if (is.list(msg)) as.character(msg$gate_id %||% "") else as.character(msg %||% "")
      if (!length(gid) || !nzchar(gid[[1]])) return()
      gid <- gid[[1]]
      gates <- shared$gating$gates()
      gate <- gates[[gid]]
      if (is.null(gate)) return()
      gate_index <- match(gid, names(gates))
      if (is.na(gate_index)) gate_index <- 1L
      count_color_target(gid)
      showModal(modalDialog(
        title = sprintf("Color for %s", gate$name %||% gid),
        motion_color_picker_ui(
          session$ns("count_gate_color_picker"),
          mode = "single",
          initial_hex = flowjo_gate_color(gate, gate_index)
        ),
        footer = tagList(
          modalButton("Cancel"),
          actionButton(session$ns("count_gate_color_apply"), "Apply", class = "btn btn-primary")
        ),
        easyClose = TRUE
      ))
    }, ignoreInit = TRUE)

    observeEvent(input$count_gate_color_apply, {
      gid <- count_color_target()
      payload <- count_color_picker()
      col <- toupper(trimws(as.character(payload$hex %||% "")))
      gates <- shared$gating$gates()
      if (!is.null(gid) && nzchar(gid) && !is.null(gates[[gid]]) && grepl("^#[0-9A-F]{6}$", col)) {
        gates[[gid]]$color <- col
        gates <- mark_gate_modified_or_sync(gates, gid)
        shared$gating$gates(gates)
      }
      count_color_target(NULL)
      removeModal()
    }, ignoreInit = TRUE)

    observeEvent(shared$ui$nav_route_request(), {
      route <- shared$ui$nav_route_request()
      if (!identical(route$tab %||% NULL, "gating_tab")) return()
      gid <- isolate(shared$gating$active_gate_id())
      gates <- isolate(shared$gating$gates())
      if (is.null(gid) || is.null(gates[[gid]])) return()
      entry <- isolate(active_entry())
      next_gid <- if (is.null(entry)) NULL else right_axis_source_gate_id(gates, gid, entry)
      # Agent note: Workspace opens follow the normal two-plot policy: show the
      # requested population on the left with its next applicable gate selected;
      # leaf populations have no next gate and therefore open on the right.
      if (!is.null(next_gid)) {
        shared$gating$active_gate_id(next_gid)
        canvas_selected_gate(next_gid)
        active_plot("left")
      } else {
        canvas_selected_gate(gid)
        active_plot("right")
      }
    }, ignoreInit = TRUE)
    membership_cache <- new.env(parent = emptyenv())
    membership_cache_order <- character()
    preview_full_data_cache <- new.env(parent = emptyenv())
    canvas_point_payload_keys <- new.env(parent = emptyenv())
    preview_cache_tick <- reactiveVal(0L)
    preload_state <- reactiveVal(list(generation = 0L, signature = NULL, queue = list(), total = 0L, done = 0L, failed = 0L, failed_keys = character(), protected_keys = character()))
    gating_preview_ready <- reactiveVal(FALSE)
    foreground_preview_scheduled <- FALSE
    foreground_preview_running <- FALSE
    foreground_preview_overlay_visible <- FALSE
    foreground_preview_ready_token <- NULL
    foreground_preview_generation <- 0L
    preload_progress_handle <- NULL
    label_pos_cache <- new.env(parent = emptyenv())

    axis_debug <- function(...) {
      if (isTRUE(getOption("flowjo.debug.axis", FALSE))) {
        message(sprintf(...))
      }
    }

    plot_event_limit <- function() {
      if (!isTRUE(shared$params$plot_event_limit_enabled())) return(Inf)
      max(1L, as.integer(shared$params$plot_event_limit() %||% 75000L))
    }

    plot_sample <- function(df) {
      lim <- plot_event_limit()
      if (is.finite(lim) && nrow(df) > lim) {
        # Stable thinning keeps an evicted/rebuilt context visually identical.
        idx <- unique(pmax(1L, pmin(nrow(df), round(seq(1, nrow(df), length.out = lim)))))
        df[idx, , drop = FALSE]
      } else df
    }

    gate_cache_ancestry <- function(gates, gate_id) {
      out <- list()
      gid <- gate_id
      while (!is.null(gid) && !identical(gid, "root") && !is.null(gates[[gid]])) {
        gate <- gates[[gid]]
        out[[length(out) + 1L]] <- gate[c("id", "parent_id", "type", "x_channel", "y_channel", "geometry", "applied_sample_id", "sync_group_id", "group_id")]
        gid <- gate$parent_id
      }
      rev(out)
    }

    gate_cache_population_id <- function(gates, gate_id) {
      gate <- gates[[gate_id %||% ""]]
      if (is.null(gate) || is.null(gate$parent_id) || identical(gate$parent_id, "root")) NULL else gate$parent_id
    }

    preview_cache_get <- function(key) {
      if (!isTRUE(shared$params$plot_cache_enabled())) return(NULL)
      item <- preview_full_data_cache[[key]]
      if (is.null(item)) return(NULL)
      item$used <- as.integer(isolate(preview_cache_tick())) + 1L
      preview_cache_tick(item$used)
      preview_full_data_cache[[key]] <- item
      item$value
    }

    preview_cache_set <- function(key, value, protected_keys = character()) {
      if (!isTRUE(shared$params$plot_cache_enabled())) return(value)
      tick <- as.integer(isolate(preview_cache_tick())) + 1L
      preview_cache_tick(tick)
      preview_full_data_cache[[key]] <- list(value = value, used = tick)
      keys <- ls(preview_full_data_cache, all.names = TRUE)
      max_items <- max(1L, as.integer(shared$params$plot_cache_max_contexts() %||% 50L))
      if (length(keys) > max_items) {
        protected_keys <- unique(c(protected_keys, isolate(preload_state())$protected_keys %||% character()))
        candidates <- setdiff(keys, protected_keys)
        remove_n <- min(length(candidates), length(keys) - max_items)
        if (remove_n > 0L) {
          used <- vapply(candidates, function(k) preview_full_data_cache[[k]]$used %||% 0L, integer(1))
          rm(list = candidates[order(used)][seq_len(remove_n)], envir = preview_full_data_cache)
        }
      }
      value
    }

    preview_cache_key <- function(entry, gates, gate_id, x_channel, y_channel, settings, cache_mode = NULL) {
      cache_mode <- cache_mode %||% shared$params$plot_cache_mode()
      digest::digest(list(
        sample = entry$id,
        revision = entry$.cache_revision %||% digest::digest(list(dim(entry$df), names(entry$df), entry$loaded_at %||% NULL, entry$path %||% NULL), algo = "xxhash64"),
        population = gate_cache_ancestry(gates, gate_cache_population_id(gates, gate_id)),
        x = x_channel,
        y = y_channel,
        settings = settings,
        cache_mode = cache_mode,
        limit_enabled = if (identical(cache_mode, "full")) FALSE else isTRUE(shared$params$plot_event_limit_enabled()),
        limit = if (identical(cache_mode, "full")) Inf else plot_event_limit()
      ), algo = "xxhash64")
    }

    preview_transform_settings <- function(entry, x_channel, y_channel) {
      settings <- effective_transform_settings(entry, x_channel, y_channel, list())
      cols <- c(
        x_channel, y_channel,
        flowjo_channel_to_marker(entry$channel_map, x_channel),
        flowjo_channel_to_marker(entry$channel_map, y_channel)
      )
      settings[intersect(names(settings), unique(cols[!is.na(cols) & nzchar(cols)]))]
    }

    preview_data_for_context <- function(entry, gate_id, x_channel, y_channel, gates = shared$gating$gates()) {
      req(entry, x_channel, y_channel)
      x_col <- flowjo_channel_to_marker(entry$channel_map, x_channel)
      y_col <- flowjo_channel_to_marker(entry$channel_map, y_channel)
      req(x_col, y_col)
      settings <- preview_transform_settings(entry, x_channel, y_channel)
      cache_mode <- shared$params$plot_cache_mode()
      key <- preview_cache_key(entry, gates, gate_id, x_channel, y_channel, settings, cache_mode)
      cached <- preview_cache_get(key)
      if (is.null(cached)) {
        df <- flowjo_transform_data(entry$df, settings)
        keep <- is.finite(df[[x_col]]) & is.finite(df[[y_col]])
        cache_population_id <- gate_cache_population_id(gates, gate_id)
        parent <- cached_gate_membership(entry, gates, cache_population_id)
        if (!is.null(parent)) keep <- keep & parent
        row_index <- which(keep)
        cached <- list(df = df[keep, , drop = FALSE], row_index = row_index, x_col = x_col, y_col = y_col, entry = entry, x_channel = x_channel, y_channel = y_channel, full_cache = identical(cache_mode, "full"))
        # Agent note: Cache the shared transformed parent population; sibling geometry is applied below so sibling selection reuses data without showing the wrong population.
        preview_cache_set(key, cached)
      }
      membership <- cached_gate_membership(entry, gates, gate_id)
      out <- cached
      if (!is.null(membership)) {
        keep <- membership[cached$row_index]
        keep[is.na(keep)] <- FALSE
        out$df <- cached$df[keep, , drop = FALSE]
      }
      if (!identical(cache_mode, "full")) out$df <- plot_sample(out$df)
      out
    }

    preview_context_is_cached <- function(entry, gate_id, x_channel, y_channel, gates = shared$gating$gates()) {
      if (!isTRUE(shared$params$plot_cache_enabled())) return(FALSE)
      settings <- preview_transform_settings(entry, x_channel, y_channel)
      key <- preview_cache_key(entry, gates, gate_id, x_channel, y_channel, settings, shared$params$plot_cache_mode())
      !is.null(preview_full_data_cache[[key]])
    }

    preview_context_key <- function(ctx, gates) {
      if (identical(ctx$type %||% "plot", "counts")) {
        return(count_context_key(ctx$entry, gates))
      }
      settings <- preview_transform_settings(ctx$entry, ctx$x, ctx$y)
      preview_cache_key(ctx$entry, gates, ctx$gate_id, ctx$x, ctx$y, settings, shared$params$plot_cache_mode())
    }

    preload_context_is_cached <- function(ctx, gates) {
      if (identical(ctx$type %||% "plot", "counts")) return(count_context_is_cached(ctx$entry, gates))
      preview_context_is_cached(ctx$entry, ctx$gate_id, ctx$x, ctx$y, gates)
    }

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

    displayed_population_gate_id <- function(gates = NULL) {
      if (is.null(gates)) gates <- shared$gating$gates()
      plot_focus_gate_id(active_plot(), gates)
    }

    active_plot_context <- reactive({
      gates <- shared$gating$gates()
      plot <- active_plot()
      gid <- plot_focus_gate_id(plot, gates)
      list(plot = plot, gate_id = gid, gate = gates[[gid %||% ""]])
    })

    view_axis_key <- function(plot, focus_id = NULL) {
      paste(active_sample_id() %||% "sample", plot %||% "plot", focus_id %||% "root", sep = "::")
    }

    reset_right_axis_override <- function(gid) {
      if (is.null(gid)) return()
      right_axis_template_gate_id(NULL)
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
      source <- "view_axes"
      pair <- view_axes()[[view_axis_key(plot, gid)]]
      if (is.null(pair) && identical(plot, "right") && !is.null(gate)) {
        axis_gid <- right_axis_source_gate_id(gates, gid, e) %||% gid
        axis_gate <- gates[[axis_gid]]
        if (!is.null(axis_gate)) {
          pair <- list(x = axis_gate$x_channel, y = axis_gate$y_channel)
          source <- sprintf("right_axis_source:%s", axis_gid)
        }
      }
      displayed_pair <- displayed_plot_axes[[plot]]
      if (is.null(pair) && !is.null(plot_axes[[plot]])) {
        pair <- plot_axes[[plot]]
        source <- "plot_axes"
      }
      if (is.null(pair) && !is.null(displayed_pair)) {
        pair <- displayed_pair
        source <- "displayed_plot_axes"
      }
      if (is.null(pair) && !is.null(gate)) {
        pair <- list(x = gate$x_channel, y = gate$y_channel)
        source <- "gate"
      }
      if ((is.null(pair$x) || is.null(pair$y)) && is.null(gate)) {
        seed_id <- first_applicable_gate_id(gates, names(gates), e)
        seed_gate <- if (!is.null(seed_id)) gates[[seed_id]] else NULL
        if (!is.null(seed_gate)) {
          pair <- list(x = seed_gate$x_channel, y = seed_gate$y_channel)
          source <- sprintf("seed_gate:%s", seed_id)
        }
      }
      defaults <- default_axis_pair(e)
      out <- list(x = pair$x %||% defaults$x, y = pair$y %||% defaults$y)
      if (is.null(pair$x) || is.null(pair$y)) source <- paste(source, "+defaults")
      axis_debug("[gating-axis] pair plot=%s gid=%s source=%s active=%s out=%s/%s displayed=%s/%s plot_axes=%s/%s",
        plot %||% "<null>",
        gid %||% "<root>",
        source,
        isolate(active_plot()) %||% "<null>",
        out$x %||% "<null>",
        out$y %||% "<null>",
        displayed_pair$x %||% "<null>",
        displayed_pair$y %||% "<null>",
        plot_axes[[plot]]$x %||% "<null>",
        plot_axes[[plot]]$y %||% "<null>"
      )
      out
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
      e <- active_entry()
      ids <- names(gates)
      if (!is.null(e)) {
        gid <- first_applicable_gate_id(gates, ids, e)
        if (!is.null(gid)) return(gid)
      }
      if (length(ids)) ids[[1]] else NULL
    }

    synced_nav_gate_id <- function(gates, gate_id) {
      gid <- gate_id
      while (!is.null(gid) && !identical(gid, "root") && !is.null(gates[[gid]])) {
        gate <- gates[[gid]]
        if (!is.null(gate$sync_group_id %||% gate$group_id)) return(gid)
        gid <- gate$parent_id
      }
      gate_id
    }

    sample_lineage_peer_id <- function(gates, gate_id, sample_id) {
      gate <- gates[[gate_id]]
      if (is.null(gate) || is.null(sample_id)) return(NULL)
      lineage_id <- flowjo_gate_lineage_id(gate)
      hits <- names(gates)[vapply(gates, function(candidate) {
        identical(flowjo_gate_lineage_id(candidate), lineage_id) &&
          identical(candidate$applied_sample_id, sample_id)
      }, logical(1))]
      if (!length(hits)) return(NULL)
      group_id <- gate$sync_group_id %||% gate$group_id
      if (!is.null(group_id)) {
        same_group <- hits[vapply(hits, function(id) {
          identical(gates[[id]]$sync_group_id %||% gates[[id]]$group_id, group_id)
        }, logical(1))]
        if (length(same_group)) return(same_group[[1]])
      }
      # Agent note: Unsyncing changes ownership, not population identity; sample navigation must still follow the preserved lineage.
      hits[[1]]
    }

    sample_gate_target_id <- function(gates, gate_id, sample_id) {
      gid <- gate_id
      while (!is.null(gid) && !identical(gid, "root") && !is.null(gates[[gid]])) {
        peer_id <- sample_lineage_peer_id(gates, gid, sample_id)
        if (!is.null(peer_id)) return(peer_id)
        gid <- gates[[gid]]$parent_id
      }
      NULL
    }

    sample_gate_switch_target <- function(gates, gate_id, sample_id) {
      if (is.null(gate_id) || is.null(gates[[gate_id]])) return(NULL)
      exact_id <- sample_lineage_peer_id(gates, gate_id, sample_id)
      if (!is.null(exact_id)) return(list(gate_id = exact_id, template_id = NULL))
      parent_id <- gates[[gate_id]]$parent_id
      parent_peer_id <- sample_gate_target_id(gates, parent_id, sample_id)
      if (is.null(parent_peer_id)) return(NULL)
      source <- gates[[gate_id]]
      child_hits <- flowjo_gate_children(gates, parent_peer_id)
      child_hits <- child_hits[vapply(child_hits, function(id) {
        child <- gates[[id]]
        identical(child$name %||% "", source$name %||% "") &&
          identical(child$type %||% "rectangle", source$type %||% "rectangle") &&
          identical(child$x_channel %||% "", source$x_channel %||% "") &&
          identical(child$y_channel %||% "", source$y_channel %||% "")
      }, logical(1))]
      if (length(child_hits)) return(list(gate_id = child_hits[[1]], template_id = NULL))
      list(gate_id = parent_peer_id, template_id = gate_id)
    }

    navigator_sample_ids <- function() {
      reg_ids <- names(shared$fcs$registry())
      groups <- shared$gating$groups()
      grouped_ids <- unlist(lapply(groups, function(group) group$sample_ids %||% character()), use.names = FALSE)
      grouped_ids <- grouped_ids[grouped_ids %in% reg_ids]
      # Agent note: This is the right-panel visual order and must remain the single source for every sample arrow.
      unique(c(grouped_ids, setdiff(reg_ids, grouped_ids)))
    }

    eligible_navigator_sample_ids <- function(gates, gate_id) {
      ids <- navigator_sample_ids()
      if (is.null(gate_id) || is.null(gates[[gate_id]])) return(character())
      ids[vapply(ids, function(id) !is.null(sample_gate_switch_target(gates, gate_id, id)), logical(1))]
    }

    prev_next_preview_sample <- function(gates, gate_id, sample_id, direction = 1L) {
      visible_ids <- navigator_sample_ids()
      eligible_ids <- eligible_navigator_sample_ids(gates, gate_id)
      if (!length(eligible_ids)) return(NULL)
      current_pos <- match(sample_id, visible_ids)
      eligible_pos <- match(eligible_ids, visible_ids)
      if (is.na(current_pos)) current_pos <- if (direction > 0) 0L else length(visible_ids) + 1L
      candidates <- if (direction > 0) which(eligible_pos > current_pos) else which(eligible_pos < current_pos)
      if (!length(candidates)) return(NULL)
      pick <- if (direction > 0) candidates[[1]] else candidates[[length(candidates)]]
      eligible_ids[[pick]]
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

    sample_membership_cache_key <- function(e) {
      digest::digest(list(
        id = e$id,
        revision = e$.cache_revision %||% list(dim(e$df), names(e$df), e$loaded_at %||% NULL, e$path %||% NULL),
        channel_map = e$channel_map
      ), algo = "xxhash64")
    }

    count_context_key <- function(e, gates) {
      gate_bits <- lapply(gates, function(gate) gate[c("id", "parent_id", "type", "x_channel", "y_channel", "geometry", "applied_sample_id", "sync_group_id", "group_id")])
      paste0("counts:", digest::digest(list(sample_membership_cache_key(e), gate_bits), algo = "xxhash64"))
    }

    count_context_is_cached <- function(e, gates) {
      bucket <- membership_cache[[sample_membership_cache_key(e)]]
      !is.null(bucket) && identical(bucket[[".count_context_key"]], count_context_key(e, gates))
    }

    membership_bucket <- function(e) {
      key <- sample_membership_cache_key(e)
      bucket <- membership_cache[[key]]
      if (is.null(bucket)) {
        bucket <- new.env(parent = emptyenv())
        membership_cache[[key]] <- bucket
      }
      membership_cache_order <<- unique(c(setdiff(membership_cache_order, key), key))
      # Agent note: Keep the selected preload scope resident; otherwise large workspaces
      # evict completed memberships and repeatedly rebuild the same count contexts.
      loaded_samples <- length(isolate(shared$fcs$registry()) %||% list())
      max_samples <- max(2L, loaded_samples, as.integer(isolate(shared$params$plot_cache_max_contexts()) %||% 50L))
      if (length(membership_cache_order) > max_samples) {
        drop <- head(membership_cache_order, length(membership_cache_order) - max_samples)
        rm(list = intersect(drop, ls(membership_cache, all.names = TRUE)), envir = membership_cache)
        membership_cache_order <<- setdiff(membership_cache_order, drop)
      }
      bucket
    }

    cached_gate_membership <- function(e, gates, gate_id) {
      if (is.null(gate_id) || identical(gate_id, "root") || is.null(gates[[gate_id]])) return(NULL)
      gate <- gates[[gate_id]]
      parent_id <- gate$parent_id %||% "root"
      parent <- cached_gate_membership(e, gates, parent_id)
      parent_signature <- if (identical(parent_id, "root")) "root" else attr(parent, "cache_signature") %||% "missing"
      # Agent note: Presentation fields stay out of this signature so renaming or recoloring a gate never recomputes event membership.
      signature <- digest::digest(list(
        parent = parent_signature,
        id = gate$id,
        parent_id = parent_id,
        type = gate$type,
        x_channel = gate$x_channel,
        y_channel = gate$y_channel,
        geometry = gate$geometry,
        applied_sample_id = gate$applied_sample_id,
        sync_group_id = gate$sync_group_id,
        group_id = gate$group_id
      ), algo = "xxhash64")
      bucket <- membership_bucket(e)
      cached <- bucket[[gate_id]]
      if (!is.null(cached) && identical(cached$signature, signature)) return(cached$keep)
      imported_keep <- gate$wsp_memberships[[e$id]] %||% NULL
      keep <- if (!is.null(imported_keep) && length(imported_keep) == nrow(e$df)) {
        # Agent note: FlowJo carries exact event membership per sample; reuse it
        # so the first Gating render does not re-evaluate every imported polygon.
        imported_keep
      } else {
        flowjo_gate_membership(e$df, e$channel_map, gate, parent = parent)
      }
      attr(keep, "cache_signature") <- signature
      bucket[[gate_id]] <- list(signature = signature, keep = keep, stats = flowjo_gate_stats(e$df, keep, parent))
      keep
    }

    cached_gate_stats <- function(e, gates, gate_id) {
      keep <- cached_gate_membership(e, gates, gate_id)
      if (is.null(keep)) return(NULL)
      membership_bucket(e)[[gate_id]]$stats
    }

    active_sample_gate_stats <- function(e = active_entry(), gates = shared$gating$gates()) {
      if (is.null(e) || !length(gates)) return(data.frame())
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
        st <- cached_gate_stats(e, gates, gid)
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
      bucket <- membership_bucket(e)
      bucket[[".count_context_key"]] <- count_context_key(e, gates)
      out
    }

    active_sample_stats <- reactive({
      # Agent note: Counts depend on sample/gate data, not canvas visibility. Tab readiness must not clear or invalidate cached table results.
      active_sample_gate_stats()
    })

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
      context <- active_plot_context()
      name <- context$gate$name %||% "Root"
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
          tags$button(
            id = session$ns("unsync_sample_gate"),
            type = "button",
            class = "gate-sync-toggle is-on action-button",
            role = "switch",
            `aria-checked` = "true",
            span(class = "gate-sync-toggle-copy",
              span(class = "gate-sync-toggle-title", "Gate sync is on"),
              span(class = "gate-sync-toggle-note", "Edits to this gate are shared with the linked group samples.")
            ),
            span(class = "gate-sync-toggle-action",
              span(class = "gate-sync-switch", `aria-hidden` = "true"),
              span(class = "gate-sync-action-text", "Unsync this sample")
            )
          )
        ))
      }
      if (identical(state, "sample-modified")) {
        return(div(class = "gate-sync-controls",
          div(class = "gate-sync-toggle is-off",
            role = "switch",
            `aria-checked` = "false",
            span(class = "gate-sync-toggle-copy",
              span(class = "gate-sync-toggle-title", "Gate sync is off"),
              span(class = "gate-sync-toggle-note", "This sample has its own gate edits now. You can reset or resync it from Workspace.")
            ),
            span(class = "gate-sync-toggle-action",
              span(class = "gate-sync-switch", `aria-hidden` = "true")
            )
          )
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

    axis_control_updating <- reactiveVal(FALSE)

    output$axis_controls <- renderUI({
      e <- active_entry()
      if (is.null(e)) return(NULL)
      axes <- flowjo_gate_axes(e)
      validate(need(length(axes) >= 2, "FCS must contain at least two numeric channels."))
      plot <- active_plot()
      displayed_pair <- displayed_plot_axes[[plot]]
      # Agent note: Controls must describe the rendered canvas; saved view axes can
      # still refer to the parent/root context while the left canvas shows child axes.
      pair <- if (!is.null(displayed_pair$x) && !is.null(displayed_pair$y)) {
        displayed_pair
      } else {
        plot_axis_pair(plot)
      }
      axis_control_updating(TRUE)
      session$onFlushed(function() axis_control_updating(FALSE), once = TRUE)
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
      if (isTRUE(axis_control_updating())) {
        axis_debug("[gating-axis] ignore x_channel=%s while controls updating active=%s", input$x_channel %||% "<null>", active_plot() %||% "<null>")
        return()
      }
      plot <- active_plot()
      pair <- plot_axes[[plot]] %||% plot_axis_pair(plot)
      pair$x <- input$x_channel
      plot_axes[[plot]] <- pair
      axes <- view_axes()
      key <- view_axis_key(plot, plot_focus_gate_id(plot))
      axes[[key]] <- pair
      view_axes(axes)
      axis_debug("[gating-axis] save x_channel=%s plot=%s key=%s pair=%s/%s", input$x_channel %||% "<null>", plot %||% "<null>", key, pair$x %||% "<null>", pair$y %||% "<null>")
    }, ignoreInit = TRUE)

    observeEvent(input$y_channel, {
      if (isTRUE(axis_control_updating())) {
        axis_debug("[gating-axis] ignore y_channel=%s while controls updating active=%s", input$y_channel %||% "<null>", active_plot() %||% "<null>")
        return()
      }
      plot <- active_plot()
      pair <- plot_axes[[plot]] %||% plot_axis_pair(plot)
      pair$y <- input$y_channel
      plot_axes[[plot]] <- pair
      axes <- view_axes()
      key <- view_axis_key(plot, plot_focus_gate_id(plot))
      axes[[key]] <- pair
      view_axes(axes)
      axis_debug("[gating-axis] save y_channel=%s plot=%s key=%s pair=%s/%s", input$y_channel %||% "<null>", plot %||% "<null>", key, pair$x %||% "<null>", pair$y %||% "<null>")
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
        e$wsp_axis_transforms[[channel]] %||% e$wsp_axis_transforms[[col]] %||%
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
      scale_type <- input$axis_scale_type %||% "none"
      w <- suppressWarnings(as.numeric(input$axis_axis_w %||% 1.25))
      p <- suppressWarnings(as.numeric(input$axis_axis_p %||% 5))
      n <- suppressWarnings(as.numeric(input$axis_axis_n %||% 0))
      current <- shared$transforms$settings()[[channel]] %||%
        shared$transforms$settings()[[col]] %||%
        e$wsp_axis_transforms[[channel]] %||% e$wsp_axis_transforms[[col]]
      if (identical(current$engine %||% NULL, "flowjo") && identical(current$type %||% NULL, "linear") && identical(scale_type, "none")) {
        return(current)
      }
      # Agent note: WSP slider values are rounded to the app's friendly step
      # size. Preserve the exact FlowJo transform when the user has not really
      # changed those rounded values; a deliberate slider move creates the
      # existing app-native manual logicle setting.
      if (identical(current$engine %||% NULL, "flowjo") && identical(scale_type, "biex") &&
          all(is.finite(c(w, p, n, current$w, current$p, current$n))) &&
          all(abs(c(w, p, n) - c(current$w, current$p, current$n)) <= 0.03)) {
        return(current)
      }
      flowjo_axis_scale_setting(scale_type, w, p, n, t = flowjo_channel_range(e, channel, col))
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
      # Agent note: These SVGs depict actual gate geometry. Advanced icons stay
      # disabled until drawing, membership, editing and export support agree.
      shapes <- c(
        rectangle = '<rect x="4" y="6" width="24" height="20"/>',
        range = '<path d="M5 8V24 M5 16H27 M27 8V24"/>',
        polygon = '<path d="M5 7L18 4L28 13L23 27L6 24Z"/>',
        ellipse = '<ellipse cx="16" cy="16" rx="13" ry="9"/>',
        quadrant = '<path d="M16 3V29 M3 16H29"/>',
        freehand = '<path d="M4 16C2 8 9 11 11 5S22 9 27 8S27 22 23 23S15 28 9 25S8 19 4 16Z"/>',
        autogate = '<path d="M3 17C2 5 15 2 25 8S31 24 20 28S4 26 3 17Z M8 17C7 9 16 7 22 12S25 22 18 24S8 23 8 17Z"/><circle cx="16" cy="16" r="2"/>',
        spider = '<path d="M16 15L17.5 2 M16 15L17.5 30 M16 15L2 16.5 M16 15L30 16.5"/>',
        curved_quadrant = '<path d="M2 17H15V30 M15 17C15 12 15.3 8 17.3 2 M15 17C21 17 25 16.8 30 15"/>',
        boolean = '<circle cx="12" cy="16" r="9"/><circle cx="21" cy="16" r="9"/><path d="M15 9L18 12 M13 12L19 18 M13 17L17 21"/>'
      )
      names <- c(rectangle = "Rectangle", range = "Range", polygon = "Polygon",
        ellipse = "Ellipse", quadrant = "Quadrants", freehand = "Freehand",
        autogate = "Autogate", spider = "Spider quadrants",
        curved_quadrant = "Curly quadrants", boolean = "Boolean")
      descriptions <- c(rectangle = "Drag a box", range = "Select an X interval",
        polygon = "Click vertices; double-click to finish", ellipse = "Drag an oval",
        quadrant = "Set X/Y thresholds for a quadrant region")
      glyph <- function(value) HTML(sprintf(
        '<svg class="gate-tool-icon" viewBox="0 0 32 32" aria-hidden="true" focusable="false" fill="none" stroke="currentColor" stroke-width="1.5" stroke-linecap="round" stroke-linejoin="round">%s</svg>', shapes[[value]]))
      available <- names(descriptions)
      unavailable <- setdiff(names(shapes), available)
      tags$div(class = "gate-toolbar gate-icon-toolbar",
        radioButtons(session$ns("gate_type"), NULL,
          choiceNames = lapply(available, function(value) tags$span(
            title = paste(names[[value]], descriptions[[value]], sep = " — "),
            glyph(value), tags$span(class = "visually-hidden", names[[value]]))),
          choiceValues = available, selected = isolate(input$gate_type) %||% "rectangle", inline = TRUE),
        lapply(unavailable, function(value) tags$span(class = "gate-tool-unavailable",
          title = paste0(names[[value]], " — unavailable"),
          tags$button(type = "button", disabled = "disabled", `aria-label` = paste0(names[[value]], " — unavailable"), glyph(value))))
      )
    })

    plot_title <- function(plot) {
      e <- active_entry()
      if (is.null(e)) return("")
      gates <- shared$gating$gates()
      if (identical(plot, "right") && is.null(preview_focus_gate_id(gates))) return("New gate")
      gid <- plot_focus_gate_id(plot, gates)
      sample_name <- e$name %||% e$id %||% ""
      path <- gate_path_label(gates, gid)
      population <- if (is.null(gid) || identical(gid, "root")) "Root" else gates[[gid]]$name %||% gid
      tagList(
        span(class = "gate-plot-population", paste(sample_name, population, sep = " | ")),
        span(class = "gate-plot-path", title = path, span(class = "gate-plot-path-text", dir = "ltr", path))
      )
    }

    output$plot_title_left <- renderUI(div(class = "gate-plot-title", plot_title("left")))
    output$plot_title_right <- renderUI(div(class = "gate-plot-title", plot_title("right")))

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
      gid <- displayed_population_gate_id(gates)
      sid <- active_sample_id()
      prev_sid <- prev_next_preview_sample(gates, gid, sid, -1L)
      next_sid <- prev_next_preview_sample(gates, gid, sid, 1L)
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
          if (!isTRUE(blank)) tags$details(class = "gate-plot-export",
            tags$summary("Export"),
            tags$label(tags$input(type = "checkbox", class = "gate-export-axis-text", checked = "checked"), " Axis text"),
            tags$button(type = "button", `data-gate-export` = "svg", "SVG"),
            tags$button(type = "button", `data-gate-export` = "png", "PNG")
          ),
          tags$canvas(class = "gate-canvas gate-canvas-points"),
          tags$canvas(class = "gate-canvas gate-canvas-overlay"),
          tags$div(
            class = "Pinhki-plot-pending",
            role = "status",
            `aria-live` = "polite",
            `aria-hidden` = "true",
            tags$div(class = "Pinhki-plot-pending-points", `aria-hidden` = "true",
              lapply(seq_len(9L), function(i) tags$span(class = "Pinhki-plot-pending-point"))
            ),
            tags$span(class = "Pinhki-plot-pending-label", "Preparing plot\u2026")
          )
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
      wsp_settings <- entry$wsp_axis_transforms %||% list()
      wsp_saved <- wsp_settings[[channel]] %||% wsp_settings[[col]]
      if (!is.null(wsp_saved)) return(wsp_saved)
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
      # Agent note: FlowJo can retain sample-specific gate geometry even when a
      # population path is shared. Use that geometry for the active sample so
      # imported overlays do not silently show the first sample's gate.
      sample_geometry <- gate$wsp_geometry_by_sample[[entry$id %||% ""]] %||% NULL
      gate_for_entry <- gate
      # Agent note: WSP sample entries are converted gate descriptors, not bare
      # geometry. Unwrap them before axis conversion or outlines lose their vertices.
      if (!is.null(sample_geometry)) {
        gate_for_entry$geometry <- sample_geometry$geometry %||% sample_geometry
      }
      display_gate <- flowjo_gate_for_plot_axes(gate_for_entry, x_channel, y_channel)
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

    canvas_auto_range <- function(vals, channel, scale_override = NULL, raw_vals = NULL, setting = NULL) {
      if (!is.null(scale_override)) return(scale_override)
      vals <- vals[is.finite(vals)]
      if (!length(vals)) return(c(0, 1))
      rng <- range(vals)
      if (length(vals)) {
        qs <- stats::quantile(vals, probs = c(0.001, 0.999), na.rm = TRUE, names = FALSE, type = 8)
        if (all(is.finite(qs)) && qs[[2]] > qs[[1]]) rng <- qs
      }
      raw_vals <- raw_vals[is.finite(raw_vals)]
      if (grepl("^(FSC|SSC)", channel %||% "", ignore.case = TRUE)) {
        zero_val <- flowjo_transform_column(0, setting)
        if (is.finite(zero_val) && zero_val < rng[[2]]) rng[[1]] <- zero_val
      }
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
      # Agent note: WSP transform domains precede app autoscale caches; imported plots must retain FlowJo channel limits.
      if (identical(setting$source %||% NULL, "wsp")) {
        wsp_rng <- if (identical(setting$type, "linear")) {
          flowjo_transform_column(c(setting$min_range, setting$max_range), setting)
        } else if (identical(setting$type, "biex") && identical(setting$engine, "flowjo")) {
          c(0, setting$channel_range)
        } else NULL
        if (length(wsp_rng) == 2 && all(is.finite(wsp_rng)) && diff(wsp_rng) > 0) return(wsp_rng)
      }
      auto_scales <- shared$transforms$auto_scales()
      auto_rng <- auto_scales[[col]]
      if (!is.null(auto_rng) && length(auto_rng) == 2 && all(is.finite(auto_rng))) {
        return(auto_rng)
      }
      # Agent note: The channel viewport is established from event data once. Gate geometry may extend beyond it and must never expand it or trigger another point-layer refresh.
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
      preview_data_for_context(e, shared$gating$active_gate_id(), input$x_channel, input$y_channel)
    })

    preview_data <- reactive({
      pd <- preview_full_data()
      if (isTRUE(pd$full_cache)) pd$df <- plot_sample(pd$df)
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
      if (identical(setting$type, "linear")) return(flowjo_inverse_transform_column(at, setting))
      if (identical(setting$type, "biex")) {
        if (identical(setting$engine %||% NULL, "flowjo")) {
          exact <- flowjo_inverse_transform_column(at, setting)
          if (!is.null(exact)) return(exact)
        }
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

    axis_canvas_tick_text <- function(x, linear = FALSE, exponent = 0L) {
      superscript <- function(value) {
        chars <- strsplit(as.character(value), "", fixed = TRUE)[[1]]
        map <- c("-" = "\u207b", "0" = "\u2070", "1" = "\u00b9", "2" = "\u00b2", "3" = "\u00b3", "4" = "\u2074", "5" = "\u2075", "6" = "\u2076", "7" = "\u2077", "8" = "\u2078", "9" = "\u2079")
        paste0(vapply(chars, function(ch) unname(map[[ch]] %||% ch), character(1)), collapse = "")
      }
      vapply(x, function(value) {
        if (!is.finite(value)) return("")
        if (abs(value) < 1e-9) return("0")
        if (isTRUE(linear)) return(format(signif(value / 10 ^ exponent, 12), scientific = FALSE, trim = TRUE, big.mark = ","))
        magnitude <- abs(value)
        power <- round(log10(magnitude))
        if (abs(magnitude - 10 ^ power) <= magnitude * 1e-8) {
          text <- if (power == 0L) "1" else if (power == 1L) "10" else paste0("10", superscript(power))
          return(paste0(if (value < 0) "\u2212" else "", text))
        }
        ""
      }, character(1))
    }

    safe_axis_tick_transform <- function(vals, setting) {
      tryCatch(
        flowjo_transform_column(vals, setting),
        error = function(e) {
          axis_debug("[gating-axis] tick transform failed type=%s message=%s", setting$type %||% "<none>", conditionMessage(e))
          rep(NA_real_, length(vals))
        }
      )
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
      # Agent note: Linear WSP transforms need additive raw-value ticks, not logarithmic decades.
      if (identical(setting$type, "linear") && length(raw_vals)) {
        raw_major <- pretty(range(raw_vals), n = 5)
        step <- if (length(raw_major) > 1) diff(raw_major)[[1]] / 5 else 1
        raw_minor <- seq(min(raw_major), max(raw_major), by = step)
        at <- safe_axis_tick_transform(raw_major, setting)
        minor <- safe_axis_tick_transform(raw_minor, setting)
        keep <- is.finite(at) & at >= min(plot_vals) & at <= max(plot_vals)
        minor <- minor[is.finite(minor) & minor >= min(plot_vals) & minor <= max(plot_vals)]
        return(list(at = at[keep], raw = raw_major[keep], minor = setdiff(minor, at[keep])))
      }
      if (!length(raw_vals)) {
        at <- pretty(plot_vals, n = 5)
        return(list(at = at, raw = axis_hist_raw_values(at, setting), minor = pretty(plot_vals, n = 20)))
      }
      pos <- raw_vals[raw_vals > 0]
      neg <- raw_vals[raw_vals < 0]
      raw_ticks <- numeric()
      if (length(pos)) {
        lo <- if (identical(setting$type %||% NULL, "biex") && max(pos) >= 10000) 3 else floor(log10(min(pos)))
        raw_ticks <- 10 ^ seq(lo, ceiling(log10(max(pos))))
      }
      if (length(neg)) {
        neg_abs <- abs(neg)
        raw_ticks <- c(-(10 ^ seq(floor(log10(min(neg_abs))), ceiling(log10(max(neg_abs))))), raw_ticks)
      }
      zero_at <- safe_axis_tick_transform(0, setting)
      keep_zero <- length(zero_at) && is.finite(zero_at) && zero_at >= min(plot_vals) && zero_at <= max(plot_vals)
      if (keep_zero) raw_ticks <- c(raw_ticks, 0)
      raw_ticks <- sort(unique(raw_ticks[(raw_ticks >= min(raw_vals) | (keep_zero & raw_ticks == 0)) & raw_ticks <= max(raw_vals)]))
      raw_minor <- numeric()
      if (length(pos)) {
        # Agent note: Canvas supplies only range endpoints; when crossing zero, the positive endpoint is not the lowest visible positive decade.
        lower <- if (min(raw_vals) <= 0) min(2, floor(log10(max(pos)))) else floor(log10(min(pos)))
        decades <- seq(lower, ceiling(log10(max(pos))))
        raw_minor <- sort(unique(as.vector(outer(2:9, 10 ^ decades))))
      }
      if (length(neg)) {
        lower <- if (max(raw_vals) >= 0) min(2, floor(log10(max(abs(neg))))) else floor(log10(min(abs(neg))))
        neg_decades <- seq(lower, ceiling(log10(max(abs(neg)))))
        raw_minor <- c(-sort(unique(as.vector(outer(2:9, 10 ^ neg_decades)))), raw_minor)
      }
      at <- safe_axis_tick_transform(raw_ticks, setting)
      minor <- safe_axis_tick_transform(raw_minor, setting)
      keep <- is.finite(at) & at >= min(plot_vals) & at <= max(plot_vals)
      # Agent note: A zoomed transformed range may contain no decade. Keep its
      # raw 2–9 decade subdivisions; never substitute linear display-space ticks.
      minor <- minor[is.finite(minor) & minor >= min(plot_vals) & minor <= max(plot_vals)]
      if (length(minor) < 2L) {
        # Tight zooms need finer raw-value subdivisions, transformed by the same axis.
        fine_raw <- pretty(range(raw_vals), n = 10)
        fine_at <- safe_axis_tick_transform(fine_raw, setting)
        minor <- fine_at[is.finite(fine_at) & fine_at >= min(plot_vals) & fine_at <= max(plot_vals)]
      }
      at <- at[keep]; raw_ticks <- raw_ticks[keep]
      if (length(at) > 6) {
        idx <- unique(round(seq(1, length(at), length.out = 6)))
        at <- at[idx]; raw_ticks <- raw_ticks[idx]
      }
      list(at = at, raw = raw_ticks, minor = setdiff(minor, at))
    }

    canvas_axis_ticks <- function(plot_range, setting) {
      plot_range <- sort(as.numeric(plot_range))
      raw_range <- axis_hist_raw_values(plot_range, setting)
      # Agent note: Tick positions use the exact transform; labels always represent
      # raw values. Linear axes share one multiplier; other axes label decades only.
      ticks <- axis_hist_ticks(raw_range, plot_range, setting)
      linear <- is.null(setting$type) || identical(setting$type, "linear")
      largest <- max(c(0, abs(ticks$raw)), na.rm = TRUE)
      exponent <- if (linear && largest >= 10000) as.integer(3 * floor(log10(largest) / 3)) else 0L
      list(
        major = unname(ticks$at),
        labels = unname(axis_canvas_tick_text(ticks$raw, linear = linear, exponent = exponent)),
        multiplier = if (exponent > 0L) paste0("×", axis_canvas_tick_text(10 ^ exponent)) else "",
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
      setting <- modal_axis_setting(e, channel, col)
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
      if (is.list(gates[[gid]]$wsp_geometry_by_sample) && nzchar(e$id %||% "")) {
        # Agent note: Imported WSP geometry can differ by sample. Once the
        # active sample is edited, keep that sample-local override so the next
        # redraw does not restore FlowJo's original coordinates.
        gates[[gid]]$wsp_geometry_by_sample[[e$id]] <- geom
      }
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
      e <- active_entry()
      if (is.list(gates[[gid]]$wsp_geometry_by_sample) && !is.null(e) && nzchar(e$id %||% "")) {
        # Agent note: Keep drag edits in the same per-sample slot used by WSP
        # overlays; otherwise the imported geometry would snap back on redraw.
        gates[[gid]]$wsp_geometry_by_sample[[e$id]] <- gates[[gid]]$geometry
      }
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
      axis_debug("[gating-axis] preview_data plot=%s gate=%s axis_gate=%s pair=%s/%s",
        plot_name %||% "<null>",
        gate_id %||% "<root>",
        axis_gate_id %||% "<none>",
        pair$x %||% "<null>",
        pair$y %||% "<null>"
      )
      pd <- preview_data_for_context(e, gate_id, pair$x, pair$y, gates)
      if (isTRUE(pd$full_cache)) pd$df <- plot_sample(pd$df)
      pd
    }

    selectable_gate_ranges <- function(gate_list, x_rng, y_rng) {
      # Agent note: Preserve gate geometry and saved zoom. Extend only to the nearest
      # finite handle when no handle is visible; apply after saved channel ranges.
      for (gate in gate_list) {
        geom <- gate$geometry %||% list()
        points <- switch(gate$type,
          polygon = canvas_points_df(geom$vertices),
          rectangle = expand.grid(x = c(geom$xmin, geom$xmax), y = c(geom$ymin, geom$ymax)),
          ellipse = data.frame(x = c(geom$xmin, geom$xmax, mean(c(geom$xmin, geom$xmax)), mean(c(geom$xmin, geom$xmax))),
            y = c(mean(c(geom$ymin, geom$ymax)), mean(c(geom$ymin, geom$ymax)), geom$ymin, geom$ymax)),
          range = data.frame(x = c(geom$xmin, geom$xmax), y = mean(y_rng)),
          quadrant = data.frame(x = geom$x, y = geom$y),
          curved_quadrant = data.frame(x = geom$x, y = geom$y),
          data.frame(x = numeric(), y = numeric()))
        if (!nrow(points)) next
        points <- points[is.finite(points$x) & is.finite(points$y), , drop = FALSE]
        if (!nrow(points)) next
        inside <- points$x >= x_rng[1] & points$x <= x_rng[2] & points$y >= y_rng[1] & points$y <= y_rng[2]
        if (any(inside)) next
        dx <- pmax(x_rng[1] - points$x, 0, points$x - x_rng[2]) / diff(x_rng)
        dy <- pmax(y_rng[1] - points$y, 0, points$y - y_rng[2]) / diff(y_rng)
        nearest <- points[which.min(dx ^ 2 + dy ^ 2), , drop = FALSE]
        extend <- function(rng, value) {
          padding <- diff(rng) * 0.015
          if (value < rng[1]) rng[1] <- value - padding
          if (value > rng[2]) rng[2] <- value + padding
          rng
        }
        x_rng <- extend(x_rng, nearest$x)
        y_rng <- extend(y_rng, nearest$y)
      }
      list(x = x_rng, y = y_rng)
    }

    canvas_payload <- function(wrap_id, plot_name, focus_gate_id, editable = FALSE, selected_gate_id = NULL, axis_gate_id = NULL) {
      # Agent note: Display mode belongs to each canvas; reading the shared dropdown here makes an inactive plot change on its next refresh.
      mode <- isolate(plot_display_modes[[plot_name]]) %||% "pseudocolor"
      pd <- preview_data_for_gate(focus_gate_id, plot_name, axis_gate_id)
      if (plot_name %in% c("left", "right")) {
        displayed_plot_axes[[plot_name]] <- list(x = pd$x_channel, y = pd$y_channel)
      }
      axis_debug("[gating-axis] payload plot=%s focus=%s selected=%s axis_gate=%s final=%s/%s editable=%s",
        plot_name %||% "<null>",
        focus_gate_id %||% "<root>",
        selected_gate_id %||% "<none>",
        axis_gate_id %||% "<none>",
        pd$x_channel %||% "<null>",
        pd$y_channel %||% "<null>",
        isTRUE(editable)
      )
      gates <- shared$gating$gates()
      # Agent note: Canvas payload must be sample-scoped; synced copies from other samples share axes/parents and otherwise stack labels on one plot.
      plot_gates <- gates[vapply(gates, function(g) isTRUE(flowjo_gate_applicability(pd$entry, g, gates)$ok), logical(1))]
      stats <- active_sample_stats()
      plot_gate_list <- function() {
        parent_id <- focus_gate_id %||% "root"
        # Agent note: Show one population layer; deeper branches can reuse these channels but gate different event subsets.
        context_ids <- flowjo_gate_children(plot_gates, parent_id)
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
      x_rng <- canvas_auto_range(pd$df[[pd$x_col]], pd$x_channel, NULL, numeric(), x_setting)
      y_rng <- canvas_auto_range(pd$df[[pd$y_col]], pd$y_channel, NULL, numeric(), y_setting)
      x_rng <- channel_canvas_range(pd$x_channel, pd$x_col, x_rng, x_setting)
      y_rng <- channel_canvas_range(pd$y_channel, pd$y_col, y_rng, y_setting)
      if (!all(is.finite(x_rng)) || x_rng[1] == x_rng[2]) x_rng <- c(0, 1)
      if (!all(is.finite(y_rng)) || y_rng[1] == y_rng[2]) y_rng <- c(0, 1)
      visible_ranges <- selectable_gate_ranges(gate_list, x_rng, y_rng)
      x_rng <- visible_ranges$x
      y_rng <- visible_ranges$y
      x_ticks <- canvas_axis_ticks(x_rng, x_setting)
      y_ticks <- canvas_axis_ticks(y_rng, y_setting)
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
      if (is.null(gid) || is.null(gates[[gid]])) return(NULL)
      e <- active_entry()
      if (!is.null(e) && !isTRUE(flowjo_gate_applicability(e, gates[[gid]], gates)$ok)) return(NULL)
      gid
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

    compact_canvas_payload <- function(payload) {
      id <- payload$id %||% ""
      point_fields <- c("blank", "plot", "parentId", "mode", "channels", "ranges", "labels", "ticks", "x", "y", "marker", "group", "groupColors")
      key <- digest::digest(payload[intersect(point_fields, names(payload))], algo = "xxhash64")
      if (nzchar(id) && identical(canvas_point_payload_keys[[id]], key)) {
        payload$pointDataUnchanged <- TRUE
        payload[c("x", "y", "marker", "group", "groupColors")] <- NULL
      } else if (nzchar(id)) {
        canvas_point_payload_keys[[id]] <- key
      }
      payload
    }

    preview_payload_messages <- function(gates, active_id, active_plot_value = isolate(active_plot())) {
      if (is.null(active_id)) return(list(active_plot_message(active_plot_value)))
      left_id <- flowjo_gate_parent_id(gates, active_id)
      left_axis_gate_id <- active_id
      right_axis_gate_id <- right_axis_template_gate_id() %||% right_axis_source_gate_id(gates, active_id, active_entry()) %||% active_id
      selected_id <- isolate(canvas_selected_gate()) %||% active_id
      left_selected_id <- if (identical(active_plot_value, "right")) active_id else selected_id
      msgs <- list(list(
        name = "flowjo-gating-payload",
        # Agent note: Parent context keeps sibling gates visible; only the selected/current gate should receive the active overlay.
        payload = compact_canvas_payload(
          canvas_payload("canvas_wrap_left", "left", left_id, identical(active_plot_value, "left"), left_selected_id, left_axis_gate_id)
        )
      ))
      if (!is.null(active_id)) {
        # Agent note: Landing/right preview must send child-gate axes for the active sample; persisting displayed axes here can freeze old parent/default axes.
        right_payload <- canvas_payload("canvas_wrap_right", "right", active_id, identical(active_plot_value, "right"), selected_id, right_axis_gate_id)
        right_payload <- compact_canvas_payload(right_payload)
        if (isTRUE(getOption("flowjo.debug.axis", FALSE))) {
          active_gate <- gates[[active_id]]
          axis_gate <- gates[[right_axis_gate_id]]
          message(sprintf(
            "[gating-axis] active=%s active_axes=%s/%s right_axis_gate=%s right_axis_axes=%s/%s payload_axes=%s/%s",
            active_id %||% "<null>",
            active_gate$x_channel %||% "<null>",
            active_gate$y_channel %||% "<null>",
            right_axis_gate_id %||% "<null>",
            axis_gate$x_channel %||% "<null>",
            axis_gate$y_channel %||% "<null>",
            right_payload$xChannel %||% "<null>",
            right_payload$yChannel %||% "<null>"
          ))
        }
        msgs <- c(msgs, list(list(
          name = "flowjo-gating-payload",
          payload = right_payload
        )))
      }
      c(msgs, list(active_plot_message(active_plot_value)))
    }

    active_plot_payload_message <- function(plot_name, gates = shared$gating$gates(), active_id = ensure_active_preview_gate(gates)) {
      if (is.null(active_id) || !plot_name %in% c("left", "right")) return(NULL)
      selected_id <- isolate(canvas_selected_gate()) %||% active_id
      if (identical(plot_name, "left")) {
        payload <- canvas_payload(
          "canvas_wrap_left", "left", flowjo_gate_parent_id(gates, active_id), TRUE,
          selected_id, active_id
        )
      } else {
        axis_gate_id <- right_axis_template_gate_id() %||% right_axis_source_gate_id(gates, active_id, active_entry()) %||% active_id
        payload <- canvas_payload("canvas_wrap_right", "right", active_id, TRUE, selected_id, axis_gate_id)
      }
      list(name = "flowjo-gating-payload", payload = compact_canvas_payload(payload))
    }

    observeEvent(input$display_mode, {
      plot_name <- isolate(active_plot())
      mode <- input$display_mode %||% "pseudocolor"
      if (!plot_name %in% c("left", "right") || identical(plot_display_modes[[plot_name]], mode)) return()
      plot_display_modes[[plot_name]] <- mode
      msg <- active_plot_payload_message(plot_name)
      if (!is.null(msg)) send_preview_messages(list(msg))
    }, ignoreInit = TRUE)

    observeEvent(active_plot(), {
      plot_name <- active_plot()
      mode <- isolate(plot_display_modes[[plot_name]]) %||% "pseudocolor"
      if (identical(isolate(input$display_mode), mode)) return()
      freezeReactiveValue(input, "display_mode")
      updateSelectInput(session, "display_mode", selected = mode)
    }, ignoreInit = TRUE)

    preload_sample_ids <- function(gates, active_id) {
      reg <- shared$fcs$registry()
      ids <- names(reg)
      scope <- shared$params$plot_preload_scope()
      if (identical(scope, "current")) return(active_sample_id() %||% character())
      if (identical(scope, "grouped")) {
        nav_id <- synced_nav_gate_id(gates, active_id)
        gate <- gates[[nav_id]]
        group_id <- gate$sync_group_id %||% gate$group_id
        ids <- if (!is.null(group_id)) gate$sync_sample_ids %||% character() else character()
        if (!length(ids) && !is.null(group_id)) {
          ids <- unique(unlist(lapply(gates, function(g) {
            if (identical(g$sync_group_id %||% g$group_id, group_id)) g$applied_sample_id else NULL
          }), use.names = FALSE))
        }
      }
      ids <- intersect(ids, names(reg))
      unique(c(active_sample_id(), ids))
    }

    preload_contexts <- function(gates, active_id) {
      if (!isTRUE(shared$params$plot_cache_enabled()) || !isTRUE(shared$params$plot_preload_enabled()) || is.null(active_id)) return(list())
      reg <- shared$fcs$registry()
      ids <- preload_sample_ids(gates, active_id)
      left_id <- flowjo_gate_parent_id(gates, active_id)
      left_axis <- gates[[active_id]]
      right_axis_id <- right_axis_template_gate_id() %||% right_axis_source_gate_id(gates, active_id, active_entry()) %||% active_id
      right_axis <- gates[[right_axis_id]]
      out <- list()
      for (sid in ids) {
        entry <- reg[[sid]]
        if (is.null(entry)) next
        target <- if (identical(sid, active_sample_id())) list(gate_id = active_id) else sample_gate_switch_target(gates, active_id, sid)
        target_id <- target$gate_id %||% active_id
        target_left_id <- flowjo_gate_parent_id(gates, target_id)
        if (!is.null(left_axis)) out[[length(out) + 1L]] <- list(type = "plot", entry = entry, gate_id = target_left_id, x = left_axis$x_channel, y = left_axis$y_channel)
        if (!is.null(right_axis)) out[[length(out) + 1L]] <- list(type = "plot", entry = entry, gate_id = target_id, x = right_axis$x_channel, y = right_axis$y_channel)
        out[[length(out) + 1L]] <- list(type = "counts", entry = entry, gate_id = target_id)
      }
      # Agent note: Warm the visible layer across samples first, then every sample's
      # own gate layers. Never evaluate another sample's gate as a fallback layer.
      for (sid in ids) {
        entry <- reg[[sid]]
        if (is.null(entry)) next
        for (gid in names(gates)) {
          gate <- gates[[gid]]
          if (!isTRUE(flowjo_gate_applicability(entry, gate, gates)$ok)) next
          if (is.null(gate$x_channel) || is.null(gate$y_channel)) next
          out[[length(out) + 1L]] <- list(type = "plot", entry = entry,
            gate_id = flowjo_gate_parent_id(gates, gid), x = gate$x_channel, y = gate$y_channel)
          axis_id <- right_axis_source_gate_id(gates, gid, entry) %||% gid
          axis <- gates[[axis_id]]
          if (!is.null(axis$x_channel) && !is.null(axis$y_channel)) {
            out[[length(out) + 1L]] <- list(type = "plot", entry = entry,
              gate_id = gid, x = axis$x_channel, y = axis$y_channel)
          }
        }
      }
      if (!length(out)) return(out)
      keys <- vapply(out, preview_context_key, character(1), gates = gates)
      out[!duplicated(keys)]
    }

    preload_signature <- function(contexts, gates) {
      digest::digest(list(
        contexts = sort(vapply(contexts, preview_context_key, character(1), gates = gates)),
        scope = shared$params$plot_preload_scope()
      ), algo = "xxhash64")
    }

    preload_label <- function(done, total, final = FALSE) {
      scope <- isolate(shared$params$plot_preload_scope())
      if (isTRUE(final)) return("Gating plots and counts are ready")
      current <- max(1L, min(as.integer(done) + 1L, as.integer(total)))
      if (identical(scope, "grouped")) return(sprintf("Preparing grouped gating data %d of %d", current, total))
      if (identical(scope, "current")) return(sprintf("Preparing current-sample gating data %d of %d", current, total))
      sprintf("Preparing gating data %d of %d across loaded samples", current, total)
    }

    close_preload_progress <- function() {
      if (!is.null(preload_progress_handle)) {
        try(preload_progress_handle$close(), silent = TRUE)
        preload_progress_handle <<- NULL
      }
    }

    session$onSessionEnded(close_preload_progress)

    show_preload_notification <- function(done, total) {
      if (!isTRUE(gating_tab_active())) {
        close_preload_progress()
        return(invisible(FALSE))
      }
      if (is.null(preload_progress_handle)) preload_progress_handle <<- shiny::Progress$new(session, min = 0, max = 1, style = "notification")
      preload_progress_handle$set(
        value = min(1, max(0, as.numeric(done %||% 0L) / max(1, as.numeric(total %||% 1L)))),
        message = "Preparing gating data in the background",
        detail = preload_label(done, total)
      )
      invisible(TRUE)
    }

    refresh_preload_queue <- function(gates, active_id, notify_ready = FALSE) {
      enabled <- isTRUE(shared$params$plot_cache_enabled()) && isTRUE(shared$params$plot_preload_enabled())
      if (!enabled || is.null(active_id)) {
        old <- preload_state()
        preload_state(modifyList(old, list(generation = as.integer(old$generation %||% 0L) + 1L, signature = NULL, queue = list(), total = 0L, done = 0L, failed = 0L, failed_keys = character(), protected_keys = character())))
        close_preload_progress()
        return(0L)
      }
      contexts <- preload_contexts(gates, active_id)
      signature <- preload_signature(contexts, gates)
      context_keys <- vapply(contexts, preview_context_key, character(1), gates = gates)
      old <- preload_state()
      failed_keys <- if (identical(old$signature, signature)) old$failed_keys %||% character() else character()
      missing_idx <- !vapply(contexts, preload_context_is_cached, logical(1), gates = gates) & !context_keys %in% failed_keys
      missing <- contexts[missing_idx]
      if (identical(old$signature, signature)) {
        old$queue <- missing
        old$protected_keys <- context_keys
        old$total <- max(as.integer(old$total %||% 0L), as.integer(old$done %||% 0L) + length(missing))
        preload_state(old)
      } else {
        close_preload_progress()
        preload_state(list(generation = as.integer(old$generation %||% 0L) + 1L, signature = signature, queue = missing, total = length(missing), done = 0L, failed = 0L, failed_keys = character(), protected_keys = context_keys))
      }
      if (isTRUE(notify_ready) && length(contexts) && !length(missing)) {
        close_preload_progress()
      }
      length(missing)
    }

    request_preload_queue <- function(gates, active_id, announce = FALSE) {
      missing <- refresh_preload_queue(gates, active_id, notify_ready = announce)
      if (missing > 0L) {
        st <- preload_state()
        show_preload_notification(st$done, st$total)
      }
      invisible(missing)
    }

    send_preview_messages <- function(msgs) {
      app_trace("send preview messages count=", length(msgs), .prefix = "[gating-ready]")
      # Agent note: Only the current left-canvas draw may release the foreground overlay.
      # Server payload completion alone does not mean the browser has drawn the plot.
      if (isTRUE(foreground_preview_overlay_visible)) {
        foreground_preview_generation <<- foreground_preview_generation + 1L
        foreground_preview_ready_token <<- as.character(foreground_preview_generation)
      }
      for (msg in msgs) {
        if (isTRUE(foreground_preview_overlay_visible) &&
            identical(msg$payload$id, session$ns("canvas_wrap_left"))) {
          msg$payload$readyInput <- session$ns("gate_canvas_ready")
          msg$payload$readyToken <- foreground_preview_ready_token
        }
        session$sendCustomMessage(msg$name, msg$payload)
      }
    }

    active_foreground_contexts <- function(gates, active_id) {
      entry <- active_entry()
      if (is.null(entry) || is.null(active_id)) return(list())
      left_id <- flowjo_gate_parent_id(gates, active_id)
      left_axis <- gates[[active_id]]
      right_axis_id <- right_axis_template_gate_id() %||% right_axis_source_gate_id(gates, active_id, entry) %||% active_id
      right_axis <- gates[[right_axis_id]]
      out <- list(list(type = "counts", entry = entry, gate_id = active_id))
      if (!is.null(left_axis)) out[[length(out) + 1L]] <- list(type = "plot", entry = entry, gate_id = left_id, x = left_axis$x_channel, y = left_axis$y_channel)
      if (!is.null(right_axis)) out[[length(out) + 1L]] <- list(type = "plot", entry = entry, gate_id = active_id, x = right_axis$x_channel, y = right_axis$y_channel)
      out
    }

    prepare_active_preview <- function(gates, active_id) {
      active_sample_gate_stats(active_entry(), gates)
      gating_preview_ready(TRUE)
      preview_payload_messages(gates, active_id)
    }

    gating_tab_active <- function() {
      identical(shiny::isolate(shared$session$input$main_nav) %||% NULL, "gating_tab")
    }

    hide_foreground_preview_overlay <- function(wait_for_canvas = FALSE) {
      if (isTRUE(foreground_preview_overlay_visible)) {
        if (isTRUE(wait_for_canvas) && !is.null(foreground_preview_ready_token)) return(invisible(NULL))
        overlay_hide(session = session)
        foreground_preview_ready_token <<- NULL
        foreground_preview_overlay_visible <<- FALSE
      }
      invisible(NULL)
    }

    observeEvent(input$gate_canvas_ready, {
      token <- input$gate_canvas_ready$token
      if (!is.null(foreground_preview_ready_token) &&
          identical(token, foreground_preview_ready_token)) hide_foreground_preview_overlay()
    }, ignoreInit = TRUE)

    schedule_foreground_preview <- function(gates, active_id) {
      if (!isTRUE(gating_tab_active())) return(invisible(FALSE))
      contexts <- active_foreground_contexts(gates, active_id)
      missing <- length(contexts) && any(!vapply(contexts, preload_context_is_cached, logical(1), gates = gates))
      if (length(contexts) && (!isTRUE(shiny::isolate(gating_preview_ready())) || isTRUE(missing))) {
        gating_preview_ready(FALSE)
        foreground_preview_ready_token <<- NULL
        if (!isTRUE(foreground_preview_overlay_visible)) {
          foreground_preview_overlay_visible <<- TRUE
          overlay_show(
            "Preparing gating page...",
            tip = "Preparing the active sample's plots and population counts.",
            progress = "indeterminate",
            session = session
          )
        }
      }
      if (isTRUE(foreground_preview_scheduled) || isTRUE(foreground_preview_running)) return(invisible(TRUE))
      foreground_preview_scheduled <<- TRUE
      session$sendCustomMessage("flowjo-gating-pending", list(pending = TRUE))
      # Agent note: The overlay must reach the browser before canvas payload construction; deferring the heavy work by one event-loop turn prevents a blank, unprotected gating page.
      session$onFlushed(function() {
        later::later(function() {
          # Agent note: Deferred preview work needs isolation as well as a session
          # domain; its readiness checks, payloads, and preload queue read reactive state.
          shiny::withReactiveDomain(session, shiny::isolate({
            foreground_preview_scheduled <<- FALSE
            if (!isTRUE(gating_tab_active())) {
              gating_preview_ready(FALSE)
              hide_foreground_preview_overlay()
              return(invisible(NULL))
            }
            foreground_preview_running <<- TRUE
            tryCatch({
              current_gates <- shared$gating$gates()
              current_id <- ensure_active_preview_gate(current_gates)
              msgs <- prepare_active_preview(current_gates, current_id)
              request_preload_queue(current_gates, current_id)
              send_preview_messages(msgs)
              st <- preload_state()
              if (length(st$queue) && isTRUE(gating_tab_active())) {
                show_preload_notification(st$done, st$total)
              }
            }, error = function(e) {
              foreground_preview_ready_token <<- NULL
              gating_preview_ready(FALSE)
              session$sendCustomMessage("flowjo-gating-pending", list(pending = FALSE))
              message("[gating-ready] foreground preview failed: ", conditionMessage(e))
              showNotification(
                paste("Gating preview could not be prepared:", conditionMessage(e)),
                type = "error",
                duration = 10
              )
            }, finally = {
              foreground_preview_running <<- FALSE
              hide_foreground_preview_overlay(wait_for_canvas = TRUE)
            })
            invisible(NULL)
          }))
        }, delay = 0)
      }, once = TRUE)
      invisible(TRUE)
    }

    observeEvent(shared$session$input$main_nav, {
      if (!isTRUE(gating_tab_active())) {
        close_preload_progress()
        gating_preview_ready(FALSE)
        hide_foreground_preview_overlay()
      }
    }, ignoreInit = FALSE)

    observe({
      # Agent note: Track navigation here; the isolated helper cannot wake preparation after leaving clears readiness.
      if (!identical(shared$session$input$main_nav, "gating_tab")) return()
      app_trace("tab observer start", .prefix = "[gating-ready]")
      gates <- shared$gating$gates()
      if (isTRUE(skip_next_gate_payload())) {
        skip_next_gate_payload(FALSE)
        app_trace("skipped next gate payload", .prefix = "[gating-ready]")
        return()
      }
      active_id <- ensure_active_preview_gate(gates)
      app_trace(
        "active_id=", active_id %||% "<none>",
        " gates=", length(gates),
        " active_sample=", shared$fcs$active_id() %||% "<none>",
        .prefix = "[gating-ready]"
      )
      if (!is.null(active_id) && is.null(gates[[active_id]])) {
        shared$gating$active_gate_id(NULL)
        active_plot("right")
        app_trace("active gate missing; reset to right plot", .prefix = "[gating-ready]")
        return()
      }
      contexts <- active_foreground_contexts(gates, active_id)
      app_trace("foreground contexts=", length(contexts), .prefix = "[gating-ready]")
      schedule_foreground_preview(gates, active_id)
    })

    observeEvent(active_plot(), {
      msg <- active_plot_message(active_plot())
      session$sendCustomMessage(msg$name, msg$payload)
    }, ignoreInit = TRUE)

    observeEvent(input$gate_canvas_request, {
      app_trace("canvas request", .prefix = "[gating-ready]")
      gates <- shared$gating$gates()
      active_id <- ensure_active_preview_gate(gates)
      app_trace("canvas active_id=", active_id %||% "<none>", " gates=", length(gates), .prefix = "[gating-ready]")
      if (!is.null(active_id) && is.null(gates[[active_id]])) {
        shared$gating$active_gate_id(NULL)
        active_plot("right")
        app_trace("canvas active gate missing; reset", .prefix = "[gating-ready]")
        return()
      }
      contexts <- active_foreground_contexts(gates, active_id)
      app_trace("canvas foreground contexts=", length(contexts), .prefix = "[gating-ready]")
      schedule_foreground_preview(gates, active_id)
    }, ignoreInit = TRUE)

    observeEvent(shared$ui$plot_preload_request(), {
      if (!isTRUE(gating_tab_active())) return()
      gates <- shared$gating$gates()
      active_id <- ensure_active_preview_gate(gates)
      if (!isTRUE(shiny::isolate(gating_preview_ready()))) {
        schedule_foreground_preview(gates, active_id)
        return()
      }
      request_preload_queue(gates, active_id, announce = TRUE)
    }, ignoreInit = TRUE)

    observe({
      st <- preload_state()
      if (!length(st$queue)) return()
      if (!isTRUE(gating_tab_active())) {
        close_preload_progress()
        return()
      }
      invalidateLater(60, session)
      generation <- st$generation
      signature <- st$signature
      ctx <- st$queue[[1]]
      st$queue <- st$queue[-1]
      gates <- shared$gating$gates()
      ctx_key <- preview_context_key(ctx, gates)
      err_msg <- NULL
      ok <- tryCatch({
        if (identical(ctx$type %||% "plot", "counts")) {
          active_sample_gate_stats(ctx$entry, gates)
        } else {
          preview_data_for_context(ctx$entry, ctx$gate_id, ctx$x, ctx$y, gates)
        }
        TRUE
      }, error = function(e) {
        err_msg <<- conditionMessage(e)
        FALSE
      })
      st$done <- st$done + 1L
      if (!isTRUE(ok)) {
        st$failed <- st$failed + 1L
        st$failed_keys <- unique(c(st$failed_keys %||% character(), ctx_key))
        message(sprintf(
          "[gating-preload] type=%s sample=%s gate=%s x=%s y=%s failed: %s",
          ctx$type %||% "plot",
          ctx$entry$id %||% "<unknown>",
          ctx$gate_id %||% "root",
          ctx$x %||% "<null>",
          ctx$y %||% "<null>",
          err_msg %||% "unknown error"
        ))
      }
      current <- isolate(preload_state())
      if (!identical(current$generation, generation) || !identical(current$signature, signature)) return()
      show_preload_notification(st$done, st$total)
      done <- !length(st$queue)
      preload_state(st)
      if (done) {
        close_preload_progress()
        if (isTRUE(gating_tab_active())) {
          if (st$failed > 0L) {
            showNotification(sprintf("Gating background preparation finished with %d failed context%s.", st$failed, if (st$failed == 1L) "" else "s"), type = "warning", duration = 8, id = "gating_cache_warning")
          } else {
            showNotification(preload_label(st$done, st$total, TRUE), type = "message", duration = 3)
          }
        }
      }
    })

    observeEvent(input$gate_canvas_activate, {
      plot <- input$gate_canvas_activate$plot %||% "right"
      if (plot %in% c("left", "right")) {
        current_plot <- active_plot()
        axis_debug("[gating-axis] activate requested=%s current=%s focus=%s selected=%s active_gate=%s",
          plot,
          current_plot %||% "<null>",
          plot_focus_gate_id(plot) %||% "<root>",
          canvas_selected_gate() %||% "<none>",
          shared$gating$active_gate_id() %||% "<none>"
        )
        if (!identical(plot, current_plot)) {
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

    switch_preview_sample <- function(target_sid) {
      gates <- shared$gating$gates()
      source_plot <- isolate(active_plot())
      gid <- displayed_population_gate_id(gates)
      target <- sample_gate_switch_target(gates, gid, target_sid)
      if (is.null(target)) return(FALSE)
      template <- gates[[target$template_id %||% ""]]
      active_sample_override(target_sid)
      shared$fcs$set_active(target_sid)
      updateSelectInput(session, "sample_id", selected = target_sid)
      next_gid <- if (identical(source_plot, "left")) {
        target_entry <- shared$fcs$registry()[[target_sid]]
        right_axis_source_gate_id(gates, target$gate_id, target_entry)
      } else NULL
      landing_gid <- next_gid %||% target$gate_id
      reset_right_axis_override(landing_gid)
      right_axis_template_gate_id(if (is.null(next_gid)) target$template_id else NULL)
      shared$gating$active_gate_id(landing_gid)
      canvas_selected_gate(landing_gid)
      active_plot(if (!is.null(next_gid)) "left" else "right")
      if (!is.null(template)) {
        session$onFlushed(function() {
          updateTextInput(session, "gate_name", value = template$name %||% "")
          updateRadioButtons(session, "gate_type", selected = template$type %||% "rectangle")
          updateSelectInput(session, "x_channel", selected = template$x_channel)
          if (!is.null(template$y_channel)) updateSelectInput(session, "y_channel", selected = template$y_channel)
        }, once = TRUE)
      }
      TRUE
    }

    observeEvent(shared$ui$gating_sample_request(), {
      if (!isTRUE(gating_tab_active())) return()
      sid <- shared$ui$gating_sample_request()$sample_id
      if (is.null(sid) || is.null(shared$fcs$registry()[[sid]])) return()
      if (identical(sid, active_sample_id())) return()
      if (!isTRUE(switch_preview_sample(sid))) {
        # Clear the prior sample's gate; preview initialization chooses an applicable gate for this sample.
        active_sample_override(sid)
        shared$fcs$set_active(sid)
        shared$gating$active_gate_id(NULL)
        canvas_selected_gate(NULL)
        right_axis_template_gate_id(NULL)
        updateSelectInput(session, "sample_id", selected = sid)
        active_plot("left")
      }
    }, ignoreInit = TRUE, priority = 100)

    preview_nav <- function(dir) {
      gates <- shared$gating$gates()
      gid <- displayed_population_gate_id(gates)
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
        prev_sid <- prev_next_preview_sample(gates, gid, sid, -1L)
        if (!is.null(prev_sid)) switch_preview_sample(prev_sid)
      } else if (identical(dir, "down")) {
        next_sid <- prev_next_preview_sample(gates, gid, sid, 1L)
        if (!is.null(next_sid)) switch_preview_sample(next_sid)
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
      gid <- displayed_population_gate_id(gates)
      sid <- active_sample_id()
      reg <- shared$fcs$registry()
      parent_id <- flowjo_gate_parent_id(gates, gid)
      child_ids <- if (is.null(gid)) flowjo_gate_children(gates, "root") else flowjo_gate_children(gates, gid)
      child_id <- if (length(child_ids)) child_ids[[1]] else NULL
      prev_sid <- prev_next_preview_sample(gates, gid, sid, -1L)
      next_sid <- prev_next_preview_sample(gates, gid, sid, 1L)
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
      gid <- displayed_population_gate_id(gates)
      sid <- prev_next_preview_sample(gates, gid, active_sample_id(), -1L)
      if (!is.null(sid)) {
        switch_preview_sample(sid)
      }
    }, ignoreInit = TRUE)
    observeEvent(input$nav_next_sample, {
      gates <- shared$gating$gates()
      gid <- displayed_population_gate_id(gates)
      sid <- prev_next_preview_sample(gates, gid, active_sample_id(), 1L)
      if (!is.null(sid)) {
        switch_preview_sample(sid)
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
      seen_group_samples <- character()
      group_blocks <- lapply(names(groups), function(gid) {
        g <- groups[[gid]]
        visible_sample_ids <- (g$sample_ids %||% character())
        visible_sample_ids <- visible_sample_ids[visible_sample_ids %in% names(reg) & !visible_sample_ids %in% seen_group_samples]
        seen_group_samples <<- c(seen_group_samples, visible_sample_ids)
        div(class = "gate-nav-group",
          div(
            class = "gate-nav-row gate-nav-group-head",
            `data-group-id` = gid,
            span(class = "gate-nav-name", icon("folder"), " ", g$name %||% gid),
            span(class = "gate-nav-meta", sprintf("%d samples", length(g$sample_ids %||% character())))
          ),
          lapply(visible_sample_ids, function(id) sample_row(id, TRUE))
        )
      })
      grouped_ids <- seen_group_samples
      ungrouped <- setdiff(names(reg), grouped_ids)
      div(class = "gate-nav-section",
        group_blocks,
        if (length(ungrouped)) div(class = "gate-nav-ungrouped", div(class = "gate-nav-section-title", "Samples"), lapply(ungrouped, sample_row)) else NULL
      )
    })

    observeEvent(input$navigator_sample_select, {
      sid <- input$navigator_sample_select
      if (nzchar(sid %||% "")) {
        active_sample_override(sid)
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
      displayed_gate_id <- displayed_population_gate_id(gates)
      row_for <- function(id, depth = 0L) {
        g <- gates[[id]]
        st <- gate_stat_row(sid, id)
        status <- gate_status(sid, id)
        avail <- gate_available(sid, id)
        cls <- paste("gate-nav-row gate-nav-gate",
          if (identical(id, displayed_gate_id)) "active" else "",
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
        div(class = paste("gate-nav-row gate-nav-root", if (is.null(displayed_gate_id)) "active" else ""),
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
      gates <- shared$gating$gates()
      if (!nrow(stats)) {
        e <- active_entry()
        req(e, cancelOutput = TRUE)
        gid <- canvas_selected_gate() %||% shared$gating$active_gate_id()
        if (is.null(gid) || is.null(gates[[gid]])) gid <- if (length(gates)) names(gates)[[1]] else NULL
        gate <- if (is.null(gid)) NULL else gates[[gid]]
        if (is.null(gate)) {
          geom <- current_geometry()
          channels <- current_draft_channels()
          req(geom, channels$x, channels$y, cancelOutput = TRUE)
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
      all_stat_gate_ids <- unique(as.character(stats$gate_id))
      collapsed <- intersect(collapsed_count_gates(), names(gates))
      if (length(collapsed) && nrow(stats)) {
        is_hidden_by_collapsed_parent <- function(gid) {
          gate <- gates[[gid]]
          if (is.null(gate)) return(FALSE)
          parent_id <- gate$parent_id %||% "root"
          seen <- character()
          while (length(parent_id) && !identical(parent_id, "root") && !(parent_id %in% seen)) {
            if (parent_id %in% collapsed) return(TRUE)
            seen <- c(seen, parent_id)
            parent <- gates[[parent_id]]
            parent_id <- if (is.null(parent)) "root" else parent$parent_id %||% "root"
          }
          FALSE
        }
        keep <- !vapply(stats$gate_id, is_hidden_by_collapsed_parent, logical(1))
        stats <- stats[keep, , drop = FALSE]
      }
      # Agent note: Highlight the current gate, not the parent population of the
      # active canvas. Include sample identity because imported gate IDs are shared.
      selected_gate <- canvas_selected_gate() %||% preview_focus_gate_id(gates) %||% ""
      selected_sample <- active_sample_id() %||% ""
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
        color <- toupper(trimws(as.character(stats$gate_color[[i]] %||% "#2563EB")))
        if (!grepl("^#[0-9A-F]{6}$", color)) color <- "#2563EB"
        foldable <- !identical(gid, "draft") && length(intersect(flowjo_gate_children(gates, gid), all_stat_gate_ids)) > 0L
        collapsed_here <- gid %in% collapsed
        fold_control <- if (foldable) {
          fold_title <- if (collapsed_here) "Expand child gates" else "Collapse child gates"
          sprintf(
            "<button type='button' class='gate-count-toggle' data-gate-id='%s' aria-expanded='%s' title='%s'>%s</button>",
            htmltools::htmlEscape(gid),
            if (collapsed_here) "false" else "true",
            htmltools::htmlEscape(fold_title),
            if (collapsed_here) "▸" else "▾"
          )
        } else {
          "<span class='gate-count-toggle-spacer' aria-hidden='true'></span>"
        }
        color_control <- if (identical(gid, "draft")) {
          "<span class='gate-count-color-spacer' aria-hidden='true'></span>"
        } else {
          sprintf(
            "<button type='button' class='gate-count-color-button' data-gate-id='%s' style='--gate-color:%s' title='Choose gate color' aria-label='Choose color for %s'></button>",
            htmltools::htmlEscape(gid),
            htmltools::htmlEscape(color),
            htmltools::htmlEscape(stats$gate_name[[i]] %||% gid)
          )
        }
        sprintf(
          "<div class='gate-count-tree-cell' style='--gate-depth:%d;--gate-color:%s' title='%s'>%s%s<span class='gate-count-tree-name'>%s</span><span class='gate-count-tree-axes'>%s</span>%s</div>",
          as.integer(stats$gate_depth[[i]] %||% 0L),
          htmltools::htmlEscape(color),
          htmltools::htmlEscape(paste(na.omit(c(stats$gate_name[[i]], axes, status)), collapse = " · ")),
          fold_control,
          color_control,
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
        selected = ifelse(nzchar(selected_gate) & stats$gate_id == selected_gate & stats$sample_id == selected_sample, "true", "false"),
        check.names = FALSE
      )
      active_gate_id <- displayed_population_gate_id(gates)
      current_sample_id <- active_sample_id()
      count_prev_sid <- prev_next_preview_sample(gates, active_gate_id, current_sample_id, -1L)
      count_next_sid <- prev_next_preview_sample(gates, active_gate_id, current_sample_id, 1L)
      count_nav_html <- paste0(
        "<div class='gate-count-sample-nav' aria-label='Sample navigation'>",
        if (!is.null(count_prev_sid)) "<button type='button' class='btn btn-light btn-sm gate-preview-arrow gate-count-sample-arrow' data-direction='previous' title='Previous sample' aria-label='Previous sample'><i class='fas fa-chevron-up' role='presentation'></i></button>" else "",
        if (!is.null(count_next_sid)) "<button type='button' class='btn btn-light btn-sm gate-preview-arrow gate-count-sample-arrow' data-direction='next' title='Next sample' aria-label='Next sample'><i class='fas fa-chevron-down' role='presentation'></i></button>" else "",
        "</div>"
      )
      DT::datatable(
        counts,
        class = "stripe hover compact gate-count-table",
        options = list(
          paging = FALSE,
          lengthChange = FALSE,
          scrollY = "420px",
          scrollCollapse = FALSE,
          autoWidth = FALSE,
          columnDefs = list(
            list(targets = c(3, 4, 5), className = "dt-body-right dt-head-right"),
            list(targets = c(6, 7, 8), visible = FALSE, searchable = FALSE)
          ),
          rowCallback = DT::JS("function(row, data) { var selected = data && (data[8] === true || ['true', 'TRUE', '1', 'yes', 'Yes'].indexOf(String(data[8])) >= 0); $(row).toggleClass('gate-count-current', !!selected).attr('aria-current', selected ? 'true' : null); }")
        ),
        callback = DT::JS(sprintf("
          var wrapper = $(table.table().container());
          // Agent note: Keep all population rows in one bounded scroll body;
          // paging hides the gate tree and makes cross-gate comparison painful.
          wrapper.addClass('gate-count-table-wrapper');
          var scrollBody = wrapper.find('.dataTables_scrollBody');
          if (scrollBody.length) {
            scrollBody[0].classList.add('Pinhki-bounded-scroll', 'Pinhki-scroll-fade');
            scrollBody[0].dispatchEvent(new Event('scroll'));
          }
          wrapper.off('click.gateCountFold').on('click.gateCountFold', 'button.gate-count-toggle', function(event) {
            event.preventDefault();
            event.stopPropagation();
            Shiny.setInputValue('%s', {gate_id: this.getAttribute('data-gate-id'), nonce: Math.random()}, {priority: 'event'});
          });
          wrapper.off('click.gateCountColor').on('click.gateCountColor', 'button.gate-count-color-button', function(event) {
            event.preventDefault();
            event.stopPropagation();
            Shiny.setInputValue('%s', {gate_id: this.getAttribute('data-gate-id'), nonce: Math.random()}, {priority: 'event'});
          });
          var filter = wrapper.find('.dataTables_filter');
          if (filter.length && !wrapper.find('.gate-count-sample-nav').length) {
            filter.addClass('gate-count-filter');
            filter.wrap('<div class=\"gate-count-filter-tools\"></div>');
            filter.before(%s);
          }
          var tools = wrapper.find('.gate-count-filter-tools');
          var header = wrapper.closest('.gate-count-card').find('.gate-count-header-tools');
          if (header.length && tools.length) header.empty().append(tools);
          header.off('click.gateCountSampleNav').on('click.gateCountSampleNav', '.gate-count-sample-arrow', function() {
            var direction = $(this).data('direction');
            var inputId = direction === 'previous' ? '%s' : '%s';
            Shiny.setInputValue(inputId, {nonce: Math.random()}, {priority: 'event'});
          });
          table.on('dblclick', 'tbody tr', function() {
            var data = table.row(this).data();
            if (!data) return;
            Shiny.setInputValue('%s', {gate_id: data[6], sample_id: data[7], nonce: Math.random()}, {priority: 'event'});
          });
          table.draw(false);
        ",
          session$ns("count_gate_toggle"),
          session$ns("count_gate_color_open"),
          jsonlite::toJSON(count_nav_html, auto_unbox = TRUE),
          session$ns("count_prev_sample"),
          session$ns("count_next_sample"),
          session$ns("count_table_open")
        )),
        escape = FALSE,
        rownames = FALSE
      )
    })

    observeEvent(input$count_prev_sample, preview_nav("up"), ignoreInit = TRUE)
    observeEvent(input$count_next_sample, preview_nav("down"), ignoreInit = TRUE)

    observeEvent(input$count_table_open, {
      msg <- input$count_table_open
      gid <- msg$gate_id %||% ""
      sid <- msg$sample_id %||% ""
      gates <- shared$gating$gates()
      reg <- shared$fcs$registry()
      if (!nzchar(gid) || identical(gid, "draft") || is.null(gates[[gid]])) return()
      if (nzchar(sid) && !is.null(reg[[sid]])) {
        active_sample_override(sid)
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
