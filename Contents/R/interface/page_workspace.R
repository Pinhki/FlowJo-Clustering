workspace_page_ui <- function(id) {
  ns <- NS(id)
  drop_action <- function(button, input_id) div(class = "workspace-toolbar-drop",
    ondragover = "event.preventDefault();this.classList.add('drop-target');",
    ondragleave = "if(!this.contains(event.relatedTarget))this.classList.remove('drop-target');",
    ondrop = sprintf("event.preventDefault();event.stopPropagation();this.classList.remove('drop-target');Shiny.setInputValue('%s',{keys:event.dataTransfer.getData('application/x-flowjo-workspace-rows'),nonce:Math.random()},{priority:'event'});", ns(input_id)), button)
  div(class = "card-page-wrap",
    singleton(tags$script(HTML(sprintf("
      (function(){
        var inputId = '%s';
        if (window.Shiny && !window.__flowjoWorkspaceSelectionHandler) {
          window.__flowjoWorkspaceSelectionHandler = true;
          Shiny.addCustomMessageHandler('flowjo-workspace-selection', function(msg){
            var keys = (msg && msg.keys) || [];
            window.flowjoWorkspaceSelectedKeys = keys.slice();
            document.querySelectorAll('.workspace-tree .workspace-select-row').forEach(function(row){
              row.classList.toggle('selected', keys.indexOf(row.dataset.rowKey) >= 0);
            });
            console.debug('[workspace-basket] equivalentSelection keys='+keys.length);
          });
        }
        window.__flowjoWorkspaceDblClickBound = window.__flowjoWorkspaceDblClickBound || {};
        if (window.__flowjoWorkspaceDblClickBound[inputId]) return;
        window.__flowjoWorkspaceDblClickBound[inputId] = true;
        document.addEventListener('dblclick', function(event){
          var row = event.target && event.target.closest && event.target.closest('.workspace-tree .gate-nav-gate[data-sample-id][data-gate-id]');
          if (!row || row.dataset.sampleId === 'group') return;
          event.preventDefault();
          event.stopPropagation();
          if (event.stopImmediatePropagation) event.stopImmediatePropagation();
          Shiny.setInputValue('%s', {
            sample: row.dataset.sampleId,
            gate: row.dataset.gateId,
            nonce: Math.random()
          }, {priority: 'event'});
        }, true);
      })();
    ", ns("open_workspace_gate"), ns("open_workspace_gate"))))),
    card_page_header("Workspace", subtitle = "Samples, groups, gates, and group application."),
    shiny_split_resize(
      id = ns("workspace_split"),
      class = "workspace-flowjo Pinhki-split-responsive-vertical Pinhki-resize-color-hover",
      default_left = "50%",
      min_left = 320,
      min_right = 360,
      default_top = "60%",
      min_top = 240,
      min_bottom = 240,
      left = card_page_card("Workspace Tree",
        content_class = "Pinhki-scroll-fade",
        div(class = "workspace-selection-toolbar d-flex gap-2 mb-2 flex-wrap",
            drop_action(actionButton(ns("new_group"), "New Group", class = "btn btn-primary btn-sm"), "workspace_new_group_drop"),
            actionButton(ns("select_equivalent_gates"), "Select equivalent gates", class = "btn btn-outline-secondary btn-sm"),
            drop_action(actionButton(ns("add_selected_basket"), "Add to Basket", class = "btn btn-outline-secondary btn-sm"), "workspace_basket_drop"),
          actionButton(ns("open_gating"), "Open Gating", class = "btn btn-outline-secondary btn-sm")
        ),
        uiOutput(ns("workspace_tree"))
      ),
      right = card_page_card("Details",
        content_class = "Pinhki-scroll-fade",
        div(class = "workspace-details-scroll Pinhki-scroll-fade",
          uiOutput(ns("workspace_details"))
        )
      )
    )
  )
}

workspace_page_server <- function(id, shared) {
  moduleServer(id, function(input, output, session) {
    selected <- reactiveVal(list(type = "root", id = NULL))
    editing_group <- reactiveVal(NULL)
    selected_rows <- reactiveVal(character())
    new_group_sample_ids <- reactiveVal(NULL)
    workspace_stats <- reactiveVal(data.frame())
    group_color_value <- reactiveVal("#2563EB")
    group_color_picker <- motion_color_picker_server("group_color_picker")

    safe_color <- function(x) {
      x <- toupper(trimws(x %||% "#2563EB"))
      if (grepl("^#[0-9A-F]{6}$", x)) x else "#2563EB"
    }

    gate_rows <- function(gates, parent_id = "root", depth = 0L) {
      ids <- flowjo_gate_children(gates, parent_id)
      out <- list()
      for (gid in ids) {
        out[[length(out) + 1L]] <- list(id = gid, depth = depth)
        out <- c(out, gate_rows(gates, gid, depth + 1L))
      }
      out
    }

    gate_rows_for_sample <- function(gates, entry, parent_id = "root", depth = 0L) {
      ids <- flowjo_gate_children(gates, parent_id)
      out <- list()
      for (gid in ids) {
        if (!isTRUE(flowjo_gate_applicability(entry, gates[[gid]], gates)$ok)) next
        out[[length(out) + 1L]] <- list(id = gid, depth = depth)
        out <- c(out, gate_rows_for_sample(gates, entry, gid, depth + 1L))
      }
      out
    }

    gate_rows_for_group <- function(gates, ids, parent_id = "root", depth = 0L) {
      children <- intersect(flowjo_gate_children(gates, parent_id), ids)
      out <- list()
      for (gid in children) {
        out[[length(out) + 1L]] <- list(id = gid, depth = depth)
        out <- c(out, gate_rows_for_group(gates, ids, gid, depth + 1L))
      }
      out
    }

    gate_stat <- function(sample_id, gate_id) {
      stats <- workspace_stats()
      if (!nrow(stats)) return(NULL)
      row <- stats[stats$sample_id == sample_id & stats$gate_id == gate_id, , drop = FALSE]
      if (nrow(row)) row[1, , drop = FALSE] else NULL
    }

    gate_stat_label <- function(sample_id, gate_id) {
      st <- gate_stat(sample_id, gate_id)
      if (is.null(st)) return("--")
      sprintf("%.1f%% · %s cells", st$percent_parent, format(st$cells, big.mark = ","))
    }

    workspace_sample_choices <- function(tbl) {
      if (is.null(tbl) || !nrow(tbl) || is.null(tbl$id)) return(character())
      stats::setNames(tbl$id, tbl$name %||% tbl$id)
    }

    workspace_group_choices <- function(groups) {
      ids <- names(groups) %||% character()
      if (!length(ids)) return(character())
      stats::setNames(ids, vapply(groups, function(x) x$name %||% x$id, character(1)))
    }

    sync_label <- function(g) {
      state <- flowjo_gate_group_state(g)
      if (identical(state, "synced") || identical(state, "group-owned")) return("Synced")
      if (identical(state, "sample-modified")) return("Local edit")
      "Not synced"
    }

    group_color_picker_ui <- function(value) {
      color <- safe_color(value)
      div(class = "group-color-picker-field",
        tags$label(class = "control-label", "Group color"),
        div(class = "group-color-picker",
          actionButton(
            session$ns("group_color_toggle"),
            label = NULL,
            class = "group-color-swatch-btn",
            style = sprintf("--group-color:%s", color),
            title = "Choose group color",
            onclick = "var p=this.parentElement.querySelector('.group-color-popover'); if(p) p.style.display=p.style.display==='block'?'none':'block';"
          ),
          span(class = "group-color-hex", color),
          div(class = "group-color-popover",
            # Agent note: Group color edits are staged; Apply commits the picker value so browsing colors does not recolor synced gate trees.
            motion_color_picker_ui(session$ns("group_color_picker"), mode = "single", initial_hex = color),
            div(class = "group-color-popover-actions",
              actionButton(session$ns("group_color_cancel"), "Cancel", class = "btn btn-outline-secondary btn-sm", onclick = "this.closest('.group-color-popover').style.display='none';"),
              actionButton(session$ns("group_color_apply"), "Apply", class = "btn btn-primary btn-sm", onclick = "this.closest('.group-color-popover').style.display='none';")
            )
          )
        )
      )
    }

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

    workspace_drag_ids <- function(dragged) {
      dragged <- unique(trimws(unlist(strsplit(paste(unlist(dragged, use.names = FALSE), collapse = ","), ",", fixed = TRUE), use.names = FALSE)))
      dragged <- dragged[nzchar(dragged)]
      if (length(dragged)) dragged else character()
    }

    # Agent note: Workspace selection is row-scoped because the same gate id can appear under multiple samples/groups.
    workspace_row_key <- function(type, sample_id = "", gate_id = "", group_id = "") {
      paste(type, sample_id %||% "", gate_id %||% "", group_id %||% "", sep = "::")
    }

    workspace_row_select_js <- function(ns_id, type, key, id = "", sample = "", gate = "") {
      sprintf(
        "var root=this.closest('.workspace-tree')||this.closest('.workspace-group-gate-tree')||document,rows=Array.from(root.querySelectorAll('.workspace-select-row')),key='%s'; window.flowjoWorkspaceLastRowKey=window.flowjoWorkspaceLastRowKey||null; if(event.shiftKey&&window.flowjoWorkspaceLastRowKey){var a=rows.findIndex(function(r){return r.dataset.rowKey===window.flowjoWorkspaceLastRowKey;}),b=rows.indexOf(this); if(a>-1&&b>-1) rows.slice(Math.min(a,b),Math.max(a,b)+1).forEach(function(r){r.classList.add('selected');});} else if(event.metaKey||event.ctrlKey){this.classList.toggle('selected'); window.flowjoWorkspaceLastRowKey=key;} else {rows.forEach(function(r){r.classList.remove('selected');}); this.classList.add('selected'); window.flowjoWorkspaceLastRowKey=key;} window.flowjoWorkspaceSelectedKeys=Array.from(root.querySelectorAll('.workspace-select-row.selected')).map(function(r){return r.dataset.rowKey;}); console.debug('[workspace-basket] selection keys='+window.flowjoWorkspaceSelectedKeys.length); Shiny.setInputValue('%s',{type:'%s',id:'%s',sample:'%s',gate:'%s',keys:window.flowjoWorkspaceSelectedKeys,nonce:Math.random()},{priority:'event'}); event.preventDefault(); event.stopPropagation();",
        key, ns_id, type, id, sample, gate
      )
    }

    workspace_selected_parts <- function(prefix = NULL) {
      keys <- selected_rows()
      if (length(prefix)) keys <- keys[startsWith(keys, paste0(prefix, "::"))]
      parts <- strsplit(keys, "::", fixed = TRUE)
      Filter(function(x) length(x) >= 3L, parts)
    }

    workspace_selected_gate_pairs <- function() {
      parts <- workspace_selected_parts("gate")
      if (!length(parts)) return(data.frame(sample_id = character(), gate_id = character(), stringsAsFactors = FALSE))
      out <- do.call(rbind, lapply(parts, function(x) data.frame(sample_id = x[[2]], gate_id = x[[3]], stringsAsFactors = FALSE)))
      unique(out[nzchar(out$sample_id) & nzchar(out$gate_id) & out$sample_id != "group", , drop = FALSE])
    }

    workspace_selected_sample_ids <- function() {
      parts <- workspace_selected_parts("sample")
      unique(vapply(parts, function(x) x[[2]], character(1)))
    }

    workspace_selected_gate_rows <- function() {
      parts <- workspace_selected_parts("gate")
      if (!length(parts)) return(data.frame(sample_id = character(), gate_id = character(), stringsAsFactors = FALSE))
      out <- do.call(rbind, lapply(parts, function(x) data.frame(sample_id = x[[2]], gate_id = x[[3]], stringsAsFactors = FALSE)))
      unique(out[nzchar(out$sample_id) & nzchar(out$gate_id), , drop = FALSE])
    }

    workspace_gate_path <- function(gates, gate_id) {
      labels <- character()
      cur <- gate_id
      seen <- character()
      while (!is.null(cur) && nzchar(cur) && !identical(cur, "root") && !(cur %in% seen)) {
        seen <- c(seen, cur)
        g <- gates[[cur]]
        if (is.null(g)) break
        labels <- c(g$name %||% cur, labels)
        cur <- g$parent_id %||% "root"
      }
      if (length(labels)) paste(labels, collapse = " / ") else "Root"
    }

    workspace_geometry_text <- function(gate) {
      fmt <- function(x) {
        if (is.null(x) || !length(x)) return("--")
        num <- suppressWarnings(as.numeric(x[[1]]))
        if (is.na(num)) as.character(x[[1]]) else format(round(num, 3), trim = TRUE, scientific = FALSE)
      }
      geom <- gate$geometry %||% list()
      type <- gate$type %||% "gate"
      summary <- switch(type,
        rectangle = sprintf("Rectangle: X %s-%s, Y %s-%s", fmt(geom$xmin), fmt(geom$xmax), fmt(geom$ymin), fmt(geom$ymax)),
        range = sprintf("Range: X %s-%s", fmt(geom$xmin), fmt(geom$xmax)),
        ellipse = sprintf("Ellipse: X %s-%s, Y %s-%s", fmt(geom$xmin), fmt(geom$xmax), fmt(geom$ymin), fmt(geom$ymax)),
        polygon = sprintf("Polygon: %s vertices", if (!is.null(geom$vertices)) nrow(geom$vertices) else 0L),
        quadrant = sprintf("Quadrant: X %s, Y %s, %s", fmt(geom$x), fmt(geom$y), geom$quadrant %||% "upper_right"),
        curved_quadrant = sprintf("Curved quadrant: X %s, Y %s, %s", fmt(geom$x), fmt(geom$y), geom$quadrant %||% "upper_right"),
        paste0(type, " geometry")
      )
      exact <- paste(capture.output(str(geom, give.attr = FALSE)), collapse = "\n")
      list(summary = summary, exact = exact)
    }

    workspace_gate_detail <- function(sample_id, gate_id, reg, groups, gates) {
      g <- gates[[gate_id]]
      if (is.null(g)) return(NULL)
      st <- if (!identical(sample_id, "group")) gate_stat(sample_id, gate_id) else NULL
      group <- groups[[g$group_id %||% g$sync_group_id %||% ""]]
      entry <- if (!identical(sample_id, "group")) reg[[sample_id]] else NULL
      list(
        sample_id = sample_id,
        gate_id = gate_id,
        gate = g,
        stat = st,
        group = group,
        sample_name = if (!is.null(entry)) entry$name %||% sample_id else NULL,
        state = flowjo_gate_group_state(g),
        path = workspace_gate_path(gates, gate_id),
        geometry = workspace_geometry_text(g)
      )
    }

    workspace_gate_detail_block <- function(detail, session, group_choices = character(), rich = FALSE) {
      g <- detail$gate
      st <- detail$stat
      state <- detail$state
      is_group <- identical(detail$sample_id, "group")
      context <- if (is_group) {
        paste("Group template", detail$group$name %||% detail$group$id %||% "")
      } else {
        paste("Sample", detail$sample_name %||% detail$sample_id)
      }
      div(class = paste("workspace-gate-detail-block", if (rich) "is-rich" else ""),
        div(class = "workspace-gate-detail-head",
          div(
            div(class = "workspace-gate-detail-title", g$name %||% detail$gate_id),
            div(class = "workspace-gate-detail-context", context)
          ),
          span(class = paste("gate-state-badge", state), sync_label(g))
        ),
        div(class = "workspace-gate-metrics",
          div(class = "workspace-gate-metric",
            span(class = "workspace-gate-metric-value", if (!is.null(st)) format(st$cells, big.mark = ",") else "--"),
            span(class = "workspace-gate-metric-label", "Cells")
          ),
          div(class = "workspace-gate-metric",
            span(class = "workspace-gate-metric-value", if (!is.null(st)) sprintf("%.2f%%", st$percent_parent) else "--"),
            span(class = "workspace-gate-metric-label", "Parent")
          )
        ),
        div(class = "workspace-gate-facts",
          div(span("Channels"), strong(sprintf("%s / %s", g$x_channel %||% "--", g$y_channel %||% "--"))),
          div(span("Type"), strong(g$type %||% "--")),
          div(span("Path"), strong(detail$path))
        ),
        tags$details(class = "workspace-gate-geometry",
          tags$summary(detail$geometry$summary),
          tags$pre(detail$geometry$exact)
        )
      )
    }

    workspace_gate_bulk_actions <- function(session, group_choices, gate_ids) {
      if (!length(group_choices) || !length(gate_ids)) return(NULL)
      target_id <- session$ns("target_group")
      gate_value <- paste(unique(gate_ids), collapse = ",")
      div(class = "workspace-gate-actionbar",
        selectInput(target_id, "Target group", choices = group_choices),
        div(class = "d-flex gap-2 flex-wrap",
          tags$button(type = "button", class = "btn btn-outline-secondary",
            onclick = sprintf("var g=document.getElementById('%s').value; if(g) Shiny.setInputValue('%s',{gate:'%s',targetType:'group',targetId:g,mode:'copy',nonce:Math.random()},{priority:'event'});", target_id, session$ns("workspace_drop"), gate_value),
            "Copy to Group"
          ),
          tags$button(type = "button", class = "btn btn-primary",
            onclick = sprintf("var g=document.getElementById('%s').value; if(g) Shiny.setInputValue('%s',{gate:'%s',targetType:'group',targetId:g,mode:'sync',nonce:Math.random()},{priority:'event'});", target_id, session$ns("workspace_drop"), gate_value),
            "Sync to Group"
          )
        )
      )
    }

    # Agent note: Keep the click-time key list authoritative; DOM selection can be transiently stale when native dragstart fires.
    workspace_drag_row_keys_js <- "var keys=(window.flowjoWorkspaceSelectedKeys||[]).slice(); if(keys.indexOf(this.dataset.rowKey)<0) keys=[this.dataset.rowKey]; var gateIds=keys.map(function(k){var p=k.split('::'); return p[0]==='gate'?p[2]:'';}).filter(Boolean); console.debug('[workspace-basket] dragstart payloadKeys='+keys.length); event.dataTransfer.setData('application/x-flowjo-workspace-rows',keys.join(',')); event.dataTransfer.setData('text/plain',gateIds.join(','));"

    add_gate_pairs_to_basket <- function(pairs) {
      if (is.null(pairs) || !nrow(pairs)) return(0L)
      added <- 0L
      for (i in seq_len(nrow(pairs))) {
        added <- added + shared$populations$add(pairs$sample_id[[i]], pairs$gate_id[[i]], set_name = "Population Set")
      }
      added
    }

    row_keys_to_gate_pairs <- function(keys) {
      parts <- strsplit(workspace_drag_ids(keys), "::", fixed = TRUE)
      parts <- Filter(function(x) length(x) >= 3L && identical(x[[1]], "gate"), parts)
      if (!length(parts)) return(data.frame(sample_id = character(), gate_id = character(), stringsAsFactors = FALSE))
      out <- do.call(rbind, lapply(parts, function(x) data.frame(sample_id = x[[2]], gate_id = x[[3]], stringsAsFactors = FALSE)))
      unique(out[nzchar(out$sample_id) & nzchar(out$gate_id) & out$sample_id != "group", , drop = FALSE])
    }

    row_keys_to_sample_ids <- function(keys) {
      parts <- strsplit(workspace_drag_ids(keys), "::", fixed = TRUE)
      parts <- Filter(function(x) length(x) >= 2L && identical(x[[1]], "sample"), parts)
      unique(vapply(parts, function(x) x[[2]], character(1)))
    }

    copy_gates_to_sample <- function(gate_ids, sample_id, parent_id = "root") {
      gates <- shared$gating$gates()
      gate_ids <- gate_subtree_ids(gates, gate_ids)
      id_map <- list()
      made <- character()
      for (gid in gate_ids) {
        src <- gates[[gid]]
        if (is.null(src)) next
        src_parent <- src$parent_id %||% "root"
        dst_parent <- if (!is.null(id_map[[src_parent]])) id_map[[src_parent]] else parent_id %||% "root"
        cp <- flowjo_copy_gate(src, parent_id = dst_parent, sample_id = sample_id)
        gates[[cp$id]] <- cp
        id_map[[gid]] <- cp$id
        made <- c(made, cp$id)
      }
      if (length(made)) {
        shared$gating$gates(gates)
        selected_rows(vapply(made, function(id) workspace_row_key("gate", sample_id, id), character(1)))
      }
      invisible(made)
    }

    copy_gates_to_group <- function(gate_ids, group_id, mode = NULL) {
      groups <- shared$gating$groups()
      group <- groups[[group_id]]
      if (is.null(group)) return(invisible(character()))
      gates <- shared$gating$gates()
      gate_ids <- gate_subtree_ids(gates, gate_ids)
      made <- character()
      sync <- identical(mode, "sync") || (is.null(mode) && isTRUE(group$sync_enabled))
      for (sid in group$sample_ids %||% character()) {
        id_map <- list()
        for (gid in gate_ids) {
          src <- gates[[gid]]
          if (is.null(src)) next
          src_parent <- src$parent_id %||% "root"
          dst_parent <- if (!is.null(id_map[[src_parent]])) id_map[[src_parent]] else src_parent
          hits <- flowjo_gate_same_path(gates, src$name, dst_parent, sid)
          if (length(hits)) {
            hit_id <- hits[[1]]
            if (isTRUE(sync)) {
              cp <- flowjo_copy_gate(
                src,
                parent_id = dst_parent,
                sample_id = sid,
                group_id = group_id,
                sync = TRUE,
                sync_sample_ids = group$sample_ids %||% character(),
                group_color = group$color %||% NULL
              )
              cp$id <- hit_id
              cp$created_at <- gates[[hit_id]]$created_at %||% cp$created_at
              gates[[hit_id]] <- cp
              made <- c(made, hit_id)
            }
            id_map[[gid]] <- hit_id
            next
          }
          cp <- flowjo_copy_gate(
            src,
            parent_id = dst_parent,
            sample_id = sid,
            group_id = group_id,
            sync = sync,
            sync_sample_ids = group$sample_ids %||% character(),
            group_color = group$color %||% NULL
          )
          gates[[cp$id]] <- cp
          id_map[[gid]] <- cp$id
          made <- c(made, cp$id)
        }
      }
      if (length(made)) {
        shared$gating$gates(gates)
        selected_rows(vapply(made, function(id) workspace_row_key("gate", "group", id), character(1)))
      }
      invisible(made)
    }

    observe({
      reg <- shared$fcs$registry()
      gates <- shared$gating$gates()
      if (!length(reg) || !length(gates) || is.null(shared$gating$recompute)) return()
      workspace_stats(shared$gating$recompute(names(gates), names(reg)))
    })

    output$workspace_tree <- renderUI({
      reg <- shared$fcs$registry()
      groups <- shared$gating$groups()
      gates <- shared$gating$gates()
      message("[workspace] render tree samples=", length(reg), " groups=", length(groups), " gates=", length(gates))
      if (!length(reg)) return(div(class = "gate-nav-empty", "No samples loaded."))
      # Agent note: Row clicks update details and client-side highlight; do not make
      # the tree itself reactive to selection or the scroll position resets.
      sel <- isolate(selected())
      picked <- isolate(selected_rows())

      fold_control <- function() tags$button(type = "button", class = "workspace-fold-toggle", `aria-expanded` = "true", title = "Fold / unfold children",
        onclick = "event.preventDefault();event.stopPropagation();var branch=this.closest('.workspace-tree-branch');var folded=branch.classList.toggle('workspace-folded');this.setAttribute('aria-expanded',String(!folded));", icon("chevron-down"))

      gate_node <- function(gid, sid, depth, group_id = "") {
        g <- gates[[gid]]
        key <- workspace_row_key("gate", sid, gid, group_id)
        chosen <- key %in% picked
        children <- flowjo_gate_children(gates, gid)
        children <- children[vapply(children, function(id) isTRUE(flowjo_gate_applicability(reg[[sid]], gates[[id]], gates)$ok), logical(1))]
        row <- div(
          class = paste("gate-nav-row gate-nav-gate workspace-drop-row workspace-select-row", if (identical(sel$type, "gate") && identical(sel$id, gid) && identical(sel$sample, sid)) "active" else "", if (chosen) "selected" else "", flowjo_gate_group_state(g)),
          draggable = "true",
          ondragstart = workspace_drag_row_keys_js,
          ondragover = "event.preventDefault(); this.classList.add('drop-target');",
          ondragleave = "this.classList.remove('drop-target');",
          ondrop = sprintf("event.preventDefault(); this.classList.remove('drop-target'); Shiny.setInputValue('%s',{gate:event.dataTransfer.getData('text/plain'),targetType:'gate',targetSampleId:'%s',targetId:'%s',nonce:Math.random()},{priority:'event'});", session$ns("workspace_drop"), sid, gid),
          `data-row-key` = key,
          `data-gate-id` = gid,
          `data-sample-id` = sid,
          ondblclick = sprintf("event.preventDefault(); event.stopPropagation(); Shiny.setInputValue('%s',{sample:'%s',gate:'%s',nonce:Math.random()},{priority:'event'}); return false;", session$ns("open_workspace_gate"), sid, gid),
          onclick = paste0(
            sprintf("if(event.detail>1){event.preventDefault(); event.stopPropagation(); Shiny.setInputValue('%s',{sample:'%s',gate:'%s',nonce:Math.random()},{priority:'event'}); return false;} ", session$ns("open_workspace_gate"), sid, gid),
            workspace_row_select_js(session$ns("workspace_select"), "gate", key, gid, sid, gid)
          ),
          span(class = "workspace-tree-label", style = sprintf("padding-left:%dpx", depth * 16L),
            if (length(children)) fold_control() else span(class = "workspace-fold-spacer"),
            span(class = "gate-tree-name", g$name %||% gid)),
          span(class = "gate-node-meta", gate_stat_label(sid, gid)),
          span(class = paste("gate-state-badge", flowjo_gate_group_state(g)), sync_label(g))
        )
        if (!length(children)) return(row)
        div(class = "workspace-tree-branch", style = sprintf("--workspace-level:%d;--workspace-parent-z:%d", depth + 1L, 100L - depth),
          row, div(class = "workspace-branch-children", lapply(children, function(id) gate_node(id, sid, depth + 1L, group_id))))
      }

      sample_node <- function(sid, grouped = FALSE, group_id = "") {
        e <- reg[[sid]]
        rows <- gate_rows_for_sample(gates, e)
        key <- workspace_row_key("sample", sid, "")
        chosen <- key %in% picked
        div(class = "workspace-sample-node workspace-tree-branch", style = sprintf("--workspace-level:%d;--workspace-parent-z:110", if (grouped) 1L else 0L),
          div(
            class = paste("gate-nav-row gate-nav-sample workspace-drop-row workspace-select-row", if (identical(sel$type, "sample") && identical(sel$id, sid)) "active" else "", if (chosen) "selected" else ""),
            draggable = "true",
            ondragstart = workspace_drag_row_keys_js,
            ondragover = "event.preventDefault(); this.classList.add('drop-target');",
            ondragleave = "this.classList.remove('drop-target');",
            ondrop = sprintf("event.preventDefault(); this.classList.remove('drop-target'); Shiny.setInputValue('%s',{gate:event.dataTransfer.getData('text/plain'),targetType:'sample',targetSampleId:'%s',nonce:Math.random()},{priority:'event'});", session$ns("workspace_drop"), sid),
            `data-row-key` = key,
            `data-sample-id` = sid,
            ondblclick = sprintf("Shiny.setInputValue('%s',{sample:'%s',nonce:Math.random()},{priority:'event'});", session$ns("open_workspace_sample"), sid),
            onclick = workspace_row_select_js(session$ns("workspace_select"), "sample", key, sid, sid, ""),
            if (length(rows)) fold_control() else span(class = "workspace-fold-spacer"),
            span(class = "gate-nav-name", e$name %||% sid),
            span(class = "gate-nav-meta", sprintf("%s events", format(e$cells %||% nrow(e$df), big.mark = ",")))
          ),
          div(class = "workspace-branch-children", lapply(flowjo_gate_children(gates, "root"), function(id) {
            if (isTRUE(flowjo_gate_applicability(e, gates[[id]], gates)$ok)) gate_node(id, sid, if (grouped) 1L else 0L, group_id)
          }))
        )
      }

      group_node <- function(gid) {
        g <- groups[[gid]]
        div(class = "gate-nav-group workspace-tree-branch", style = "--workspace-level:0;--workspace-parent-z:120",
          div(
            class = paste("gate-nav-row gate-nav-group-head", if (identical(sel$type, "group") && identical(sel$id, gid)) "active" else ""),
            style = sprintf("--group-color:%s", g$color %||% "#2563EB"),
            ondragover = "event.preventDefault(); this.classList.add('drop-target');",
            ondragleave = "this.classList.remove('drop-target');",
            ondrop = sprintf("event.preventDefault(); this.classList.remove('drop-target'); Shiny.setInputValue('%s',{gate:event.dataTransfer.getData('text/plain'),targetType:'group',targetId:'%s',nonce:Math.random()},{priority:'event'});", session$ns("workspace_drop"), gid),
            onclick = sprintf("Shiny.setInputValue('%s',{type:'group',id:'%s',nonce:Math.random()},{priority:'event'});", session$ns("workspace_select"), gid),
            fold_control(),
            span(class = "gate-nav-name", icon("folder"), " ", g$name %||% gid),
            span(class = "gate-nav-meta", sprintf("%d samples", length(g$sample_ids %||% character()))),
            span(class = paste("gate-state-badge", if (isTRUE(g$sync_enabled)) "synced" else "sample-modified"), if (isTRUE(g$sync_enabled)) "Sync on" else "Sync off"),
            span(class = "workspace-drop-target-label", "drop here")
          ),
          div(class = "workspace-branch-children", lapply(g$sample_ids %||% character(), function(sid) if (!is.null(reg[[sid]])) sample_node(sid, TRUE, gid)))
        )
      }

      grouped_ids <- unique(unlist(lapply(groups, function(g) g$sample_ids %||% character()), use.names = FALSE))
      div(class = "gate-nav-section workspace-tree Pinhki-scroll-fade",
        if (length(groups)) lapply(names(groups), group_node),
        div(class = "gate-nav-ungrouped",
          div(class = "gate-nav-section-title", "All Samples"),
          lapply(names(reg), sample_node)
        )
      )
    })

    output$workspace_details <- renderUI({
      reg <- shared$fcs$registry()
      groups <- shared$gating$groups()
      gates <- shared$gating$gates()
      sel <- selected()
      tbl <- shared$fcs$table()
      group_choices <- workspace_group_choices(groups)
      message("[workspace] render details samples=", length(reg), " table_rows=", nrow(tbl), " selected=", sel$type %||% "<none>")

      if (identical(sel$type, "group") || identical(sel$type, "new_group")) {
        g <- if (!is.null(sel$id)) groups[[sel$id]] else NULL
        g <- g %||% list(name = "New group", color = "#2563EB", sample_ids = new_group_sample_ids() %||% tbl$id, sync_enabled = FALSE)
        if (is.null(g)) return(helpText("Select a group."))
        tagList(
          h4(g$name %||% "Group"),
          textInput(session$ns("group_name"), "Group name", value = g$name %||% ""),
          group_color_picker_ui(group_color_value()),
          checkboxInput(session$ns("group_sync_enabled"), "Synchronized group", value = isTRUE(g$sync_enabled)),
          checkboxGroupInput(session$ns("group_samples"), "Samples", choices = workspace_sample_choices(tbl), selected = g$sample_ids %||% character()),
          div(class = "d-flex gap-2",
            actionButton(session$ns("save_group"), "Save Group", class = "btn btn-primary"),
            if (!identical(sel$type, "new_group")) actionButton(session$ns("delete_group"), "Delete", class = "btn btn-outline-danger")
          ),
          p("Gate hierarchy is shown in the Workspace Tree. Synchronization applies edits to the selected group samples.")
        )
      } else if (identical(sel$type, "sample")) {
        e <- reg[[sel$id]]
        if (is.null(e)) return(helpText("Select a sample."))
        member_groups <- names(groups)[vapply(groups, function(g) sel$id %in% (g$sample_ids %||% character()), logical(1))]
        tagList(
          h4(e$name %||% sel$id),
          p(sprintf("%s events", format(e$cells %||% nrow(e$df), big.mark = ","))),
          tags$dl(tags$dt("Source file"), tags$dd(e$path %||% "Unavailable"),
            tags$dt("Groups"), tags$dd(paste(vapply(member_groups, function(id) groups[[id]]$name %||% id, character(1)), collapse = ", ")),
            tags$dt("Compensation"), tags$dd(e$compensation_name %||% "Not recorded"),
            tags$dt("Notes"), tags$dd(e$notes %||% "")),
          tags$details(tags$summary("Channel transforms"), tags$ul(lapply(names(e$wsp_axis_transforms %||% list()), function(ch) tags$li(paste(ch, e$wsp_axis_transforms[[ch]]$type, sep = ": ")))))
        )
      } else if (identical(sel$type, "gate")) {
        selected_gate_rows <- workspace_selected_gate_rows()
        if (!nrow(selected_gate_rows)) {
          selected_gate_rows <- data.frame(sample_id = sel$sample %||% "", gate_id = sel$id %||% "", stringsAsFactors = FALSE)
        }
        details <- Filter(Negate(is.null), lapply(seq_len(nrow(selected_gate_rows)), function(i) {
          workspace_gate_detail(selected_gate_rows$sample_id[[i]], selected_gate_rows$gate_id[[i]], reg, groups, gates)
        }))
        if (!length(details)) return(helpText("Select a gate."))
        gate_ids <- vapply(details, function(x) x$gate_id, character(1))

        if (length(details) > 1L) {
          return(tagList(
            div(class = "workspace-gate-detail-summary",
              h4(sprintf("%d selected gates", length(details))),
              div(class = "gate-node-meta", "Stacked gate details use sample metrics where available; group gates are shown as templates.")
            ),
            workspace_gate_bulk_actions(session, group_choices, gate_ids),
            div(class = "workspace-gate-detail-stack",
              lapply(details, workspace_gate_detail_block, session = session, group_choices = group_choices, rich = FALSE)
            )
          ))
        }

        detail <- details[[1]]
        g <- detail$gate
        tagList(
          workspace_gate_detail_block(detail, session, group_choices, rich = TRUE),
          if (identical(detail$state, "sample-modified")) div(class = "mt-2",
            div(class = "gate-state-badge sample-modified", "Sample modified"),
            div(class = "gate-node-meta", "This sample has a local gate edit."),
            div(class = "d-flex gap-2 mt-2",
              actionButton(session$ns("reset_sample_gate"), "Reset to group gate", class = "btn btn-outline-secondary btn-sm"),
              actionButton(session$ns("promote_sample_gate"), "Sync this edit to group", class = "btn btn-primary btn-sm")
            )
          ),
          workspace_gate_bulk_actions(session, group_choices, detail$gate_id)
        )
      } else {
        tagList(
          h4("Workspace"),
          p("Select a group, sample, or gate from the tree."),
          actionButton(session$ns("new_group_detail"), "Create Group", class = "btn btn-primary")
        )
      }
    })
    output$workspace_group_gates <- renderUI({
      gates <- shared$gating$gates()
      groups <- shared$gating$groups()
      sel <- selected()
      if (!identical(sel$type, "group")) return(NULL)
      gid <- sel$id
      ids <- names(gates)[vapply(gates, function(g) identical(g$group_id, gid) || identical(g$sync_group_id, gid), logical(1))]
      if (!length(ids)) {
        return(tags$div(
          class = "workspace-group-gate-tree empty",
          ondragover = "event.preventDefault(); this.classList.add('drop-target');",
          ondragleave = "this.classList.remove('drop-target');",
          ondrop = sprintf("event.preventDefault(); this.classList.remove('drop-target'); Shiny.setInputValue('%s',{gate:event.dataTransfer.getData('text/plain'),targetType:'group',targetId:'%s',nonce:Math.random()},{priority:'event'});", session$ns("workspace_drop"), gid),
          "Drop gates here to build this group's synced gate tree."
        ))
      }
      rows <- gate_rows_for_group(gates, ids)
      picked <- selected_rows()
      orphan_ids <- setdiff(ids, vapply(rows, `[[`, character(1), "id"))
      if (length(orphan_ids)) {
        rows <- c(rows, lapply(orphan_ids, function(id) list(id = id, depth = 0L)))
      }
      tags$div(
        class = "workspace-group-gate-tree",
        ondragover = "event.preventDefault(); this.classList.add('drop-target');",
        ondragleave = "this.classList.remove('drop-target');",
        ondrop = sprintf("event.preventDefault(); this.classList.remove('drop-target'); Shiny.setInputValue('%s',{gate:event.dataTransfer.getData('text/plain'),targetType:'group',targetId:'%s',nonce:Math.random()},{priority:'event'});", session$ns("workspace_drop"), gid),
        lapply(rows, function(row) {
          g <- gates[[row$id]]
          key <- workspace_row_key("gate", "group", row$id)
          state <- flowjo_gate_group_state(g)
          group <- groups[[g$group_id %||% g$sync_group_id %||% gid]]
          color <- safe_color(group$color %||% g$group_color %||% "#2563EB")
          div(
            class = paste("gate-nav-row gate-nav-gate workspace-drop-row workspace-select-row", if (key %in% picked) "selected" else "", state),
            style = sprintf("--group-color:%s", color),
            draggable = "true",
            ondragstart = workspace_drag_row_keys_js,
            `data-row-key` = key,
            `data-gate-id` = row$id,
            `data-sample-id` = "group",
            onclick = workspace_row_select_js(session$ns("workspace_select"), "gate", key, row$id, "group", row$id),
            span(class = "gate-tree-title", style = sprintf("padding-left:%dpx", row$depth * 16L), sprintf("%s [%s]", g$name, g$type)),
            span(class = "gate-node-meta",
              sprintf("%s / %s", g$x_channel, g$y_channel %||% ""),
              if (!is.null(group)) tagList(" · ", span(class = "gate-group-name", group$name %||% group$id))
            ),
            span(class = paste("gate-state-badge", state), sync_label(g))
          )
        })
      )
    })

    observeEvent(input$workspace_select, {
      msg <- input$workspace_select
      keys <- as.character(msg$keys %||% character())
      selected_rows(unique(keys[nzchar(keys)]))
      selected(list(type = msg$type, id = msg$id, sample = msg$sample %||% NULL))
      if (identical(msg$type, "group")) editing_group(msg$id)
      if (identical(msg$type, "sample")) shared$fcs$set_active(msg$id)
    }, ignoreInit = TRUE)

    observeEvent(selected(), {
      sel <- selected()
      if (!identical(sel$type, "group") && !identical(sel$type, "new_group")) return()
      groups <- shared$gating$groups()
      g <- if (!is.null(sel$id)) groups[[sel$id]] else NULL
      group_color_value(safe_color(g$color %||% "#2563EB"))
    }, ignoreInit = FALSE)

    observeEvent(input$group_color_apply, {
      group_color_value(safe_color(group_color_picker()$hex))
      updateActionButton(session, "group_color_toggle", label = NULL)
    }, ignoreInit = TRUE)

    observeEvent(input$group_color_cancel, {
      updateActionButton(session, "group_color_toggle", label = NULL)
    }, ignoreInit = TRUE)

    observeEvent(input$add_selected_basket, {
      added <- add_gate_pairs_to_basket(workspace_selected_gate_pairs())
      showNotification(sprintf("Added %d population%s to the basket.", added, if (added == 1L) "" else "s"), type = if (added) "message" else "warning")
    }, ignoreInit = TRUE)

    observeEvent(input$select_equivalent_gates, {
      parts <- workspace_selected_parts("gate")
      if (!length(parts)) {
        showNotification("Select a gate in a group first.", type = "warning")
        return()
      }
      current <- selected()
      anchor_idx <- which(vapply(parts, function(x) identical(x[[2]], current$sample %||% "") && identical(x[[3]], current$id %||% ""), logical(1)))
      anchor <- parts[[if (length(anchor_idx)) anchor_idx[[1]] else length(parts)]]
      group_id <- if (length(anchor) >= 4L) anchor[[4]] else ""
      group <- shared$gating$groups()[[group_id]]
      gates <- shared$gating$gates()
      if (is.null(group) || is.null(gates[[anchor[[3]]]])) {
        showNotification("Select a gate from the current group, not All Samples.", type = "warning")
        return()
      }
      path_names <- function(gid) {
        out <- character(); seen <- character()
        while (nzchar(gid %||% "") && !identical(gid, "root") && !(gid %in% seen) && !is.null(gates[[gid]])) {
          seen <- c(seen, gid); out <- c(gates[[gid]]$name %||% gid, out); gid <- gates[[gid]]$parent_id %||% "root"
        }
        out
      }
      group_parts <- Filter(function(x) length(x) >= 4L && identical(x[[4]], group_id), parts)
      wanted_paths <- lapply(group_parts, function(x) path_names(x[[3]]))
      wanted_paths <- wanted_paths[!duplicated(vapply(wanted_paths, function(x) paste(x, collapse = "\r"), character(1)))]
      keys <- character()
      reg <- shared$fcs$registry()
      for (sid in group$sample_ids %||% character()) {
        if (is.null(reg[[sid]])) next
        for (wanted in wanted_paths) {
          hits <- names(gates)[vapply(names(gates), function(gid) {
            isTRUE(flowjo_gate_applicability(reg[[sid]], gates[[gid]], gates)$ok) && identical(path_names(gid), wanted)
          }, logical(1))]
          if (length(hits)) keys <- c(keys, workspace_row_key("gate", sid, hits[[1]], group_id))
        }
      }
      selected_rows(unique(keys))
      session$sendCustomMessage("flowjo-workspace-selection", list(keys = unique(keys)))
      showNotification(sprintf("Selected %d equivalent gate%s.", length(unique(keys)), if (length(unique(keys)) == 1L) "" else "s"), type = "message")
    }, ignoreInit = TRUE)

    observeEvent(input$workspace_basket_drop, {
      pairs <- row_keys_to_gate_pairs(input$workspace_basket_drop$keys)
      message("[workspace-basket] drop_keys=", length(workspace_drag_ids(input$workspace_basket_drop$keys)), " parsed_pairs=", nrow(pairs))
      added <- add_gate_pairs_to_basket(pairs)
      showNotification(sprintf("Added %d population%s to the basket.", added, if (added == 1L) "" else "s"), type = if (added) "message" else "warning")
    }, ignoreInit = TRUE)

    observeEvent(input$workspace_new_group_drop, {
      sids <- row_keys_to_sample_ids(input$workspace_new_group_drop$keys)
      if (!length(sids)) return()
      new_group_sample_ids(sids)
      selected(list(type = "new_group", id = NULL))
      editing_group(NULL)
    }, ignoreInit = TRUE)

    observeEvent(input$workspace_drop, {
      msg <- input$workspace_drop
      gate_ids <- workspace_drag_ids(msg$gate)
      if (!length(gate_ids)) return()
      target_sample <- msg$targetSampleId
      if (identical(msg$targetType, "sample")) {
        if (!nzchar(target_sample %||% "")) return()
        copy_gates_to_sample(gate_ids, target_sample, parent_id = "root")
      } else if (identical(msg$targetType, "gate")) {
        if (!nzchar(target_sample %||% "")) return()
        if (msg$targetId %in% gate_subtree_ids(shared$gating$gates(), gate_ids)) return()
        copy_gates_to_sample(gate_ids, target_sample, parent_id = msg$targetId)
      } else if (identical(msg$targetType, "group")) {
        copy_gates_to_group(gate_ids, msg$targetId, msg$mode %||% NULL)
      }
    }, ignoreInit = TRUE)

    observeEvent(input$new_group, {
      new_group_sample_ids(NULL)
      selected(list(type = "new_group", id = NULL))
      editing_group(NULL)
    }, ignoreInit = TRUE)

    observeEvent(input$new_group_detail, {
      new_group_sample_ids(NULL)
      selected(list(type = "new_group", id = NULL))
      editing_group(NULL)
    }, ignoreInit = TRUE)

    observeEvent(input$save_group, {
      nm <- trimws(input$group_name %||% "")
      req(nzchar(nm), length(input$group_samples) > 0)
      gid <- editing_group()
      if (is.null(gid)) {
        gid <- shared$gating$add_group(nm, input$group_samples, group_color_value(), input$group_sync_enabled)
        selected(list(type = "group", id = gid))
        editing_group(gid)
        new_group_sample_ids(NULL)
      } else {
        shared$gating$update_group(gid, name = nm, sample_ids = input$group_samples, color = group_color_value(), sync_enabled = input$group_sync_enabled)
      }
    }, ignoreInit = TRUE)

    observeEvent(input$delete_group, {
      gid <- editing_group()
      req(gid)
      shared$gating$delete_group(gid)
      selected(list(type = "root", id = NULL))
      editing_group(NULL)
    }, ignoreInit = TRUE)

    observeEvent(input$open_workspace_sample, {
      msg <- input$open_workspace_sample
      ready_token <- paste0("workspace-open-", as.integer(Sys.time()), "-", sample.int(1e9, 1L))
      overlay_show("Opening Gating...", tip = "Preparing the selected sample.")
      isolate({
        shared$fcs$set_active(msg$sample)
        shared$gating$active_gate_id(NULL)
      })
      shared$ui$request_nav_route("gating_tab")
      overlay_hide_when_output_ready(
        "gating-canvas_wrap_left",
        ready_selector = ".gate-canvas-points[data-flowjo-canvas-ready='true']",
        ready_token = ready_token,
        timeout_ms = 120000L
      )
    }, ignoreInit = TRUE)

    observeEvent(input$open_workspace_gate, {
      msg <- input$open_workspace_gate
      ready_token <- paste0("workspace-open-", as.integer(Sys.time()), "-", sample.int(1e9, 1L))
      message("[workspace] open gate sample=", msg$sample %||% "<null>", " gate=", msg$gate %||% "<null>")
      overlay_show("Opening Gating...", tip = "Preparing the selected gate.")
      isolate({
        shared$fcs$set_active(msg$sample)
        shared$gating$active_gate_id(msg$gate)
      })
      shared$ui$request_nav_route("gating_tab")
      overlay_hide_when_output_ready(
        "gating-canvas_wrap_left",
        ready_selector = ".gate-canvas-points[data-flowjo-canvas-ready='true']",
        ready_token = ready_token,
        timeout_ms = 120000L
      )
    }, ignoreInit = TRUE)

    observeEvent(input$reset_sample_gate, {
      sel <- selected()
      req(identical(sel$type, "gate"), sel$id)
      ok <- shared$gating$reset_gate_to_group(sel$id)
      showNotification(
        if (isTRUE(ok)) "Reset this sample to the synced group gate." else "No synced group version was found for this gate.",
        type = if (isTRUE(ok)) "message" else "warning"
      )
    }, ignoreInit = TRUE)

    observeEvent(input$promote_sample_gate, {
      sel <- selected()
      req(identical(sel$type, "gate"), sel$id)
      ok <- shared$gating$promote_gate_to_group(sel$id)
      showNotification(
        if (isTRUE(ok)) "Synced this sample edit back to the group." else "This gate is not linked to a group.",
        type = if (isTRUE(ok)) "message" else "warning"
      )
    }, ignoreInit = TRUE)
  })
}
