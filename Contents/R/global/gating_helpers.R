# R/gating_helpers.R

if (!exists("%||%", mode = "function")) `%||%` <- function(a, b) if (!is.null(a)) a else b

flowjo_gate_id <- function(prefix = "gate") {
  paste0(prefix, "_", format(Sys.time(), "%Y%m%d%H%M%OS3"), "_", sprintf("%04d", sample.int(9999, 1)))
}

flowjo_channel_label <- function(channel_map, channel) {
  if (is.null(channel_map) || !nrow(channel_map)) return(channel)
  idx <- match(channel, channel_map$channel)
  marker <- if (!is.na(idx)) channel_map$marker[[idx]] else channel
  if (identical(marker, channel) || !nzchar(marker)) channel else sprintf("%s (%s)", marker, channel)
}

flowjo_channel_to_marker <- function(channel_map, channel) {
  if (is.null(channel_map) || !nrow(channel_map)) return(channel)
  idx <- match(channel, channel_map$channel)
  if (is.na(idx)) channel else channel_map$marker[[idx]]
}

flowjo_gate_axes <- function(entry) {
  cm <- entry$channel_map %||% data.frame(channel = entry$numeric_cols, marker = entry$numeric_cols)
  cm <- cm[cm$marker %in% entry$numeric_cols, , drop = FALSE]
  stats::setNames(cm$channel, vapply(cm$channel, flowjo_channel_label, character(1), channel_map = cm))
}

flowjo_gate_palette <- function(n) {
  if (n <= 0) return(character())
  grDevices::hcl.colors(max(n, 3L), palette = "Dark 3")[seq_len(n)]
}

flowjo_gate_color <- function(gate, index = 1L) {
  col <- toupper(gate$color %||% "")
  if (grepl("^#[0-9A-F]{6}$", col)) return(col)
  flowjo_gate_palette(max(index, 1L))[[max(index, 1L)]]
}

flowjo_make_gate <- function(name, type, parent_id, x_channel, y_channel = NULL, geometry = list(), sample_id = NULL, color = NULL) {
  list(
    id = flowjo_gate_id(),
    name = name,
    type = type,
    parent_id = parent_id %||% "root",
    x_channel = x_channel,
    y_channel = y_channel,
    geometry = geometry,
    color = toupper(color %||% flowjo_gate_palette(1)[[1]]),
    source_sample_id = sample_id,
    applied_sample_id = sample_id,
    created_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  )
}

flowjo_gate_applicability <- function(entry, gate, gates = list()) {
  if (is.null(entry) || is.null(gate)) return(list(ok = FALSE, status = "missing_sample"))
  if (!is.null(gate$applied_sample_id) && !identical(entry$id, gate$applied_sample_id)) {
    return(list(ok = FALSE, status = "not_applied"))
  }
  # Agent note: WSP paths are a union of sample trees; absent geometry means this population does not exist in this sample.
  if (!is.null(gate$wsp_geometry_by_sample) && is.null(gate$wsp_geometry_by_sample[[entry$id]])) {
    return(list(ok = FALSE, status = "not_applied"))
  }
  cm <- entry$channel_map
  cols <- colnames(entry$df)
  x_col <- flowjo_channel_to_marker(cm, gate$x_channel)
  y_col <- flowjo_channel_to_marker(cm, gate$y_channel)
  if (!(x_col %in% cols) || (!is.null(gate$y_channel) && !(y_col %in% cols))) {
    return(list(ok = FALSE, status = "missing_channel"))
  }
  parent_id <- gate$parent_id %||% "root"
  if (!identical(parent_id, "root") && is.null(gates[[parent_id]])) {
    return(list(ok = FALSE, status = "missing_parent"))
  }
  list(ok = TRUE, status = if (!is.null(gate$sync_group_id)) "synced" else "available")
}

flowjo_gate_children <- function(gates, parent_id = "root") {
  ids <- names(gates)
  ids[vapply(gates, function(g) identical(g$parent_id %||% "root", parent_id), logical(1))]
}

flowjo_gate_parent_id <- function(gates, gate_id) {
  if (is.null(gate_id) || !nzchar(gate_id %||% "")) return(NULL)
  gate <- gates[[gate_id]]
  if (is.null(gate)) return(NULL)
  gate$parent_id %||% "root"
}

flowjo_gate_same_path <- function(gates, name, parent_id = "root", sample_id = NULL) {
  ids <- names(gates)
  hit <- vapply(gates, function(g) {
    identical(g$name, name) &&
      identical(g$parent_id %||% "root", parent_id %||% "root") &&
      (is.null(sample_id) || is.null(g$applied_sample_id) || identical(g$applied_sample_id, sample_id))
  }, logical(1))
  ids[hit]
}

flowjo_gate_lineage_id <- function(gate) {
  gate$group_lineage_id %||% gate$source_gate_id %||% gate$id
}

flowjo_gate_group_state <- function(gate) {
  if (is.null(gate)) return("missing")
  if (!is.null(gate$sync_group_id)) return("synced")
  gate$group_state %||% if (!is.null(gate$group_id)) "group-owned" else if (!is.null(gate$applied_sample_id)) "sample" else "global"
}

flowjo_gate_peer_id <- function(gates, gate_id, sample_id) {
  gate <- gates[[gate_id]]
  group_id <- gate$sync_group_id %||% gate$group_id
  if (is.null(gate) || is.null(group_id) || is.null(sample_id)) return(NULL)
  key <- flowjo_gate_lineage_id(gate)
  hits <- names(gates)[vapply(gates, function(g) {
    identical(g$sync_group_id %||% g$group_id, group_id) &&
      identical(flowjo_gate_lineage_id(g), key) &&
      identical(g$applied_sample_id, sample_id)
  }, logical(1))]
  if (length(hits)) hits[[1]] else NULL
}

flowjo_prev_next_sample <- function(registry, gates, gate_id, current_sample_id, direction = 1L) {
  ids <- names(registry)
  if (!length(ids)) return(NULL)
  if (is.null(gate_id) || !nzchar(gate_id %||% "")) return(NULL)
  gate <- gates[[gate_id]]
  if (is.null(gate)) return(NULL)
  group_id <- gate$sync_group_id %||% gate$group_id
  sample_ids <- if (!is.null(group_id)) gate$sync_sample_ids %||% character() else ids
  if (!length(sample_ids) && !is.null(group_id)) {
    key <- flowjo_gate_lineage_id(gate)
    sample_ids <- unique(unlist(lapply(gates, function(g) {
      if (identical(g$sync_group_id %||% g$group_id, group_id) &&
          identical(flowjo_gate_lineage_id(g), key)) g$applied_sample_id else NULL
    }), use.names = FALSE))
  }
  sample_ids <- intersect(sample_ids, ids)
  if (!length(sample_ids)) sample_ids <- ids
  ok <- vapply(sample_ids, function(id) {
    peer_id <- flowjo_gate_peer_id(gates, gate_id, id)
    peer <- gates[[peer_id %||% gate_id]]
    isTRUE(flowjo_gate_applicability(registry[[id]], peer, gates)$ok)
  }, logical(1))
  ids <- sample_ids[ok]
  if (!length(ids)) return(NULL)
  idx <- match(current_sample_id, ids)
  if (is.na(idx)) idx <- if (direction > 0) 0L else length(ids) + 1L
  next_idx <- idx + if (direction > 0) 1L else -1L
  if (next_idx < 1L || next_idx > length(ids)) return(NULL)
  ids[[next_idx]]
}

flowjo_copy_gate <- function(gate, parent_id = gate$parent_id, sample_id = NULL, group_id = NULL, sync = FALSE, sync_sample_ids = NULL, group_color = NULL) {
  out <- gate
  lineage_id <- flowjo_gate_lineage_id(gate)
  out$id <- flowjo_gate_id()
  out$parent_id <- parent_id %||% "root"
  out$source_gate_id <- lineage_id
  out$group_lineage_id <- lineage_id
  out$applied_sample_id <- sample_id
  out$group_id <- group_id
  out$group_color <- toupper(group_color %||% out$group_color %||% "")
  out$group_state <- if (!is.null(group_id)) "group-owned" else if (!is.null(sample_id)) "sample" else out$group_state %||% "global"
  out$sync_group_id <- if (isTRUE(sync)) group_id else NULL
  out$sync_sample_ids <- if (isTRUE(sync)) sync_sample_ids %||% character() else NULL
  out$created_at <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  out
}

flowjo_gate_membership <- function(df, channel_map, gate, parent = NULL) {
  x_col <- flowjo_channel_to_marker(channel_map, gate$x_channel)
  y_col <- flowjo_channel_to_marker(channel_map, gate$y_channel)
  if (!(x_col %in% colnames(df))) return(rep(FALSE, nrow(df)))
  x <- df[[x_col]]
  y <- if (!is.null(gate$y_channel) && y_col %in% colnames(df)) df[[y_col]] else rep(NA_real_, length(x))
  g <- gate$geometry %||% list()
  keep <- rep(FALSE, nrow(df))

  if (identical(gate$type, "rectangle")) {
    keep <- x >= g$xmin & x <= g$xmax & y >= g$ymin & y <= g$ymax
  } else if (identical(gate$type, "range")) {
    keep <- x >= g$xmin & x <= g$xmax
  } else if (identical(gate$type, "polygon")) {
    keep <- flowjo_points_in_polygon(x, y, g$vertices)
  } else if (identical(gate$type, "ellipse")) {
    cx <- (g$xmin + g$xmax) / 2; cy <- (g$ymin + g$ymax) / 2
    rx <- abs(g$xmax - g$xmin) / 2; ry <- abs(g$ymax - g$ymin) / 2
    keep <- rx > 0 & ry > 0 & (((x - cx) / rx)^2 + ((y - cy) / ry)^2 <= 1)
  } else if (identical(gate$type, "quadrant") || identical(gate$type, "curved_quadrant")) {
    q <- g$quadrant %||% "upper_right"
    x_hi <- x >= g$x; y_hi <- y >= g$y
    keep <- switch(q,
      upper_right = x_hi & y_hi,
      upper_left = !x_hi & y_hi,
      lower_left = !x_hi & !y_hi,
      lower_right = x_hi & !y_hi,
      x_hi & y_hi
    )
  }

  keep[is.na(keep)] <- FALSE
  if (!is.null(parent)) keep <- keep & parent
  keep
}

flowjo_points_in_polygon <- function(x, y, vertices) {
  if (is.null(vertices) || nrow(vertices) < 3) return(rep(FALSE, length(x)))
  px <- vertices$x; py <- vertices$y
  inside <- rep(FALSE, length(x))
  j <- length(px)
  for (i in seq_along(px)) {
    crosses <- ((py[i] > y) != (py[j] > y)) &
      (x < (px[j] - px[i]) * (y - py[i]) / (py[j] - py[i] + .Machine$double.eps) + px[i])
    inside <- xor(inside, crosses)
    j <- i
  }
  inside
}

flowjo_gate_parent_membership <- function(gates, memberships, gate) {
  if (is.null(gate$parent_id) || identical(gate$parent_id, "root")) return(NULL)
  memberships[[gate$parent_id]]
}

flowjo_gate_stats <- function(df, keep, parent_keep = NULL) {
  n <- sum(keep, na.rm = TRUE)
  parent_n <- if (is.null(parent_keep)) nrow(df) else sum(parent_keep, na.rm = TRUE)
  data.frame(
    cells = n,
    parent_cells = parent_n,
    percent_parent = if (parent_n > 0) 100 * n / parent_n else NA_real_
  )
}

flowjo_gate_channel_match <- function(gate, x_channel, y_channel) {
  if (is.null(gate$y_channel) || is.null(y_channel)) return(identical(gate$x_channel, x_channel))
  identical(c(gate$x_channel, gate$y_channel), c(x_channel, y_channel)) ||
    identical(c(gate$x_channel, gate$y_channel), c(y_channel, x_channel))
}

flowjo_gate_for_plot_axes <- function(gate, x_channel, y_channel) {
  if (is.null(gate$y_channel) || is.null(y_channel) ||
      !identical(c(gate$x_channel, gate$y_channel), c(y_channel, x_channel))) return(gate)
  out <- gate
  g <- out$geometry %||% list()
  if (out$type %in% c("rectangle", "ellipse")) {
    out$geometry <- list(xmin = g$ymin, xmax = g$ymax, ymin = g$xmin, ymax = g$xmax)
  } else if (identical(out$type, "polygon") && !is.null(g$vertices)) {
    vs <- g$vertices
    out$geometry <- list(vertices = data.frame(x = vs$y, y = vs$x))
  } else if (out$type %in% c("quadrant", "curved_quadrant")) {
    q <- switch(g$quadrant %||% "upper_right",
      upper_left = "lower_right",
      lower_right = "upper_left",
      g$quadrant %||% "upper_right"
    )
    out$geometry <- modifyList(g, list(x = g$y, y = g$x, quadrant = q))
  }
  out
}

flowjo_gate_plot_groups <- function(df, channel_map, gates, x_channel, y_channel) {
  label <- rep("Ungated", nrow(df))
  if (!length(gates) || !nrow(df)) return(factor(label, levels = "Ungated"))
  memberships <- list()
  for (gate in gates) {
    if (!flowjo_gate_channel_match(gate, x_channel, y_channel)) next
    parent <- flowjo_gate_parent_membership(gates, memberships, gate)
    keep <- flowjo_gate_membership(df, channel_map, gate, parent = parent)
    memberships[[gate$id]] <- keep
    label[keep] <- gate$name
  }
  factor(label, levels = unique(c("Ungated", vapply(gates, `[[`, character(1), "name"))))
}
