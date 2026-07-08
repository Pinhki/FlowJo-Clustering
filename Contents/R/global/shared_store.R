# R/shared_store.R
make_shared_store <- function(session) {
  # global state
  df_raw            <- reactiveVal(NULL)
  df_processed      <- reactiveVal(NULL)
  numeric_choices   <- reactiveVal(NULL)
  facet_choices     <- reactiveVal(NULL)
  axis_pairs_val    <- reactiveVal(data.frame(x=character(), y=character()))
  transform_settings<- reactiveVal(list())
  scale_settings    <- reactiveVal(list())
  auto_scale_settings <- reactiveVal(list())
  feature_scale_setting <- reactiveVal(list(type = "none"))
  
  flowsom_df        <- reactiveVal(NULL)  # either merged CSV or sent from UMAP page
  flowsom_order     <- reactiveVal(NULL)
  
  umap_fsom_df      <- reactiveVal(NULL)
  umap_fsom_params  <- reactiveVal(NULL)
  fcs_registry      <- reactiveVal(list())
  active_fcs_id     <- reactiveVal(NULL)
  auto_switch_umap  <- reactiveVal(FALSE)

  preprocess_result <- reactiveVal(NULL)
  isolated_pops     <- reactiveVal(NULL)
  concat_object     <- reactiveVal(NULL)
  concat_data       <- reactiveVal(NULL)
  concat_markers    <- reactiveVal(NULL)
  cluster_result    <- reactiveVal(NULL)
  parity_result     <- reactiveVal(NULL)
  analysis_objects  <- reactiveVal(list())
  active_analysis_id <- reactiveVal(NULL)
  gate_groups       <- reactiveVal(list())
  gate_tree         <- reactiveVal(list())
  gate_memberships  <- reactiveVal(list())
  gate_label_positions <- reactiveVal(list())
  gate_active_id    <- reactiveVal(NULL)
  gate_sync_status  <- reactiveVal(data.frame())
  gate_wsp_report   <- reactiveVal(NULL)
  population_items  <- reactiveVal(list())
  
  # shared plot params (reactive; synced from sidebar)
  params <- list(
    plot_theme      = reactiveVal("UMAP Arrows"),
    point_size      = reactiveVal(1),
    point_alpha     = reactiveVal(0.8),
    point_shape     = reactiveVal(19),
    point_stroke    = reactiveVal(0),
    color_map       = reactiveVal("color"),
    low_color       = reactiveVal("#2300AD"),
    high_color      = reactiveVal("#FF8D0A"),
    use_mid_color   = reactiveVal(FALSE),
    mid_color       = reactiveVal("white"),
    title           = reactiveVal(""),
    xlab            = reactiveVal(""),
    ylab            = reactiveVal(""),
    title_size      = reactiveVal(20),
    axis_title_size = reactiveVal(14),
    axis_text_size  = reactiveVal(12)
  )
  
  # choices bundle
  choices <- list(
    numeric = numeric_choices,
    facet   = facet_choices
  )

  set_active_from_entry <- function(entry) {
    stopifnot(is.list(entry), !is.null(entry$df))
    df <- entry$df
    df_raw(df)
    numeric_choices(entry$numeric_cols)
    facet_choices(entry$facet_cols)
    axis_pairs_val(entry$axis_pairs)
    df_processed(NULL)
    flowsom_df(NULL)
    flowsom_order(NULL)
    umap_fsom_df(NULL)
    umap_fsom_params(NULL)
    preprocess_result(NULL)
    isolated_pops(NULL)
    concat_object(NULL)
    concat_data(NULL)
    concat_markers(NULL)
    cluster_result(NULL)
    parity_result(NULL)
    gate_active_id(NULL)
    active_fcs_id(entry$id)
  }

  has_umap_cols <- function(entry) {
    cols <- unique(c(entry$numeric_cols %||% character(), colnames(entry$df) %||% character()))
    any(grepl("UMAP", cols, ignore.case = TRUE))
  }

  has_advanced_gating <- function() {
    gates <- gate_tree()
    wsp <- gate_wsp_report()
    length(gates) > 0 || (!is.null(wsp) && length(wsp$populations %||% character()) > 0)
  }

  should_land_on_gating <- function(entry) {
    !has_umap_cols(entry) && !has_advanced_gating()
  }

  fcs_add <- function(entry, activate = TRUE) {
    reg <- fcs_registry()
    reg[[entry$id]] <- entry
    fcs_registry(reg)
    if (isTRUE(activate)) {
      set_active_from_entry(entry)
      auto_switch_umap(should_land_on_gating(entry))
    }
  }

  fcs_add_many <- function(entries) {
    entries <- entries[lengths(entries) > 0]
    if (!length(entries)) return(invisible(FALSE))
    reg <- fcs_registry()
    for (entry in entries) reg[[entry$id]] <- entry
    fcs_registry(reg)
    set_active_from_entry(entries[[length(entries)]])
    auto_switch_umap(should_land_on_gating(entries[[length(entries)]]))
    invisible(TRUE)
  }

  fcs_remove <- function(id) {
    reg <- fcs_registry()
    if (is.null(reg[[id]])) return(invisible(FALSE))
    was_active <- identical(active_fcs_id(), id)
    reg[[id]] <- NULL
    fcs_registry(reg)
    if (was_active) {
      if (length(reg) > 0) {
        set_active_from_entry(reg[[1]])
      } else {
        active_fcs_id(NULL)
        df_raw(NULL)
        df_processed(NULL)
        numeric_choices(NULL)
        facet_choices(NULL)
        axis_pairs_val(data.frame(x=character(), y=character()))
        flowsom_df(NULL)
        flowsom_order(NULL)
        umap_fsom_df(NULL)
        umap_fsom_params(NULL)
        preprocess_result(NULL)
        isolated_pops(NULL)
        concat_object(NULL)
        concat_data(NULL)
        concat_markers(NULL)
        cluster_result(NULL)
        parity_result(NULL)
        gate_active_id(NULL)
      }
    }
    invisible(TRUE)
  }

  fcs_rename <- function(id, new_name) {
    reg <- fcs_registry()
    if (is.null(reg[[id]])) return(invisible(FALSE))
    reg[[id]]$name <- new_name
    fcs_registry(reg)
    invisible(TRUE)
  }

  fcs_set_notes <- function(id, new_notes) {
    reg <- fcs_registry()
    if (is.null(reg[[id]])) return(invisible(FALSE))
    reg[[id]]$notes <- new_notes
    fcs_registry(reg)
    invisible(TRUE)
  }

  fcs_set_active <- function(id) {
    reg <- fcs_registry()
    if (is.null(reg[[id]])) return(invisible(FALSE))
    set_active_from_entry(reg[[id]])
    invisible(TRUE)
  }

  clear_auto_switch_umap <- function() {
    auto_switch_umap(FALSE)
    invisible(TRUE)
  }

  fcs_table <- function() {
    reg <- fcs_registry()
    if (!length(reg)) {
      return(data.frame(
        id = character(), name = character(), path = character(),
        cells = integer(), channels = integer(), notes = character(),
        loaded_at = character(), is_active = logical(),
        stringsAsFactors = FALSE
      ))
    }
    out <- lapply(reg, function(e) {
      data.frame(
        id = e$id,
        name = e$name,
        path = e$path,
        cells = as.integer(e$cells),
        channels = as.integer(e$channels),
        notes = e$notes %||% "",
        loaded_at = e$loaded_at,
        is_active = identical(active_fcs_id(), e$id),
        stringsAsFactors = FALSE
      )
    })
    do.call(rbind, out)
  }

  analysis_make_id <- function(prefix = "ao") {
    paste(prefix, format(Sys.time(), "%Y%m%d%H%M%OS3"), sample.int(99999, 1), sep = "_")
  }

  analysis_numeric_cols <- function(df) {
    if (is.null(df) || !ncol(df)) return(character(0))
    names(df)[vapply(df, is.numeric, logical(1))]
  }

  analysis_source_data <- function(source = c("fcs", "batch")) {
    source <- match.arg(source)
    if (identical(source, "batch")) {
      dat <- concat_data()
      mk <- concat_markers() %||% analysis_numeric_cols(dat)
      return(list(data = dat, markers = intersect(mk, names(dat)), badge = "Batch", label = "Batch Concatenation"))
    }
    dat <- df_raw()
    mk <- analysis_numeric_cols(dat)
    list(data = dat, markers = mk, badge = "FCS", label = "Current FCS")
  }

  analysis_validate_source <- function(src) {
    dat <- src$data
    if (is.null(dat) || !nrow(dat)) stop("Analysis object needs event data.")
    markers <- intersect(src$markers %||% character(0), names(dat))
    markers <- markers[vapply(dat[markers], is.numeric, logical(1))]
    if (length(markers) < 2) stop("Analysis object needs at least two numeric marker columns.")
    markers
  }

  analysis_unique_name <- function(name, exclude_id = NULL) {
    name <- trimws(name %||% "")
    if (!nzchar(name)) stop("Analysis object name is required.")
    objs <- analysis_objects()
    names_now <- vapply(objs[setdiff(names(objs), exclude_id %||% character(0))], function(x) x$name %||% "", character(1))
    if (name %in% names_now) stop("Analysis object name must be unique.")
    name
  }

  analysis_create <- function(name, source = c("fcs", "batch"), notes = "") {
    name <- analysis_unique_name(name)
    src <- analysis_source_data(match.arg(source))
    markers <- analysis_validate_source(src)
    selected_markers <- flowjo_default_cluster_markers(markers)
    id <- analysis_make_id()
    obj <- list(
      id = id,
      name = name,
      source_badge = src$badge,
      source_label = src$label,
      created_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
      notes = notes %||% "",
      data = as.data.frame(src$data, stringsAsFactors = FALSE),
      markers = markers,
      selected_markers = selected_markers,
      feature_settings = list(),
      cluster_settings = list(seed = 42L, neighbors = 15L, min_dist = 0.3, metric = "euclidean", scale = TRUE, xdim = 10L, ydim = 10L, rlen = 10L, k = 20L),
      runs = list(),
      active_run_id = NULL,
      provenance = list(events = nrow(src$data), columns = ncol(src$data), markers = length(markers))
    )
    objs <- analysis_objects()
    objs[[id]] <- obj
    analysis_objects(objs)
    active_analysis_id(id)
    invisible(obj)
  }

  analysis_active <- function() {
    id <- active_analysis_id()
    if (is.null(id) || !nzchar(id)) return(NULL)
    analysis_objects()[[id]]
  }

  analysis_set_active <- function(id) {
    if (is.null(analysis_objects()[[id]])) return(invisible(FALSE))
    active_analysis_id(id)
    invisible(TRUE)
  }

  analysis_delete <- function(id = active_analysis_id()) {
    objs <- analysis_objects()
    if (is.null(id) || is.null(objs[[id]])) return(invisible(FALSE))
    objs[[id]] <- NULL
    analysis_objects(objs)
    if (identical(active_analysis_id(), id)) active_analysis_id(names(objs)[1] %||% NULL)
    invisible(TRUE)
  }

  analysis_add_run <- function(object_id = active_analysis_id(), name, method, result, params = list(), notes = "") {
    objs <- analysis_objects()
    obj <- objs[[object_id]]
    if (is.null(obj)) stop("Select an analysis object first.")
    name <- trimws(name %||% "")
    if (!nzchar(name)) stop("Run name is required.")
    run_names <- vapply(obj$runs %||% list(), function(x) x$name %||% "", character(1))
    if (name %in% run_names) stop("Run name must be unique within this object.")
    run_id <- analysis_make_id("run")
    obj$runs[[run_id]] <- list(
      id = run_id,
      name = name,
      method = method,
      created_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
      notes = notes %||% "",
      params = params,
      result = result
    )
    obj$active_run_id <- run_id
    objs[[object_id]] <- obj
    analysis_objects(objs)
    invisible(run_id)
  }

  analysis_set_active_run <- function(object_id = active_analysis_id(), run_id) {
    objs <- analysis_objects()
    obj <- objs[[object_id]]
    if (is.null(obj) || is.null(obj$runs[[run_id]])) return(invisible(FALSE))
    obj$active_run_id <- run_id
    objs[[object_id]] <- obj
    analysis_objects(objs)
    invisible(TRUE)
  }

  analysis_export_bundle <- function(object_id = active_analysis_id(), file) {
    obj <- analysis_objects()[[object_id]]
    if (is.null(obj)) stop("Select an analysis object first.")
    tmp <- tempfile("analysis_bundle_")
    dir.create(tmp, recursive = TRUE, showWarnings = FALSE)
    saveRDS(list(format = "flowjo_analysis_bundle_v1", object = obj), file.path(tmp, "analysis_object.rds"))
    utils::write.csv(data.frame(
      object = obj$name,
      source = obj$source_badge %||% "",
      events = nrow(obj$data),
      markers = length(obj$markers %||% character(0)),
      stringsAsFactors = FALSE
    ), file.path(tmp, "metadata.csv"), row.names = FALSE)
    old <- getwd()
    on.exit(setwd(old), add = TRUE)
    setwd(tmp)
    utils::zip(zipfile = file, files = list.files(tmp, all.files = FALSE))
  }

  analysis_import_bundle <- function(path, name = NULL) {
    tmp <- tempfile("analysis_import_")
    dir.create(tmp, recursive = TRUE, showWarnings = FALSE)
    utils::unzip(path, exdir = tmp)
    obj <- readRDS(file.path(tmp, "analysis_object.rds"))
    if (!is.list(obj) || !identical(obj$format, "flowjo_analysis_bundle_v1") || !is.list(obj$object)) {
      stop("Analysis bundle is not valid.")
    }
    ao <- obj$object
    ao$name <- analysis_unique_name(name %||% ao$name)
    ao$id <- analysis_make_id()
    ao$source_badge <- "Bundle"
    objs <- analysis_objects()
    objs[[ao$id]] <- ao
    analysis_objects(objs)
    active_analysis_id(ao$id)
    invisible(ao)
  }

  gate_recompute <- function(gate_ids = names(gate_tree()), sample_ids = names(fcs_registry())) {
    gates <- gate_tree()
    reg <- fcs_registry()
    memberships <- gate_memberships()
    stats_rows <- list()
    sync_rows <- list()

    for (sid in intersect(sample_ids, names(reg))) {
      e <- reg[[sid]]
      settings <- transform_settings()
      for (gate in gates) {
        for (channel in c(gate$x_channel, gate$y_channel)) {
          if (is.null(channel) || !nzchar(channel)) next
          col <- flowjo_channel_to_marker(e$channel_map, channel)
          if (is.null(col) || !nzchar(col)) next
          if (is.null(settings[[col]])) {
            setting <- flowjo_axis_scale_setting(flowjo_default_axis_scale_type(channel), t = flowjo_channel_range(e, channel, col))
            if (!is.null(setting)) settings[[col]] <- setting
          } else if (identical(settings[[col]]$type %||% NULL, "biex") && is.null(settings[[col]]$t)) {
            settings[[col]]$t <- flowjo_channel_range(e, channel, col) %||% 262144
          }
        }
      }
      gate_df <- e$df
      sample_memberships <- memberships[[sid]] %||% list()
      for (gid in gate_ids) {
        gate <- gates[[gid]]
        if (is.null(gate)) next
        app <- flowjo_gate_applicability(e, gate, gates)
        if (!isTRUE(app$ok)) {
          sync_rows[[length(sync_rows) + 1]] <- data.frame(sample_id = sid, gate_id = gid, status = app$status, stringsAsFactors = FALSE)
          next
        }
        x_col <- flowjo_channel_to_marker(e$channel_map, gate$x_channel)
        y_col <- flowjo_channel_to_marker(e$channel_map, gate$y_channel)
        missing <- is.null(x_col) || !(x_col %in% colnames(gate_df)) ||
          (!is.null(gate$y_channel) && (is.null(y_col) || !(y_col %in% colnames(gate_df))))
        if (missing) {
          sync_rows[[length(sync_rows) + 1]] <- data.frame(sample_id = sid, gate_id = gid, status = "missing_channel", stringsAsFactors = FALSE)
          next
        }
        parent <- flowjo_gate_parent_membership(gates, sample_memberships, gate)
        keep <- flowjo_gate_membership(gate_df, e$channel_map, gate, parent = parent)
        sample_memberships[[gid]] <- keep
        st <- flowjo_gate_stats(gate_df, keep, parent)
        stats_rows[[length(stats_rows) + 1]] <- data.frame(
          sample_id = sid,
          sample_name = e$name %||% sid,
          gate_id = gid,
          gate_name = gate$name,
          gate_type = gate$type,
          st,
          stringsAsFactors = FALSE
        )
        sync_rows[[length(sync_rows) + 1]] <- data.frame(sample_id = sid, gate_id = gid, status = "applied", stringsAsFactors = FALSE)
      }
      memberships[[sid]] <- sample_memberships
    }

    gate_memberships(memberships)
    gate_sync_status(if (length(sync_rows)) do.call(rbind, sync_rows) else data.frame())
    if (length(stats_rows)) do.call(rbind, stats_rows) else data.frame()
  }

  gate_stats <- reactive({
    gates <- gate_tree()
    if (!length(gates)) {
      return(data.frame(
        sample_id = character(), sample_name = character(), gate_id = character(),
        gate_name = character(), gate_type = character(), cells = integer(),
        parent_cells = integer(), percent_parent = numeric(),
        stringsAsFactors = FALSE
      ))
    }
    gate_recompute(names(gates), active_fcs_id())
  })

  gate_add <- function(gate) {
    gates <- gate_tree()
    gates[[gate$id]] <- gate
    gate_tree(gates)
    gate_recompute(gate$id, active_fcs_id())
    invisible(gate$id)
  }

  gate_remove <- function(gate_id) {
    gates <- gate_tree()
    gates[[gate_id]] <- NULL
    gate_tree(gates)
    memberships <- gate_memberships()
    memberships <- lapply(memberships, function(x) { x[[gate_id]] <- NULL; x })
    gate_memberships(memberships)
    if (identical(gate_active_id(), gate_id)) gate_active_id(NULL)
    invisible(TRUE)
  }

  gate_add_group <- function(name, sample_ids, color = "#2563EB", sync_enabled = FALSE) {
    groups <- gate_groups()
    id <- flowjo_gate_id("group")
    groups[[id]] <- list(
      id = id,
      name = name,
      color = toupper(color %||% "#2563EB"),
      sample_ids = unique(sample_ids),
      sync_enabled = isTRUE(sync_enabled),
      created_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    )
    gate_groups(groups)
    invisible(id)
  }

  gate_update_group <- function(group_id, name = NULL, sample_ids = NULL, color = NULL, sync_enabled = NULL) {
    groups <- gate_groups()
    if (is.null(groups[[group_id]])) return(invisible(FALSE))
    if (!is.null(name)) groups[[group_id]]$name <- name
    if (!is.null(sample_ids)) groups[[group_id]]$sample_ids <- unique(sample_ids)
    if (!is.null(color)) groups[[group_id]]$color <- toupper(color)
    if (!is.null(sync_enabled)) groups[[group_id]]$sync_enabled <- isTRUE(sync_enabled)
    gate_groups(groups)

    gates <- gate_tree()
    changed <- FALSE
    for (gid in names(gates)) {
      if (identical(gates[[gid]]$sync_group_id, group_id)) {
        gates[[gid]]$sync_sample_ids <- groups[[group_id]]$sample_ids %||% character()
        gates[[gid]]$group_color <- groups[[group_id]]$color %||% gates[[gid]]$group_color
        changed <- TRUE
      } else if (identical(gates[[gid]]$group_id, group_id)) {
        gates[[gid]]$group_color <- groups[[group_id]]$color %||% gates[[gid]]$group_color
        changed <- TRUE
      }
    }
    if (changed) gate_tree(gates)
    invisible(TRUE)
  }

  gate_delete_group <- function(group_id) {
    groups <- gate_groups()
    if (is.null(groups[[group_id]])) return(invisible(FALSE))
    groups[[group_id]] <- NULL
    gate_groups(groups)
    gates <- gate_tree()
    changed <- FALSE
    for (gid in names(gates)) {
      if (identical(gates[[gid]]$sync_group_id, group_id)) {
        gates[[gid]]$sync_group_id <- NULL
        gates[[gid]]$sync_sample_ids <- NULL
        gates[[gid]]$group_state <- "sample-modified"
        changed <- TRUE
      }
      if (identical(gates[[gid]]$group_id, group_id)) {
        gates[[gid]]$group_id <- NULL
        gates[[gid]]$group_color <- NULL
        changed <- TRUE
      }
    }
    if (changed) gate_tree(gates)
    invisible(TRUE)
  }

  gate_sync_to_group <- function(gate_id, group_id) {
    gates <- gate_tree()
    groups <- gate_groups()
    if (is.null(gates[[gate_id]]) || is.null(groups[[group_id]])) return(invisible(FALSE))
    lineage_id <- flowjo_gate_lineage_id(gates[[gate_id]])
    gates[[gate_id]]$source_gate_id <- lineage_id
    gates[[gate_id]]$group_lineage_id <- lineage_id
    gates[[gate_id]]$group_id <- group_id
    gates[[gate_id]]$group_color <- groups[[group_id]]$color %||% gates[[gate_id]]$group_color
    gates[[gate_id]]$group_state <- "group-owned"
    gates[[gate_id]]$sync_group_id <- group_id
    gates[[gate_id]]$sync_sample_ids <- groups[[group_id]]$sample_ids
    gate_tree(gates)
    gate_recompute(gate_id, groups[[group_id]]$sample_ids %||% active_fcs_id())
    invisible(TRUE)
  }

  gate_group_peer_id <- function(gates, gate_id) {
    gate <- gates[[gate_id]]
    group_id <- gate$sync_group_id %||% gate$group_id
    key <- flowjo_gate_lineage_id(gate)
    hits <- names(gates)[vapply(gates, function(g) {
      identical(g$sync_group_id, group_id) && identical(flowjo_gate_lineage_id(g), key)
    }, logical(1))]
    others <- setdiff(hits, gate_id)
    if (length(others)) others[[1]] else if (length(hits)) hits[[1]] else NULL
  }

  gate_reset_to_group <- function(gate_id) {
    gates <- gate_tree()
    groups <- gate_groups()
    gate <- gates[[gate_id]]
    if (is.null(gate)) return(invisible(FALSE))
    group_id <- gate$sync_group_id %||% gate$group_id
    peer_id <- gate_group_peer_id(gates, gate_id)
    if (is.null(group_id) || is.null(peer_id) || is.null(gates[[peer_id]])) return(invisible(FALSE))
    keep <- gates[[gate_id]][c("id", "parent_id", "applied_sample_id", "created_at")]
    gates[[gate_id]] <- gates[[peer_id]]
    gates[[gate_id]]$id <- keep$id
    gates[[gate_id]]$parent_id <- keep$parent_id %||% gates[[peer_id]]$parent_id
    gates[[gate_id]]$applied_sample_id <- keep$applied_sample_id
    gates[[gate_id]]$created_at <- keep$created_at %||% gates[[peer_id]]$created_at
    gates[[gate_id]]$group_id <- group_id
    gates[[gate_id]]$sync_group_id <- group_id
    gates[[gate_id]]$sync_sample_ids <- groups[[group_id]]$sample_ids %||% gates[[peer_id]]$sync_sample_ids
    gates[[gate_id]]$group_state <- "group-owned"
    gate_tree(gates)
    gate_recompute(gate_id, gates[[gate_id]]$applied_sample_id %||% active_fcs_id())
    invisible(TRUE)
  }

  gate_promote_to_group <- function(gate_id) {
    gates <- gate_tree()
    groups <- gate_groups()
    gate <- gates[[gate_id]]
    if (is.null(gate)) return(invisible(FALSE))
    group_id <- gate$sync_group_id %||% gate$group_id
    if (is.null(group_id)) return(invisible(FALSE))
    gates[[gate_id]]$sync_group_id <- group_id
    gates[[gate_id]]$sync_sample_ids <- groups[[group_id]]$sample_ids %||% gate$sync_sample_ids
    gates[[gate_id]]$group_state <- "group-owned"
    key <- flowjo_gate_lineage_id(gates[[gate_id]])
    peers <- names(gates)[vapply(gates, function(g) {
      identical(g$sync_group_id, group_id) && identical(flowjo_gate_lineage_id(g), key)
    }, logical(1))]
    for (pid in peers) {
      keep <- gates[[pid]][c("id", "parent_id", "applied_sample_id", "created_at")]
      gates[[pid]] <- gates[[gate_id]]
      gates[[pid]]$id <- keep$id
      gates[[pid]]$parent_id <- keep$parent_id %||% gates[[gate_id]]$parent_id
      gates[[pid]]$applied_sample_id <- keep$applied_sample_id
      gates[[pid]]$created_at <- keep$created_at %||% gates[[gate_id]]$created_at
      gates[[pid]]$group_state <- "group-owned"
    }
    gate_tree(gates)
    gate_recompute(peers, groups[[group_id]]$sample_ids %||% active_fcs_id())
    invisible(TRUE)
  }

  population_clear_outputs <- function() {
    concat_object(NULL); concat_data(NULL); concat_markers(NULL); cluster_result(NULL)
  }

  population_add <- function(sample_ids, gate_ids, set_name = "Population Set", label = NULL, volume_uL = NA_real_) {
    reg <- fcs_registry(); gates <- gate_tree(); stats <- gate_stats()
    out <- population_items(); added <- 0L
    for (sid in intersect(sample_ids, names(reg))) for (gid in intersect(gate_ids, names(gates))) {
      if (!any(stats$sample_id == sid & stats$gate_id == gid, na.rm = TRUE)) next
      dup <- vapply(out, function(x) identical(x$sample_id, sid) && identical(x$gate_id, gid) && identical(x$set_name, set_name), logical(1))
      if (any(dup)) next
      item <- list(id = flowjo_gate_id("pop"), sample_id = sid, gate_id = gid, set_name = set_name,
        label = label %||% gates[[gid]]$name %||% gid, volume_uL = suppressWarnings(as.numeric(volume_uL %||% NA_real_)),
        include = TRUE, created_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
      out[[item$id]] <- item; added <- added + 1L
    }
    population_items(out); population_clear_outputs(); invisible(added)
  }

  population_update <- function(id, include = NULL, label = NULL, set_name = NULL, volume_uL = NULL) {
    items <- population_items()
    if (is.null(items[[id]])) return(invisible(FALSE))
    if (!is.null(include)) items[[id]]$include <- isTRUE(include)
    if (!is.null(label)) items[[id]]$label <- label
    if (!is.null(set_name)) items[[id]]$set_name <- set_name
    if (!is.null(volume_uL)) items[[id]]$volume_uL <- suppressWarnings(as.numeric(volume_uL))
    population_items(items); population_clear_outputs(); invisible(TRUE)
  }

  population_remove <- function(id) {
    items <- population_items(); items[[id]] <- NULL
    population_items(items); population_clear_outputs(); invisible(TRUE)
  }

  population_table <- reactive({
    items <- population_items()
    if (!length(items)) return(data.frame(id = character(), include = logical(), population_set = character(), label = character(), sample_name = character(), gate_name = character(), events = integer(), percent_parent = numeric(), sync_status = character(), volume_uL = numeric(), stringsAsFactors = FALSE))
    reg <- fcs_registry(); gates <- gate_tree(); stats <- gate_stats()
    do.call(rbind, lapply(items, function(x) {
      gate <- gates[[x$gate_id]]
      st <- stats[stats$sample_id == x$sample_id & stats$gate_id == x$gate_id, , drop = FALSE]
      data.frame(id = x$id, include = isTRUE(x$include), population_set = x$set_name %||% "Population Set",
        label = x$label %||% gate$name %||% x$gate_id, sample_name = reg[[x$sample_id]]$name %||% x$sample_id,
        gate_name = gate$name %||% x$gate_id, events = if (nrow(st)) st$cells[[1]] else 0L,
        percent_parent = if (nrow(st)) st$percent_parent[[1]] else NA_real_,
        sync_status = if (!is.null(gate$sync_group_id)) "synced" else if (!is.null(gate$applied_sample_id)) "sample" else "global",
        volume_uL = suppressWarnings(as.numeric(x$volume_uL %||% NA_real_)), stringsAsFactors = FALSE)
    }))
  })
  
  # export/import state for Save/Load
  export_state <- function() {
    wsp_state <- gate_wsp_report()
    if (!is.null(wsp_state)) wsp_state$gating_set <- NULL
    list(
      params = lapply(params, function(rv) rv()),
      transform_settings = transform_settings(),
      scale_settings     = scale_settings(),
      auto_scale_settings = auto_scale_settings(),
      feature_scale_setting = feature_scale_setting(),
      fcs_registry = fcs_registry(),
      active_fcs_id = active_fcs_id(),
      preprocess_result = preprocess_result(),
      isolated_populations = isolated_pops(),
      concat_object = concat_object(),
      concat_data = concat_data(),
      concat_markers = concat_markers(),
      cluster_result = cluster_result(),
      analysis_objects = analysis_objects(),
      active_analysis_id = active_analysis_id(),
      parity_result = parity_result(),
      gate_groups = gate_groups(),
      gate_tree = gate_tree(),
      gate_label_positions = gate_label_positions(),
      gate_memberships = gate_memberships(),
      gate_sync_status = gate_sync_status(),
      gate_wsp_report = wsp_state,
      population_items = population_items()
    )
  }
  import_state <- function(obj) {
    if (!is.null(obj$params)) {
      for (nm in intersect(names(params), names(obj$params))) params[[nm]](obj$params[[nm]])
    }
    if (!is.null(obj$transform_settings)) transform_settings(obj$transform_settings)
    if (!is.null(obj$scale_settings))     scale_settings(obj$scale_settings)
    if (!is.null(obj$auto_scale_settings)) auto_scale_settings(obj$auto_scale_settings)
    if (!is.null(obj$feature_scale_setting)) feature_scale_setting(obj$feature_scale_setting)
    if (!is.null(obj$preprocess_result)) preprocess_result(obj$preprocess_result)
    if (!is.null(obj$isolated_populations)) isolated_pops(obj$isolated_populations)
    if (!is.null(obj$concat_object)) concat_object(obj$concat_object)
    if (!is.null(obj$concat_data)) concat_data(obj$concat_data)
    if (!is.null(obj$concat_markers)) concat_markers(obj$concat_markers)
    if (!is.null(obj$cluster_result)) cluster_result(obj$cluster_result)
    if (!is.null(obj$analysis_objects) && is.list(obj$analysis_objects)) analysis_objects(obj$analysis_objects)
    if (!is.null(obj$active_analysis_id) && !is.null((obj$analysis_objects %||% list())[[obj$active_analysis_id]])) active_analysis_id(obj$active_analysis_id)
    if (!is.null(obj$parity_result)) parity_result(obj$parity_result)
    if (!is.null(obj$gate_groups)) gate_groups(obj$gate_groups)
    if (!is.null(obj$gate_tree)) gate_tree(obj$gate_tree)
    if (!is.null(obj$gate_label_positions)) gate_label_positions(obj$gate_label_positions)
    if (!is.null(obj$gate_memberships)) gate_memberships(obj$gate_memberships)
    if (!is.null(obj$gate_sync_status)) gate_sync_status(obj$gate_sync_status)
    if (!is.null(obj$gate_wsp_report)) gate_wsp_report(obj$gate_wsp_report)
    if (!is.null(obj$population_items)) population_items(obj$population_items)
    if (!is.null(obj$fcs_registry) && is.list(obj$fcs_registry)) {
      fcs_registry(obj$fcs_registry)
      if (!is.null(obj$active_fcs_id) && !is.null(obj$fcs_registry[[obj$active_fcs_id]])) {
        set_active_from_entry(obj$fcs_registry[[obj$active_fcs_id]])
      } else if (length(obj$fcs_registry) > 0) {
        set_active_from_entry(obj$fcs_registry[[1]])
      }
    }
  }
  
  list(
    session = session,
    df = list(raw = df_raw, processed = df_processed),
    choices = choices,
    axis_pairs = axis_pairs_val,
    transforms = list(settings = transform_settings, scales = scale_settings, auto_scales = auto_scale_settings, feature_scale = feature_scale_setting),
    flowsom = list(df = flowsom_df, order = flowsom_order),
    umap = list(df = umap_fsom_df, params = umap_fsom_params),
    pipeline = list(
      preprocess_result = preprocess_result,
      isolated_populations = isolated_pops,
      concat_object = concat_object,
      concat_data = concat_data,
      concat_markers = concat_markers,
      cluster_result = cluster_result,
      parity_result = parity_result
    ),
    analysis = list(
      objects = analysis_objects,
      active_id = active_analysis_id,
      active = analysis_active,
      create = analysis_create,
      delete = analysis_delete,
      set_active = analysis_set_active,
      add_run = analysis_add_run,
      set_active_run = analysis_set_active_run,
      export_bundle = analysis_export_bundle,
      import_bundle = analysis_import_bundle
    ),
    gating = list(
      groups = gate_groups,
      gates = gate_tree,
      label_positions = gate_label_positions,
      memberships = gate_memberships,
      active_gate_id = gate_active_id,
      sync_status = gate_sync_status,
      wsp_report = gate_wsp_report,
      stats = gate_stats,
      recompute = gate_recompute,
      add_gate = gate_add,
      remove_gate = gate_remove,
      add_group = gate_add_group,
      update_group = gate_update_group,
      delete_group = gate_delete_group,
      sync_gate_to_group = gate_sync_to_group,
      reset_gate_to_group = gate_reset_to_group,
      promote_gate_to_group = gate_promote_to_group
    ),
    populations = list(
      items = population_items,
      table = population_table,
      add = population_add,
      update = population_update,
      remove = population_remove
    ),
    fcs = list(
      registry = fcs_registry,
      active_id = active_fcs_id,
      auto_switch_to_umap = auto_switch_umap,
      clear_auto_switch_to_umap = clear_auto_switch_umap,
      add = fcs_add,
      add_many = fcs_add_many,
      remove = fcs_remove,
      rename = fcs_rename,
      set_notes = fcs_set_notes,
      set_active = fcs_set_active,
      table = fcs_table
    ),
    params = params,
    export_state = export_state,
    import_state = import_state
  )
}
