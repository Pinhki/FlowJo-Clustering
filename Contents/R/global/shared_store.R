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
  compensation_library <- reactiveVal(list())
  
  flowsom_df        <- reactiveVal(NULL)  # either merged CSV or sent from UMAP page
  flowsom_order     <- reactiveVal(NULL)
  
  umap_fsom_df      <- reactiveVal(NULL)
  umap_fsom_params  <- reactiveVal(NULL)
  fcs_registry      <- reactiveVal(list())
  active_fcs_id     <- reactiveVal(NULL)
  auto_switch_umap  <- reactiveVal(FALSE)
  plot_preload_request <- reactiveVal(0L)
  nav_route_request <- reactiveVal(NULL)

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
    axis_text_size  = reactiveVal(12),
    plot_cache_enabled = reactiveVal(TRUE),
    plot_event_limit_enabled = reactiveVal(TRUE),
    plot_event_limit = reactiveVal(75000L),
    plot_cache_mode = reactiveVal("settings"),
    plot_cache_max_contexts = reactiveVal(50L),
    plot_preload_enabled = reactiveVal(TRUE),
    plot_preload_after_load = reactiveVal(TRUE),
    plot_preload_scope = reactiveVal("all")
  )

  request_plot_preload <- function() {
    # Agent note: This helper is called from delayed workspace-restore callbacks; isolate the read so background preload can be requested outside a reactive consumer.
    plot_preload_request(as.integer(shiny::isolate(plot_preload_request())) + 1L)
    invisible(TRUE)
  }

  gating_sample_request <- reactiveVal(NULL)
  request_gating_sample <- function(sample_id) {
    gating_sample_request(list(sample_id = sample_id, nonce = stats::runif(1)))
    invisible(TRUE)
  }

  request_nav_route <- function(tab) {
    nav_route_request(list(tab = tab, nonce = stats::runif(1)))
    invisible(TRUE)
  }
  
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

  compensation_normalize_matrix <- function(value) {
    matrix_value <- tryCatch(as.matrix(value), error = function(e) NULL)
    if (is.null(matrix_value) || length(dim(matrix_value)) != 2L || nrow(matrix_value) < 2L || nrow(matrix_value) != ncol(matrix_value)) return(NULL)
    suppressWarnings(storage.mode(matrix_value) <- "double")
    if (any(!is.finite(matrix_value))) return(NULL)
    channels <- colnames(matrix_value)
    if (is.null(channels) || length(channels) != ncol(matrix_value) || any(!nzchar(channels))) channels <- rownames(matrix_value)
    if (is.null(channels) || length(channels) != ncol(matrix_value) || any(!nzchar(channels))) channels <- paste0("Channel ", seq_len(ncol(matrix_value)))
    channels <- make.unique(as.character(channels), sep = "_")
    dimnames(matrix_value) <- list(channels, channels)
    matrix_value
  }

  compensation_signature <- function(matrix_value) {
    if (is.null(matrix_value)) return("")
    payload <- list(dim = dim(matrix_value), channels = colnames(matrix_value), values = signif(as.numeric(matrix_value), 12))
    if (requireNamespace("digest", quietly = TRUE)) return(digest::digest(payload, algo = "xxhash64"))
    paste(c(dim(matrix_value), colnames(matrix_value), signif(as.numeric(matrix_value), 12)), collapse = "\u001f")
  }

  compensation_is_invertible <- function(matrix_value) {
    isTRUE(tryCatch({ solve(matrix_value); TRUE }, error = function(e) FALSE))
  }

  compensation_map_entry_channels <- function(matrix_value, entry) {
    matrix_value <- compensation_normalize_matrix(matrix_value)
    if (is.null(matrix_value) || is.null(entry$channel_map) || !nrow(entry$channel_map) || !all(c("channel", "marker") %in% names(entry$channel_map))) return(matrix_value)
    raw_channels <- colnames(matrix_value)
    mapped <- raw_channels
    hits <- match(raw_channels, as.character(entry$channel_map$channel))
    mapped[!is.na(hits)] <- as.character(entry$channel_map$marker[hits[!is.na(hits)]])
    mapped[is.na(mapped) | !nzchar(mapped)] <- raw_channels[is.na(mapped) | !nzchar(mapped)]
    mapped <- make.unique(mapped, sep = "_")
    dimnames(matrix_value) <- list(mapped, mapped)
    matrix_value
  }

  compensation_register <- function(matrix_value, preferred_id = NULL, name = NULL, source = "manual", sample_ids = character()) {
    matrix_value <- compensation_normalize_matrix(matrix_value)
    if (is.null(matrix_value)) return(NULL)
    preferred <- if (length(preferred_id) && !is.na(preferred_id[[1]])) trimws(as.character(preferred_id[[1]])) else ""
    display_name <- if (length(name) && !is.na(name[[1]]) && nzchar(trimws(as.character(name[[1]])))) trimws(as.character(name[[1]])) else preferred
    base_id <- if (nzchar(preferred)) gsub("[^A-Za-z0-9_.-]+", "_", preferred) else paste0("manual_", substr(compensation_signature(matrix_value), 1L, 12L))
    if (!nzchar(base_id)) base_id <- "manual_matrix"
    if (!nzchar(display_name)) display_name <- base_id
    signature <- compensation_signature(matrix_value)
    candidate <- base_id
    library <- compensation_library()
    if (!is.null(library[[candidate]]) && !identical(library[[candidate]]$signature, signature)) candidate <- paste0(base_id, "_", substr(signature, 1L, 12L))
    prior <- library[[candidate]]
    if (!is.null(prior)) {
      prior$source_sample_ids <- unique(c(prior$source_sample_ids %||% character(), sample_ids %||% character()))
      prior$updated_at <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
      if (is.null(prior$source) || !nzchar(prior$source)) prior$source <- source %||% "manual"
      library[[candidate]] <- prior
    } else {
      library[[candidate]] <- list(
        id = candidate,
        name = display_name %||% candidate,
        matrix = matrix_value,
        original_matrix = matrix_value,
        channels = colnames(matrix_value),
        signature = signature,
        source = source %||% "manual",
        source_sample_ids = unique(sample_ids %||% character()),
        created_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
        updated_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
      )
    }
    compensation_library(library)
    candidate
  }

  compensation_register_entry <- function(entry) {
    if (is.null(entry) || is.null(entry$spillover)) return(entry)
    raw_channels <- colnames(entry$spillover) %||% rownames(entry$spillover) %||% character()
    entry$compensation_raw_channels <- entry$compensation_raw_channels %||% raw_channels
    entry$spillover <- compensation_map_entry_channels(entry$spillover, entry)
    source <- entry$compensation_source %||% if (!is.null(entry$wsp_sample_id)) "WSP" else "FCS"
    id <- compensation_register(
      entry$spillover,
      preferred_id = entry$compensation_id,
      name = entry$compensation_name %||% entry$compensation_id,
      source = source,
      sample_ids = entry$id %||% character()
    )
    if (is.null(id)) return(entry)
    entry$compensation_id <- id
    entry$compensation_name <- compensation_library()[[id]]$name %||% id
    entry$compensation_source <- source
    entry$compensation_channels <- colnames(compensation_library()[[id]]$matrix)
    entry$spillover <- compensation_library()[[id]]$matrix
    entry
  }

  compensation_register_entries <- function(entries) {
    if (!length(entries)) return(entries)
    lapply(entries, compensation_register_entry)
  }

  compensation_update <- function(id, matrix_value, name = NULL, source = "manual") {
    library <- compensation_library()
    if (is.null(library[[id]])) return(FALSE)
    matrix_value <- compensation_normalize_matrix(matrix_value)
    if (is.null(matrix_value) || !compensation_is_invertible(matrix_value)) return(FALSE)
    library[[id]]$matrix <- matrix_value
    library[[id]]$channels <- colnames(matrix_value)
    library[[id]]$signature <- compensation_signature(matrix_value)
    library[[id]]$source <- source %||% "manual"
    if (!is.null(name) && nzchar(trimws(name))) library[[id]]$name <- trimws(name)
    library[[id]]$updated_at <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    compensation_library(library)
    reg <- fcs_registry()
    for (sample_id in names(reg)) {
      if (identical(reg[[sample_id]]$compensation_id %||% NULL, id)) {
        reg[[sample_id]]$spillover <- matrix_value
        reg[[sample_id]]$compensation_channels <- colnames(matrix_value)
        reg[[sample_id]]$compensation_name <- library[[id]]$name
        reg[[sample_id]]$compensation_source <- source %||% "manual"
      }
    }
    fcs_registry(reg)
    preprocess_result(NULL)
    df_processed(NULL)
    TRUE
  }

  compensation_reset <- function(id) {
    original <- compensation_library()[[id]]$original_matrix %||% NULL
    if (is.null(original)) return(FALSE)
    compensation_update(id, original, source = compensation_library()[[id]]$source %||% "WSP")
  }

  compensation_assign <- function(id, sample_ids) {
    matrix_value <- compensation_library()[[id]]$matrix %||% NULL
    if (is.null(matrix_value)) return(FALSE)
    reg <- fcs_registry()
    sample_ids <- intersect(as.character(sample_ids %||% character()), names(reg))
    if (!length(sample_ids)) return(FALSE)
    for (sample_id in sample_ids) {
      reg[[sample_id]]$compensation_id <- id
      reg[[sample_id]]$compensation_name <- compensation_library()[[id]]$name %||% id
      reg[[sample_id]]$compensation_source <- "assigned"
      reg[[sample_id]]$compensation_channels <- colnames(matrix_value)
      reg[[sample_id]]$spillover <- matrix_value
    }
    fcs_registry(reg)
    preprocess_result(NULL)
    df_processed(NULL)
    TRUE
  }

  compensation_remove <- function(id) {
    library <- compensation_library()
    if (is.null(library[[id]])) return(FALSE)
    reg <- fcs_registry()
    if (any(vapply(reg, function(entry) identical(entry$compensation_id %||% NULL, id), logical(1)))) return(FALSE)
    library[[id]] <- NULL
    compensation_library(library)
    TRUE
  }

  compensation_rebuild <- function(entries = fcs_registry()) {
    compensation_library(list())
    rebuilt <- compensation_register_entries(entries)
    rebuilt
  }

  fcs_add <- function(entry, activate = TRUE) {
    entry <- compensation_register_entry(entry)
    entry$.cache_revision <- paste0(format(Sys.time(), "%Y%m%d%H%M%OS6"), "-", sample.int(.Machine$integer.max, 1L))
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
    entries <- compensation_register_entries(entries)
    reg <- fcs_registry()
    for (entry in entries) {
      entry$.cache_revision <- paste0(format(Sys.time(), "%Y%m%d%H%M%OS6"), "-", sample.int(.Machine$integer.max, 1L))
      reg[[entry$id]] <- entry
    }
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

  analysis_source_data <- function(source = c("fcs", "basket", "batch"), population_ids = NULL) {
    source <- match.arg(source)
    if (identical(source, "basket")) {
      items <- population_items()
      if (is.null(population_ids) || !length(population_ids)) stop("Select populations before creating an analysis object.")
      if (!all(population_ids %in% names(items))) stop("The population basket changed. Review your selection again.")
      items <- items[population_ids]
      valid <- vapply(items, function(item) {
        entry <- fcs_registry()[[item$sample_id]]
        keep <- gate_memberships()[[item$sample_id]][[item$gate_id]]
        !is.null(entry) && !is.null(gate_tree()[[item$gate_id]]) && length(keep) == nrow(entry$df) && any(keep, na.rm = TRUE)
      }, logical(1))
      if (!all(valid)) stop("A selected population is empty or unavailable. Review the basket before creating the object.")
      for (i in seq_along(items)) items[[i]]$include <- TRUE
      cc <- flowjo_population_concat(
        fcs_registry(), gate_tree(), gate_memberships(), items, mode = "all"
      )
      mk <- intersect(cc$markers %||% analysis_numeric_cols(cc$data), names(cc$data))
      return(list(
        data = cc$data,
        markers = mk,
        channel_map = data.frame(channel = mk, marker = mk, stringsAsFactors = FALSE),
        transform_settings = list(),
        badge = "Basket",
        label = "Population Basket",
        source_id = "basket", selection = cc$mapping
      ))
    }
    if (identical(source, "batch")) {
      dat <- concat_data()
      mk <- concat_markers() %||% analysis_numeric_cols(dat)
      mk <- intersect(mk, names(dat))
      return(list(data = dat, markers = mk, channel_map = data.frame(channel = mk, marker = mk, stringsAsFactors = FALSE), transform_settings = list(), badge = "Batch", label = "Batch Concatenation", source_id = "batch"))
    }
    dat <- df_raw()
    mk <- analysis_numeric_cols(dat)
    active_id <- active_fcs_id()
    entry <- if (!is.null(active_id) && isTRUE(nzchar(active_id))) fcs_registry()[[active_id]] else NULL
    entry <- entry %||% list()
    if (!is.null(entry) && !is.null(dat) && nrow(dat) > 0L) {
      metadata <- c(
        list(sample_id = entry$id, sample_name = entry$name, source_file = entry$path),
        entry$sample_meta %||% list()
      )
      for (bucket_name in c("keywords", "keyword", "metadata")) {
        bucket <- entry[[bucket_name]]
        if (is.list(bucket)) metadata <- c(metadata, bucket)
        else if (!is.null(bucket) && !is.null(names(bucket))) metadata <- c(metadata, as.list(bucket))
      }
      metadata <- metadata[!duplicated(names(metadata))]
      keep <- vapply(metadata, function(value) {
        length(value) == 1L && !is.list(value) && !is.null(value) && !is.na(value) && nzchar(trimws(as.character(value)))
      }, logical(1))
      metadata_names <- names(metadata)
      metadata <- metadata[keep & !is.na(metadata_names) & nzchar(metadata_names)]
      metadata <- metadata[setdiff(names(metadata), names(dat))]
      if (length(metadata)) {
        metadata_df <- as.data.frame(lapply(metadata, function(value) rep(as.character(value), nrow(dat))),
          stringsAsFactors = FALSE, check.names = FALSE)
        dat <- cbind(dat, metadata_df)
      }
    }
    cm <- entry$channel_map %||% data.frame(channel = mk, marker = mk, stringsAsFactors = FALSE)
    cm <- cm[cm$marker %in% mk, c("channel", "marker"), drop = FALSE]
    missing <- setdiff(mk, cm$marker)
    if (length(missing)) cm <- rbind(cm, data.frame(channel = missing, marker = missing, stringsAsFactors = FALSE))
    cm <- cm[match(mk, cm$marker), , drop = FALSE]
    list(data = dat, markers = mk, channel_map = cm,
      transform_settings = entry$wsp_axis_transforms %||% list(),
      badge = "FCS", label = "Root / all events", source_id = active_id,
      selection = data.frame(sample_id = active_id %||% "", sample_name = entry$name %||% "Current sample",
        gate_id = "root", gate_path = "root", events = nrow(dat), stringsAsFactors = FALSE))
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

  analysis_create <- function(name, source = c("fcs", "basket", "batch"), notes = "", population_ids = NULL) {
    name <- analysis_unique_name(name)
    # Agent note: Object input is the reviewed basket subset; export concatenation is a downstream operation.
    source <- match.arg(source)
    if (identical(source, "batch")) stop("Create analysis objects from reviewed populations or explicitly selected Root events.")
    src <- analysis_source_data(source, population_ids = population_ids)
    markers <- analysis_validate_source(src)
    selected_markers <- flowjo_default_cluster_markers(markers)
    id <- analysis_make_id()
    obj <- list(
      id = id,
      name = name,
      source_badge = src$badge,
      source_label = src$label,
      source_id = src$source_id,
      created_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
      notes = notes %||% "",
      data = as.data.frame(src$data, stringsAsFactors = FALSE),
      markers = markers,
      channel_map = src$channel_map,
      transform_settings = src$transform_settings %||% list(),
      selected_markers = selected_markers,
      feature_settings = list(),
      cluster_settings = list(seed = 42L, neighbors = 15L, min_dist = 0.3, metric = "euclidean", scale = TRUE, xdim = 10L, ydim = 10L, rlen = 10L, k = 20L),
      runs = list(),
      active_run_id = NULL,
      provenance = list(events = nrow(src$data), columns = ncol(src$data), markers = length(markers),
        population_ids = population_ids, selection = src$selection %||% data.frame())
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

  analysis_set_selected_markers <- function(object_id = active_analysis_id(), markers) {
    objs <- analysis_objects()
    obj <- objs[[object_id]]
    if (is.null(obj)) return(invisible(FALSE))
    obj$selected_markers <- intersect(obj$markers %||% character(0), markers %||% character(0))
    objs[[object_id]] <- obj
    analysis_objects(objs)
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

  analysis_set_run_colors <- function(object_id = active_analysis_id(), run_id, colors) {
    objs <- analysis_objects()
    obj <- objs[[object_id]]
    if (is.null(obj) || is.null(obj$runs[[run_id]])) return(invisible(FALSE))
    obj$runs[[run_id]]$cluster_colors <- colors
    objs[[object_id]] <- obj
    analysis_objects(objs)
    invisible(TRUE)
  }

  analysis_set_run_visualization <- function(object_id = active_analysis_id(), run_id, visualization) {
    objs <- analysis_objects()
    obj <- objs[[object_id]]
    if (is.null(obj) || is.null(obj$runs[[run_id]])) return(invisible(FALSE))
    if (identical(obj$runs[[run_id]]$visualization %||% list(), visualization %||% list())) return(invisible(TRUE))
    # Agent note: Visualization state belongs to the immutable result's run so switching runs restores its own view.
    obj$runs[[run_id]]$visualization <- visualization %||% list()
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
    zip_file <- paste0(file, ".zip")
    utils::zip(zipfile = zip_file, files = list.files(tmp, all.files = FALSE))
    if (!file.rename(zip_file, file)) stop("Could not finalize the analysis bundle download.")
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
        # Agent note: FlowJo imports carry exact per-sample memberships; use them
        # when available so WSP statistics are not changed by geometry re-evaluation.
        imported_keep <- gate$wsp_memberships[[sid]] %||% NULL
        keep <- if (!is.null(imported_keep) && length(imported_keep) == nrow(gate_df)) {
          imported_keep
        } else {
          flowjo_gate_membership(gate_df, e$channel_map, gate, parent = parent)
        }
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
    out <- population_items(); added <- 0L; duplicates <- 0L; missing_stats <- 0L; invalid <- 0L
    for (sid in intersect(sample_ids, names(reg))) for (gid in intersect(gate_ids, names(gates))) {
      stat_row <- stats[stats$sample_id == sid & stats$gate_id == gid, , drop = FALSE]
      if (!nrow(stat_row)) {
        # Agent note: Basket drops may arrive before background Workspace statistics;
        # gate_recompute returns the requested rows but gate_stats only reflects the active sample.
        computed <- gate_recompute(gid, sid)
        stat_row <- computed[computed$sample_id == sid & computed$gate_id == gid, , drop = FALSE]
        if (!nrow(stat_row)) {
          missing_stats <- missing_stats + 1L
          next
        }
      }
      dup <- vapply(out, function(x) identical(x$sample_id, sid) && identical(x$gate_id, gid) && identical(x$set_name, set_name), logical(1))
      if (any(dup)) { duplicates <- duplicates + 1L; next }
      default_label <- sub("^.*/", "", gates[[gid]]$name %||% gid)
      item <- list(id = flowjo_gate_id("pop"), sample_id = sid, gate_id = gid, set_name = set_name,
        label = label %||% default_label, volume_uL = suppressWarnings(as.numeric(volume_uL %||% NA_real_)),
        include = TRUE, events = stat_row$cells[[1]], percent_parent = stat_row$percent_parent[[1]],
        created_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
      out[[item$id]] <- item; added <- added + 1L
    }
    invalid <- (length(sample_ids) * length(gate_ids)) - added - duplicates - missing_stats
    population_items(out); population_clear_outputs(); invisible(added)
    message("[workspace-basket] requested=", length(sample_ids) * length(gate_ids), " added=", added,
      " duplicate=", duplicates, " missing_stats=", missing_stats, " invalid=", max(0L, invalid))
    invisible(added)
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
        gate_name = gate$name %||% x$gate_id, events = if (nrow(st)) st$cells[[1]] else x$events %||% 0L,
        percent_parent = if (nrow(st)) st$percent_parent[[1]] else x$percent_parent %||% NA_real_,
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
      compensation_library = compensation_library(),
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
    if (!is.null(obj$compensation_library) && is.list(obj$compensation_library)) compensation_library(obj$compensation_library)
    else compensation_library(list())
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
      reg <- obj$fcs_registry
      reg <- lapply(reg, function(entry) {
        entry$.cache_revision <- paste0(format(Sys.time(), "%Y%m%d%H%M%OS6"), "-", sample.int(.Machine$integer.max, 1L))
        entry
      })
      ids <- vapply(reg, function(e) e$id %||% "", character(1))
      keep <- nzchar(ids)
      reg <- reg[keep]
      names(reg) <- ids[keep]
      for (sample_id in names(reg)) {
        entry <- reg[[sample_id]]
        comp_id <- entry$compensation_id %||% ""
        if (nzchar(comp_id) && !is.null(compensation_library()[[comp_id]])) {
          entry$spillover <- compensation_library()[[comp_id]]$matrix
          entry$compensation_name <- compensation_library()[[comp_id]]$name %||% comp_id
          entry$compensation_channels <- compensation_library()[[comp_id]]$channels %||% colnames(entry$spillover)
        } else {
          entry <- compensation_register_entry(entry)
        }
        reg[[sample_id]] <- entry
      }
      fcs_registry(reg)
      if (!is.null(obj$active_fcs_id) && !is.null(reg[[obj$active_fcs_id]])) {
        set_active_from_entry(reg[[obj$active_fcs_id]])
      } else if (length(reg) > 0) {
        set_active_from_entry(reg[[1]])
      }
    }
  }
  
  list(
    session = session,
    df = list(raw = df_raw, processed = df_processed),
    choices = choices,
    axis_pairs = axis_pairs_val,
    transforms = list(settings = transform_settings, scales = scale_settings, auto_scales = auto_scale_settings, feature_scale = feature_scale_setting),
    compensation = list(
      library = compensation_library,
      matrix = function(id) compensation_library()[[id]]$matrix %||% NULL,
      add = compensation_register,
      update = compensation_update,
      reset = compensation_reset,
      assign = compensation_assign,
      remove = compensation_remove,
      rebuild = compensation_rebuild
    ),
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
      set_selected_markers = analysis_set_selected_markers,
      add_run = analysis_add_run,
      set_active_run = analysis_set_active_run,
      set_run_colors = analysis_set_run_colors,
      set_run_visualization = analysis_set_run_visualization,
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
    ui = list(
      plot_preload_request = plot_preload_request,
      request_plot_preload = request_plot_preload,
      nav_route_request = nav_route_request,
      request_nav_route = request_nav_route,
      gating_sample_request = gating_sample_request,
      request_gating_sample = request_gating_sample
    ),
    export_state = export_state,
    import_state = import_state
  )
}
