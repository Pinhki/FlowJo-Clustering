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
    active_fcs_id(entry$id)
  }

  has_umap_cols <- function(entry) {
    cols <- unique(c(entry$numeric_cols %||% character(), colnames(entry$df) %||% character()))
    any(grepl("UMAP", cols, ignore.case = TRUE))
  }

  fcs_add <- function(entry) {
    reg <- fcs_registry()
    reg[[entry$id]] <- entry
    fcs_registry(reg)
    set_active_from_entry(entry)
    auto_switch_umap(!has_umap_cols(entry))
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
  
  # export/import state for Save/Load
  export_state <- function() {
    list(
      params = lapply(params, function(rv) rv()),
      transform_settings = transform_settings(),
      scale_settings     = scale_settings(),
      fcs_registry = fcs_registry(),
      active_fcs_id = active_fcs_id(),
      preprocess_result = preprocess_result(),
      isolated_populations = isolated_pops(),
      concat_object = concat_object(),
      concat_data = concat_data(),
      concat_markers = concat_markers(),
      cluster_result = cluster_result(),
      parity_result = parity_result()
    )
  }
  import_state <- function(obj) {
    if (!is.null(obj$params)) {
      for (nm in intersect(names(params), names(obj$params))) params[[nm]](obj$params[[nm]])
    }
    if (!is.null(obj$transform_settings)) transform_settings(obj$transform_settings)
    if (!is.null(obj$scale_settings))     scale_settings(obj$scale_settings)
    if (!is.null(obj$preprocess_result)) preprocess_result(obj$preprocess_result)
    if (!is.null(obj$isolated_populations)) isolated_pops(obj$isolated_populations)
    if (!is.null(obj$concat_object)) concat_object(obj$concat_object)
    if (!is.null(obj$concat_data)) concat_data(obj$concat_data)
    if (!is.null(obj$concat_markers)) concat_markers(obj$concat_markers)
    if (!is.null(obj$cluster_result)) cluster_result(obj$cluster_result)
    if (!is.null(obj$parity_result)) parity_result(obj$parity_result)
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
    transforms = list(settings = transform_settings, scales = scale_settings),
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
    fcs = list(
      registry = fcs_registry,
      active_id = active_fcs_id,
      auto_switch_to_umap = auto_switch_umap,
      clear_auto_switch_to_umap = clear_auto_switch_umap,
      add = fcs_add,
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
