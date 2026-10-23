# FlowJo-style preprocessing and post-gating pipeline helpers

flowjo_extract_spillover <- function(ff) {
  sp <- NULL
  sp <- tryCatch(flowCore::keyword(ff, "$SPILLOVER")[[1]], error = function(e) NULL)
  if (is.null(sp)) sp <- tryCatch(flowCore::keyword(ff, "SPILL")[[1]], error = function(e) NULL)
  if (is.null(sp)) {
    sp <- tryCatch(flowCore::spillover(ff)$SPILL, error = function(e) NULL)
  }
  if (is.null(sp)) return(NULL)
  as.matrix(sp)
}

flowjo_compensate_exprs <- function(expr_mat, spill) {
  if (is.null(spill) || !is.matrix(spill) || nrow(spill) < 2) return(expr_mat)
  cn <- colnames(expr_mat)
  use_ch <- intersect(cn, colnames(spill))
  if (length(use_ch) < 2) return(expr_mat)

  x <- expr_mat[, use_ch, drop = FALSE]
  inv_spill <- tryCatch(solve(spill[use_ch, use_ch, drop = FALSE]), error = function(e) NULL)
  if (is.null(inv_spill)) return(expr_mat)
  expr_mat[, use_ch] <- x %*% inv_spill
  expr_mat
}

flowjo_logicle_transform <- function(expr_mat) {
  num_cols <- vapply(as.data.frame(expr_mat), is.numeric, logical(1))
  ch <- names(num_cols)[num_cols]
  if (length(ch) < 2) return(list(data = as.data.frame(expr_mat), transform = NULL))

  ff <- flowCore::flowFrame(as.matrix(expr_mat[, ch, drop = FALSE]))
  lg <- tryCatch(flowCore::estimateLogicle(ff, channels = ch), error = function(e) NULL)
  if (is.null(lg)) {
    return(list(data = as.data.frame(expr_mat), transform = NULL))
  }
  ff_t <- flowCore::transform(ff, lg)
  out <- expr_mat
  out[, ch] <- flowCore::exprs(ff_t)
  list(data = as.data.frame(out), transform = lg)
}

flowjo_preprocess_entry <- function(entry, mode = "exact") {
  stopifnot(is.list(entry), !is.null(entry$raw_exprs))
  raw <- as.data.frame(entry$raw_exprs)
  comp <- flowjo_compensate_exprs(as.matrix(raw), entry$spillover)
  tf <- flowjo_logicle_transform(comp)
  list(
    mode = mode,
    raw = raw,
    compensated = as.data.frame(comp),
    transformed = tf$data,
    transform_model = tf$transform,
    compensation_id = entry$compensation_id %||% NULL,
    compensation_name = entry$compensation_name %||% NULL,
    channel_map = entry$channel_map %||% data.frame(channel = colnames(raw), marker = colnames(raw), stringsAsFactors = FALSE),
    sample_meta = entry$sample_meta %||% list()
  )
}

flowjo_build_concat_object <- function(selected_df, marker_cols, assay_name = "exprs") {
  stopifnot(length(marker_cols) >= 2)
  x <- t(as.matrix(selected_df[, marker_cols, drop = FALSE]))
  rownames(x) <- marker_cols
  colnames(x) <- paste0("cell_", seq_len(ncol(x)))

  cdat <- selected_df[, setdiff(colnames(selected_df), marker_cols), drop = FALSE]
  cdat <- S4Vectors::DataFrame(cdat)

  sce <- SingleCellExperiment::SingleCellExperiment(
    assays = list(exprs = x),
    colData = cdat
  )
  metadata(sce)$assay_name <- assay_name
  sce
}

flowjo_gate_path <- function(gates, gate_id) {
  parts <- character()
  seen <- character()
  cur <- gate_id
  while (!is.null(cur) && !identical(cur, "root") && !is.null(gates[[cur]]) && !(cur %in% seen)) {
    seen <- c(seen, cur)
    parts <- c(gates[[cur]]$name %||% cur, parts)
    cur <- gates[[cur]]$parent_id %||% "root"
  }
  paste(c("root", parts), collapse = "/")
}

flowjo_default_cluster_markers <- function(marker_cols) {
  preferred <- c(
    "CD45RA", "CD11a", "CD49d", "CCR4", "KLRG1", "CCR6", "CXCR3", "PD1",
    "CD127", "CD25", "CD28", "CCR7", "CD45RO", "HLA-DR", "CD95", "CD226",
    "CXCR5", "CD27", "CD38"
  )
  hit <- marker_cols[toupper(marker_cols) %in% toupper(preferred)]
  if (length(hit) >= 2) return(hit)
  drop <- grepl("^(TIME|FSC|SSC)|LIVE|DEAD|TETRAMER|CD19|CD14|^CD3$|^CD4$|^CD8$", marker_cols, ignore.case = TRUE)
  fallback <- marker_cols[!drop]
  if (length(fallback) >= 2) fallback else marker_cols
}

flowjo_population_concat <- function(registry, gates, memberships, items, mode = "cap", cap = 5000L) {
  items <- items[vapply(items, function(x) isTRUE(x$include), logical(1))]
  if (!length(items)) stop("Population basket is empty.")
  rows <- list()
  map <- list()
  set_ids <- stats::setNames(seq_along(unique(vapply(items, function(x) x$set_name %||% "Population Set", character(1)))),
                             unique(vapply(items, function(x) x$set_name %||% "Population Set", character(1))))

  for (i in seq_along(items)) {
    it <- items[[i]]
    e <- registry[[it$sample_id]]
    gate <- gates[[it$gate_id]]
    keep <- memberships[[it$sample_id]][[it$gate_id]]
    if (is.null(e) || is.null(gate) || is.null(keep) || !any(keep, na.rm = TRUE)) next
    idx <- which(keep)
    if (identical(mode, "cap")) {
      n <- min(length(idx), as.integer(cap %||% 5000L))
      if (n < length(idx)) idx <- sample(idx, n)
    }
    dat <- e$df[idx, , drop = FALSE]
    dat$event_id <- idx
    dat$sample_id <- it$sample_id
    dat$sample_name <- e$name %||% it$sample_id
    dat$gate_id <- it$gate_id
    dat$gate_name <- gate$name %||% it$gate_id
    dat$gate_path <- flowjo_gate_path(gates, it$gate_id)
    basket_label <- it$label %||% dat$gate_name[[1]]
    if (identical(basket_label, gate$name %||% it$gate_id)) basket_label <- sub("^.*/", "", basket_label)
    dat$population_label <- basket_label
    dat$population_set <- it$set_name %||% "Population Set"
    dat$PopulationID <- i
    dat$SampleID <- match(it$sample_id, names(registry))
    dat$GateID <- match(it$gate_id, names(gates))
    dat$PopulationSetID <- unname(set_ids[[dat$population_set[[1]]]])
    dat$Volume_uL <- suppressWarnings(as.numeric(it$volume_uL %||% NA_real_))
    rows[[length(rows) + 1L]] <- dat
    map[[length(map) + 1L]] <- data.frame(
      PopulationID = i,
      PopulationSetID = dat$PopulationSetID[[1]],
      SampleID = dat$SampleID[[1]],
      GateID = dat$GateID[[1]],
      population_set = dat$population_set[[1]],
      population_label = dat$population_label[[1]],
      sample_id = it$sample_id,
      sample_name = dat$sample_name[[1]],
      gate_id = it$gate_id,
      gate_name = dat$gate_name[[1]],
      gate_path = dat$gate_path[[1]],
      volume_uL = dat$Volume_uL[[1]],
      events = nrow(dat),
      sync_status = if (!is.null(gate$sync_group_id)) "synced" else if (!is.null(gate$applied_sample_id)) "sample" else "global",
      stringsAsFactors = FALSE
    )
  }
  if (!length(rows)) stop("No included basket populations have events.")
  out <- data.table::rbindlist(rows, use.names = TRUE, fill = TRUE)
  marker_cols <- setdiff(names(registry[[out$sample_id[[1]]]]$df), character())
  list(data = as.data.frame(out), markers = intersect(marker_cols, names(out)), mapping = do.call(rbind, map))
}

flowjo_export_population_fcs_zip <- function(concat_df, marker_cols, mapping, file) {
  ann <- intersect(c("SampleID", "GateID", "PopulationID", "PopulationSetID", "Volume_uL"), names(concat_df))
  cols <- intersect(c(marker_cols, ann), names(concat_df))
  mat <- as.matrix(concat_df[, cols, drop = FALSE])
  storage.mode(mat) <- "double"
  mat[!is.finite(mat)] <- 0
  tmp <- tempfile("flowjo_concat_export_")
  dir.create(tmp, showWarnings = FALSE)
  fcs_file <- file.path(tmp, "concatenated_populations.fcs")
  map_file <- file.path(tmp, "population_mapping.csv")
  flowCore::write.FCS(flowCore::flowFrame(mat), filename = fcs_file)
  utils::write.csv(mapping, map_file, row.names = FALSE)
  zip::zipr(file, c(fcs_file, map_file), root = tmp)
  invisible(file)
}

flowjo_cluster_summary <- function(data, marker_values, marker_cols, baseline = "all_analysis_cells") {
  marker_cols <- intersect(marker_cols, names(marker_values))
  if (!length(marker_cols)) return(data.frame())
  long <- data |>
    dplyr::select(dplyr::any_of(c("FlowSOM_Cluster", "population_label", "gate_name")), dplyr::all_of(marker_cols)) |>
    tidyr::pivot_longer(dplyr::all_of(marker_cols), names_to = "marker", values_to = "value")

  base_cols <- "marker"
  if (identical(baseline, "within_population")) {
    base_cols <- c("population_label", "marker")
  }
  base <- long |>
    dplyr::group_by(dplyr::across(dplyr::all_of(base_cols))) |>
    dplyr::summarise(parent_median = stats::median(value, na.rm = TRUE), .groups = "drop")

  out <- long |>
    dplyr::group_by(FlowSOM_Cluster, dplyr::across(dplyr::all_of(base_cols))) |>
    dplyr::summarise(
      n = dplyr::n(),
      median = stats::median(value, na.rm = TRUE),
      iqr = stats::IQR(value, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::left_join(base, by = base_cols) |>
    dplyr::mutate(
      delta_parent = median - parent_median,
      pct_parent = dplyr::if_else(is.finite(parent_median) & abs(parent_median) > 1e-8, 100 * delta_parent / abs(parent_median), NA_real_)
    )

  if (identical(baseline, "within_population")) {
    out |>
      dplyr::group_by(FlowSOM_Cluster, marker) |>
      dplyr::summarise(
        n = sum(n),
        median = stats::weighted.mean(median, n, na.rm = TRUE),
        iqr = stats::weighted.mean(iqr, n, na.rm = TRUE),
        parent_median = stats::weighted.mean(parent_median, n, na.rm = TRUE),
        delta_parent = median - parent_median,
        pct_parent = dplyr::if_else(is.finite(parent_median) & abs(parent_median) > 1e-8, 100 * delta_parent / abs(parent_median), NA_real_),
        .groups = "drop"
      )
  } else {
    out
  }
}

flowjo_cluster_freq <- function(out) {
  out |>
    dplyr::count(sample_name, FlowSOM_Cluster, name = "n") |>
    dplyr::group_by(sample_name) |>
    dplyr::mutate(freq = n / sum(n)) |>
    dplyr::ungroup()
}

flowjo_recluster_concat <- function(cluster_result, k, seed = 42L) {
  rc <- flowjo_recluster_flowsom(cluster_result$flowsom, k = k, seed = seed)
  cluster_result$flowsom$cluster <- rc$cluster
  cluster_result$flowsom$meta <- rc$meta
  cluster_result$flowsom$k_used <- rc$k_used
  cluster_result$flowsom$meta_method <- rc$meta_method
  cluster_result$data$FlowSOM_Cluster <- factor(rc$cluster)
  cluster_result$sample_freq <- flowjo_cluster_freq(cluster_result$data)
  cluster_result$group_freq <- if ("condition" %in% colnames(cluster_result$data)) {
    cluster_result$data |>
      dplyr::count(condition, FlowSOM_Cluster, name = "n") |>
      dplyr::group_by(condition) |>
      dplyr::mutate(freq = n / sum(n)) |>
      dplyr::ungroup()
  } else NULL
  cluster_result$marker_summary <- flowjo_cluster_summary(cluster_result$data, cluster_result$marker_values, cluster_result$marker_cols)
  cluster_result$run_signature$k <- as.integer(k)
  cluster_result
}

flowjo_cluster_concat <- function(concat_df, marker_cols, seed = 42L, k = 20L, xdim = NULL, ydim = NULL, rlen = 10L) {
  stopifnot(length(marker_cols) >= 2)
  set.seed(as.integer(seed))
  tf <- flowjo_logicle_transform(concat_df[, marker_cols, drop = FALSE])$data
  mat <- scale(as.matrix(tf[, marker_cols, drop = FALSE]))
  mat[!is.finite(mat)] <- 0
  cluster_df <- concat_df
  cluster_df[, marker_cols] <- mat

  xdim <- as.integer(xdim %||% max(5L, floor(sqrt(as.integer(k)))))
  ydim <- as.integer(ydim %||% max(5L, ceiling(sqrt(as.integer(k)))))
  rlen <- as.integer(rlen %||% 10L)
  fs <- flowsom_cluster(
    df = cluster_df,
    features = marker_cols,
    xdim = xdim,
    ydim = ydim,
    rlen = rlen,
    seed = as.integer(seed),
    scale_data = FALSE,
    k = as.integer(k)
  )

  phenograph <- NULL
  if (requireNamespace("Rphenograph", quietly = TRUE)) {
    phenograph <- tryCatch({
      g <- Rphenograph::Rphenograph(mat, k = 30)
      as.integer(igraph::membership(g[[2]]))
    }, error = function(e) NULL)
  }

  umap_settings <- list(n_neighbors = 15L, min_dist = 0.3, metric = "euclidean", n_components = 3L)
  um <- uwot::umap(mat, n_neighbors = umap_settings$n_neighbors, min_dist = umap_settings$min_dist,
                   metric = umap_settings$metric, n_components = umap_settings$n_components, verbose = FALSE)
  colnames(um) <- c("UMAP_1", "UMAP_2", "UMAP_3")

  out <- concat_df
  out$FlowSOM_Cluster <- factor(fs$cluster)
  if (!is.null(phenograph) && length(phenograph) == nrow(out)) {
    out$PhenoGraph_Cluster <- factor(phenograph)
  }
  out$UMAP_1 <- um[, 1]
  out$UMAP_2 <- um[, 2]
  out$UMAP_3 <- um[, 3]
  out[, marker_cols] <- tf[, marker_cols, drop = FALSE]

  freq_sample <- flowjo_cluster_freq(out)

  freq_group <- NULL
  if ("condition" %in% colnames(out)) {
    freq_group <- out |>
      dplyr::count(condition, FlowSOM_Cluster, name = "n") |>
      dplyr::group_by(condition) |>
      dplyr::mutate(freq = n / sum(n)) |>
      dplyr::ungroup()
  }

  marker_summary <- flowjo_cluster_summary(out, tf, marker_cols)

  list(
    data = out,
    flowsom = fs,
    marker_cols = marker_cols,
    marker_values = tf,
    sample_freq = freq_sample,
    group_freq = freq_group,
    marker_summary = marker_summary,
    run_signature = list(
      marker_cols = marker_cols,
      seed = as.integer(seed),
      k = as.integer(k),
      xdim = xdim,
      ydim = ydim,
      rlen = rlen,
      umap = umap_settings,
      row_count = nrow(concat_df),
      columns = names(concat_df),
      metadata_counts = vapply(intersect(c("sample_name", "sample_id", "gate_name", "gate_path", "population_label", "condition"), names(concat_df)),
                               function(x) length(unique(concat_df[[x]])), integer(1))
    )
  )
}

flowjo_parity_metrics <- function(raw_df, transformed_df, channels, tol_median = 0.15, tol_iqr = 0.2) {
  ch <- intersect(channels, intersect(colnames(raw_df), colnames(transformed_df)))
  if (!length(ch)) return(data.frame())

  out <- lapply(ch, function(cc) {
    r <- raw_df[[cc]]
    t <- transformed_df[[cc]]
    data.frame(
      channel = cc,
      raw_median = stats::median(r, na.rm = TRUE),
      transformed_median = stats::median(t, na.rm = TRUE),
      raw_iqr = stats::IQR(r, na.rm = TRUE),
      transformed_iqr = stats::IQR(t, na.rm = TRUE),
      median_shift = abs(stats::median(t, na.rm = TRUE) - stats::median(r, na.rm = TRUE)),
      iqr_shift = abs(stats::IQR(t, na.rm = TRUE) - stats::IQR(r, na.rm = TRUE)),
      pass = TRUE,
      stringsAsFactors = FALSE
    )
  })
  out <- dplyr::bind_rows(out)
  out$pass <- (out$median_shift <= tol_median) & (out$iqr_shift <= tol_iqr)
  out
}
