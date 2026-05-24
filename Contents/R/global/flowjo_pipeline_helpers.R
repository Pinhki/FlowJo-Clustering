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

flowjo_cluster_concat <- function(concat_df, marker_cols, seed = 42L, k = 20L) {
  stopifnot(length(marker_cols) >= 2)
  set.seed(as.integer(seed))
  mat <- as.matrix(concat_df[, marker_cols, drop = FALSE])

  fs <- flowsom_cluster(
    df = concat_df,
    features = marker_cols,
    xdim = max(5L, floor(sqrt(as.integer(k)))),
    ydim = max(5L, ceiling(sqrt(as.integer(k)))),
    rlen = 10L,
    seed = as.integer(seed),
    scale_data = TRUE,
    k = as.integer(k)
  )

  phenograph <- NULL
  if (requireNamespace("Rphenograph", quietly = TRUE)) {
    phenograph <- tryCatch({
      g <- Rphenograph::Rphenograph(mat, k = 30)
      as.integer(igraph::membership(g[[2]]))
    }, error = function(e) NULL)
  }

  um <- uwot::umap(mat, n_neighbors = 15, min_dist = 0.3, metric = "euclidean", verbose = FALSE)
  colnames(um) <- c("UMAP_1", "UMAP_2")

  out <- concat_df
  out$FlowSOM_Cluster <- factor(fs$cluster)
  if (!is.null(phenograph) && length(phenograph) == nrow(out)) {
    out$PhenoGraph_Cluster <- factor(phenograph)
  }
  out$UMAP_1 <- um[, 1]
  out$UMAP_2 <- um[, 2]

  freq_sample <- out |>
    dplyr::count(sample_name, FlowSOM_Cluster, name = "n") |>
    dplyr::group_by(sample_name) |>
    dplyr::mutate(freq = n / sum(n)) |>
    dplyr::ungroup()

  freq_group <- NULL
  if ("condition" %in% colnames(out)) {
    freq_group <- out |>
      dplyr::count(condition, FlowSOM_Cluster, name = "n") |>
      dplyr::group_by(condition) |>
      dplyr::mutate(freq = n / sum(n)) |>
      dplyr::ungroup()
  }

  marker_summary <- out |>
    dplyr::group_by(FlowSOM_Cluster) |>
    dplyr::summarise(dplyr::across(dplyr::all_of(marker_cols), ~ mean(.x, na.rm = TRUE)), .groups = "drop")

  list(
    data = out,
    flowsom = fs,
    sample_freq = freq_sample,
    group_freq = freq_group,
    marker_summary = marker_summary
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
