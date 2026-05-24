# R/utils_helpers.R

find_axis_pairs <- function(columns) {
  pairs_list <- list(); processed <- character(0)
  dr <- columns[grepl("UMAP|TSNE|T-SNE|DIM|PC", columns, ignore.case = TRUE)]
  for (col in dr) {
    if (col %in% processed) next
    partner <- NULL
    if (grepl("[._-]1", col))       partner <- sub("([._-])1", "\\12", col)
    else if (grepl("[._-]2", col))  partner <- sub("([._-])2", "\\11", col)
    else if (grepl("[._-]X", col, ignore.case=TRUE)) partner <- sub("([._-])X", "\\1Y", col, ignore.case=TRUE)
    else if (grepl("[._-]Y", col, ignore.case=TRUE)) partner <- sub("([._-])Y", "\\1X", col, ignore.case=TRUE)
    if (!is.null(partner) && partner %in% dr) {
      pairs_list[[length(pairs_list)+1]] <- sort(c(col, partner))
      processed <- c(processed, col, partner)
    }
  }
  if (!length(pairs_list)) return(data.frame(x=character(), y=character()))
  uniq <- unique(pairs_list)
  data.frame(x = sapply(uniq, `[`, 1), y = sapply(uniq, `[`, 2))
}

flowsom_cluster <- function(df, features, xdim=10, ydim=10, rlen=10, seed=1234, scale_data=TRUE, k=20) {
  stopifnot(length(features) >= 2)
  mat <- as.matrix(df[, features, drop = FALSE])
  if (!is.matrix(mat) || nrow(mat) < 2 || ncol(mat) < 2) {
    stop("FlowSOM requires at least 2 rows and 2 selected channels.")
  }
  if (isTRUE(scale_data)) mat <- scale(mat)
  mat <- mat[, colSums(is.na(mat)) < nrow(mat), drop = FALSE]
  if (ncol(mat) < 2) {
    stop("After preprocessing, fewer than 2 valid channels remain for FlowSOM.")
  }

  xdim_i <- as.integer(xdim)
  ydim_i <- as.integer(ydim)
  rlen_i <- as.integer(rlen)
  seed_i <- as.integer(seed)
  k_i <- as.integer(k)

  if (!is.finite(xdim_i) || xdim_i < 2L) stop("SOM xdim must be >= 2.")
  if (!is.finite(ydim_i) || ydim_i < 2L) stop("SOM ydim must be >= 2.")
  if (!is.finite(rlen_i) || rlen_i < 1L) stop("Training iterations (rlen) must be >= 1.")
  if (!is.finite(seed_i) || seed_i < 1L) seed_i <- 42L

  n_nodes <- xdim_i * ydim_i
  max_k <- max(2L, n_nodes - 1L)
  if (!is.finite(k_i) || k_i < 2L) k_i <- 2L
  if (k_i > max_k) {
    k_i <- max_k
  }

  ff <- flowCore::flowFrame(mat)
  set.seed(seed_i)
  fsom <- tryCatch(
    FlowSOM::FlowSOM(
      input = ff, scale = FALSE, colsToUse = seq_len(ncol(mat)),
      xdim = xdim_i, ydim = ydim_i, rlen = rlen_i, nClus = NULL, seed = seed_i
    ),
    error = function(e) {
      warning(
        sprintf(
          "FlowSOM core run failed (%s). Falling back to direct k-means clustering on selected channels.",
          conditionMessage(e)
        ),
        call. = FALSE
      )
      NULL
    }
  )
  if (is.null(fsom)) {
    k_direct <- min(max(2L, k_i), max(2L, nrow(mat) - 1L))
    km <- stats::kmeans(mat, centers = k_direct, iter.max = max(50L, rlen_i * 10L), nstart = 10)
    return(list(
      cluster = as.integer(km$cluster),
      fsom = NULL,
      meta = seq_len(k_direct),
      k_used = k_direct,
      meta_method = "direct_kmeans_fallback"
    ))
  }
  codes <- fsom$map$codes
  if (!is.matrix(codes)) {
    codes <- as.matrix(codes)
  }
  if (nrow(codes) < 2) {
    stop("FlowSOM map codes are insufficient for meta-clustering (fewer than 2 nodes).")
  }
  if (k_i >= nrow(codes)) {
    k_i <- max(2L, nrow(codes) - 1L)
  }

  meta_method <- "consensus"
  meta <- tryCatch(
    FlowSOM::metaClustering_consensus(codes, k = k_i, seed = seed_i),
    error = function(e) {
      meta_method <<- "kmeans_fallback"
      warning(
        sprintf(
          "Consensus meta-clustering failed (%s). Falling back to k-means meta-clustering.",
          conditionMessage(e)
        ),
        call. = FALSE
      )
      stats::kmeans(codes, centers = k_i, iter.max = 100, nstart = 10)$cluster
    }
  )
  node_idx <- fsom$map$mapping[,1]
  clusters <- meta[node_idx]
  list(cluster = as.integer(clusters), fsom = fsom, meta = meta, k_used = k_i, meta_method = meta_method)
}

apply_theme <- function(p, scale_factor = 1, theme_choice = "UMAP Arrows",
                        title_size = 20, axis_title_size = 14, axis_text_size = 12,
                        arrow_length = 0.15, arrow_thickness = 0.3, arrow_label_size = 2,
                        arrow_type = "Line", xlab = "", ylab = "") {
  base_theme <- switch(theme_choice,
                       "Classic" = theme_classic(),
                       "Black & White" = theme_bw(),
                       "Cowplot" = theme_cowplot(),
                       "UMAP Arrows" = theme_cowplot(),
                       theme_minimal()
  )
  p <- p + base_theme +
    theme(
      plot.title = element_text(hjust = 0.5, size = title_size * scale_factor, face = "bold"),
      axis.title = element_text(size = axis_title_size * scale_factor),
      axis.text  = element_text(size = axis_text_size  * scale_factor),
      legend.position = "right"
    )
  
  if (theme_choice != "UMAP Arrows") return(p + labs(title = NULL, x = xlab, y = ylab))
  
  pdf(NULL); pb <- ggplot_build(p); dev.off()
  xr <- pb$layout$panel_params[[1]]$x.range
  yr <- pb$layout$panel_params[[1]]$y.range
  req(xr, yr)
  
  ax_len_x <- (xr[2]-xr[1]) * 0.2
  ax_len_y <- (yr[2]-yr[1]) * 0.2
  x_label_pos <- xr[1] + ax_len_x/2
  y_label_pos <- yr[1] + ax_len_y/2
  
  if (arrow_type == "Line") {
    p <- p +
      geom_segment(aes(x=xr[1], xend=xr[1]+ax_len_x, y=yr[1], yend=yr[1]),
                   arrow=arrow(length=unit(arrow_length,"cm")), color="black", size=arrow_thickness*scale_factor) +
      geom_segment(aes(x=xr[1], xend=xr[1], y=yr[1], yend=yr[1]+ax_len_y),
                   arrow=arrow(length=unit(arrow_length,"cm")), color="black", size=arrow_thickness*scale_factor)
  } else {
    tri_x <- (xr[2]-xr[1]) * 0.02; tri_y <- (yr[2]-yr[1]) * 0.02
    x_arrow_df <- data.frame(x=c(xr[1]+ax_len_x, xr[1]+ax_len_x, xr[1]+ax_len_x+tri_x*1.5),
                             y=c(yr[1]-tri_y,     yr[1]+tri_y,     yr[1]))
    y_arrow_df <- data.frame(x=c(xr[1]-tri_y, xr[1]+tri_y, xr[1]),
                             y=c(yr[1]+ax_len_y, yr[1]+ax_len_y, yr[1]+ax_len_y+tri_x*1.5))
    p <- p +
      geom_segment(aes(x=xr[1], xend=xr[1]+ax_len_x, y=yr[1], yend=yr[1]), size=arrow_thickness*scale_factor) +
      geom_segment(aes(x=xr[1], xend=xr[1], y=yr[1], yend=yr[1]+ax_len_y), size=arrow_thickness*scale_factor) +
      geom_polygon(data=x_arrow_df, aes(x=x,y=y), fill="black") +
      geom_polygon(data=y_arrow_df, aes(x=x,y=y), fill="black")
  }
  
  y_offset <- (yr[2]-yr[1]) * 0.03; x_offset <- (xr[2]-xr[1]) * 0.03
  p +
    annotate("text", x = x_label_pos, y = yr[1]-y_offset, label = xlab, size = arrow_label_size*scale_factor) +
    annotate("text", x = xr[1]-x_offset, y = y_label_pos, label = ylab, size = arrow_label_size*scale_factor, angle = 90) +
    theme(axis.text = element_blank(), axis.ticks = element_blank(),
          axis.line = element_blank(), axis.title = element_blank())
}
