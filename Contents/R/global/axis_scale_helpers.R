# R/axis_scale_helpers.R

flowjo_is_embedding_axis <- function(x) {
  grepl("UMAP|TSNE|T-SNE|DIM|PC", x %||% "", ignore.case = TRUE)
}

flowjo_is_color_channel <- function(x) {
  x <- x %||% ""
  if (isTRUE(flowjo_is_embedding_axis(x))) return(FALSE)
  if (grepl("^(FSC|SSC|TIME|EVENT)", x, ignore.case = TRUE)) return(FALSE)
  TRUE
}

flowjo_default_axis_scale_type <- function(x) {
  if (isTRUE(flowjo_is_embedding_axis(x))) return("none")
  if (isTRUE(flowjo_is_color_channel(x))) return("biex")
  "none"
}

flowjo_channel_range <- function(entry, channel = NULL, marker = NULL) {
  ranges <- entry$channel_ranges
  if (is.null(ranges) || !nrow(ranges)) return(NULL)
  hit <- rep(FALSE, nrow(ranges))
  if (!is.null(channel)) hit <- hit | identical(channel, ranges$channel) | ranges$channel == channel
  if (!is.null(marker)) hit <- hit | identical(marker, ranges$marker) | ranges$marker == marker
  vals <- suppressWarnings(as.numeric(ranges$range[hit]))
  vals <- vals[is.finite(vals) & vals > 0]
  if (length(vals)) vals[[1]] else NULL
}

flowjo_axis_scale_setting <- function(type, w = 1.25, p = 5, n = 0, t = NULL) {
  type <- tolower(type %||% "none")
  if (type == "log1p") return(list(type = "log1p"))
  if (type != "biex") return(NULL)

  w <- as.numeric(w); p <- as.numeric(p); n <- as.numeric(n); t <- as.numeric(t %||% 262144)
  if (!all(is.finite(c(w, p, n, t))) || t <= 0 || (2 * w + n) >= p) return(NULL)
  list(type = "biex", w = w, p = p, n = n, t = t)
}

flowjo_transform_column <- function(x, setting) {
  if (is.null(setting) || is.null(setting$type)) return(x)
  if (identical(setting$type, "log1p")) return(suppressWarnings(log1p(x)))
  if (!identical(setting$type, "biex")) return(x)
  if (is.null(setting$w) || is.null(setting$p) || is.null(setting$n)) return(x)
  if ((2 * setting$w + setting$n) >= setting$p) return(x)

  tf <- tryCatch(flowCore::logicleTransform(w = setting$w, t = setting$t %||% 262144, m = setting$p, a = setting$n), error = function(e) NULL)
  if (is.null(tf)) x else tf(x)
}

flowjo_inverse_transform_column <- function(x, setting) {
  if (is.null(setting) || is.null(setting$type)) return(x)
  if (identical(setting$type, "log1p")) return(suppressWarnings(expm1(x)))
  if (!identical(setting$type, "biex")) return(x)
  if (is.null(setting$w) || is.null(setting$p) || is.null(setting$n)) return(x)
  if ((2 * setting$w + setting$n) >= setting$p) return(x)

  tf <- tryCatch(flowCore::logicleTransform(w = setting$w, t = setting$t %||% 262144, m = setting$p, a = setting$n), error = function(e) NULL)
  if (is.null(tf)) return(x)
  inv <- flowCore::inverseLogicleTransform(trans = tf)
  tryCatch(inv(x), error = function(e) x)
}

flowjo_transform_data <- function(df, settings) {
  out <- df
  for (marker in intersect(names(settings), colnames(out))) {
    out[[marker]] <- flowjo_transform_column(out[[marker]], settings[[marker]])
  }
  out
}

flowjo_mfi_labels <- function(x) {
  superscript <- function(v) {
    chars <- strsplit(as.character(v), "", fixed = TRUE)[[1]]
    map <- c("-" = "\u207b", "0" = "\u2070", "1" = "\u00b9", "2" = "\u00b2", "3" = "\u00b3", "4" = "\u2074", "5" = "\u2075", "6" = "\u2076", "7" = "\u2077", "8" = "\u2078", "9" = "\u2079")
    paste0(vapply(chars, function(ch) unname(map[[ch]] %||% ch), character(1)), collapse = "")
  }
  vapply(x, function(val) {
    if (!is.finite(val)) return("")
    if (val == 0) return("0")
    av <- abs(val)
    if (av >= 1e6 || av < 1e-2) {
      pow <- floor(log10(av))
      coef <- val / (10 ^ pow)
      coef <- sub("\\.?0+$", "", format(signif(coef, 3), scientific = FALSE, trim = TRUE))
      prefix <- if (identical(coef, "1")) "" else paste0(coef, "\u00d7")
      return(sprintf("%s10%s", prefix, superscript(pow)))
    }
    if (av >= 1e3) return(format(round(val), scientific = FALSE, trim = TRUE, big.mark = ","))
    out <- format(signif(val, 4), scientific = FALSE, trim = TRUE)
    if (grepl(".", out, fixed = TRUE)) out <- sub("\\.?0+$", "", out)
    out
  }, character(1))
}

flowjo_axis_labeler <- function(setting = NULL) {
  function(breaks) {
    vals <- breaks
    if (!is.null(setting) && identical(setting$type, "log1p")) {
      vals <- expm1(breaks)
    } else if (!is.null(setting) && identical(setting$type, "biex")) {
        tf <- tryCatch(flowCore::logicleTransform(w = setting$w, t = setting$t %||% 262144, m = setting$p, a = setting$n), error = function(e) NULL)
      if (!is.null(tf)) {
        inv <- flowCore::inverseLogicleTransform(trans = tf)
        vals <- tryCatch(inv(breaks), error = function(e) breaks)
      }
    }
    flowjo_mfi_labels(vals)
  }
}

flowjo_feature_theme_choice <- function(xvar, yvar, theme_choice) {
  if (isTRUE(flowjo_is_embedding_axis(xvar)) && isTRUE(flowjo_is_embedding_axis(yvar))) return(theme_choice)
  if (identical(theme_choice, "UMAP Arrows")) "Cowplot" else theme_choice
}

flowjo_real_axis_theme <- function(scale_factor = 1) {
  theme(
    axis.line = element_line(linewidth = 0.35 * scale_factor, colour = "black"),
    axis.ticks = element_line(linewidth = 0.35 * scale_factor, colour = "black"),
    axis.ticks.length = unit(3, "pt")
  )
}
