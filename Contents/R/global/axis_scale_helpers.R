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

# Agent note: FlowJo's WSP biexponential is calibrated in a 0..channelRange
# display space and is not numerically identical to flowCore's logicle output.
# Keep the original calibration parameters so imported gates and points use the
# same display transform FlowJo recorded; manual axis edits still use flowCore.
flowjo_wsp_biexp_setting <- function(parameters) {
  if (is.null(parameters) || !is.list(parameters)) return(NULL)
  get_num <- function(name, fallback = NA_real_) {
    value <- suppressWarnings(as.numeric(parameters[[name]] %||% fallback))
    if (length(value)) value[[1]] else fallback
  }
  channel_range <- get_num("channelRange")
  max_value <- get_num("maxValue")
  pos <- get_num("pos")
  neg <- get_num("neg", 0)
  width_basis <- get_num("widthBasis")
  if (!all(is.finite(c(channel_range, max_value, pos, neg, width_basis))) ||
      channel_range <= 0 || max_value <= 0 || pos <= 0 || width_basis >= 0) {
    return(NULL)
  }
  list(
    type = "biex",
    engine = "flowjo",
    source = "wsp",
    # These proxy values keep the existing axis modal readable. The exact
    # transform path below uses width_basis/channel_range/max_value instead.
    w = log10(abs(width_basis)) / 2,
    p = pos,
    n = neg,
    t = max_value,
    channel_range = channel_range,
    width_basis = width_basis,
    wsp_parameters = parameters
  )
}

flowjo_default_biexp_setting <- function(channel_range = 4096, max_value = 262144,
                                         pos = 4.5, neg = 0, width_basis = -10) {
  setting <- flowjo_wsp_biexp_setting(list(
    channelRange = channel_range,
    maxValue = max_value,
    pos = pos,
    neg = neg,
    widthBasis = width_basis
  ))
  if (!is.null(setting)) setting$source <- "default"
  setting
}

flowjo_wsp_transform_column <- function(x, setting, inverse = FALSE) {
  if (is.null(setting) || !identical(setting$engine %||% NULL, "flowjo")) return(NULL)
  if (!requireNamespace("flowWorkspace", quietly = TRUE)) return(NULL)
  channel_range <- suppressWarnings(as.numeric(setting$channel_range %||% 256))
  max_value <- suppressWarnings(as.numeric(setting$t %||% setting$max_value %||% 262144))
  pos <- suppressWarnings(as.numeric(setting$p %||% setting$pos %||% 4.5))
  neg <- suppressWarnings(as.numeric(setting$n %||% setting$neg %||% 0))
  width_basis <- suppressWarnings(as.numeric(setting$width_basis %||% -10))
  if (!all(is.finite(c(channel_range, max_value, pos, neg, width_basis))) ||
      channel_range <= 0 || max_value <= 0 || pos <= 0 || width_basis >= 0) {
    return(NULL)
  }
  transform <- tryCatch(
    flowWorkspace::flowjo_biexp(
      channelRange = channel_range,
      maxValue = max_value,
      pos = pos,
      neg = neg,
      widthBasis = width_basis,
      inverse = isTRUE(inverse)
    ),
    error = function(e) NULL
  )
  if (is.null(transform)) return(NULL)
  tryCatch(transform(x), error = function(e) NULL)
}

flowjo_transform_column <- function(x, setting) {
  if (is.null(setting) || is.null(setting$type)) return(x)
  if (identical(setting$type, "log1p")) return(suppressWarnings(log1p(x)))
  if (identical(setting$type, "linear")) {
    gain <- suppressWarnings(as.numeric(setting$gain %||% 1))
    min_range <- suppressWarnings(as.numeric(setting$min_range %||% 0))
    if (all(is.finite(c(gain, min_range))) && gain != 0) return((x - min_range) * gain)
    return(x)
  }
  if (!identical(setting$type, "biex")) return(x)
  if (identical(setting$engine %||% NULL, "flowjo")) {
    out <- flowjo_wsp_transform_column(x, setting, inverse = FALSE)
    if (!is.null(out)) return(out)
  }
  if (is.null(setting$w) || is.null(setting$p) || is.null(setting$n)) return(x)
  if ((2 * setting$w + setting$n) >= setting$p) return(x)

  tf <- tryCatch(flowCore::logicleTransform(w = setting$w, t = setting$t %||% 262144, m = setting$p, a = setting$n), error = function(e) NULL)
  if (is.null(tf)) x else tf(x)
}

flowjo_inverse_transform_column <- function(x, setting) {
  if (is.null(setting) || is.null(setting$type)) return(x)
  if (identical(setting$type, "log1p")) return(suppressWarnings(expm1(x)))
  if (identical(setting$type, "linear")) {
    gain <- suppressWarnings(as.numeric(setting$gain %||% 1))
    min_range <- suppressWarnings(as.numeric(setting$min_range %||% 0))
    if (all(is.finite(c(gain, min_range))) && gain != 0) return(x / gain + min_range)
    return(x)
  }
  if (!identical(setting$type, "biex")) return(x)
  if (identical(setting$engine %||% NULL, "flowjo")) {
    out <- flowjo_wsp_transform_column(x, setting, inverse = TRUE)
    if (!is.null(out)) return(out)
  }
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
    } else if (!is.null(setting) && identical(setting$type, "linear")) {
      vals <- flowjo_inverse_transform_column(breaks, setting)
    } else if (!is.null(setting) && identical(setting$type, "biex")) {
      if (identical(setting$engine %||% NULL, "flowjo")) {
        exact <- flowjo_inverse_transform_column(breaks, setting)
        if (!is.null(exact)) return(flowjo_mfi_labels(exact))
      }
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
