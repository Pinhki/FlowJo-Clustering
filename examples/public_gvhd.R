# Run from the repository root: Rscript --vanilla examples/public_gvhd.R
# Public package example; this demonstration gate does not identify a cell type.
library(flowCore)
cat("R", as.character(getRversion()), "flowCore", as.character(packageVersion("flowCore")), "\n")
`%||%` <- function(a, b) if (is.null(a)) b else a
source("Contents/R/global/gating_helpers.R")
source("Contents/R/global/flowjo_pipeline_helpers.R")
data("GvHD", package = "flowCore")
frame <- GvHD[[1]]
events <- as.data.frame(exprs(frame))
map <- data.frame(channel = names(events), marker = names(events))
bounds <- list(xmin = 200, xmax = 800, ymin = 100, ymax = 800)
gate <- flowjo_make_gate("example", "rectangle", "root", "FSC-H", "SSC-H", bounds)
selected <- flowjo_gate_membership(events, map, gate)
# Independent direct comparison checks the helper against the stated rule.
expected <- with(events, `FSC-H` >= 200 & `FSC-H` <= 800 & `SSC-H` >= 100 & `SSC-H` <= 800)
stopifnot(identical(selected, expected), length(selected) == nrow(events))
args <- commandArgs(trailingOnly = TRUE)
if (length(args)) {
  dir.create(args[1], recursive = TRUE, showWarnings = FALSE)
  flowCore::write.FCS(frame, file.path(args[1], "public-gvhd-example.fcs"))
  png(file.path(args[1], "public-gvhd-gate.png"), width = 1000, height = 700)
  plot(events[["FSC-H"]], events[["SSC-H"]], pch = 16, cex = 0.35,
       col = ifelse(selected, "#2463A5", "#999999"), xlab = "FSC-H", ylab = "SSC-H",
       main = "Public GvHD example: demonstration rectangle")
  rect(200, 100, 800, 800, border = "#2463A5", lwd = 2)
  dev.off()
}
cat(sprintf("PASS: %d events; %d satisfy the stated rectangle.\n", nrow(events), sum(selected)))
cat("Basis: assistant-defined FSC-H 200-800 and SSC-H 100-800, inclusive; no cell-type annotation.\n")
