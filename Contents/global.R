# global.R
options(shiny.maxRequestSize = 10 * 1024^3) # 10 GB upload limit for large FCS files

if (!requireNamespace("BiocManager", quietly = TRUE)) install.packages("BiocManager")
cran_packages <- c(
  "uwot", "RColorBrewer", "bslib", "DT", "data.table", "igraph",
  "arrow", "digest", "uuid", "fs", "withr", "sp", "sf", "geometry",
  "pracma", "qs2", "jsonlite", "vroom", "tibble", "dplyr", "ggplot2",
  "ggrastr", "scattermore", "hexbin", "viridisLite", "svglite", "ragg",
  "Cairo", "htmlwidgets", "webshot2", "zip", "quarto", "knitr",
  "rmarkdown", "testthat", "waldo", "validate", "checkmate", "cli",
  "logger"
)
bioc_packages <- c(
  "flowCore", "FlowSOM", "flowWorkspace", "CytoML", "openCyto",
  "SingleCellExperiment", "SummarizedExperiment", "S4Vectors"
)
for (pkg in cran_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) install.packages(pkg)
}
for (pkg in bioc_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) BiocManager::install(pkg)
}

library(shiny); library(shinyjs); library(colourpicker); library(ggplot2)
library(dplyr); library(stringr); library(RColorBrewer); library(patchwork)
library(shinycssloaders); library(scales); library(cowplot); library(flowCore)
library(htmltools); library(bslib); library(DT); library(plotly); library(htmlwidgets); library(svglite)
library(data.table); library(SingleCellExperiment); library(SummarizedExperiment); library(S4Vectors)

addResourcePath("res", "Resources")
`%||%` <- function(a,b) if(!is.null(a)) a else b

# Helpers & shared store
source("R/global/utils_helpers.R")
source("R/global/axis_scale_helpers.R")
source("R/global/gating_helpers.R")
source("R/global/wsp_helpers.R")
source("R/global/flowjo_pipeline_helpers.R")
source("R/global/shared_store.R")
source("R/interface/shared/spinner_overlay.R")
source("R/interface/shared/motion_color_picker_module.R")

# Shared UI components
source("R/interface/shared/card_page_template.R")
source("R/interface/components/component_navbar.R")
source("R/interface/components/component_footer.R")
source("R/interface/components/component_right_sidebar.R")

# Pages
source("R/interface/page_import.R")
source("R/interface/page_workspace.R")
source("R/interface/page_compensation.R")
source("R/interface/page_gating.R")
source("R/interface/page_statistics.R")
source("R/interface/page_layout_editor.R")
source("R/interface/page_import_export.R")
source("R/interface/page_feature.R")
source("R/interface/page_flowsom.R")
source("R/interface/page_umap_flowsom.R")
source("R/interface/page_gating_concat.R")
