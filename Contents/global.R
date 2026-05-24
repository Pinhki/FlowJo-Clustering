# global.R
options(shiny.maxRequestSize = 10 * 1024^3) # 10 GB upload limit for large FCS files

if (!requireNamespace("BiocManager", quietly = TRUE)) install.packages("BiocManager")
if (!requireNamespace("flowCore", quietly = TRUE))    BiocManager::install("flowCore")
if (!requireNamespace("uwot", quietly = TRUE))        install.packages("uwot")
if (!requireNamespace("FlowSOM", quietly = TRUE))     BiocManager::install("FlowSOM")
if (!requireNamespace("SingleCellExperiment", quietly = TRUE)) BiocManager::install("SingleCellExperiment")
if (!requireNamespace("SummarizedExperiment", quietly = TRUE)) BiocManager::install("SummarizedExperiment")
if (!requireNamespace("S4Vectors", quietly = TRUE)) BiocManager::install("S4Vectors")
if (!requireNamespace("RColorBrewer", quietly = TRUE))install.packages("RColorBrewer")
if (!requireNamespace("bslib", quietly = TRUE))       install.packages("bslib")
if (!requireNamespace("DT", quietly = TRUE))          install.packages("DT")
if (!requireNamespace("data.table", quietly = TRUE))  install.packages("data.table")
if (!requireNamespace("igraph", quietly = TRUE))      install.packages("igraph")

library(shiny); library(shinyjs); library(colourpicker); library(ggplot2)
library(dplyr); library(stringr); library(RColorBrewer); library(patchwork)
library(shinycssloaders); library(scales); library(cowplot); library(flowCore)
library(htmltools); library(bslib); library(DT); library(plotly); library(htmlwidgets); library(svglite)
library(data.table); library(SingleCellExperiment); library(SummarizedExperiment); library(S4Vectors)

addResourcePath("res", "Resources")
`%||%` <- function(a,b) if(!is.null(a)) a else b

# Helpers & shared store
source("R/global/utils_helpers.R")
source("R/global/flowjo_pipeline_helpers.R")
source("R/global/shared_store.R")
source("R/interface/shared/spinner_overlay.R")

# Shared UI components
source("R/interface/shared/card_page_template.R")
source("R/interface/components/component_navbar.R")
source("R/interface/components/component_footer.R")
source("R/interface/components/component_right_sidebar.R")

# Pages
source("R/interface/page_import.R")
source("R/interface/page_feature.R")
source("R/interface/page_flowsom.R")
source("R/interface/page_umap_flowsom.R")
source("R/interface/page_gating_concat.R")
