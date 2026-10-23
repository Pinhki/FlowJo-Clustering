# This script prepares the environment with the CORE UI packages,
# then launches the main FlowJo clustering application.
# --- Step 1: Find this script's own location and set the working directory ---
# This is the most robust way to ensure all relative paths in the app work correctly,
# regardless of how or from where this script is launched.
message("\n======================= R files successfully loaded ========================")

options(repos = c(CRAN = "https://cloud.r-project.org"))

tryCatch({
  # This command gets the path of the currently running script
  script_path <- dirname(normalizePath(sub("--file=", "", commandArgs(trailingOnly = FALSE)[4])))
  # Set the working directory to that path
  setwd(script_path)
  message("--- Working directory successfully set to: ", getwd(), " ---")
}, error = function(e) {
  message("--- Could not automatically set working directory. Assuming it is correct. ---")
})

# --- Stage 1: Install the "UI Shell" Packages ---
# These packages are required just to display and run the app's interface.
# The heavy scientific packages are handled by the app's "Setup" page.
if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")
if (!requireNamespace("flowCore", quietly = TRUE))
  BiocManager::install("flowCore")

core_ui_packages <- c(
  "here",
  "remotes",
  "shiny",
  "shinyFiles",
  "bslib",
  "DT",
  "plotly",
  "htmlwidgets",
  "svglite",
  "qs2",
  "shinyjs",
  "colourpicker",
  "ggplot2",
  "ggalluvial",
  "dplyr",
  "stringr",
  "RColorBrewer",
  "patchwork",
  "shinycssloaders",
  "scales",
  "cowplot"
  )
library(here)
cran_mirror <- "https://cloud.r-project.org"
message("--- Checking for Core UI Packages ---")

#pkgbuild::has_build_tools(debug = TRUE)

# Loop through the core packages
for (pkg in core_ui_packages) {
  # requireNamespace checks without loading the package. It's cleaner.
  if (!requireNamespace(pkg, quietly = TRUE)) {
    
    # If not installed, inform the user and install it.
    message(paste("Required package '", pkg, "' not found. Installing now...", sep = ""))
    install.packages(pkg, repos = "https://cloud.r-project.org")
    
  } else {
    message(paste("Found required package '", pkg, "'.", sep = ""))
  }
}

message("--- Core UI packages are ready. ---")
message("")

# --- Stage 2: Launch the Shiny Application ---
message("Starting the application... Your web browser should open shortly.")

# Use shiny::runApp from the now-guaranteed 'shiny' package.
tryCatch({
  
  # This is the command that launches your app.
  shiny::runApp(
    appDir = getwd(), 
    launch.browser = TRUE
  )
  
}, error = function(e) {
  
  # If any error happens while trying to start the app, this code will run.
  message("\n======================= FATAL ERROR ========================")
  message("The application failed to start. The specific error was:")
  message(e) # This prints the exact R error message.
  message("\n==========================================================")
  message("Please review the error above. It is likely an issue in app.R or one of the sourced UI/server files.")
  
})
