# R/wsp_helpers.R

if (!exists("%||%", mode = "function")) `%||%` <- function(a, b) if (!is.null(a)) a else b

flowjo_reference_inventory <- function(path) {
  if (!dir.exists(path)) return(data.frame(type = character(), path = character(), name = character()))
  files <- list.files(path, pattern = "\\.(fcs|wsp)$", full.names = TRUE, ignore.case = TRUE)
  data.frame(
    type = tolower(tools::file_ext(files)),
    path = files,
    name = basename(files),
    stringsAsFactors = FALSE
  )
}

flowjo_wsp_groups <- function(wsp_file) {
  txt <- readLines(wsp_file, warn = FALSE)
  cur <- sub('.*curGroup="([^"]+)".*', "\\1", txt[grep("curGroup=", txt)][1] %||% "")
  groups <- sub('.*<GroupNode name="([^"]+)".*', "\\1", grep("<GroupNode name=", txt, value = TRUE))
  groups <- unique(groups[nzchar(groups)])
  list(current = if (nzchar(cur)) cur else groups[1] %||% NULL, groups = groups)
}

flowjo_wsp_import_report <- function(wsp_file, fcs_path = dirname(wsp_file), execute = FALSE, group_name = NULL) {
  out <- list(
    wsp_file = wsp_file,
    fcs_path = fcs_path,
    group_name = group_name,
    groups = character(),
    samples = data.frame(),
    populations = character(),
    unsupported = character(),
    error = NULL,
    gating_set = NULL
  )
  if (!file.exists(wsp_file)) {
    out$error <- "WSP file does not exist."
    return(out)
  }

  group_info <- flowjo_wsp_groups(wsp_file)
  out$groups <- group_info$groups
  group_name <- group_name %||% group_info$current
  out$group_name <- group_name

  ws <- tryCatch(CytoML::open_flowjo_xml(wsp_file), error = function(e) e)
  if (inherits(ws, "error")) {
    out$error <- conditionMessage(ws)
    return(out)
  }

  gs <- tryCatch(
    CytoML::flowjo_to_gatingset(ws, name = group_name, path = fcs_path, execute = execute, includeGates = TRUE, skip_faulty_gate = TRUE),
    error = function(e) e
  )
  if (inherits(gs, "error")) {
    out$error <- conditionMessage(gs)
    return(out)
  }

  out$gating_set <- gs
  out$populations <- tryCatch(flowWorkspace::gs_get_pop_paths(gs, path = "full"), error = function(e) character())
  out$samples <- tryCatch(as.data.frame(flowWorkspace::pData(gs)), error = function(e) data.frame())
  out
}

flowjo_wsp_export_report <- function(gating_set, out_file) {
  if (is.null(gating_set)) return(list(ok = FALSE, error = "No imported GatingSet is available."))
  res <- tryCatch({
    CytoML::gatingset_to_flowjo(gating_set, out_file)
    list(ok = TRUE, error = NULL, out_file = out_file)
  }, error = function(e) list(ok = FALSE, error = conditionMessage(e), out_file = out_file))
  res
}
