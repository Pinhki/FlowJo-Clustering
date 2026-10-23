# R/wsp_helpers.R

if (!exists("%||%", mode = "function")) `%||%` <- function(a, b) if (!is.null(a)) a else b

flowjo_reference_inventory <- function(path) {
  empty <- data.frame(
    type = character(), path = character(), name = character(), relative_path = character(),
    stringsAsFactors = FALSE
  )
  if (!dir.exists(path)) return(empty)
  root <- normalizePath(path, winslash = "/", mustWork = FALSE)
  files <- list.files(
    root,
    pattern = "\\.(fcs|wsp)$",
    full.names = TRUE,
    recursive = TRUE,
    include.dirs = FALSE,
    ignore.case = TRUE
  )
  relative <- vapply(normalizePath(files, winslash = "/", mustWork = FALSE), function(file) {
    prefix <- if (endsWith(root, "/")) root else paste0(root, "/")
    if (startsWith(file, prefix)) substring(file, nchar(prefix) + 1L) else basename(file)
  }, character(1))
  data.frame(
    type = tolower(tools::file_ext(files)),
    path = files,
    name = basename(files),
    relative_path = relative,
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

flowjo_wsp_xml_metadata <- function(wsp_file) {
  empty <- list(workspace = list(), samples = list(), groups = list(), populations = list(), transforms = list(), error = NULL)
  if (length(wsp_file) != 1L || is.na(wsp_file) || !nzchar(wsp_file) || !file.exists(wsp_file)) {
    empty$error <- "WSP file does not exist."
    return(empty)
  }
  if (!requireNamespace("xml2", quietly = TRUE)) {
    empty$error <- "Package 'xml2' is required to read WSP metadata."
    return(empty)
  }
  doc <- tryCatch(xml2::read_xml(wsp_file), error = function(e) e)
  if (inherits(doc, "error")) {
    empty$error <- conditionMessage(doc)
    return(empty)
  }

  attr_value <- function(node, key, fallback = "") {
    if (!length(node)) return(fallback)
    attrs <- xml2::xml_attrs(node)
    key_local <- sub("^.*:", "", key)
    hits <- which(names(attrs) == key | sub("^.*:", "", names(attrs)) == key_local)
    value <- if (length(hits)) attrs[[hits[[1]]]] else NA_character_
    if (!length(value) || is.na(value) || !nzchar(value)) fallback else as.character(value)
  }
  number_value <- function(value, fallback = NA_real_) {
    out <- suppressWarnings(as.numeric(value %||% fallback))
    if (length(out) && is.finite(out[[1]])) out[[1]] else fallback
  }
  local_name <- function(node) {
    if (!length(node)) return("")
    sub("^.*:", "", xml2::xml_name(node))
  }
  attrs_as_list <- function(node) {
    attrs <- if (length(node)) xml2::xml_attrs(node) else character()
    if (!length(attrs)) return(list())
    as.list(attrs)
  }
  population_path <- function(node) {
    parts <- attr_value(node, "name")
    parent <- xml2::xml_parent(node)
    while (length(parent)) {
      if (identical(local_name(parent), "Population")) {
        parts <- c(attr_value(parent, "name"), parts)
      }
      if (identical(local_name(parent), "GroupNode")) break
      parent <- xml2::xml_parent(parent)
    }
    parts <- parts[nzchar(parts)]
    if (length(parts)) paste0("/", paste(parts, collapse = "/")) else ""
  }
  node_axes <- function(graph) {
    if (!length(graph)) return(list(x = "", y = ""))
    axes <- xml2::xml_find_all(graph, "./*[local-name()='Axis']")
    out <- list(x = "", y = "")
    for (axis in axes) {
      dimension <- attr_value(axis, "dimension")
      if (dimension %in% names(out)) out[[dimension]] <- attr_value(axis, "name")
    }
    out
  }

  root <- xml2::xml_root(doc)
  workspace <- list(
    version = attr_value(root, "version"),
    modified_at = attr_value(root, "modDate"),
    flowjo_version = attr_value(root, "flowJoVersion"),
    current_group = attr_value(root, "curGroup")
  )

  samples <- list()
  datasets <- xml2::xml_find_all(root, "//*[local-name()='Sample']/*[local-name()='DataSet']")
  for (dataset in datasets) {
    sample_id <- attr_value(dataset, "sampleID")
    if (!nzchar(sample_id)) next
    samples[[sample_id]] <- list(
      sample_id = sample_id,
      uri = attr_value(dataset, "uri"),
      name = "",
      annotation = "",
      owning_group = "",
      count = NA_real_,
      wsp_attributes = attrs_as_list(dataset)
    )
  }
  sample_nodes <- xml2::xml_find_all(root, "//*[local-name()='SampleNode']")
  for (sample_node in sample_nodes) {
    sample_id <- attr_value(sample_node, "sampleID")
    if (!nzchar(sample_id)) next
    current <- samples[[sample_id]] %||% list(sample_id = sample_id)
    current$name <- attr_value(sample_node, "name", current$name %||% "")
    current$annotation <- attr_value(sample_node, "annotation", current$annotation %||% "")
    current$owning_group <- attr_value(sample_node, "owningGroup", current$owning_group %||% "")
    current$count <- number_value(attr_value(sample_node, "count"), current$count %||% NA_real_)
    current$wsp_attributes <- c(current$wsp_attributes %||% list(), attrs_as_list(sample_node))
    samples[[sample_id]] <- current
  }

  groups <- list()
  populations <- list()
  group_nodes <- xml2::xml_find_all(root, "//*[local-name()='GroupNode']")
  for (group_node in group_nodes) {
    group <- xml2::xml_find_first(group_node, "./*[local-name()='Group']")
    refs <- if (length(group)) xml2::xml_find_all(group, ".//*[local-name()='SampleRef']") else list()
    sample_ref_ids <- if (length(refs)) vapply(refs, attr_value, character(1), key = "sampleID") else character()
    population_nodes <- xml2::xml_find_all(group_node, ".//*[local-name()='Population']")
    population_paths <- character()
    for (population in population_nodes) {
      path <- population_path(population)
      if (!nzchar(path)) next
      graph <- xml2::xml_find_first(population, "./*[local-name()='Graph']")
      gate <- xml2::xml_find_first(population, "./*[local-name()='Gate']/*")
      gate_type <- local_name(gate)
      tint <- attr_value(gate, "tint")
      pop_meta <- list(
        path = path,
        name = attr_value(population, "name"),
        annotation = attr_value(population, "annotation"),
        count = number_value(attr_value(population, "count")),
        gate_id = attr_value(population, "gating:id", attr_value(gate, "gating:id")),
        gate_type = gate_type,
        tint = tint,
        is_tinted = identical(attr_value(gate, "isTinted"), "1"),
        line_weight = attr_value(gate, "lineWeight"),
        annotation_offset = c(
          x = number_value(attr_value(gate, "annoOffsetX")),
          y = number_value(attr_value(gate, "annoOffsetY"))
        ),
        graph = list(
          type = attr_value(graph, "type"),
          heat_map_stat_parameter = attr_value(graph, "heatMapStatParameter"),
          axes = node_axes(graph),
          settings = if (length(graph)) {
            settings <- xml2::xml_find_first(graph, "./*[local-name()='GraphSettings']")
            if (length(settings) && !inherits(settings, "xml_missing")) {
              as.list(stats::setNames(
                vapply(xml2::xml_attrs(settings), as.character, character(1)),
                names(xml2::xml_attrs(settings))
              ))
            } else list()
          } else list(),
          environment = if (length(graph)) {
            environment <- xml2::xml_find_first(graph, "./*[local-name()='GraphEnvironment']")
            attrs_as_list(environment)
          } else list()
        )
      )
      pop_meta$wsp_attributes <- attrs_as_list(population)
      pop_meta$gate_attributes <- attrs_as_list(gate)
      populations[[path]] <- pop_meta
      population_paths <- c(population_paths, path)
    }
    name <- attr_value(group_node, "name", attr_value(group, "name"))
    groups[[length(groups) + 1L]] <- list(
      name = name,
      annotation = attr_value(group_node, "annotation"),
      owning_group = attr_value(group_node, "owningGroup"),
      role = attr_value(group, "role"),
      color = attr_value(group, "foreground"),
      synchronized = identical(attr_value(group, "synchronized"), "1"),
      sample_ref_ids = unique(sample_ref_ids[nzchar(sample_ref_ids)]),
      population_paths = unique(population_paths),
      wsp_attributes = attrs_as_list(group_node),
      group_attributes = attrs_as_list(group)
    )
  }

  transforms <- list()
  matrix_nodes <- xml2::xml_find_all(root, "//*[local-name()='MatrixID']")
  coerce_transform_attribute <- function(value) {
    numeric_value <- suppressWarnings(as.numeric(value))
    if (length(numeric_value) && is.finite(numeric_value[[1]])) numeric_value[[1]] else as.character(value)
  }
  for (matrix_node in matrix_nodes) {
    matrix_id <- attr_value(matrix_node, "matrixId")
    transform_nodes <- xml2::xml_find_all(matrix_node, "./*[local-name()='Transforms']/*")
    for (transform_node in transform_nodes) {
      parameter <- xml2::xml_find_first(transform_node, "./*[local-name()='parameter']")
      parameter_name <- attr_value(parameter, "name")
      if (!nzchar(parameter_name)) next
      attrs <- xml2::xml_attrs(transform_node)
      parameters <- lapply(attrs, coerce_transform_attribute)
      names(parameters) <- names(attrs)
      transforms[[parameter_name]] <- list(
        type = local_name(transform_node),
        parameters = parameters,
        matrix_id = matrix_id
      )
    }
  }

  list(workspace = workspace, samples = samples, groups = groups, populations = populations, transforms = transforms, error = NULL)
}

flowjo_wsp_xml_transform_specs <- function(metadata, channel_map = NULL) {
  empty <- list(settings = list(), metadata = list(), unsupported = character())
  transforms <- metadata$transforms %||% list()
  if (!length(transforms)) return(empty)
  settings <- list()
  transform_metadata <- list()
  unsupported <- character()
  add_setting_keys <- function(name, setting) {
    keys <- name
    if (!is.null(channel_map) && nrow(channel_map) && all(c("channel", "marker") %in% names(channel_map))) {
      keys <- unique(c(
        keys,
        as.character(channel_map$channel[match(name, channel_map$marker)]),
        as.character(channel_map$marker[match(name, channel_map$channel)])
      ))
    }
    keys <- keys[!is.na(keys) & nzchar(keys)]
    for (key in keys) settings[[key]] <<- setting
  }
  for (name in names(transforms)) {
    item <- transforms[[name]]
    type <- tolower(as.character(item$type %||% ""))
    transform_metadata[[name]] <- item
    if (!identical(type, "linear")) {
      unsupported <- c(unsupported, name)
      next
    }
    params <- item$parameters %||% list()
    min_range <- suppressWarnings(as.numeric(params$minRange %||% 0))
    max_range <- suppressWarnings(as.numeric(params$maxRange %||% NA_real_))
    gain <- suppressWarnings(as.numeric(params$gain %||% 1))
    if (!all(is.finite(c(min_range, max_range, gain))) || gain == 0) {
      unsupported <- c(unsupported, name)
      next
    }
    add_setting_keys(name, list(
      type = "linear", engine = "flowjo", source = "wsp",
      min_range = min_range, max_range = max_range, gain = gain,
      wsp_parameters = params, matrix_id = item$matrix_id %||% ""
    ))
  }
  list(settings = settings, metadata = transform_metadata, unsupported = unique(unsupported))
}

flowjo_gate_raw_coordinates <- function(gate, inverse) {
  # Agent note: CytoML gate geometry is transformed too; undo exactly the sample's transform before canvas transforms it.
  restore <- function(values, channel) {
    fn <- inverse[[channel]]
    if (is.null(fn)) return(values)
    finite <- is.finite(values)
    values[finite] <- fn(values[finite])
    values
  }
  geom <- gate$geometry
  if (identical(gate$type, "polygon")) {
    geom$vertices$x <- restore(geom$vertices$x, gate$x_channel)
    geom$vertices$y <- restore(geom$vertices$y, gate$y_channel)
  } else {
    for (key in intersect(c("xmin", "xmax"), names(geom))) geom[[key]] <- restore(geom[[key]], gate$x_channel)
    for (key in intersect(c("ymin", "ymax"), names(geom))) geom[[key]] <- restore(geom[[key]], gate$y_channel)
  }
  gate$geometry <- geom
  gate
}

flowjo_wsp_transform_specs <- function(gating_hierarchy, channel_map = NULL) {
  empty <- list(settings = list(), metadata = list(), unsupported = character())
  if (is.null(gating_hierarchy) || !requireNamespace("flowWorkspace", quietly = TRUE)) return(empty)
  transforms <- tryCatch(
    flowWorkspace::gh_get_transformations(gating_hierarchy, only.function = FALSE),
    error = function(e) NULL
  )
  if (is.null(transforms) || !length(transforms)) return(empty)
  settings <- list()
  metadata <- list()
  unsupported <- character()
  for (channel in names(transforms)) {
    transform <- transforms[[channel]]
    parameters <- attr(transform$transform, "parameters")
    type <- tolower(as.character(attr(transform$transform, "type") %||% ""))
    metadata[[channel]] <- list(
      type = type,
      parameters = parameters,
      domain = transform$domain %||% NULL
    )
    setting <- if (type %in% c("biexp", "biex", "flowjo_biexp")) flowjo_wsp_biexp_setting(parameters) else NULL
    if (is.null(setting)) {
      unsupported <- c(unsupported, channel)
      next
    }
    settings[[channel]] <- setting
    if (!is.null(channel_map) && nrow(channel_map) && "channel" %in% names(channel_map) && "marker" %in% names(channel_map)) {
      marker <- channel_map$marker[match(channel, channel_map$channel)]
      if (length(marker) && !is.na(marker) && nzchar(marker)) settings[[marker]] <- setting
    }
  }
  list(settings = settings, metadata = metadata, unsupported = unique(unsupported))
}

flowjo_wsp_compensation_spec <- function(compensation_object, channel_map = NULL) {
  if (is.null(compensation_object)) return(NULL)
  spill <- if (is.matrix(compensation_object)) compensation_object else tryCatch(compensation_object@spillover, error = function(e) NULL)
  if (is.null(spill) || !is.matrix(spill) || nrow(spill) < 2L) return(NULL)
  spill <- as.matrix(spill)
  raw_channels <- rownames(spill) %||% colnames(spill) %||% character()
  if (!is.null(channel_map) && length(raw_channels) && nrow(channel_map) && "channel" %in% names(channel_map) && "marker" %in% names(channel_map)) {
    marker_map <- stats::setNames(as.character(channel_map$marker), as.character(channel_map$channel))
    mapped <- unname(marker_map[raw_channels])
    mapped[is.na(mapped) | !nzchar(mapped)] <- raw_channels[is.na(mapped) | !nzchar(mapped)]
    mapped <- make.unique(mapped, sep = "_")
    dimnames(spill) <- list(mapped, mapped)
  }
  list(
    matrix = spill,
    id = tryCatch(as.character(compensation_object@compensationId), error = function(e) ""),
    channels = raw_channels
  )
}

flowjo_wsp_dataset_paths <- function(wsp_file) {
  doc <- xml2::read_xml(wsp_file)
  uris <- xml2::xml_attr(xml2::xml_find_all(doc, "//*[local-name()='DataSet']"), "uri")
  uris <- uris[!is.na(uris)]
  paths <- sub("^file:(//)?", "", utils::URLdecode(uris))
  if (.Platform$OS.type == "windows") paths <- sub("^/([A-Za-z]:)", "\\1", paths)
  paths[nzchar(paths)]
}

# Agent note: The import page's reference table must stay scoped to the selected WSP;
# sibling WSPs in the same Raw/Unmixed parent are not references and must not appear.
flowjo_wsp_reference_inventory <- function(wsp_file, search_root = dirname(wsp_file)) {
  empty <- data.frame(
    type = character(), path = character(), name = character(), relative_path = character(),
    stringsAsFactors = FALSE
  )
  if (length(wsp_file) != 1L || is.na(wsp_file) || !nzchar(wsp_file) || !file.exists(wsp_file)) {
    return(empty)
  }

  wsp_path <- normalizePath(wsp_file, winslash = "/", mustWork = FALSE)
  search_root <- search_root %||% dirname(wsp_path)
  refs <- unique(flowjo_wsp_dataset_paths(wsp_path))
  if (!length(refs)) {
    return(data.frame(
      type = "wsp",
      path = wsp_path,
      name = basename(wsp_path),
      relative_path = basename(wsp_path),
      stringsAsFactors = FALSE
    ))
  }

  candidate_files <- if (dir.exists(search_root)) {
    list.files(
      search_root,
      pattern = "[.]fcs$",
      full.names = TRUE,
      recursive = TRUE,
      include.dirs = FALSE,
      ignore.case = TRUE
    )
  } else {
    character()
  }
  resolve_ref <- function(ref) {
    direct <- unique(c(ref, file.path(dirname(wsp_path), ref), file.path(search_root, ref)))
    existing <- direct[file.exists(direct)]
    if (length(existing)) return(normalizePath(existing[[1]], winslash = "/", mustWork = FALSE))
    hits <- candidate_files[basename(candidate_files) == basename(ref)]
    if (length(hits) == 1L) normalizePath(hits[[1]], winslash = "/", mustWork = FALSE) else NA_character_
  }
  resolved <- vapply(refs, resolve_ref, character(1))
  display_paths <- ifelse(!is.na(resolved), resolved, refs)
  root <- normalizePath(search_root, winslash = "/", mustWork = FALSE)
  relative <- vapply(seq_along(display_paths), function(i) {
    if (is.na(resolved[[i]])) return(refs[[i]])
    prefix <- if (endsWith(root, "/")) root else paste0(root, "/")
    if (startsWith(display_paths[[i]], prefix)) substring(display_paths[[i]], nchar(prefix) + 1L) else display_paths[[i]]
  }, character(1))

  data.frame(
    type = c("wsp", rep("fcs", length(display_paths))),
    path = c(wsp_path, display_paths),
    name = c(basename(wsp_path), basename(display_paths)),
    relative_path = c(basename(wsp_path), relative),
    stringsAsFactors = FALSE
  )
}

flowjo_wsp_data_path <- function(wsp_file, search_root = dirname(wsp_file)) {
  refs <- flowjo_wsp_dataset_paths(wsp_file)
  if (!length(refs)) return(NULL)
  existing <- refs[file.exists(refs)]
  if (length(existing) == length(refs)) {
    dirs <- unique(dirname(normalizePath(existing, winslash = "/", mustWork = FALSE)))
    if (length(dirs) == 1L) return(dirs[[1]])
  }

  if (!dir.exists(search_root)) return(NULL)
  candidates <- list.files(
    search_root,
    pattern = "[.]fcs$",
    full.names = TRUE,
    recursive = TRUE,
    include.dirs = FALSE,
    ignore.case = TRUE
  )
  resolved <- vapply(refs, function(ref) {
    if (file.exists(ref)) return(normalizePath(ref, winslash = "/", mustWork = FALSE))
    hits <- candidates[basename(candidates) == basename(ref)]
    if (length(hits) == 1L) normalizePath(hits[[1]], winslash = "/", mustWork = FALSE) else NA_character_
  }, character(1))
  if (all(!is.na(resolved))) {
    dirs <- unique(dirname(resolved))
    if (length(dirs) == 1L) return(dirs[[1]])
  }
  NULL
}

# Agent note: Stage explicit file mappings under original names; never modify the user's WSP or silently choose duplicate basenames.
flowjo_wsp_stage_files <- function(wsp_file, resolved_files, directory) {
  refs <- unique(flowjo_wsp_dataset_paths(wsp_file))
  if (!length(refs) || !all(refs %in% names(resolved_files))) stop("Every WSP FCS reference must have a resolved file.")
  paths <- unname(resolved_files[refs])
  if (anyNA(paths) || !all(file.exists(paths))) stop("Referenced FCS files are missing. Locate them before importing.")
  if (anyDuplicated(basename(refs))) stop("This WSP has duplicate FCS filenames. Import requires distinct source names.")
  dir.create(directory, recursive = TRUE, showWarnings = FALSE)
  targets <- file.path(directory, basename(refs))
  if (!all(file.symlink(normalizePath(paths, mustWork = TRUE), targets))) stop("Could not prepare the selected FCS files for import.")
  doc <- xml2::read_xml(wsp_file)
  datasets <- xml2::xml_find_all(doc, "//*[local-name()='DataSet']")
  for (node in datasets) {
    uri <- xml2::xml_attr(node, "uri")
    if (is.na(uri)) next
    ref <- sub("^file:(//)?", "", utils::URLdecode(uri))
    if (.Platform$OS.type == "windows") ref <- sub("^/([A-Za-z]:)", "\\1", ref)
    target <- targets[match(ref, refs)]
    if (length(target) != 1L || is.na(target)) stop("Could not match a WSP dataset to its source file.")
    xml2::xml_set_attr(node, "uri", paste0("file://", utils::URLencode(target, reserved = FALSE)))
  }
  staged_wsp <- file.path(directory, basename(wsp_file))
  xml2::write_xml(doc, staged_wsp)
  staged_wsp
}

flowjo_wsp_import_report <- function(wsp_file, fcs_path = dirname(wsp_file), execute = FALSE, group_name = NULL, resolved_files = NULL) {
  search_root <- fcs_path %||% dirname(wsp_file)
  out <- list(
    wsp_file = wsp_file,
    fcs_path = search_root,
    group_name = group_name,
    groups = character(),
    samples = data.frame(),
    populations = character(),
    unsupported = character(),
    error = NULL,
    gating_set = NULL,
    metadata = flowjo_wsp_xml_metadata(wsp_file)
  )
  if (!file.exists(wsp_file)) {
    out$error <- "WSP file does not exist."
    return(out)
  }

  group_info <- flowjo_wsp_groups(wsp_file)
  out$groups <- group_info$groups
  group_name <- group_name %||% group_info$current
  out$group_name <- group_name
  resolved_fcs_path <- flowjo_wsp_data_path(wsp_file, search_root)
  out$fcs_path <- resolved_fcs_path %||% search_root

  import_wsp <- wsp_file
  if (!is.null(resolved_files)) {
    staging_dir <- tempfile("flowjo-wsp-")
    on.exit(unlink(staging_dir, recursive = TRUE), add = TRUE)
    import_wsp <- flowjo_wsp_stage_files(wsp_file, resolved_files, staging_dir)
    out$fcs_path <- staging_dir
    out$resolved_files <- resolved_files
  }
  ws <- tryCatch(CytoML::open_flowjo_xml(import_wsp), error = function(e) e)
  if (inherits(ws, "error")) {
    out$error <- conditionMessage(ws)
    return(out)
  }

  gs <- tryCatch(
    CytoML::flowjo_to_gatingset(
      ws,
      name = group_name,
      path = out$fcs_path,
      execute = execute,
      includeGates = TRUE,
      include_empty_tree = TRUE,
      skip_faulty_gate = TRUE
    ),
    error = function(e) e
  )
  if (inherits(gs, "error")) {
    out$error <- conditionMessage(gs)
    return(out)
  }

  out$fcs_path <- if (!is.null(resolved_files)) search_root else out$fcs_path
  out$gating_set <- gs
  out$populations <- tryCatch(flowWorkspace::gs_get_pop_paths(gs, path = "full"), error = function(e) character())
  out$samples <- tryCatch(as.data.frame(flowWorkspace::pData(gs)), error = function(e) data.frame())
  out
}

# Agent note: A WSP report is not enough for the rest of this app. Workspace,
# gating, and analysis pages consume the shared FCS registry and gate memberships,
# so this adapter keeps the successful GatingSet import connected to those contracts.
flowjo_wsp_app_state <- function(report) {
  empty <- list(
    entries = list(), gates = list(), memberships = list(), groups = list(),
    sync_status = data.frame(), metadata = list(), error = NULL
  )
  if (is.null(report) || !is.null(report$error) || is.null(report$gating_set)) {
    empty$error <- report$error %||% "WSP import did not produce a usable GatingSet."
    return(empty)
  }

  gs <- report$gating_set
  sample_names <- tryCatch(flowWorkspace::sampleNames(gs), error = function(e) character())
  # Agent note: Undo transforms in each extracted frame below; inverse.transform=TRUE clones the entire on-disk cytoset.
  root_data <- tryCatch(flowWorkspace::gs_pop_get_data(gs, "root"), error = function(e) e)
  if (inherits(root_data, "error") || !length(sample_names) || length(root_data) != length(sample_names)) {
    empty$error <- if (inherits(root_data, "error")) conditionMessage(root_data) else "WSP samples could not be loaded into the app."
    return(empty)
  }

  refs <- flowjo_wsp_dataset_paths(report$wsp_file)
  sample_info <- report$samples %||% data.frame()
  sample_labels <- if (nrow(sample_info) == length(sample_names) && "name" %in% names(sample_info)) {
    as.character(sample_info$name)
  } else {
    sample_names
  }
  sample_ids <- paste0(
    "wsp_", sprintf("%03d", seq_along(sample_names)), "_",
    gsub("[^A-Za-z0-9]+", "_", tools::file_path_sans_ext(sample_labels))
  )
  wsp_metadata <- report$metadata %||% flowjo_wsp_xml_metadata(report$wsp_file)
  compensations <- tryCatch(flowWorkspace::gs_get_compensations(gs), error = function(e) vector("list", length(sample_names)))
  sample_metadata_for <- function(i) {
    key <- as.character(i)
    item <- wsp_metadata$samples[[key]] %||% NULL
    if (is.null(item) && length(wsp_metadata$samples) >= i) item <- wsp_metadata$samples[[i]]
    item %||% list(sample_id = key)
  }
  sample_group_names_for <- function(sample_key) {
    if (!length(wsp_metadata$groups)) return(character())
    names_now <- vapply(wsp_metadata$groups, function(group) {
      if (sample_key %in% (group$sample_ref_ids %||% character())) group$name %||% "" else ""
    }, character(1))
    unique(names_now[nzchar(names_now)])
  }

  # Agent note: Reuse sample inverse functions for events and gates; do not rebuild splines per gate or clone the GatingSet.
  inverse_by_sample <- lapply(seq_along(sample_names), function(i) {
    flowWorkspace::gh_get_transformations(gs[[i]], inverse = TRUE)
  })
  entries <- vector("list", length(sample_names))
  for (i in seq_along(sample_names)) {
    frame <- tryCatch(root_data[[i]], error = function(e) e)
    if (inherits(frame, "error")) {
      empty$error <- conditionMessage(frame)
      return(empty)
    }
    df <- tryCatch(as.data.frame(flowCore::exprs(frame)), error = function(e) NULL)
    if (is.null(df) || !ncol(df)) {
      empty$error <- paste0("No event data found for WSP sample: ", sample_labels[[i]])
      return(empty)
    }
    for (channel in intersect(names(inverse_by_sample[[i]]), colnames(df))) {
      df[[channel]] <- inverse_by_sample[[i]][[channel]](df[[channel]])
    }

    pd <- tryCatch(flowCore::pData(flowCore::parameters(frame)), error = function(e) data.frame())
    if (nrow(pd) == ncol(df) && "name" %in% names(pd)) {
      channels <- make.unique(as.character(pd$name), sep = "_")
      desc <- if ("desc" %in% names(pd)) as.character(pd$desc) else rep(NA_character_, nrow(pd))
      markers <- ifelse(!is.na(desc) & nzchar(desc), desc, channels)
      markers <- make.unique(markers, sep = "_")
      colnames(df) <- markers
      channel_map <- data.frame(channel = channels, marker = markers, stringsAsFactors = FALSE)
      range_col <- intersect(c("range", "$PnR", "maxRange"), names(pd))
      channel_ranges <- data.frame(
        channel = channels,
        marker = markers,
        range = suppressWarnings(as.numeric(if (length(range_col)) pd[[range_col[[1]]]] else NA_real_)),
        stringsAsFactors = FALSE
      )
    } else {
      channels <- colnames(df)
      channel_map <- data.frame(channel = channels, marker = channels, stringsAsFactors = FALSE)
      channel_ranges <- data.frame(channel = channels, marker = channels, range = NA_real_, stringsAsFactors = FALSE)
    }

    xml_transforms <- flowjo_wsp_xml_transform_specs(wsp_metadata, channel_map)
    wsp_transforms <- flowjo_wsp_transform_specs(gs[[i]], channel_map)
    # CytoML exposes FlowJo's nonlinear transforms as exact functions, while
    # the WSP XML also records the linear channel transforms. Keep both in the
    # workspace entry; the exact CytoML settings win when the same channel is
    # present in both representations.
    wsp_transforms$settings <- c(xml_transforms$settings, wsp_transforms$settings)
    wsp_transforms$metadata <- c(xml_transforms$metadata, wsp_transforms$metadata)
    # XML-only nonlinear records are covered by the exact CytoML transform
    # extractor; report only channels that neither representation supports.
    wsp_transforms$unsupported <- setdiff(wsp_transforms$unsupported, names(wsp_transforms$settings))
    comp_obj <- if (length(compensations) >= i) compensations[[i]] else NULL
    wsp_compensation <- flowjo_wsp_compensation_spec(comp_obj, channel_map)
    frame_spillover <- if (exists("flowjo_extract_spillover", mode = "function")) {
      tryCatch(flowjo_extract_spillover(frame), error = function(e) NULL)
    } else NULL
    sample_wsp <- sample_metadata_for(i)
    sample_key <- as.character(sample_wsp$sample_id %||% i)
    sample_group_names <- sample_group_names_for(sample_key)

    numeric_cols <- names(df)[vapply(df, is.numeric, logical(1))]
    suitable_for_facet <- vapply(df, function(x) length(unique(x)) <= 100L, logical(1))
    facet_cols <- names(suitable_for_facet[suitable_for_facet])
    sample_path <- refs[match(sample_labels[[i]], basename(refs))]
    if (!length(sample_path) || is.na(sample_path) || !nzchar(sample_path)) {
      sample_path <- file.path(report$fcs_path, sample_labels[[i]])
    }
    if (!is.null(report$resolved_files) && sample_path %in% names(report$resolved_files)) {
      sample_path <- unname(report$resolved_files[[sample_path]])
    }
    sample_meta <- list(condition = NA_character_, batch = NA_character_, timepoint = NA_character_, subject = NA_character_)
    if (nrow(sample_info) >= i) {
      row <- sample_info[i, , drop = FALSE]
      for (nm in names(sample_meta)) {
        if (nm %in% names(row)) sample_meta[[nm]] <- as.character(row[[nm]][[1]])
      }
    }
    if (nzchar(sample_wsp$annotation %||% "")) sample_meta$wsp_annotation <- sample_wsp$annotation
    if (length(sample_group_names)) sample_meta$wsp_groups <- paste(sample_group_names, collapse = "; ")
    keywords <- tryCatch(flowCore::keyword(frame), error = function(e) list())
    entries[[i]] <- list(
      id = sample_ids[[i]], name = sample_labels[[i]], path = sample_path,
      cells = nrow(df), channels = ncol(df), notes = "Imported from FlowJo WSP",
      loaded_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S"), df = df, raw_exprs = df,
      spillover = wsp_compensation$matrix %||% frame_spillover,
      compensation_id = wsp_compensation$id %||% NULL,
      compensation_channels = wsp_compensation$channels %||% NULL,
      channel_map = channel_map, channel_ranges = channel_ranges, keywords = keywords,
      wsp_sample_id = sample_key, wsp_sample_metadata = sample_wsp,
      wsp_group_names = sample_group_names,
      wsp_axis_transforms = wsp_transforms$settings,
      wsp_transform_metadata = wsp_transforms$metadata,
      wsp_transform_unsupported = wsp_transforms$unsupported,
      sample_meta = sample_meta, numeric_cols = numeric_cols, facet_cols = facet_cols,
      axis_pairs = if (exists("find_axis_pairs", mode = "function")) find_axis_pairs(numeric_cols) else data.frame(x = character(), y = character())
    )
  }

  paths <- tryCatch(flowWorkspace::gs_get_pop_paths(gs, path = "full"), error = function(e) character())
  paths <- setdiff(paths, "root")
  gate_ids <- paste0("wsp_gate_", sprintf("%03d", seq_along(paths)))
  path_to_id <- stats::setNames(gate_ids, paths)
  gates <- list()
  memberships <- stats::setNames(vector("list", length(entries)), sample_ids)

  sample_keys <- names(wsp_metadata$samples)
  if (!length(sample_keys)) sample_keys <- as.character(seq_along(sample_ids))
  n_sample_map <- min(length(sample_keys), length(sample_ids))
  sample_ref_to_id <- if (n_sample_map) {
    stats::setNames(sample_ids[seq_len(n_sample_map)], sample_keys[seq_len(n_sample_map)])
  } else setNames(character(), character())
  wsp_groups <- list()
  group_path_ids <- list()
  group_slug <- function(name) {
    slug <- gsub("[^A-Za-z0-9]+", "_", name %||% "group")
    slug <- gsub("(^_+|_+$)", "", slug)
    if (!nzchar(slug)) slug <- "group"
    paste0("wsp_group_", slug)
  }
  for (definition in wsp_metadata$groups %||% list()) {
    ref_ids <- as.character(definition$sample_ref_ids %||% character())
    group_sample_ids <- unname(sample_ref_to_id[ref_ids])
    group_sample_ids <- unique(group_sample_ids[!is.na(group_sample_ids) & nzchar(group_sample_ids)])
    if (!length(group_sample_ids)) next
    group_id <- group_slug(definition$name)
    if (group_id %in% names(wsp_groups)) group_id <- paste0(group_id, "_", length(wsp_groups) + 1L)
    sync_enabled <- isTRUE(definition$synchronized) || grepl("sync", definition$name %||% "", ignore.case = TRUE)
    color <- toupper(definition$color %||% "")
    if (!grepl("^#[0-9A-F]{6}$", color)) color <- "#2563EB"
    wsp_groups[[group_id]] <- list(
      id = group_id,
      name = definition$name %||% group_id,
      sample_ids = group_sample_ids,
      color = color,
      sync_enabled = sync_enabled,
      wsp_role = definition$role %||% "",
      wsp_annotation = definition$annotation %||% "",
      wsp_owning_group = definition$owning_group %||% "",
      wsp_sample_ref_ids = ref_ids,
      wsp_population_paths = definition$population_paths %||% character()
    )
    for (path in definition$population_paths %||% character()) {
      group_path_ids[[path]] <- unique(c(group_path_ids[[path]] %||% character(), group_id))
    }
  }
  if (!length(wsp_groups) && length(entries)) {
    group_id <- paste0("wsp_group_", gsub("[^A-Za-z0-9]+", "_", tools::file_path_sans_ext(basename(report$wsp_file))))
    wsp_groups[[group_id]] <- list(
      id = group_id, name = paste0("FlowJo: ", report$group_name %||% "WSP samples"),
      sample_ids = sample_ids, color = "#2563EB", sync_enabled = FALSE
    )
  }

  convert_gate <- function(gate) {
    if (is.null(gate)) return(NULL)
    if (inherits(gate, "polygonGate")) {
      boundaries <- as.matrix(gate@boundaries)
      if (ncol(boundaries) < 2L) return(NULL)
      return(list(
        type = "polygon", x_channel = colnames(boundaries)[[1]], y_channel = colnames(boundaries)[[2]],
        geometry = list(vertices = data.frame(x = as.numeric(boundaries[, 1]), y = as.numeric(boundaries[, 2])))
      ))
    }
    if (inherits(gate, "rectangleGate")) {
      mins <- gate@min
      maxs <- gate@max
      if (!length(mins)) return(NULL)
      if (length(mins) == 1L) {
        return(list(type = "range", x_channel = names(mins)[[1]], y_channel = NULL, geometry = list(xmin = mins[[1]], xmax = maxs[[1]])))
      }
      return(list(
        type = "rectangle", x_channel = names(mins)[[1]], y_channel = names(mins)[[2]],
        geometry = list(xmin = mins[[1]], xmax = maxs[[1]], ymin = mins[[2]], ymax = maxs[[2]])
      ))
    }
    NULL
  }

  for (j in seq_along(paths)) {
    path <- paths[[j]]
    flowjo_gates <- tryCatch(flowWorkspace::gs_pop_get_gate(gs, path), error = function(e) list())
    converted <- lapply(seq_along(flowjo_gates), function(i) {
      gate <- convert_gate(flowjo_gates[[i]])
      if (is.null(gate)) return(NULL)
      flowjo_gate_raw_coordinates(gate, inverse_by_sample[[i]])
    })
    valid_gate_ids <- which(vapply(converted, function(x) !is.null(x), logical(1)))
    if (!length(valid_gate_ids)) next
    first_gate <- converted[[valid_gate_ids[[1]]]]
    parts <- strsplit(sub("^/", "", path), "/", fixed = TRUE)[[1]]
    parent_path <- if (length(parts) <= 1L) "root" else paste0("/", paste(parts[-length(parts)], collapse = "/"))
    parent_id <- if (identical(parent_path, "root")) "root" else path_to_id[[parent_path]] %||% "root"
    gate_id <- path_to_id[[path]]
    wsp_population <- wsp_metadata$populations[[path]] %||% list()
    gate_color <- grDevices::hcl.colors(max(j, 3L), palette = "Dark 3")[[((j - 1L) %% max(j, 3L)) + 1L]]
    wsp_tint <- toupper(wsp_population$tint %||% "")
    if (isTRUE(wsp_population$is_tinted) && grepl("^#[0-9A-F]{6}$", wsp_tint)) gate_color <- wsp_tint
    assigned_groups <- group_path_ids[[path]] %||% character()
    sync_groups <- assigned_groups[vapply(assigned_groups, function(id) isTRUE(wsp_groups[[id]]$sync_enabled), logical(1))]
    gate <- c(
      list(
        id = gate_id, name = parts[[length(parts)]], parent_id = parent_id,
        color = gate_color,
        source_sample_id = NULL, applied_sample_id = NULL,
        created_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
        wsp_path = path, wsp_memberships = stats::setNames(vector("list", length(sample_ids)), sample_ids),
        wsp_geometry_by_sample = stats::setNames(converted, sample_ids),
        wsp_gate_id = wsp_population$gate_id %||% NULL,
        wsp_metadata = wsp_population,
        wsp_group_ids = assigned_groups
      ),
      first_gate
    )
    if (length(sync_groups)) {
      sync_id <- sync_groups[[1]]
      gate$group_id <- sync_id
      gate$sync_group_id <- sync_id
      gate$sync_sample_ids <- wsp_groups[[sync_id]]$sample_ids %||% sample_ids
      gate$group_color <- wsp_groups[[sync_id]]$color %||% gate$color
    }
    for (i in seq_along(sample_ids)) {
      truth <- tryCatch(flowWorkspace::gh_pop_get_indices(gs[[i]], path), error = function(e) NULL)
      if (!is.null(truth) && length(truth) == entries[[i]]$cells) {
        gate$wsp_memberships[[sample_ids[[i]]]] <- truth
        memberships[[sample_ids[[i]]]][[gate_id]] <- truth
      }
    }
    gates[[gate_id]] <- gate
  }

  list(
    entries = entries,
    gates = gates,
    memberships = memberships,
    groups = wsp_groups,
    sync_status = data.frame(),
    metadata = wsp_metadata,
    error = NULL
  )
}

flowjo_wsp_native_compensation_matrix <- function(entry, native_channels) {
  if (is.null(entry) || is.null(entry$spillover) || !length(native_channels)) return(NULL)
  spill <- tryCatch(as.matrix(entry$spillover), error = function(e) NULL)
  if (is.null(spill) || nrow(spill) != ncol(spill)) return(NULL)
  matrix_channels <- colnames(spill) %||% rownames(spill) %||% character()
  if (length(matrix_channels) != ncol(spill)) return(NULL)
  mapped <- matrix_channels
  channel_map <- entry$channel_map
  if (!is.null(channel_map) && nrow(channel_map) && all(c("channel", "marker") %in% names(channel_map))) {
    marker_to_channel <- stats::setNames(as.character(channel_map$channel), as.character(channel_map$marker))
    hits <- unname(marker_to_channel[matrix_channels])
    mapped[!is.na(hits) & nzchar(hits)] <- hits[!is.na(hits) & nzchar(hits)]
  }
  prefixed <- paste0("Comp-", matrix_channels)
  mapped[!(mapped %in% native_channels) & prefixed %in% native_channels] <- prefixed[!(mapped %in% native_channels) & prefixed %in% native_channels]
  keep <- !is.na(mapped) & nzchar(mapped) & mapped %in% native_channels
  if (sum(keep) < 2L) return(NULL)
  spill <- spill[keep, keep, drop = FALSE]
  mapped <- make.unique(mapped[keep], sep = "_")
  dimnames(spill) <- list(mapped, mapped)
  spill
}

# Agent note: FlowWorkspace stores compensation names in detector space while
# the app registry uses mapped marker names. Export must remap the current app
# matrix back to each GatingSet's native `Comp-*` channels before calling the
# package setter; passing the marker-space matrix directly is rejected.
flowjo_wsp_prepare_export_gatingset <- function(gating_set, entries = list()) {
  if (is.null(gating_set) || !requireNamespace("flowWorkspace", quietly = TRUE)) return(gating_set)
  if (!length(entries)) return(gating_set)
  if (!any(vapply(entries, function(entry) !is.null(entry$spillover), logical(1)))) return(gating_set)
  sample_names <- tryCatch(flowWorkspace::sampleNames(gating_set), error = function(e) character())
  if (!length(sample_names)) return(gating_set)
  entry_names <- vapply(entries, function(entry) entry$name %||% "", character(1))
  sample_labels <- sub("_[0-9]+$", "", sample_names)
  entry_index <- match(sample_labels, entry_names)
  if (anyNA(entry_index) && length(entries) == length(sample_names)) entry_index <- seq_along(sample_names)
  if (anyNA(entry_index)) stop("Current compensation assignments do not cover every WSP sample.")
  matrices <- vector("list", length(sample_names))
  for (i in seq_along(sample_names)) {
    frame <- tryCatch(flowWorkspace::gs_pop_get_data(gating_set, "root")[[i]], error = function(e) NULL)
    native_channels <- if (!is.null(frame)) colnames(flowCore::exprs(frame)) else character()
    matrices[[i]] <- flowjo_wsp_native_compensation_matrix(entries[[entry_index[[i]]]], native_channels)
    if (is.null(matrices[[i]])) stop(sprintf("Could not map the compensation matrix for WSP sample '%s'.", sample_names[[i]]))
  }
  names(matrices) <- sample_names
  flowWorkspace::compensate(gating_set, matrices)
}

flowjo_wsp_export_report <- function(gating_set, out_file, compensation_entries = list()) {
  if (is.null(gating_set)) return(list(ok = FALSE, error = "No imported GatingSet is available."))
  res <- tryCatch({
    export_set <- flowjo_wsp_prepare_export_gatingset(gating_set, compensation_entries)
    CytoML::gatingset_to_flowjo(export_set, out_file)
    list(ok = TRUE, error = NULL, out_file = out_file)
  }, error = function(e) list(ok = FALSE, error = conditionMessage(e), out_file = out_file))
  res
}
