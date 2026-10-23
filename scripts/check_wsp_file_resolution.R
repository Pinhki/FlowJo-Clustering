library(shiny)
`%||%` <- function(x, y) if (is.null(x)) y else x
source("Contents/R/global/wsp_helpers.R")
local({
  source("Contents/R/global/axis_scale_helpers.R", local = TRUE)
  locate <- function(node) {
    if (!is.call(node) && !is.expression(node)) return(NULL)
    if (is.call(node) && identical(node[[1]], as.name("<-")) && identical(node[[2]], as.name("channel_canvas_range"))) return(node)
    for (i in seq_along(node)) {
      if (identical(node[[i]], quote(expr = ))) next
      hit <- locate(node[[i]])
      if (!is.null(hit)) return(hit)
    }
    NULL
  }
  manual <- list()
  shared <- list(transforms = list(scales = function() manual,
    auto_scales = function(...) stop("WSP ranges must bypass stale autoscale")))
  eval(locate(parse("Contents/R/interface/page_gating.R")))
  setting <- list(type = "linear", source = "wsp", engine = "flowjo", min_range = 0, max_range = 4194304, gain = 1)
  stopifnot(identical(channel_canvas_range("FSC-H", "FSC-H", c(-10, 3200000), setting), c(0, 4194304)))
  manual <- list("FSC-H" = c(100, 200))
  stopifnot(identical(channel_canvas_range("FSC-H", "FSC-H", c(0, 1), setting), c(100, 200)))
  manual <- list()
  setting <- list(type = "biex", source = "wsp", engine = "flowjo", channel_range = 256)
  stopifnot(identical(channel_canvas_range("CD8", "CD8", c(12, 200), setting), c(0, 256)))
  cat("PASS: WSP linear/biex domains override autoscale; manual ranges still win.\n")
})
if (requireNamespace("flowWorkspace", quietly = TRUE)) {
  # Real WSP rectangle bounds and transform parameters: both geometry and events must enter canvas in raw coordinates.
  params <- list(channelRange = 256, maxValue = 4194304, pos = 5.62266, neg = 0, widthBasis = -250)
  forward <- do.call(flowWorkspace::flowjo_biexp, params)
  inverse <- do.call(flowWorkspace::flowjo_biexp, c(params, list(inverse = TRUE)))
  raw <- c(-2536.1747059064064, 1516306.0081565762, -3378.772862133023, 4184554.476015081)
  transformed <- forward(raw)
  gate <- list(type = "rectangle", x_channel = "x", y_channel = "y",
    geometry = as.list(setNames(transformed, c("xmin", "xmax", "ymin", "ymax"))))
  restored <- flowjo_gate_raw_coordinates(gate, list(x = inverse, y = inverse))
  stopifnot(max(abs(unlist(restored$geometry) - raw) / pmax(1, abs(raw))) < 0.002)
  stopifnot(isTRUE(all.equal(forward(unlist(restored$geometry)), transformed, tolerance = 0.002)))
  polygon <- list(type = "polygon", x_channel = "x", y_channel = "y",
    geometry = list(vertices = data.frame(x = transformed[1:2], y = transformed[3:4])))
  restored_polygon <- flowjo_gate_raw_coordinates(polygon, list(x = inverse, y = inverse))
  stopifnot(max(abs(restored_polygon$geometry$vertices$x - raw[1:2]) / pmax(1, abs(raw[1:2]))) < 0.002)
  cat("PASS: real WSP transform parameters restore rectangle and polygon geometry before canvas transformation.\n")
}
source("Contents/R/interface/page_import_export.R")
card_page_card <- function(title, ...) div(h3(title), ...)
local({
  d <- tempfile("wsp-test-"); dir.create(d)
  on.exit(unlink(d, recursive = TRUE))
  f <- file.path(d, "sample.fcs"); invisible(file.create(f))
  w <- file.path(d, "sample.wsp")
  writeLines('<Workspace><DataSet\n uri="file:///missing/sample.fcs" /></Workspace>', w)
  original <- readBin(w, "raw", n = file.info(w)$size)
  refs <- flowjo_wsp_dataset_paths(w)
  stopifnot(identical(refs, "/missing/sample.fcs"))
  mapping <- setNames(f, refs)
  staged <- flowjo_wsp_stage_files(w, mapping, file.path(d, "stage"))
  stopifnot(file.exists(flowjo_wsp_dataset_paths(staged)),
    identical(readBin(w, "raw", n = file.info(w)$size), original))
  err <- tryCatch(flowjo_wsp_stage_files(w, setNames("/missing/no.fcs", refs), file.path(d, "fail")), error = conditionMessage)
  stopifnot(grepl("missing", err))
  shared <- list(gating = list(wsp_report = reactiveVal(NULL)))
  testServer(import_export_page_server, args = list(shared = shared), {
    session$flushReact()
    stopifnot(grepl("disabled", output$wsp_import_control$html))
    selected_wsp(w); inventory(flowjo_wsp_reference_inventory(w))
    resolved_files(setNames("/missing/sample.fcs", refs)); session$flushReact()
    stopifnot(nrow(missing_files()) == 1, grepl("disabled", output$wsp_import_control$html))
    bad_pieces <- c("..", "..", strsplit(sub("^/", "", f), "/")[[1]])
    session$setInputs(missing_reference = refs[[1]],
      locate_fcs = list(root = "Home", files = list(bad_pieces)))
    session$flushReact()
    stopifnot(nrow(missing_files()) == 1, grepl("disabled", output$wsp_import_control$html))
    frame <- flowCore::flowFrame(matrix(c(1, 2, 3, 4), ncol = 2,
      dimnames = list(NULL, c("FSC-A", "SSC-A"))))
    flowCore::write.FCS(frame, f)
    session$setInputs(locate_fcs = 0)
    pieces <- c("..", "..", strsplit(sub("^/", "", f), "/")[[1]])
    session$setInputs(missing_reference = refs[[1]],
      locate_fcs = list(root = "Home", files = list(pieces)))
    session$flushReact()
    stopifnot(nrow(missing_files()) == 0, identical(unname(resolved_files()), normalizePath(f)))
    stopifnot(!grepl("disabled", output$wsp_import_control$html),
      grepl("wsp-import-ready", output$wsp_import_control$html),
      grepl("Ready to import", output$wsp_report$html))
    importing_wsp(TRUE); session$flushReact()
    stopifnot(grepl("Importing", output$wsp_import_control$html), grepl("disabled", output$wsp_import_control$html))
    importing_wsp(FALSE); import_error("test failure"); session$flushReact()
    stopifnot(grepl("Retry WSP import", output$wsp_import_control$html), grepl("test failure", output$wsp_report$html))
    import_error(NULL)
    shared$gating$wsp_report(list(gating_set = TRUE, error = NULL, wsp_file = w,
      samples = data.frame(name = "sample"), populations = "root"))
    session$flushReact()
    stopifnot(grepl("wsp-import-complete", output$wsp_import_control$html), grepl("WSP imported", output$wsp_import_control$html))
  })
  # Exercise the deferred callback outside a reactive consumer, as in a live session.
  overlay_show <- function(...) invisible(NULL)
  overlay_hide <- function(...) invisible(NULL)
  flowjo_wsp_import_report <- function(...) list(wsp_file = w, samples = data.frame(name = "sample"),
    populations = "root", gating_set = TRUE, error = NULL)
  flowjo_wsp_app_state <- function(...) list(
    entries = list(sample = list(wsp_axis_transforms = list(`FSC-A` = list(type = "linear")))),
    gates = list(gate = list(name = "gate")), memberships = list(sample = list(gate = TRUE)),
    groups = list(group = list(name = "group")), error = NULL)
  deferred <- NULL
  schedule_import <- function(func, delay) { deferred <<- func }
  module_text <- paste(deparse(import_export_page_server), collapse = "\n")
  module_text <- sub("later::later(", "schedule_import(", module_text, fixed = TRUE)
  import_export_page_server <- eval(parse(text = module_text))
  environment(import_export_page_server) <- environment()
  registry <- reactiveVal(list())
  shared <- list(
    fcs = list(add_many = function(entries) registry(c(registry(), entries))),
    transforms = list(settings = reactiveVal(list())),
    gating = list(wsp_report = reactiveVal(NULL), gates = reactiveVal(list()),
      memberships = reactiveVal(list()), groups = reactiveVal(list())))
  testServer(import_export_page_server, args = list(shared = shared), {
    session$flushReact()
    selected_wsp(w)
    inv <- flowjo_wsp_reference_inventory(w); inv$path[inv$type == "fcs"] <- f
    inventory(inv); resolved_files(mapping)
    session$setInputs(import_wsp = 1)
    stopifnot(is.function(deferred))
    shiny::maskReactiveContext(deferred())
    session$flushReact()
    stopifnot(is.null(import_error()), !importing_wsp(),
      length(registry()) == 1, length(shared$gating$gates()) == 1,
      length(shared$gating$memberships()) == 1, length(shared$gating$groups()) == 1,
      length(shared$transforms$settings()) == 1,
      grepl("wsp-import-complete", output$wsp_import_control$html))
  })
})
cat("PASS: XML references, exact file staging, original WSP preserved, missing-file guard, import UI states, and deferred shared-state import.\n")

# Run the actual deferred Gating callback with a minimal preview state.
local({
  callbacks <- function(node) {
    if (is.call(node) && identical(node[[1]], quote(later::later))) return(list(node[[2]]))
    if (!is.call(node) && !is.expression(node)) return(list())
    unlist(lapply(as.list(node), function(child) tryCatch(callbacks(child), error = function(e) list())), recursive = FALSE)
  }
  pending <- callbacks(parse(file = "Contents/R/interface/page_gating.R"))
  stopifnot(length(pending) == 1)
  session <- NULL
  gating_tab_active <- reactiveVal(TRUE)
  gating_preview_ready <- reactiveVal(FALSE)
  shared <- list(gating = list(gates = reactiveVal(list(gate = list(name = "Gate")))))
  foreground_preview_scheduled <- TRUE
  foreground_preview_running <- FALSE
  sent <- FALSE; hidden <- FALSE; notified <- FALSE
  preload_state <- reactiveVal(list(queue = list("next"), done = 0, total = 1))
  hide_foreground_preview_overlay <- function(...) { gating_preview_ready(); hidden <<- TRUE }
  ensure_active_preview_gate <- function(gates) names(gates)[[1]]
  prepare_active_preview <- function(gates, id) { shared$gating$gates(); list(id = id) }
  request_preload_queue <- function(...) preload_state()
  send_preview_messages <- function(msgs) { sent <<- TRUE; gating_preview_ready(TRUE) }
  show_preload_notification <- function(...) { notified <<- TRUE }
  showNotification <- function(...) stop("Preview entered its error handler")
  callback <- eval(pending[[1]])
  shiny::maskReactiveContext(callback())
  stopifnot(sent, hidden, notified, !foreground_preview_scheduled, !foreground_preview_running)
  gating_tab_active(FALSE); sent <- FALSE; hidden <- FALSE
  shiny::maskReactiveContext(callback())
  stopifnot(!sent, hidden, !isolate(gating_preview_ready()))
  old_callback <- callback
  old_body <- body(old_callback)
  old_body[[2]][[3]] <- old_body[[2]][[3]][[2]]
  body(old_callback) <- old_body
  gating_tab_active(TRUE)
  regression <- tryCatch(shiny::maskReactiveContext(old_callback()), error = conditionMessage)
  stopifnot(is.character(regression), grepl("active reactive context", regression, fixed = TRUE))
  cat("PASS: deferred Gating preview prepares and sends payloads without a reactive context; inactive-tab cancellation remains safe.\n")
})

local({
  nodes <- function(node) {
    if (!is.call(node) && !is.expression(node)) return(list())
    c(if (is.call(node)) list(node) else list(),
      unlist(lapply(as.list(node), function(x) tryCatch(nodes(x), error = function(e) list())), recursive = FALSE))
  }
  calls <- nodes(parse(file = "Contents/R/interface/page_gating.R"))
  definition <- function(name) {
    matches <- Filter(function(x) identical(x[[1]], quote(`<-`)) && identical(x[[2]], as.name(name)), calls)
    stopifnot(length(matches) == 1)
    eval(matches[[1]][[3]])
  }
  acks <- Filter(function(x) identical(x[[1]], quote(observeEvent)) &&
    identical(x[[2]], quote(input$gate_canvas_ready)), calls)
  stopifnot(length(acks) == 1)
  foreground_preview_overlay_visible <- TRUE
  foreground_preview_ready_token <- NULL
  foreground_preview_generation <- 0L
  sent <- list(); hidden <- 0L
  session <- list(ns = function(id) paste0("different-module-", id),
    sendCustomMessage = function(name, payload) { sent[[length(sent) + 1L]] <<- payload })
  app_trace <- function(...) invisible(NULL)
  overlay_hide <- function(...) { hidden <<- hidden + 1L }
  send_preview_messages <- definition("send_preview_messages")
  hide_foreground_preview_overlay <- definition("hide_foreground_preview_overlay")
  msgs <- list(list(name = "flowjo-gating-payload", payload = list(id = session$ns("canvas_wrap_left"))))
  send_preview_messages(msgs)
  first <- sent[[1]]$readyToken
  stopifnot(identical(sent[[1]]$readyInput, session$ns("gate_canvas_ready")))
  hide_foreground_preview_overlay(wait_for_canvas = TRUE)
  stopifnot(hidden == 0, foreground_preview_overlay_visible)
  send_preview_messages(msgs)
  input <- list(gate_canvas_ready = list(token = first))
  eval(acks[[1]][[3]])
  stopifnot(hidden == 0, foreground_preview_overlay_visible)
  input$gate_canvas_ready$token <- sent[[2]]$readyToken
  eval(acks[[1]][[3]])
  stopifnot(hidden == 1, !foreground_preview_overlay_visible, is.null(foreground_preview_ready_token))
  cat("PASS: drawn-canvas acknowledgement hides the overlay; stale acknowledgements do not hide newer work; module namespace is respected.\n")
})

local({
  walk <- function(node) {
    if (!is.call(node) && !is.expression(node)) return(list())
    c(if (is.call(node)) list(node) else list(),
      unlist(lapply(as.list(node), function(x) tryCatch(walk(x), error = function(e) list())), recursive = FALSE))
  }
  calls <- walk(parse(file = "Contents/R/interface/page_gating.R"))
  getfun <- function(name) {
    found <- Filter(function(n) identical(n[[1]], quote(`<-`)) && identical(n[[2]], as.name(name)), calls)
    stopifnot(length(found) == 1)
    eval(found[[1]][[3]])
  }
  flowjo_transform_column <- function(vals, setting) vals
  axis_debug <- function(...) NULL
  safe_axis_tick_transform <- getfun("safe_axis_tick_transform")
  axis_hist_ticks <- getfun("axis_hist_ticks")
  axis_canvas_tick_text <- getfun("axis_canvas_tick_text")
  ticks <- axis_hist_ticks(c(-1e5, 4.2e6), c(-1e5, 4.2e6), list(type = "linear"))
  stopifnot(length(ticks$at) >= 4, max(ticks$raw) > 1e6,
    length(ticks$minor) > length(ticks$at),
    all(nzchar(axis_canvas_tick_text(ticks$raw, linear = TRUE))), length(unique(diff(ticks$at))) == 1)
  stopifnot(identical(unname(axis_canvas_tick_text(c(1e6, 1.5e6, 5e5))), c("10\u2076", "", "")))
  stopifnot(identical(unname(axis_canvas_tick_text(c(0, 1e6, 1.5e6, 4e6), linear = TRUE, exponent = 6)), c("0", "1", "1.5", "4")),
    identical(unname(axis_canvas_tick_text(c(10, 1e7))), c("10", "10\u2077")))
  cat("PASS: linear major ticks remain evenly spaced and labelled above 10^6, with subticks.\n")
  source("Contents/R/global/axis_scale_helpers.R", local = TRUE)
  setting <- flowjo_wsp_biexp_setting(list(channelRange = 256, maxValue = 4194304, pos = 5.62266, neg = 0, widthBasis = -250))
  raw <- c(-10000, 200000)
  ticks <- axis_hist_ticks(raw, flowjo_transform_column(raw, setting), setting)
  expected <- c(-9000, -2000, 2000, 9000, 20000, 90000)
  stopifnot(all(vapply(flowjo_transform_column(expected, setting), function(value) any(abs(ticks$minor - value) < 1e-8), logical(1))))
  cat("PASS: zero-crossing WSP biex axes retain transformed 2–9 minor ticks in interior decades.\n")
})

local({
  walk <- function(n) {
    if (!is.call(n) && !is.expression(n)) return(list())
    c(if (is.call(n)) list(n) else list(), unlist(lapply(as.list(n), function(x)
      tryCatch(walk(x), error = function(e) list())), recursive = FALSE))
  }
  gating <- walk(parse(file = "Contents/R/interface/page_gating.R"))
  sidebar <- walk(parse(file = "Contents/R/interface/components/component_right_sidebar.R"))
  body_for <- function(calls, condition) {
    x <- Filter(function(n) identical(n[[1]], quote(observeEvent)) && identical(n[[2]], condition), calls)
    stopifnot(length(x) == 1)
    x[[1]][[3]]
  }
  selected_fcs_id <- reactiveVal(NULL)
  request <- reactiveVal(NULL); active <- reactiveVal("s1")
  gate <- reactiveVal("old-gate"); override <- reactiveVal(NULL)
  shared <- list(session = list(input = list(main_nav = "gating_tab")),
    fcs = list(registry = reactiveVal(list(s1 = list(), s2 = list())), set_active = function(sid) active(sid)),
    ui = list(request_gating_sample = function(sid) request(list(sample_id = sid)), gating_sample_request = request),
    params = list(plot_preload_scope = reactiveVal("all")), gating = list(active_gate_id = gate))
  input <- list(selected_fcs_id = "s2")
  isolate(eval(body_for(sidebar, quote(input$selected_fcs_id))))
  stopifnot(identical(isolate(request())$sample_id, "s2"))
  active_sample_id <- function() active()
  preload <- Filter(function(n) identical(n[[1]], quote(`<-`)) && identical(n[[2]], quote(preload_sample_ids)), gating)
  preload_sample_ids <- eval(preload[[1]][[3]])
  stopifnot(identical(isolate(preload_sample_ids(list(), NULL)), c("s1", "s2")))
  shared$params$plot_cache_enabled <- function() TRUE
  shared$params$plot_preload_enabled <- function() TRUE
  shared$fcs$registry <- reactiveVal(list(s1 = list(id = "s1"), s2 = list(id = "s2")))
  flowjo_gate_parent_id <- function(gates, id) gates[[id]]$parent_id
  right_axis_template_gate_id <- function() NULL
  right_axis_source_gate_id <- function(gates, id, entry) id
  active_entry <- function() list(id = "s1")
  sample_gate_switch_target <- function(gates, id, sid) list(gate_id = "g2")
  preview_context_key <- function(ctx, gates) paste(ctx$type, ctx$entry$id, ctx$gate_id, ctx$x, ctx$y)
  flowjo_gate_applicability <- function(entry, gate, gates) list(ok = identical(gate$applied_sample_id, entry$id))
  make_gate <- function(sid, parent, channel) list(applied_sample_id = sid, parent_id = parent, x_channel = channel, y_channel = "y")
  gates <- list(g1 = make_gate("s1", "root", "current"), g2 = make_gate("s2", "root", "current"),
    deeper = make_gate("s2", "g2", "deep"))
  preload_contexts <- eval(Filter(function(n) identical(n[[1]], quote(`<-`)) &&
    identical(n[[2]], quote(preload_contexts)), gating)[[1]][[3]])
  contexts <- isolate(preload_contexts(gates, "g1"))
  stopifnot(length(contexts) > 6L, all(vapply(contexts[1:6], function(x) !identical(x$x, "deep"), logical(1))),
    any(vapply(contexts[-(1:6)], function(x) identical(x$x, "deep") && identical(x$entry$id, "s2"), logical(1))))
  cat("PASS: current layer across samples precedes deeper sample-owned layers.\n")
  raw_gate_for_canvas <- eval(Filter(function(n) identical(n[[1]], quote(`<-`)) &&
    identical(n[[2]], quote(raw_gate_for_canvas)), gating)[[1]][[3]])
  flowjo_gate_for_plot_axes <- function(gate, ...) gate
  axis_setting_for_channel <- function(...) NULL
  convert_gate_geometry_scale <- function(geom, ...) geom
  geometry <- list(vertices = data.frame(x = c(1, 2, 3), y = c(4, 5, 6)))
  imported <- list(type = "polygon", geometry = list(vertices = data.frame(x = 0, y = 0)),
    wsp_geometry_by_sample = list(s2 = list(type = "polygon", geometry = geometry)))
  stopifnot(identical(raw_gate_for_canvas(imported, "x", "y", list(id = "s2"))$geometry, geometry))
  imported$wsp_geometry_by_sample$s2 <- geometry
  stopifnot(identical(raw_gate_for_canvas(imported, "x", "y", list(id = "s2"))$geometry, geometry))
  cat("PASS: WSP descriptor and bare sample geometry retain polygon vertices for outlines.\n")

  switched <- NULL
  gating_tab_active <- function() TRUE
  switch_preview_sample <- function(sid) { switched <<- sid; active(sid); TRUE }
  callback <- body_for(gating, quote(shared$ui$gating_sample_request()))
  isolate(eval(callback))
  stopifnot(identical(switched, "s2"))
  active("s1"); switch_preview_sample <- function(sid) FALSE
  active_sample_override <- override
  canvas_selected_gate <- reactiveVal("old-gate")
  right_axis_template_gate_id <- reactiveVal("old-template")
  active_plot <- reactiveVal("right")
  updateSelectInput <- function(...) NULL; session <- NULL
  isolate(eval(callback))
  stopifnot(identical(isolate(active()), "s2"), is.null(isolate(gate())),
    is.null(isolate(canvas_selected_gate())), is.null(isolate(right_axis_template_gate_id())))
  request(NULL); shared$session$input$main_nav <- "workspace_tab"
  isolate(eval(body_for(sidebar, quote(input$selected_fcs_id))))
  stopifnot(is.null(isolate(request())))
  cat("PASS: Gating sidebar clicks request population-aware switching; unmatched populations clear stale selection; all-sample preload includes both samples; other pages retain selection-only behavior.\n")
})

local({
  walk <- function(n) {
    if (!is.call(n) && !is.expression(n)) return(list())
    c(if (is.call(n)) list(n) else list(), unlist(lapply(as.list(n), walk), recursive = FALSE))
  }
  calls <- walk(parse(file = "Contents/R/interface/page_gating.R"))
  counts <- Filter(function(n) identical(n[[1]], quote(data.frame)) && "selected" %in% names(n), calls)
  stopifnot(length(counts) == 1L)
  selected_gate <- "shared-gate"; selected_sample <- "s2"
  stats <- data.frame(gate_id = c("shared-gate", "shared-gate", "parent"), sample_id = c("s1", "s2", "s2"))
  stopifnot(identical(eval(counts[[1]][["selected"]]), c("false", "true", "false")))
  selected_gate <- ""
  stopifnot(all(eval(counts[[1]][["selected"]]) == "false"))
  cat("PASS: current-gate highlight matches both gate and sample; no gate highlights no row.\n")
})
