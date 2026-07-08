`%||%` <- function(a, b) if (!is.null(a)) a else b
source("Contents/R/global/gating_helpers.R")
source("Contents/R/global/flowjo_pipeline_helpers.R")

df <- data.frame(X = c(1, 2, 3, 4), Y = c(1, 3, 2, 4))
cm <- data.frame(channel = c("FSC-A", "SSC-A"), marker = c("X", "Y"), stringsAsFactors = FALSE)

rect <- flowjo_make_gate("rect", "rectangle", "root", "FSC-A", "SSC-A", list(xmin = 1.5, xmax = 3.5, ymin = 1.5, ymax = 3.5))
stopifnot(identical(flowjo_gate_membership(df, cm, rect), c(FALSE, TRUE, TRUE, FALSE)))

rng <- flowjo_make_gate("range", "range", "root", "FSC-A", "SSC-A", list(xmin = 2, xmax = 3, ymin = -Inf, ymax = Inf))
stopifnot(identical(flowjo_gate_membership(df, cm, rng), c(FALSE, TRUE, TRUE, FALSE)))

poly <- flowjo_make_gate("poly", "polygon", "root", "FSC-A", "SSC-A", list(vertices = data.frame(x = c(0, 5, 5, 0), y = c(0, 0, 5, 5))))
stopifnot(all(flowjo_gate_membership(df, cm, poly)))

ellipse <- flowjo_make_gate("ellipse", "ellipse", "root", "FSC-A", "SSC-A", list(xmin = 0, xmax = 4, ymin = 0, ymax = 4))
stopifnot(identical(flowjo_gate_membership(df, cm, ellipse), c(TRUE, TRUE, TRUE, FALSE)))

quad <- flowjo_make_gate("quad", "quadrant", "root", "FSC-A", "SSC-A", list(x = 2.5, y = 2.5, quadrant = "upper_right"))
stopifnot(identical(flowjo_gate_membership(df, cm, quad), c(FALSE, FALSE, FALSE, TRUE)))

curved <- flowjo_make_gate("curved", "curved_quadrant", "root", "FSC-A", "SSC-A", list(x = 2.5, y = 2.5, quadrant = "lower_left", curve = 0.25))
stopifnot(identical(flowjo_gate_membership(df, cm, curved), c(TRUE, FALSE, FALSE, FALSE)))

colored <- flowjo_make_gate("colored", "rectangle", "root", "FSC-A", "SSC-A", list(xmin = 1.5, xmax = 3.5, ymin = 1.5, ymax = 3.5), color = "#0072B2")
stopifnot(identical(flowjo_gate_color(colored), "#0072B2"))
groups <- flowjo_gate_plot_groups(df, cm, list(colored), "FSC-A", "SSC-A")
stopifnot(identical(as.character(groups), c("Ungated", "colored", "colored", "Ungated")))

entry1 <- list(id = "sample1", df = df, channel_map = cm)
entry2 <- list(id = "sample2", df = df, channel_map = cm)
entry_missing <- list(id = "sample3", df = data.frame(X = 1:3), channel_map = cm)
stopifnot(isTRUE(flowjo_gate_applicability(entry1, rect, list(rect = rect))$ok))
stopifnot(identical(flowjo_gate_applicability(entry_missing, rect, list(rect = rect))$status, "missing_channel"))

parent <- flowjo_make_gate("parent", "rectangle", "root", "FSC-A", "SSC-A", list(xmin = 0, xmax = 4, ymin = 0, ymax = 4))
child <- flowjo_make_gate("child", "rectangle", parent$id, "FSC-A", "SSC-A", list(xmin = 1, xmax = 3, ymin = 1, ymax = 3))
gates <- list()
gates[[parent$id]] <- parent
gates[[child$id]] <- child
stopifnot(identical(flowjo_gate_parent_id(gates, child$id), parent$id))
stopifnot(identical(flowjo_gate_children(gates, parent$id), child$id))
stopifnot(identical(flowjo_gate_same_path(gates, "child", parent$id), child$id))

registry <- list(sample1 = entry1, sample2 = entry2, sample3 = entry_missing)
stopifnot(is.null(flowjo_prev_next_sample(registry, gates, child$id, "sample1", 1L)))

copy <- flowjo_copy_gate(child, parent_id = "root", sample_id = "sample2", group_id = "group1", sync = TRUE, sync_sample_ids = c("sample1", "sample2"))
stopifnot(!identical(copy$id, child$id))
stopifnot(identical(copy$parent_id, "root"))
stopifnot(identical(copy$applied_sample_id, "sample2"))
stopifnot(identical(copy$sync_group_id, "group1"))
stopifnot(identical(flowjo_gate_applicability(entry1, copy, gates)$status, "not_applied"))
stopifnot(isTRUE(flowjo_gate_applicability(entry2, copy, gates)$ok))

copy1 <- flowjo_copy_gate(child, parent_id = parent$id, sample_id = "sample1", group_id = "group1", sync = TRUE, sync_sample_ids = c("sample1", "sample2"))
copy2 <- flowjo_copy_gate(child, parent_id = parent$id, sample_id = "sample2", group_id = "group1", sync = TRUE, sync_sample_ids = c("sample1", "sample2"))
peer_gates <- gates
peer_gates[[copy1$id]] <- copy1
peer_gates[[copy2$id]] <- copy2
stopifnot(identical(flowjo_gate_lineage_id(copy1), flowjo_gate_lineage_id(copy2)))
stopifnot(identical(flowjo_gate_peer_id(peer_gates, copy1$id, "sample2"), copy2$id))
stopifnot(identical(flowjo_prev_next_sample(registry, peer_gates, copy1$id, "sample1", 1L), "sample2"))
stopifnot(is.null(flowjo_prev_next_sample(registry, peer_gates, copy1$id, "sample2", 1L)))

memberships <- list(sample1 = list(), sample2 = list())
memberships$sample1[[child$id]] <- flowjo_gate_membership(df, cm, child, parent = flowjo_gate_membership(df, cm, parent))
memberships$sample2[[child$id]] <- memberships$sample1[[child$id]]
registry2 <- list(
  sample1 = c(entry1, list(name = "Sample 1")),
  sample2 = c(entry2, list(name = "Sample 2"))
)
items <- list(list(sample_id = "sample1", gate_id = child$id, set_name = "CD8", label = "CD8 child", include = TRUE, volume_uL = 12))
cc <- flowjo_population_concat(registry2, gates, memberships, items, mode = "all")
stopifnot(nrow(cc$data) == sum(memberships$sample1[[child$id]]))
stopifnot(all(c("SampleID", "GateID", "PopulationID", "PopulationSetID", "Volume_uL") %in% names(cc$data)))
stopifnot(nrow(cc$mapping) == 1L)
stopifnot(length(flowjo_default_cluster_markers(c("FSC-A", "CD45RA", "CCR7", "Live Dead"))) == 2L)

cat("OK gating helper checks\n")
