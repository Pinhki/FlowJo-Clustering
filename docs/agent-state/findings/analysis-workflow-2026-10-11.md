# User-approved analysis workflow

Select gated populations into basket -> review the subset for this object -> create Analysis Object -> cluster/analyze -> select resulting populations -> concatenate/export. Export concatenation must not be an upstream prerequisite or analysis source.

Object creation lists sample, full gate path, and counts. Object-only exclusions leave basket unchanged. Empty/unavailable selections are blocked; Root/all events is an explicit alternative. Exact input mapping is saved in object provenance and inspectable from Input populations.

Population Export follows Cluster and Feature Plots in navigation. It selects an object, run, and FlowSOM clusters. Sampling is applied only to the exported rows; original marker values are retained. Existing saved objects/runs remain supported.

Verified with scripts/check_analysis_review.R and R parsing. Browser appearance pending restart.
