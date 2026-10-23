wan_lab_footer <- function(APP_VERSION = "0.1.0") {
  app_version <- APP_VERSION
  app_version_label <- if (is.na(app_version) || !nzchar(app_version)) "NA" else app_version
  tagList(
    tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "footer.css")),
    div(class = "footer",
        div(class = "footer-inner",
            span(sprintf("FlowJo Re-plotting Tool v%s", app_version_label)),
            span(" • "),
            span("© 2025 Wan Lab"),
            span(" • "),
            tags$a(href = "https://sites.wustl.edu/wanlab/", "Lab Website", target = "_blank"),
            span(" • "),
            span(
              class = "footer-policy",
              HTML("Original code is licensed under GNU GPL v3. Third-party components retain their own licenses.")
            ),
            span(" • "),
            tags$a(href = "mailto:rongzhen@wustl.edu", "Contact Developer", target = "_blank")
        )
    )
  )
}
