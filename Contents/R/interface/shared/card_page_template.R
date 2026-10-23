card_page_header <- function(title, subtitle = NULL, icon_name = NULL, class = NULL) {
  title_icon <- if (!is.null(icon_name) && nzchar(icon_name)) icon(icon_name) else NULL
  div(
    class = paste(c("card-page-header", class), collapse = " "),
    div(class = "card-page-title-row", div(class = "card-page-title card-page-header-title", title_icon, tags$span(title))),
    if (!is.null(subtitle) && nzchar(subtitle)) p(class = "card-page-subtitle", subtitle)
  )
}

card_page_card <- function(title, ..., icon_name = NULL, subtitle = NULL, class = NULL, content_class = NULL) {
  title_icon <- if (!is.null(icon_name) && nzchar(icon_name)) icon(icon_name) else NULL
  div(
    class = paste(c("card", "card-page-card", class), collapse = " "),
    div(class = "card-page-title-row", div(class = "card-page-title card-page-card-title", title_icon, tags$span(title))),
    if (!is.null(subtitle) && nzchar(subtitle)) p(class = "card-page-subtitle", subtitle),
    div(class = paste(c("card-page-content", content_class), collapse = " "), ...)
  )
}
