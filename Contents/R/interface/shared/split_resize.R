shiny_split_resize <- function(left, right, id = NULL, class = NULL, default_left = "50%", min_left = 260, min_right = 320,
                               default_top = "50%", min_top = 240, min_bottom = 240) {
  id_attr <- if (!is.null(id)) id else NULL
  div(
    id = id_attr,
    class = paste("Pinhki-split-resize", class %||% ""),
    style = sprintf(
      "--Pinhki-split-left:%s; --Pinhki-split-min-left:%dpx; --Pinhki-split-min-right:%dpx; --Pinhki-split-top:%s; --Pinhki-split-min-top:%dpx; --Pinhki-split-min-bottom:%dpx;",
      default_left, min_left, min_right, default_top, min_top, min_bottom
    ),
    `data-split-default` = default_left,
    `data-split-default-top` = default_top,
    `data-split-min-left` = min_left,
    `data-split-min-right` = min_right,
    `data-split-min-top` = min_top,
    `data-split-min-bottom` = min_bottom,
    div(class = "Pinhki-split-pane Pinhki-split-pane-left", left),
    div(
      class = "Pinhki-split-resizer",
      role = "separator",
      `aria-orientation` = "vertical",
      tabindex = "0",
      span(class = "Pinhki-split-resizer-grip")
    ),
    div(class = "Pinhki-split-pane Pinhki-split-pane-right", right)
  )
}
