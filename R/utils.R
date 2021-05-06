is_empty <- function(tibble) {
  nrow(tibble) == 0
}

cus_p <- function(...) {
  p(..., class = "text")
}
cus_ul <- function(...) {
  shiny::tags$ul(..., class = "text")
}
cus_li <- function(...) {
  shiny::tags$li(..., class = "text")
}
cus_b <- function(...) {
  shiny::tags$b(..., class = "text")
}
longdiv <- function(x, ...){
  shiny::tags$div(style = paste0("min-height:", x, "vh;"), ...)
}
wellPanel2 <- function(...) {
  shiny::wellPanel(
    class = "wellp",
    ...
  )
}
center_panel <- function(...) {
  tagList(
    column(width = 3),
    column(width = 6, ...),
    column(width = 3)
  )
}
hr_white <- function() {
  shiny::tags$hr(style = "color: white;")
}
