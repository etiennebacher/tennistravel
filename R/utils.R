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