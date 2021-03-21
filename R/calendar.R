#' Show the location of tournaments month per month for a given year
#'
#' @param year Year 
#'
#' @return A plot
#' @export
#'

tournament_calendar <- function(year) {
  
  month_names <- c("January", "February", "March",
                   "April", "May", "June", "July", 
                   "August", "September", "October", 
                   "November", "December")
  
  plot_title <- lapply(month_names, function(x) {
    list(text = paste0(x, " ", year), left = "40%", color = "white")
  })
  
  tennis_data %>% 
    filter(tourney_year == year) %>% 
    select(tourney_month, tourney_location, lat, long) %>% 
    distinct() %>% 
    arrange(tourney_month) %>% 
    mutate(
      tourney_month = month.name[tourney_month],
      tourney_month = factor(tourney_month, 
                             levels = month_names)
    ) %>% 
    group_by(tourney_month) %>% 
    e_charts(long, timeline = T) %>% 
    e_geo(
      roam = TRUE, 
      itemStyle = list(
        color = '#003399', 
        borderColor = "#fff")
    ) %>% 
    e_scatter(
      lat, 
      coord_system = "geo", 
      symbol_size = 15,
      bind = tourney_location, 
      itemStyle = list(
        color = "yellow", 
        borderColor = "#000")
    ) %>% 
    e_legend(show = F) %>% 
    e_tooltip(
      trigger = "item",
      formatter = htmlwidgets::JS("
      function(params){
        return(
          params.name
        )
      }
   ")) %>% 
    e_timeline_opts(
      top = "90%",
      autoPlay = FALSE,
      symbol = "roundRect",
      lineStyle = list(color = "#003399"),
      itemStyle = list(color = "#003399"),
      controlStyle = list(color = "#003399"),
      checkpointStyle = list(color = "yellow",
                             symbol = "roundRect"),
      progress = list(lineStyle = list(color = "#003399"),
                      itemStyle = list(color = "#003399")),
      emphasis = list(itemStyle = list(color = "#003399"),
                      controlStyle = "#003399"),
      label = list(show = FALSE)) %>% 
    e_timeline_serie(title = plot_title) %>% 
    e_title(textStyle = list(color = "#fff"))
  
}
