tournament_calendar <- function(year) {
  
  tennis_data %>% 
    filter(tourney_year == year) %>% 
    select(tourney_month, tourney_location, lat, long) %>% 
    distinct() %>% 
    arrange(tourney_month) %>% 
    mutate(
      tourney_month = month.name[tourney_month],
      tourney_month = factor(tourney_month, 
                             levels = c("January", "February", "March",
                                        "April", "May", "June", "July", 
                                        "August", "September", "October", 
                                        "November", "December"))
    ) %>% 
    group_by(tourney_month) %>% 
    e_charts(long, timeline = T) %>% 
    e_geo(roam = TRUE) %>% 
    e_scatter(lat, coord_system = "geo", symbol_size = 8,
              bind = tourney_location) %>% 
    e_legend(show = F) %>% 
    e_tooltip(
      trigger = "item",
      formatter = htmlwidgets::JS("
      function(params){
        return(
          params.name
        )
      }
   ")
    )
  
}
