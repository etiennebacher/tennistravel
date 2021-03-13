map_travel <- function(player, year) {
  
  filter_player_year(player, year) %>%
    mutate(infos = paste(order, lead(arrival), sep = ",")) %>% 
    # group_by(order) %>%
    e_charts(long_d) %>%
    e_geo(roam = TRUE) %>%
    e_lines(
      long_d,
      lat_d,
      long_a,
      lat_a,
      departure,
      arrival,
      infos,
      lineStyle = list(normal = list(curveness = 0.3))
    ) %>%
    e_tooltip(
      trigger = "item",
      formatter = htmlwidgets::JS("
      function(params){
        var vals = params.value.split(',')
        return(
          'Travel #' + vals[0] + '<br/>' +
          'From <b>' + params.data.source_name + '</b> to <b>' +
          params.data.target_name + '</b> <br/> Next destination: <b>' + 
          vals[1] + '</b>'
        )
      }
   ")
    ) %>% 
    e_scatter(
      lat_d,
      bind = Location,
      coord_system = "geo",
      symbol_size = 10
    ) %>%
    e_legend(show = FALSE)
  
}