#' Show on a map the trips of a particular player for a a year
#'
#' @param player Player name
#' @param year Year
#'
#' @export
#'

map_travel <- function(player, year) {
  
  filter_player_year(player, year) %>%
    mutate(infos = paste(order, lead(arrival), sep = ",")) %>% 
    # group_by(order) %>%
    e_charts(long_d) %>%
    e_geo(
      roam = TRUE, 
      itemStyle = list(
        color = '#003399', 
        borderColor = "#fff"
      ),
      emphasis = list(
        itemStyle = list(
          areaColor = "#80b3ff"
        ),
        label = list(
          color = "yellow"
        )
      )
    ) %>%
    e_lines(
      long_d,
      lat_d,
      long_a,
      lat_a,
      departure,
      arrival,
      infos,
      lineStyle = list(normal = list(curveness = 0.3, color = "yellow")),
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
      coord_system = "geo",
      symbol_size = 15,
      itemStyle = list(
        color = "yellow", 
        borderColor = "#000"
      )
    ) %>%
    e_legend(show = FALSE)
  
}