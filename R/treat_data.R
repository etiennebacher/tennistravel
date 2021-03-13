#' Get the cleaned data 
#'
#' @return A tibble
#' @export

get_tennis_data <- function() {
  
  load("data/tennis_data_geocodes.rda") 
  
  tennis_data <- tennis_data_geocodes %>% 
    mutate(
    departure = Location,
    arrival = lead(Location),
    lat_d = lat,
    long_d = long,
    lat_a = lead(lat),
    long_a = lead(long),
    lat_a = ifelse(is.na(lat_a), lat_d, lat_a),
    long_a = ifelse(is.na(long_a), long_d, long_a)
  )
  
  rm(tennis_data_geocodes)
  
  return(tennis_data)
  
}


#' Filter by player and year
#' 
#' Format correctly latitudes and longitudes to compute distance
#'
#' @param player Player name
#' @param year Year
#'
#' @return A tibble
#' @export
#'
filter_player_year <- function(player, year) {

  tennis_data <- get_tennis_data()

  tennis_data %>%
    filter(Year == year, grepl(player, player_name))

}


# filter_player_year("Federer", 2004)%>% 
#   # group_by(order) %>% 
#   e_charts(long_d) %>%
#   e_geo(roam = TRUE) %>%
#   e_lines(
#     long_d,
#     lat_d,
#     long_a,
#     lat_a,
#     lineStyle = list(normal = list(curveness = 0.3))
#   ) %>%
#   e_scatter(
#     lat_d,
#     bind = Location,
#     coord_system = "geo",
#     symbol_size = 10
#   ) %>%
#   e_tooltip(
#     formatter = htmlwidgets::JS("
#       function(params){
#         return(params.name)
#       }
#     ")) %>% 
#   e_legend(show = FALSE)
