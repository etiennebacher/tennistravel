#' Get the cleaned data 
#'
#' @return A tibble
#' @export

get_tennis_data <- function() {
  
  load("data/tennis_data_geocodes.rda") 
  load("data/tennis_players.rda")
  
  tennis_data <- tennis_data_geocodes %>% 
    group_by(player_name, Year) %>% 
    mutate(
      departure = Location,
      arrival = lead(Location),
      arrival = ifelse(is.na(arrival), "End of year", arrival),
      lat_d = lat,
      long_d = long,
      lat_a = lead(lat),
      long_a = lead(long),
      lat_a = ifelse(is.na(lat_a), lat_d, lat_a),
      long_a = ifelse(is.na(long_a), long_d, long_a)
    ) %>% 
    left_join(
      tennis_players,
      by = c("player_name" = "abb_name")
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

