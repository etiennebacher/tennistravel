#' Title
#'
#' @param departure_lat 
#' @param departure_long 
#' @param arrival_lat 
#' @param arrival_long 
#' @param flightClass 
#' @param output 
#'
#' @return
#' @export
#'
#' @examples
custom_footprint <- function(
  departure_lat,
  departure_long,
  arrival_lat,
  arrival_long,
  flightClass = "Unknown",
  output = "co2e"
) {
  
  distance <- custom_dist(
    departure_lat,
    departure_long,
    arrival_lat,
    arrival_long
  )
  
  emissions_vector <- as_tibble(dplyr::case_when(
    distance <= 483 ~ "short",
    distance >= 3700 ~ "long",
    TRUE ~ "medium"
  )) %>% 
    left_join(footprint:::conversion_factors, by = c("value" = "distance")) %>% 
    filter(flightclass == flightClass) %>% 
    dplyr::pull(output) 
  
  round(distance * emissions_vector, 3)
}


#' Compute the total footprint for a specific player and year
#'
#' @param player Player name
#' @param year Year
#' @param output Output type, can take several values, see ?footprint::latlong_footprint
#'
#' @return A number (in kilograms of output)
#' @export
#' 
footprint_player_year <- function(player, year, output = "co2e") {
  
  data_filtered <- filter_player_year(player, year)
  
  if (is_empty(data_filtered))
    return(0)

  complete_footprint <- data_filtered %>%
    mutate(
      footprint_per_tourn = custom_footprint(lat_d, long_d, lat_a, long_a)
    ) %>%
    pull(footprint_per_tourn) 
  
  return(
    sum(complete_footprint, na.rm = T)
  )
  
}


#' Compute the evolution of footprint per year for a specific player
#'
#' @param player Player name
#'
#' @return A tibble
#' @export
#'
evol_footprint <- function(player) {
  
  get_tennis_data() %>% 
    filter(grepl(paste(player, collapse="|"), player_name)) %>% 
    select(player_name, Year, contains(c("_d", "_a"))) %>% 
    mutate(
      footprint = custom_footprint(lat_d, long_d, lat_a, long_a),
      Year = factor(Year)
    ) %>% 
    select(-contains(c("_a", "_d"))) %>% 
    group_by(player_name, Year) %>% 
    summarise(footprint = sum(footprint, na.rm = T)) %>% 
    arrange(player_name, Year)
  
}

