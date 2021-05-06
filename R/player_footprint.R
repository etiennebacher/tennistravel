#' Used in data-raw scripts
#'
#' @param departure_lat Latitude of departure city
#' @param departure_long Longitude of departure city
#' @param arrival_lat Latitude of arrival city
#' @param arrival_long Longitude of arrival city
#' @param flightClass Flight class of individual
#' @param output Unit of carbon footprint
#'
#' @export
#'
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
    left_join(conversion_factors, by = c("value" = "distance")) %>% 
    dplyr::filter(flightclass == flightClass) %>% 
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
    pull(footprint_per_tourn)  %>%
    sum(., na.rm = T) %>% 
    round(., 0)
  

  return(complete_footprint)
  
}


