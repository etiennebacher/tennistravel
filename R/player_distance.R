#' Function to compute the distance between two points
#' 
#' This function was adapted from the source code of footprint::latlong_footprint(). Credits go to Anthony Schmidt.
#'
#' @param departure_lat Latitude of the first city
#' @param departure_long Longitude of the first city
#' @param arrival_lat Latitude of the second city
#' @param arrival_long Longitude of the second city
#'
#' @return A numeric value
#'

custom_dist <- function(
  departure_lat,
  departure_long,
  arrival_lat,
  arrival_long
) {
  if (!(all(is.numeric(c(departure_long, arrival_long))) &&
    departure_long >= -180 && arrival_long >= -180 && departure_long <=
    180 && arrival_long <= 180)) {
    stop("Longitude must be numeric and has values between -180 and 180")
  }
  if (!(all(is.numeric(c(departure_lat, arrival_lat))) &&
    departure_lat >= -90 && arrival_lat >= -90 && departure_lat <=
    90 && arrival_lat <= 90)) {
    stop("Latitude must be numeric and has values between -90 and 90")
  }
  lon1 <- departure_long * pi / 180
  lat1 <- departure_lat * pi / 180
  lon2 <- arrival_long * pi / 180
  lat2 <- arrival_lat * pi / 180
  radius <- 6373
  dlon <- lon2 - lon1
  dlat <- lat2 - lat1
  a <- (sin(dlat / 2))^2 + cos(lat1) * cos(lat2) * (sin(dlon / 2))^2
  b <- 2 * atan2(sqrt(a), sqrt(1 - a))
  distance <- radius * b
  return(distance)
}


#' Compute the total distance for a specific player and year
#'
#' @param player Name of the player
#' @param year Year 
#'
#' @return A number (in km)
#' @export
#'
dist_player_year <- function(player, year) {
  
  data_filtered <- filter_player_year(player, year)
  
  if (is_empty(data_filtered))
    return(0)
  
  complete_dist <- data_filtered %>%
    mutate(
      dist_per_tourn = custom_dist(lat_d, long_d, lat_a, long_a)
    ) %>%
    pull(dist_per_tourn) %>%
    sum(., na.rm = T)
  
  return(complete_dist)
  
}


