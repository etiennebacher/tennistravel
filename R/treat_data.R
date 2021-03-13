#' Clean and geocode the data
#' 
#' This function imports the raw dataset, cleans it, and uses OSM API (nominatim) to geocode (i.e detect the latitude and longitude) the cities.
#'
#' @return Writes a CSV file named "Data_geo.csv"
#' @import dplyr

clean_and_geocode <- function() {
  
  tennis_data_raw <- fread("inst/tennis_data/Data.csv")
  
  tennis_data <- tennis_data_raw %>%
    select(Location, Date, Winner, Loser) %>%
    pivot_longer(
      cols = c("Winner", "Loser"),
      names_to = "win_lose",
      values_to = "player_name"
    ) %>%
    mutate(
      Year = case_when(
        nchar(Date) == 9 ~ substr(Date, 6, 9),
        nchar(Date) == 10 ~ substr(Date, 7, 10),
        TRUE ~ NA_character_
      ),
      Year = as.numeric(Year),
      Date = format(as.Date(Date, format = "%d/%m/%Y"), "%Y-%m-%d")
    ) %>% 
    select(-win_lose) %>% 
    distinct() 
  
  
  get_lat_long <- function(city) {
    x <- tryCatch(jsonlite::fromJSON(
      paste0(
        "https://nominatim.openstreetmap.org/search?q=",
        paste(city, collapse = "+"),
        "&format=json")
    ), error = function(e) NULL)
    
    if (!is.null(x)) {
      x <- filter(x, importance == max(importance))
      return(
        data.frame(
          city = city,
          lat = x$lat,
          long = x$lon
        )
      )
    }
  }
  
  
  geocodes <- sapply(unique(tennis_data$Location), get_lat_long)
  geocodes_2 <- do.call(rbind.data.frame, geocodes) %>%
    mutate(
      across(c("long", "lat"), as.numeric)
    ) %>% 
    distinct() %>% 
    tibble::rownames_to_column() %>% 
    filter(
      !substr(rowname, nchar(rowname)-1, nchar(rowname)) %in% c(".2", ".3")
    ) %>% 
    tibble::column_to_rownames()
  
  
  tennis_data_geocodes <- left_join(
    tennis_data,
    geocodes_2,
    by = c("Location" = "city")
  )
  
  fwrite(tennis_data_geocodes, file = "inst/tennis_data/Data_geo.csv")
  
  
}


#' Get the data and create the order in which the players play the tournaments
#'
#' @return A tibble
#' @export
#'
get_tennis_data <- function() {
  
  data.table::fread("inst/tennis_data/Data_geo.csv") %>%
    arrange(player_name, Date) %>% 
    select(-Date) %>% 
    group_by(player_name, Year) %>% 
    distinct() %>% 
    mutate(order = row_number()) %>% 
    ungroup() 
  
}


#' Compute the total distance for a specific player and year
#'
#' @param player Name of the player
#' @param year Year 
#'
#' @return A number (in km)
#' @export
#'
distance_player_year <- function(player, year) {
  
  data_filtered <- filter_player_year(player, year)
  
  complete_dist <- data_filtered %>% 
    mutate(
      dist_per_tourn = custom_dist(lat_d, long_d, lat_a, long_a)
    ) %>% 
    pull(dist_per_tourn) %>% 
    sum(., na.rm = T)
  
  return(complete_dist)
  
}


#' Function to compute the distance between two points
#' 
#' This function was adapted from the source code of footprint::latlong_footprint()
#'
#' @param departure_lat Latitude of the first city
#' @param departure_long Longitude of the first city
#' @param arrival_lat Latitude of the second city
#' @param arrival_long Longitude of the second city
#'
#' @return A numeric value
#'

custom_dist <- function (departure_lat, departure_long, arrival_lat, arrival_long) 
{
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
  lon1 = departure_long * pi/180
  lat1 = departure_lat * pi/180
  lon2 = arrival_long * pi/180
  lat2 = arrival_lat * pi/180
  radius = 6373
  dlon = lon2 - lon1
  dlat = lat2 - lat1
  a = (sin(dlat/2))^2 + cos(lat1) * cos(lat2) * (sin(dlon/2))^2
  b = 2 * atan2(sqrt(a), sqrt(1 - a))
  distance = radius * b
  return(distance)
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
  
  # For loop because using footprint::latlong_footprint() in mutate
  # creates a warning
  complete_footprint <- c()
  for (i in 1:nrow(data_filtered)) {
    data_row <- data_filtered[i, ]
    complete_footprint[i] <- footprint::latlong_footprint(
      departure_lat = data_row$lat_d,
      departure_long = data_row$long_d,
      arrival_lat = data_row$lat_a,
      arrival_long = data_row$long_a,
      output = output
    )
  }
  
  return(
    sum(complete_footprint, na.rm = T)
  )
  
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
  
  tennis_data %>% 
    filter(Year == year, grepl(player, player_name)) %>%
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
