# Merge together the two bases 

library(dplyr)
library(tidyr)

load("data/tennis_data_geocodes.rda") 
load("data/tennis_players.rda")

tennis_data <- tennis_data_geocodes %>% 
  group_by(player_name, tourney_year) %>% 
  mutate(
    departure = tourney_location,
    arrival = lead(tourney_location),
    arrival = ifelse(is.na(arrival), "End of year", arrival),
    lat_d = lat,
    long_d = long,
    lat_a = lead(lat),
    long_a = lead(long),
    lat_a = ifelse(is.na(lat_a), lat_d, lat_a),
    long_a = ifelse(is.na(long_a), long_d, long_a)
  ) 

birth_dates <- tennis_players %>% 
  dplyr::select(full_name, birth_date) %>% 
  dplyr::distinct()

flags <- tennis_players %>% 
  dplyr::select(country_iso, flag_runes, flag) %>% 
  dplyr::distinct()

tennis_data <- tennis_data %>% 
  left_join(
    birth_dates,
    by = c("player_name" = "full_name")
  ) %>% 
  left_join(
    flags,
    by = c("player_iso" = "country_iso")
  ) %>% 
  ungroup() %>% 
  dplyr::filter(!is.na(lat_d), !is.na(long_d), 
           !is.na(lat_a), !is.na(long_a)) %>% 
  mutate(
    dist_per_tourn = custom_dist(lat_d, long_d, lat_a, long_a),
    footprint_per_tourn = custom_footprint(lat_d, long_d, lat_a, long_a)
  )

usethis::use_data(tennis_data, overwrite = T)
