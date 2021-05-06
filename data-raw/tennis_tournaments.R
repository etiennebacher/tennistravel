# This data comes from hhttps://github.com/JeffSackmann/tennis_atp

library(dplyr)
library(tidyr)
library(data.table)
library(jsonlite)
library(tibble)
library(lubridate)

### Import all files from github

# years <- seq(1968, 2020, 1)
# for (i in years) {
#   x <- read.csv(
#     paste0("https://raw.githubusercontent.com/JeffSackmann/tennis_atp/master/atp_matches_", i, ".csv"))
#   write.csv(x, file = paste0("data-raw/atp_matches_", i, ".csv"))
#   remove(x)
# }


### Utils

is_even <- function(x) {
  sapply(x, function(y) {
    if((y %% 2) == 0) {
      TRUE
    } else {
      FALSE
    }
  })
}


### Treat all files

atp_files <- list.files(path = "data-raw", pattern = "atp_matches.*.csv")

tennis_data <- lapply(atp_files, function(x) {
  
  read.csv(paste0("data-raw/", x)) %>%
    dplyr::select(tourney_name, tourney_date, winner_name,
           winner_ioc, loser_name, loser_ioc) %>%
    mutate(
      tourney_year = as.numeric(substr(tourney_date, 1, 4)),
      tourney_month = as.numeric(substr(tourney_date, 5, 6)),
      tourney_date = ymd(tourney_date)
    ) %>% 
    relocate(tourney_year, .before = "tourney_date") %>% 
    pivot_longer(
      cols = c("winner_name", "loser_name"),
      names_to = "win_lose",
      values_to = "player_name"
    ) %>%
    mutate(
      winner_ioc = ifelse(win_lose == "winner_name", winner_ioc, NA),
      loser_ioc = ifelse(win_lose == "loser_name", loser_ioc, NA)
    ) %>% 
    unite(winner_ioc, loser_ioc, col = "player_iso", na.rm = T) %>%
    dplyr::select(- win_lose) 
  
})
  
tennis_data_2 <- data.table::rbindlist(tennis_data) %>%
  dplyr::filter(
    !grepl("Davis Cup", tourney_name),
    !grepl("Tournament of Champions", tourney_name),
    !grepl("ATP", tourney_name),
    !grepl("Atp", tourney_name),
    !grepl("Chps.", tourney_name),
    !grepl("Challenge Cup", tourney_name),
    !grepl("Nations Cup", tourney_name),
    !grepl("Grand Slam Cup", tourney_name),
    !grepl("Pepsi Grand Slam", tourney_name),
    !grepl("Masters Dec", tourney_name)
  ) %>% 
  mutate(
    tourney_name = ifelse(tourney_name == "Tour Finals", "Masters", tourney_name),
    tourney_name = ifelse(tourney_name == "Masters Cup", "Masters", tourney_name),
    tourney_location = tourney_name,
    tourney_location = gsub(" WCT", "", tourney_location),
    tourney_location = gsub(" WTC", "", tourney_location),
    tourney_location = gsub("WCT ", "", tourney_location),
    tourney_location = gsub("-WCT", "", tourney_location),
    tourney_location = gsub(" Finals", "", tourney_location),
    tourney_location = gsub(" Masters", "", tourney_location),
    tourney_location = gsub(" Olympics", "", tourney_location),
    tourney_location = gsub(" 1", "", tourney_location),
    tourney_location = gsub(" 2", "", tourney_location),
    tourney_location = gsub("-1", "", tourney_location),
    tourney_location = gsub("-2", "", tourney_location),
    tourney_location = gsub(" Indoor", "", tourney_location),
    tourney_location = gsub(" Outdoor", "", tourney_location),
    tourney_location = gsub(" PSW", "", tourney_location),
    tourney_location = gsub(" SPW", "", tourney_location),
    tourney_location = gsub("Cap D'Adge", "Cap d'Agde", tourney_location),
    tourney_location = case_when(
      tourney_location == "US Open" ~ "New York",
      tourney_location == "Roland Garros" ~ "Paris",
      tourney_location == "Wimbledon" ~ "London",
      tourney_location == "Australian Open" ~ "Melbourne",
      tourney_location == "Djkarta" ~ "Djakarta",
      tourney_location == "s Hertogenbosch" ~ "'s-Hertogenbosch",
      tourney_location == "St. Petersburg" ~ "Saint Petersburg",
      tourney_location == "Sofia" ~ "Sofia, Bulgaria",
      tourney_location == "Masters" & tourney_year == 1970 ~ "Tokyo",
      tourney_location == "Masters" & tourney_year == 1971 ~ "Paris",
      tourney_location == "Masters" & tourney_year == 1972 ~ "Barcelona",
      tourney_location == "Masters" & tourney_year == 1973 ~ "Boston",
      tourney_location == "Masters" & tourney_year == 1974 ~ "Melbourne",
      tourney_location == "Masters" & tourney_year == 1975 ~ "Stockholm",
      tourney_location == "Masters" & tourney_year == 1976 ~ "Houston",
      tourney_location == "Masters" & dplyr::between(tourney_year, 1977, 1989) ~ "New York",
      tourney_location == "Masters" & dplyr::between(tourney_year, 1990, 1995) ~ "Frankfurt",
      tourney_location == "Masters" & dplyr::between(tourney_year, 1996, 1999) ~ "Hanover",
      tourney_location == "Masters" & tourney_year == 2000 ~ "Lisbon",
      tourney_location == "Masters" & tourney_year == 2001 ~ "Sidney",
      tourney_location == "Masters" & tourney_year == 2002 ~ "Shanghai",
      tourney_location == "Masters" & dplyr::between(tourney_year, 2003, 2004) ~ "Houston",
      tourney_location == "Masters" & dplyr::between(tourney_year, 2005, 2008) ~ "Shanghai",
      tourney_location == "Masters" & dplyr::between(tourney_year, 2009, 2020) ~ "London",
      tourney_location == "Canada" & is_even(tourney_year)  ~ "Montreal",
      tourney_location == "Canada" & !is_even(tourney_year)  ~ "Toronto",
      TRUE ~ tourney_location
    )
  )
  

### Geocode the cities

get_lat_long <- function(city) {
  x <- tryCatch(jsonlite::fromJSON(
    paste0(
      "https://nominatim.openstreetmap.org/search?q=",
      paste(city, collapse = "+"),
      "&format=json")
  ), error = function(e) NULL)
  
  if (!is.null(x)) {
  #   # Take the coords with the most importance
  #   # If there are duplicates, choose the ones that are the most present
  #   # If this is not enough, round lat and long (which are very close but not
  #   # identical in some cases)
    x <- x %>%
      dplyr::filter(importance == max(importance)) %>%
      dplyr::select(lon, lat) %>%
      group_by(lon) %>%
      mutate(n = n()) %>%
      ungroup() %>%
      dplyr::filter(n == max(n))

    return(
      data.frame(
        city = city,
        lat = x$lat,
        long = x$lon
      )
    )
  }
}

# Takes time
geocodes <- sapply(unique(tennis_data_2$tourney_location), get_lat_long)


geocodes_2 <- do.call(rbind.data.frame, geocodes) %>%
  mutate(
    across(c("long", "lat"), as.numeric)
  ) %>% 
  dplyr::distinct() %>% 
  # Some destinations appear in double because lat and long are slightly
  # different. These differences are not important in our case, so I remove 
  # these duplicates
  tibble::rownames_to_column() %>% 
  dplyr::filter(
    !substr(rowname, nchar(rowname)-1, nchar(rowname)) %in% c(".2", ".3")
  ) %>% 
  tibble::column_to_rownames()


### Add geocoding data to tennis data

tennis_data_geocodes <- left_join(
  tennis_data_2,
  geocodes_2,
  by = c("tourney_location" = "city")
) %>%
  arrange(player_name, tourney_date) %>% 
  dplyr::select(-tourney_date) %>% 
  group_by(player_name, tourney_year) %>% 
  dplyr::distinct() %>% 
  mutate(order = row_number()) %>% 
  ungroup() %>% 
  mutate(
    player_name = trimws(player_name, which = "both", whitespace = "[ \t\r\n]")
  )

usethis::use_data(tennis_data_geocodes, overwrite = TRUE)
