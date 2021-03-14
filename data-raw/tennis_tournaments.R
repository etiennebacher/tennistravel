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


### Treat all files

atp_files <- list.files(path = "data-raw", pattern = "atp_matches.*.csv")

test <- read.csv("data-raw/atp_matches_1968.csv")

tennis_data <- lapply(atp_files, function(x) {
  
  read.csv(paste0("data-raw/", x)) %>%
    select(tourney_name, tourney_date, winner_name,
           winner_ioc, loser_name, loser_ioc) %>%
    mutate(
      tourney_year = as.numeric(substr(tourney_date, 1, 4)),
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
    select(- win_lose) 
  
})
  
tennis_data_2 <- data.table::rbindlist(tennis_data) %>%
  filter(
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
    tourney_name = gsub(" WCT", "", tourney_name),
    tourney_name = gsub(" WTC", "", tourney_name),
    tourney_name = gsub("WCT ", "", tourney_name),
    tourney_name = gsub("-WCT", "", tourney_name),
    tourney_name = gsub(" Finals", "", tourney_name),
    tourney_name = gsub(" Masters", "", tourney_name),
    tourney_name = gsub(" Olympics", "", tourney_name),
    tourney_name = gsub(" 1", "", tourney_name),
    tourney_name = gsub(" 2", "", tourney_name),
    tourney_name = gsub("-1", "", tourney_name),
    tourney_name = gsub("-2", "", tourney_name),
    tourney_name = gsub(" Indoor", "", tourney_name),
    tourney_name = gsub(" Outdoor", "", tourney_name),
    tourney_name = gsub(" PSW", "", tourney_name),
    tourney_name = gsub(" SPW", "", tourney_name),
    tourney_name = gsub("Cap D'Adge", "Cap d'Agde", tourney_name),
    tourney_name = case_when(
      tourney_name == "US Open" ~ "New York",
      tourney_name == "Roland Garros" ~ "Paris",
      tourney_name == "Wimbledon" ~ "London",
      tourney_name == "Australian Open" ~ "Melbourne",
      tourney_name == "Djkarta" ~ "Djakarta",
      TRUE ~ tourney_name
    )
  ) %>% 
  rename("Location" = "tourney_name")
  

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
      filter(importance == max(importance)) %>%
      select(lon, lat) %>%
      group_by(lon) %>%
      mutate(n = n()) %>%
      ungroup() %>%
      filter(n == max(n))

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
geocodes <- sapply(unique(tennis_data_2$Location), get_lat_long)


geocodes_2 <- do.call(rbind.data.frame, geocodes) %>%
  mutate(
    across(c("long", "lat"), as.numeric)
  ) %>% 
  distinct() %>% 
  # Some destinations appear in double because lat and long are slightly
  # different. These differences are not important in our case, so I remove 
  # these duplicates
  tibble::rownames_to_column() %>% 
  filter(
    !substr(rowname, nchar(rowname)-1, nchar(rowname)) %in% c(".2", ".3")
  ) %>% 
  tibble::column_to_rownames()


### Add geocoding data to tennis data

tennis_data_geocodes <- left_join(
  tennis_data_2,
  geocodes_2,
  by = c("Location" = "city")
) %>%
  arrange(player_name, tourney_date) %>% 
  select(-tourney_date) %>% 
  group_by(player_name, tourney_year) %>% 
  distinct() %>% 
  mutate(order = row_number()) %>% 
  ungroup() %>% 
  mutate(
    player_name = trimws(player_name, which = "both", whitespace = "[ \t\r\n]")
  )

usethis::use_data(tennis_data_geocodes, overwrite = TRUE)
