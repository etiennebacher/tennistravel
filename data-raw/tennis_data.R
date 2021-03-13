## code to prepare `tennis_data` dataset goes here

library(dplyr)
library(tidyr)
library(jsonlite)
library(tibble)

tennis_data_raw <- read.csv("data-raw/tennis_data.csv")

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

# Takes time
geocodes <- sapply(unique(tennis_data$Location), get_lat_long)

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


tennis_data_geocodes <- left_join(
  tennis_data,
  geocodes_2,
  by = c("Location" = "city")
) %>%
  arrange(player_name, Date) %>% 
  select(-Date) %>% 
  group_by(player_name, Year) %>% 
  distinct() %>% 
  mutate(order = row_number()) %>% 
  ungroup() %>% 
  mutate(
    player_name = trimws(player_name, which = "right", whitespace = "[ \t\r\n]")
  )

usethis::use_data(tennis_data_geocodes, overwrite = TRUE)
