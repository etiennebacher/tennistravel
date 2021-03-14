# This data comes from https://github.com/JeffSackmann/tennis_atp/blob/master/atp_players.csv

library(countrycode)
library(dplyr)
library(tidyr)
library(emo)

tennis_players_raw <- read.csv("data-raw/tennis_players.csv")

tennis_players <-
  tennis_players_raw %>% 
  select(first_name, last_name, birth_date, country_code) %>% 
  rename("country_iso" = "country_code") %>% 
  # Eliminate all players that were above 45 in 2000
  mutate(
    birth_year = as.numeric(substr(birth_date, 1, 4)),
    age_2000 = 2000 - birth_year
  ) %>% 
  filter(age_2000 <= 40) %>% 
  select(- age_2000, - birth_year) %>% 
  mutate(
    # countrycode from International Olympic Committee
    country_name = countrycode(country_iso, "ioc", "country.name"),
    birth_date = lubridate::ymd(birth_date),
    abb_name = paste0(last_name, " ", substr(first_name, 1, 1), ".")
  ) 
  
list_flags <- emo::jis %>%
  filter(group == "Flags") %>% 
  select(runes, emoji, name) %>% 
  separate(runes, into = c("runes_1", "runes_2"), sep = " ") %>% 
  mutate(
    runes_1 = ifelse(!is.na(runes_1), paste0("U+", runes_1), NA),
    runes_2 = ifelse(!is.na(runes_2), paste0("U+", runes_2), NA)
  ) %>% 
  unite(runes_1, runes_2, col = "flag_runes", sep = " ", na.rm = T) %>% 
  rename("flag" = "emoji")

tennis_players <- tennis_players %>% 
  left_join(list_flags, by = c("country_name" = "name")) 

usethis::use_data(tennis_players, overwrite = TRUE)
