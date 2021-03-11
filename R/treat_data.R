library(data.table)
library(dplyr)
library(tidyr)
library(lubridate)
library(echarts4r)


###########################
## PREPARATION OF DATA ##
###########################

# tennis_data_raw <- fread("inst/tennis_data/Data.csv")
# 
# tennis_data <- tennis_data_raw %>% 
#   select(Location, Date, Winner, Loser) %>% 
#   pivot_longer(
#     cols = c("Winner", "Loser"),
#     names_to = "win_lose",
#     values_to = "player_name"
#   ) %>% 
#   mutate(
#     Date = case_when(
#       nchar(Date) == 9 ~ substr(Date, 6, 9),
#       nchar(Date) == 10 ~ substr(Date, 7, 10),
#       TRUE ~ NA_character_
#     ),
#     Date = as.numeric(Date)
#   )
# 
# 
# get_lat_long <- function(city) {
#   x <- tryCatch(jsonlite::fromJSON(
#     paste0(
#       "https://nominatim.openstreetmap.org/search?q=", 
#       paste(city, collapse = "+"),
#       "&format=json")
#     ), error = function(e) NULL)
#   
#   if (!is.null(x)) {
#     x <- filter(x, importance == max(importance))
#     return(
#       data.frame(
#         city = city,
#         lat = x$lat,
#         long = x$lon
#       ) 
#     )
#   }
# }
# 
# 
# geocodes <- sapply(unique(tennis_data$Location), get_lat_long)
# geocodes_2 <- do.call(rbind.data.frame, test) %>% 
#   mutate(
#     across(c("long", "lat"),
#     as.numeric)
#   )
# 
# 
# tennis_data_geocodes <- left_join(
#   tennis_data,
#   geocodes_2,
#   by = c("Location" = "city")
# )
# 
# fwrite(tennis_data_geocodes, file = "inst/tennis_data/Data_geo.csv")



###########################
## LOAD TREATED DATA ##
###########################

tennis_data <- fread("inst/tennis_data/Data_geo.csv")

tennis_data %>%
  filter(Date == "2015", grepl("Federer", player_name)) %>%
  e_charts(long) %>%
  e_geo() %>%
  e_scatter(
    lat,
    bind= Location,
    coord_system = "geo",
    symbol_size = 10
  ) %>%
  e_tooltip(
    formatter = htmlwidgets::JS("
      function(params){
        return(params.name)
      }
    "))
