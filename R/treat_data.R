#' Clean and geocode the data
#' 
#' This function imports the raw dataset, cleans it, and uses OSM API (nominatim) to geocode (i.e detect the latitude and longitude) the cities.
#'
#' @return Writes a CSV file named "Data_geo.csv"
#'

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



#' Function to compute the distance between two points
#' 
#' Credit for this function goes to Mario Pineda-Krch: https://www.r-bloggers.com/2010/11/great-circle-distance-calculations-in-r/
#'
#' @param long1 Longitude of the first city
#' @param lat1 Latitude of the first city
#' @param long2 Longitude of the second city
#' @param lat2 Latitude of the second city
#'
#' @return A numeric value
#'

gcd.vif <- function(long1, lat1, long2, lat2) {
  
  # WGS-84 ellipsoid parameters
  
  a <- 6378137         # length of major axis of the ellipsoid (radius at equator)
  b <- 6356752.314245  # ength of minor axis of the ellipsoid (radius at the poles)
  f <- 1/298.257223563 # flattening of the ellipsoid
  L <- long2-long1 # difference in longitude
  U1 <- atan((1-f) * tan(lat1)) # reduced latitude
  U2 <- atan((1-f) * tan(lat2)) # reduced latitude
  sinU1 <- sin(U1)
  cosU1 <- cos(U1)
  sinU2 <- sin(U2)
  cosU2 <- cos(U2)
  cosSqAlpha <- NULL
  sinSigma <- NULL
  cosSigma <- NULL
  cos2SigmaM <- NULL
  sigma <- NULL
  lambda <- L
  lambdaP <- 0
  iterLimit <- 100
  while (abs(lambda-lambdaP) > 1e-12 & iterLimit>0) {
    sinLambda <- sin(lambda)
    cosLambda <- cos(lambda)
    sinSigma <- sqrt( (cosU2*sinLambda) * (cosU2*sinLambda) +
                        (cosU1*sinU2-sinU1*cosU2*cosLambda) * (cosU1*sinU2-sinU1*cosU2*cosLambda) )
    if (sinSigma==0) return(0)  # Co-incident points
    cosSigma <- sinU1*sinU2 + cosU1*cosU2*cosLambda
    sigma <- atan2(sinSigma, cosSigma)
    sinAlpha <- cosU1 * cosU2 * sinLambda / sinSigma
    cosSqAlpha <- 1 - sinAlpha*sinAlpha
    cos2SigmaM <- cosSigma - 2*sinU1*sinU2/cosSqAlpha
    if (is.na(cos2SigmaM)) cos2SigmaM <- 0  # Equatorial line: cosSqAlpha=0
    C <- f/16*cosSqAlpha*(4+f*(4-3*cosSqAlpha))
    lambdaP <- lambda
    lambda <- L + (1-C) * f * sinAlpha *
      (sigma + C*sinSigma*(cos2SigmaM+C*cosSigma*(-1+2*cos2SigmaM*cos2SigmaM)))
    iterLimit <- iterLimit - 1
  }
  if (iterLimit==0) return(NA)  # formula failed to converge
  uSq <- cosSqAlpha * (a*a - b*b) / (b*b)
  A <- 1 + uSq/16384*(4096+uSq*(-768+uSq*(320-175*uSq)))
  B <- uSq/1024 * (256+uSq*(-128+uSq*(74-47*uSq)))
  deltaSigma = B*sinSigma*(cos2SigmaM+B/4*(cosSigma*(-1+2*cos2SigmaM^2) -
                                             B/6*cos2SigmaM*(-3+4*sinSigma^2)*(-3+4*cos2SigmaM^2)))
  s <- b*A*(sigma-deltaSigma) / 1000
  return(s) # Distance in km
}




library(data.table)
library(dplyr)
library(tidyr)
library(echarts4r)


tennis_data <- fread("inst/tennis_data/Data_geo.csv")

tennis_data %>% 
  arrange(player_name, Date) %>% 
  group_by(player_name, Date) %>% 
  mutate(order = row_number()) %>% 
  filter(Year == "2015", grepl("Federer", player_name)) %>%
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
