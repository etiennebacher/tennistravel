#' Compute the evolution of distance per year for a specific player
#'
#' @param player Player name
#'
#' @return A tibble
#' @export
#'
evol_dist <- function(player) {
  
  get_tennis_data() %>% 
    filter(grepl(paste(player, collapse="|"), player_name)) %>% 
    select(player_name, Year, contains(c("_d", "_a"))) %>% 
    mutate(
      dist = custom_dist(lat_d, long_d, lat_a, long_a),
      Year = factor(Year)
    ) %>% 
    select(-contains(c("_a", "_d"))) %>% 
    group_by(player_name, Year) %>% 
    summarise(dist = sum(dist, na.rm = T)) %>% 
    ungroup() %>% 
    arrange(player_name, Year)
  
}

#' Compute the evolution of footprint per year for a specific player
#'
#' @param player Player name
#'
#' @return A tibble
#' @export
#'
evol_footprint <- function(player, flightClass = "Unknown", output = "co2e") {
  
  get_tennis_data() %>% 
    filter(grepl(paste(player, collapse="|"), player_name)) %>% 
    select(player_name, Year, contains(c("_d", "_a"))) %>% 
    mutate(
      footprint = custom_footprint(lat_d, long_d, lat_a, long_a,
                                   flightClass = flightClass,
                                   output = output),
      Year = factor(Year)
    ) %>% 
    select(-contains(c("_a", "_d"))) %>% 
    group_by(player_name, Year) %>% 
    summarise(footprint = sum(footprint, na.rm = T)) %>% 
    ungroup() %>% 
    arrange(player_name, Year)
  
}


#' Plot the footprint or the distance made by one or several players
#'
#' @param player Player(s) name(s)
#' @param indicator "dist" or "footprint"
#'
#' @return A graph
#' @export
#'

plot_evol <- function(player, indicator) {
  
  if (indicator == "dist") {
    data_type <- evol_dist(player)
    plot_title <- paste0("Distance (in km) per year made by ", 
                         knitr::combine_words(player))
  } else if (indicator == "footprint") {
    data_type <- evol_footprint(player)
    plot_title <- paste0("Carbon footprint (in kg of CO2) of ", 
                         knitr::combine_words(player), " per year")
  }
  
  data_type %>%
    complete(player_name, Year) %>% 
    group_by(player_name) %>% 
    arrange(indicator) %>% 
    e_charts(Year) %>%
    e_line_(indicator) %>% 
    e_legend(show = FALSE) %>%
    e_title(text =  plot_title)
  
}
