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
    filter(tourney_year == year, grepl(player, player_name))

}



