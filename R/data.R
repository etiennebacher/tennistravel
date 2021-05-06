#' Conversion of distance in a carbon footprint
#'
#' A dataset containing the level of CO2 (among others) by categories of
#' distance (short, medium, long, international) and of flight class (unknown, economy, economy+, business, first).
#' 
#' This data comes from the package footprint (\code{footprint:::conversion_factors}).
#'
#' @docType data
#' @keywords datasets
#' @name conversion_factors
#' @format A data frame with 20 rows and 10 variables
NULL

#' List of male tennis players
#'
#' A dataset containing the name, date of birth, and country of origin of all male tennis players since 1968.
#' 
#' This data comes from https://github.com/JeffSackmann/tennis_atp/blob/master/atp_players.csv.
#'
#' @docType data
#' @keywords datasets
#' @name tennis_players
#' @format A data frame with 40957 rows and 7 variables
NULL

#' List of pairs player-tournament
#'
#' A dataset containing every pair player-tournament for each year. It also contains the latitude and longitude of the tournament.
#' 
#' This data comes from https://github.com/JeffSackmann/tennis_atp.
#'
#' @docType data
#' @keywords datasets
#' @name tennis_data_geocodes
#' @format A data frame with 166065 rows and 9 variables
NULL

#' Final dataset
#'
#' A dataset combining \code{tennis_players} and \code{tennis_data_geocodes}.
#' 
#'
#' @docType data
#' @keywords datasets
#' @name tennis_data
#' @format A data frame with 164804 rows and 21 variables
NULL