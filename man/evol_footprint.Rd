% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/evol.R
\name{evol_footprint}
\alias{evol_footprint}
\title{Compute the evolution of footprint per year for a specific player}
\usage{
evol_footprint(player, flightClass = "Unknown", output = "co2e")
}
\arguments{
\item{player}{Player name}

\item{flightClass}{Flight class of the individual. Default is 'Unknown', as in package footprint.}

\item{output}{Unit of footprint. Default is CO2, as in package footprint.}
}
\value{
A tibble
}
\description{
Compute the evolution of footprint per year for a specific player
}
