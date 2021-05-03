#' method UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_method_ui <- function(id){
  ns <- NS(id)
  tagList(
    wellPanel2(
      h2("Methodology", style = "text-align: center; color: white;"),
      h3("Latitude and longitude", style = "color: white;"),
      hr_white(),
      cus_p("First, I used datasets that contained all ATP matches from 1968 to 2020. What I needed was to know the list of tournaments a player participated in for each year, regarless of their performance. From this list (and the dates of each tournament), I could reconstruct their season chronologically. I had to convert some tournament names to match the city in which they took place (e.g the Masters Cup was relocated very often since 1970). Once I had all the cities that hosted tournaments, I used the ", tags$a(href = "https://nominatim.org/release-docs/develop/", target = "_blank", "Nominatim API"), ", which gives access to OpenStreetMap data, to get the latitude and longitude for each city."),
      br(),
      h3("Distance", style = "color: white;"),
      hr_white(),
      cus_p("After having obtained the coordinates of each city, I used the package ", tags$code("footprint"), ", which applies the ", tags$a(href = "https://en.wikipedia.org/wiki/Haversine_formula", target = "_blank", "Haversine great-circle distance formula"), " to calculate distance between latitude and longitude pairs."),
      br(),
      h3("Carbon footprint", style = "color: white;"),
      hr_white(),
      cus_p("From this distance, I once again used the package ", tags$code("footprint"), ", which also computes the carbon footprint generated from a plane trip on this distance."),
      br(),
      h3("Caveats", style = "color: white;"),
      hr_white(),
      cus_p("The distance and carbon footprints obtained may not be the exact ones. First, I only took into account the distance between two tournaments, but players also go home during the season. These travels are not accounted for in the computation. Also, the distance measure is between two points on the globe, but doesn't necessarily reflect the actual flight.")
    ),
    longdiv(45)
  )
}
    
#' method Server Function
#'
#' @noRd 
mod_method_server <- function(input, output, session){
  ns <- session$ns
 
}
    
## To be copied in the UI
# mod_method_ui("method_ui_1")
    
## To be copied in the server
# callModule(mod_method_server, "method_ui_1")
 
