#' credits UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_credits_ui <- function(id){
  ns <- NS(id)
  tagList(
    wellPanel2(
      h2("Credits", style = "text-align: center; color: white;"),
      h3("Data", style = "color: white;"),
      hr(style = "color: white;"),
      cus_p("This Shiny app was made possible thanks to the huge work of ", 
            shiny::tags$a(href = "http://www.jeffsackmann.com/", "Jeff Sackmann,"),
            "who compiled all ATP matches from 1968 until today on a ",
            shiny::tags$a(href = "https://github.com/JeffSackmann/tennis_atp",
                          "GitHub repository."),
            "This provides many information on each tournament, including the 
            participants and the location."),
      br(),
      h3("Distance and footprint", style = "color: white;"),
      hr(style = "color: white;"),
      cus_p("I used or inspired from the package", 
            shiny::tags$a(href= "https://github.com/acircleda/footprint",
                          "footprint,"),
            "created by ",
            shiny::tags$a(href = "https://github.com/acircleda", "Anthony Schmidt"),
            "to compute the distance between two cities, and the carbon footprint",
            "corresponding to a plane trip between these two cities."),
      br(),
      h3("Image", style = "color: white;"),
      hr(style = "color: white;"),
      cus_p("The image in the background comes from",
            shiny::tags$a(href = "https://commons.wikimedia.org/wiki/File:2013_ATP_World_Tour_Finals_Berdych_vs_Ferrer.jpg",
                          "Wikimedia Commons,"),
            " more specifically from andrewrendell. It is under CC BY 2.0 license."),
      br(),
      br(),
      fluidRow(
        column(5),
        column(
          2,
          actionButton(
            ns("see_source"), "Source code", 
            icon = shiny::icon("github"),
            onclick ="window.open('https://github.com/etiennebacher/tennistravel/', '_blank')"
          )
        ),
        column(5)
      )
    ),
    longdiv(15)
  )
}
    
#' credits Server Function
#'
#' @noRd 
mod_credits_server <- function(input, output, session){
  ns <- session$ns
 
}
    
## To be copied in the UI
# mod_credits_ui("credits_ui_1")
    
## To be copied in the server
# callModule(mod_credits_server, "credits_ui_1")
 
