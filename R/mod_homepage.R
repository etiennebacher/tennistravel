#' homepage UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_homepage_ui <- function(id){
  ns <- NS(id)
  tagList(
    wellPanel2(
      h1("What is the carbon footprint of tennis players?",
          style = "text-align: center; color: white;"
      ) %>% 
        vov::fade_in(duration = "slower")
    ),
    longdiv(5),
    # Mouse animation
    # Comes from: https://josefzacek.com/blog/how-to-create-scroll-down-animated-mouse-effect-using-html-and-css.html
    fluidRow(
      tags$div(
        class = "moving-mouse-holder",
        tags$div(
          class = "mouse",
          tags$div(
            class = "mouse-button",
            HTML("&nbsp;")
          )
        )
      )
    ),
    longdiv(35)
  )
}
    
#' homepage Server Function
#'
#' @noRd 
mod_homepage_server <- function(input, output, session){
  ns <- session$ns
}
    
## To be copied in the UI
# mod_homepage_ui("homepage_ui_1")
    
## To be copied in the server
# callModule(mod_homepage_server, "homepage_ui_1")
 
