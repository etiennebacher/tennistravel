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
      h3("Distance", style = "color: white;"),
      hr(style = "color: white;"),
      br(),
      h3("Carbon footprint", style = "color: white;"),
      hr(style = "color: white;"),
    ),
    longdiv(65)
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
 
