#' ui_card_1 UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_ui_card_1_ui <- function(id){
  ns <- NS(id)
  
  tagList(
    fluidRow(
      column(3),
      column(
        6,
        mod_panel_card_ui("single_card")
      ),
      column(
        3,
        actionButton(ns("compare"), "Compare")
      )
    )
  )
}
    
#' ui_card_1 Server Function
#'
#' @noRd 
mod_ui_card_1_server <- function(input, output, session){
  ns <- session$ns
 
}
    
## To be copied in the UI
# mod_ui_card_1_ui("ui_card_1_ui_1")
    
## To be copied in the server
# callModule(mod_ui_card_1_server, "ui_card_1_ui_1")
 
