#' ui_card_2 UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_ui_card_2_ui <- function(id){
  ns <- NS(id)
  
  tagList(
    fluidRow(
      column(1),
      column(
        4,
        mod_panel_card_ui("double_card_1")
      ),
      column(
        2,
        actionButton(ns("back"), "Back to one panel")
      ),
      column(
        4,
        mod_panel_card_ui("double_card_2")
      ),
      column(1)
    )
  )
}
    
#' ui_card_2 Server Function
#'
#' @noRd 
mod_ui_card_2_server <- function(input, output, session){
  ns <- session$ns
 
}
    
## To be copied in the UI
# mod_ui_card_2_ui("ui_card_2_ui_1")
    
## To be copied in the server
# callModule(mod_ui_card_2_server, "ui_card_2_ui_1")
 
