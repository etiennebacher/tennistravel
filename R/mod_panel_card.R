#' panel_card UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_panel_card_ui <- function(id){
  ns <- NS(id)
  
  for_picker <- tennis_data %>% 
    select(player_name, player_iso, tourney_year) %>% 
    filter(!grepl("Unknown", player_name), !grepl("UNK", player_iso)) %>% 
    distinct()
  
  tagList(
    wellPanel2(
      fluidRow(
        column(2),
        column(
          8,
          cus_p('Pick a player and a year, or let me pick for you, and click on "Go":',
                style = "text-align: center;")
        ),
        column(2)
      ),
      fluidRow(
        column(
          4, 
          shinyWidgets::pickerInput(
            inputId = ns("player"),
            label = "",
            choices = unique(for_picker$player_name),
            choicesOpt = list(
              subtext = unique(for_picker$player_iso)
            ),
            options = list(
              `live-search` = TRUE
            ),
            selected = "Roger Federer",
            multiple = FALSE
          )
        ),
        column(
          4, 
          shinyWidgets::pickerInput(
            inputId = ns("year"),
            label = "",
            choices = NULL,
            selected = NULL,
            multiple = FALSE
          )
        ),
        column(1),
        column(
          2, 
          actionButton(
            ns("random_pick"),
            "Pick for me"
          )
        ),
        column(1)
      ),
      br(),
      fluidRow(
        column(5),
        column(
          2,
          shiny::actionButton(
            ns("run"),
            "Go!"
          )
        ),
        column(5)
      ),
      br(),
      uiOutput(ns("player_card"))
    )
  )
}
    
#' panel_card Server Function
#'
#' @noRd 
mod_panel_card_server <- function(input, output, session){
  ns <- session$ns
 
}
    
## To be copied in the UI
# mod_panel_card_ui("panel_card_ui_1")
    
## To be copied in the server
# callModule(mod_panel_card_server, "panel_card_ui_1")
 
