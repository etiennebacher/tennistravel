#' player_card UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_player_card_ui <- function(id){
  ns <- NS(id)
  
  for_picker <- tennis_data %>% 
    select(player_name, player_iso, tourney_year) %>% 
    filter(!grepl("Unknown", player_name), !grepl("UNK", player_iso)) %>% 
    distinct() 
  
  tagList(
    wellPanel2(
      fluidRow(
        column(
          6, 
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
          6, 
          shinyWidgets::pickerInput(
            inputId = ns("year"),
            label = "",
            choices = NULL,
            selected = NULL,
            multiple = FALSE
          )
        )
      ),
      fluidRow(
        column(5),
        column(
          2,
          shinyWidgets::actionBttn(
            ns("run"),
            "Go!"
          )
        ),
        column(5)
      ),
      br(),
      uiOutput(ns("player_card")),
      br(),
      countup::countupOutput(ns("count_distance")),
      countup::countupOutput(ns("count_footprint")),
      longdiv(65)
    )
  )
}
    
#' player_card Server Function
#'
#' @noRd 
mod_player_card_server <- function(input, output, session){
  ns <- session$ns
 
  observeEvent(input$player, {
    shinyWidgets::updatePickerInput(
      session,
      "year",
      choices = tennis_data %>% 
        filter(player_name == input$player) %>% 
        pull(tourney_year) %>% 
        unique
    )
  })
  
  observeEvent(input$run, {
    output$count_distance <- countup::renderCountup({
      countup::countup(
        dist_player_year(input$player, input$year),
        options = list(
          suffix = ' km' 
        )
      )
    })
  })
  
}
    
## To be copied in the UI
# mod_player_card_ui("player_card_ui_1")
    
## To be copied in the server
# callModule(mod_player_card_server, "player_card_ui_1")
 
