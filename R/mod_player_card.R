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

  tagList(
    uiOutput(ns("card")),
    fluidRow(
      longdiv(65)
    )
  )
}
    
#' player_card Server Function
#'
#' @noRd 
mod_player_card_server <- function(input, output, session){
  ns <- session$ns
  
  output$card <- renderUI({
    mod_ui_card_1_ui("ui_card_1_ui_1")
  })
  
  observeEvent(input[["ui_card_1_ui_1-compare"]], {
    # output$card <- renderUI({
    #   mod_ui_card_2_ui("ui_card_2_ui_1")
    # })
    htmlwidgets::JS("console.log('hello')")
  })
  
  filtered_data <- reactive({
    req(input$player)
    tennis_data %>% 
      filter(player_name == input$player)
  }) 
  
  observe({
    req(filtered_data())
    shinyWidgets::updatePickerInput(
      session,
      "year",
      choices = filtered_data() %>% 
        pull(tourney_year) %>% 
        unique
    )
  })
  
  observeEvent(input$run, {
    output$player_card <- renderUI({
      req(filtered_data(), input$year, input$player)
      tagList(
        br(),
        wellPanel2(
          fluidRow(
            column(2, 
                   div(
                     textOutput(ns("flag")),
                     id = "pl_flag"
                    )
            ),
            column(10, 
                   div(
                     textOutput(ns("name")),
                     id = "pl_name"
                   )
            )
          ),
          fluidRow(
            column(
              8, 
              shiny::tags$span(
                p("Distance", id = "dist_label"), 
                p("(km)", id = "dist_label_2"),
                p(": ", id = "dist_label")
              )
            ), 
            column(4, countup::odometerOutput(ns("count_distance")))
          ),
          br(),
          fluidRow(
            column(8, p("Carbon footprint", id = "carb_label"),
                   p("(kg of CO2)", id = "carb_label_2"),
                   p(": ", id = "carb_label")), 
            column(4, countup::odometerOutput(ns("count_footprint")))
          ),
          br(),
          fluidRow(
            column(5),
            column(
              2,
              actionButton(ns("see_evol"), "Evolution", 
                           icon = shiny::icon("line-chart"))
            ),
            column(5)
          )
        )
      )
    })
    output$flag <- renderText({
      filtered_data() %>%
        pull(flag) %>%
        unique
    })
    output$name <- renderText({
      filtered_data() %>%
        pull(player_name) %>%
        unique
    })
    output$count_distance <- countup::renderOdometer({
      countup::odometer(dist_player_year(input$player, input$year))
    })
    output$count_footprint <- countup::renderOdometer({
      countup::odometer(footprint_player_year(input$player, input$year))
    })
  })
  
}
    
## To be copied in the UI
# mod_player_card_ui("player_card_ui_1")
    
## To be copied in the server
# callModule(mod_player_card_server, "player_card_ui_1")
 
