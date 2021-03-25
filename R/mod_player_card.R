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
        column(2),
        column(
          8,
          cus_p('Pick a player, or let me pick for you, and click on "Go":',
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
    ),
    longdiv(65)
  )
}
    
#' player_card Server Function
#'
#' @noRd 
mod_player_card_server <- function(input, output, session){
  ns <- session$ns
 
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
 
