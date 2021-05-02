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
        cus_p('Pick a player and a year, or let me pick for you, and click on "Go":',
              style = "text-align: center;")
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
      uiOutput(ns("player_card")),
      uiOutput(ns("player_evolution"))
    ),
    longdiv(65)
  )
}
    
#' player_card Server Function
#'
#' @noRd 
mod_player_card_server <- function(input, output, session){
  ns <- session$ns
  
  
  for_picker <- tennis_data %>% 
    select(player_name, player_iso, tourney_year) %>% 
    filter(!grepl("Unknown", player_name), !grepl("UNK", player_iso)) %>% 
    distinct()
  
  ### Use "Pick for me" button
  observeEvent(input$random_pick, {
    random_player <- sample(for_picker$player_name, 1, replace = T)
    random_year <- for_picker %>% 
      filter(player_name == random_player) %>% 
      pull(tourney_year) %>% 
      sample(., size = 1)
    shinyWidgets::updatePickerInput(
      session = session,
      inputId = "player",
      selected = random_player
    )
    shinyWidgets::updatePickerInput(
      session = session,
      inputId = "year",
      selected = random_year
    )
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
    
    ### Card of player-season
    output$player_card <- renderUI({
      req(filtered_data(), input$year, input$player)
      tagList(
        br(),
        fluidRow(
          column(
            2,
            div(
              textOutput(ns("flag")),
              id = "pl_flag"
            )
          ),
          column(
            10,
            div(
              textOutput(ns("name")),
              id = "pl_name"
            )
          )
        ),
        fluidRow(
          column(
            6,
            shiny::tags$span(
              p("Distance", id = "dist_label"),
              p(": ", id = "dist_label")
            )
          ),
          column(
            6,
            countup::odometerOutput(ns("count_distance")),
            p("km", id = "dist_label_2")
          )
        ),
        br(),
        fluidRow(
          column(
            6,
            p("Carbon footprint", id = "carb_label"),
            p(": ", id = "carb_label")
          ),
          column(
            6,
            countup::odometerOutput(ns("count_footprint")),
            p("kg of CO2", id = "carb_label_2")
          )
        ),
        br()
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
    
    
    ### Evolution for a player
    output$player_evolution <- renderUI({
      tagList(
        hr(),
        echarts4r::echarts4rOutput(
          ns("evol_km")
        ),
        echarts4r::echarts4rOutput(
          ns("evol_co2")
        )
      )
    })
    output$evol_km <- echarts4r::renderEcharts4r({
      plot_evol(input$player, "dist")
    })
    output$evol_co2 <- echarts4r::renderEcharts4r({
      plot_evol(input$player, "footprint")
    })
  })
  
}
    
## To be copied in the UI
# mod_player_card_ui("player_card_ui_1")
    
## To be copied in the server
# callModule(mod_player_card_server, "player_card_ui_1")
 
