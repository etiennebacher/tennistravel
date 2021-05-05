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
    dplyr::select(player_name, player_iso, tourney_year) %>% 
    dplyr::filter(!grepl("Unknown", player_name), !grepl("UNK", player_iso)) %>% 
    dplyr::distinct()
  
  tagList(
    wellPanel2(
      fluidRow(
        column(2),
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
          ) %>% 
            prompter::add_prompt(
              message = "Choose a player",
              position = "top"
            )
        ),
        column(1),
        column(
          1, 
          actionButton(
            ns("random_pick"),
            "Pick for me"
          ) %>% 
            prompter::add_prompt(
              message = "Click to randomly select a player"
            )
        ),
        column(4)
      ),
      br(),
      br(),
      fluidRow(
        column(
          2,
          div(
            imageOutput(ns("flag")),
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
          p("Distance max: ", id = "dist_label") %>% 
            prompter::add_prompt(
              message = "The largest distance a player made in a single season on the ATP Tour in his career."
            )
        ),
        column(
          6,
          countup::odometerOutput(ns("count_distance")),
          p(" km", id = "dist_label_2")
        )
      ),
      br(),
      fluidRow(
        column(
          6,
          p("Carbon footprint max: ", id = "carb_label") %>% 
            prompter::add_prompt(
              message = "The largest carbon footprint a player generated in a single season on the ATP Tour in his career."
            ),
        ),
        column(
          6,
          countup::odometerOutput(ns("count_footprint")),
          p(" kg of CO2", id = "carb_label_2")
        )
      ),
      br(),
      uiOutput(ns("player_evolution"))
    ),
    longdiv(45)
  )
}
    
#' player_card Server Function
#'
#' @noRd 
mod_player_card_server <- function(input, output, session){
  ns <- session$ns
  
  
  for_picker <- tennis_data %>% 
    dplyr::select(player_name, player_iso, tourney_year) %>% 
    dplyr::filter(!grepl("Unknown", player_name), !grepl("UNK", player_iso)) %>% 
    dplyr::distinct()
  
  ### Use "Pick for me" button
  observeEvent(input$random_pick, {
    shinyWidgets::updatePickerInput(
      session = session,
      inputId = "player",
      selected = sample(for_picker$player_name, 1, replace = T)
    )
  })
  

  filtered_data <- reactive({
    req(input$player)
    tennis_data %>% 
      dplyr::filter(player_name == input$player)
  }) 
  
  
  ### Player card
  output$flag <- renderImage({
    country <- filtered_data() %>% 
      pull(player_iso_2) %>% 
      unique %>% 
      tolower
    filename <- normalizePath(
      file.path(
        paste0('./inst/app/www/flags/', country, '.svg')
      )
    )
    list(
      src = filename,
      alt = paste("Image number")
    )
  }, deleteFile = FALSE)
  
  output$name <- renderText({
    filtered_data() %>%
      pull(player_name) %>%
      unique()
  })
  output$count_distance <- countup::renderOdometer({
    max_dist <- evol_dist(input$player) %>% 
      pull(dist) %>% 
      max %>% 
      round(., 0)
    countup::odometer(max_dist)
  })
  output$count_footprint <- countup::renderOdometer({
    max_footprint <- evol_footprint(input$player) %>% 
      pull(footprint) %>% 
      max %>% 
      round(., 0)
    countup::odometer(max_footprint)
  })

  
  ### Evolution for a player
  output$player_evolution <- renderUI({
    tagList(
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

  
}
    
## To be copied in the UI
# mod_player_card_ui("player_card_ui_1")
    
## To be copied in the server
# callModule(mod_player_card_server, "player_card_ui_1")
 
