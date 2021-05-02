#' atp_calendar UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_atp_calendar_ui <- function(id){
  ns <- NS(id)
  tagList(
    wellPanel2(
      cus_p("The ATP (Association of Tennis Professionals) Tour takes place all over the world."),
      br(),
      fluidRow(
        column(5),
        column(
          2, 
          shinyWidgets::pickerInput(
            inputId = ns("year"),
            label = "",
            choices = sort(unique(tennis_data$tourney_year)),
            selected = 2019,
            multiple = FALSE
          )
        ),
        column(5) 
      ),
      br(),
      echarts4r::echarts4rOutput(ns("plot_calendar")),
      br(),
      cus_p("All of these trips imply that tennis players in general have a huge carbon footprint compared to average people. But can we quantify this carbon footprint?")
    ),
    longdiv(45)
  )
}
    
#' atp_calendar Server Function
#'
#' @noRd 
mod_atp_calendar_server <- function(input, output, session){
  ns <- session$ns
  
  output$plot_calendar <- echarts4r::renderEcharts4r({
    tournament_calendar(input$year)
  })
 
}
    
## To be copied in the UI
# mod_atp_calendar_ui("atp_calendar_ui_1")
    
## To be copied in the server
# callModule(mod_atp_calendar_server, "atp_calendar_ui_1")
 
