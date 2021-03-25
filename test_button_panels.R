library(shiny)


mod_1 <- function(id){
  ns <- NS(id)
  tagList(
    column(4),
    column(
      4,
      wellPanel(
        selectInput("names_iris", "names of iris", choices = names(iris)),
        br(),
        plotOutput("plot_iris")
      )
    ),
    column(
      4,
      actionButton("compare", "Compare")
    )
  )
}

mod_2 <- function(id){
  ns <- NS(id)
  tagList(
    column(1),
    column(
      4,
      wellPanel(
        selectInput("names_iris", "names of iris", choices = names(iris)),
        br(),
        plotOutput("plot_iris")
      )
    ),
    column(
      2,
      actionButton("back", "Back to single panel")
    ),
    column(
      4,
      wellPanel(
        selectInput("names_iris_2", "names of iris", choices = names(iris)),
        br(),
        plotOutput("plot_iris_2")
      )
    ),
    column(1)
  )
}

ui <- fluidPage(
  tags$head(
    tags$style(
      "#compare {
        -moz-transform:rotate(90deg);
        -ms-transform:rotate(90deg);
        -o-transform:rotate(90deg);
        -webkit-transform:rotate(90deg);
        transform-origin: -17%;
      }"
    )
  ),
  uiOutput("panel")
)

server <- function(input, output, session) {
  
  output$panel <- renderUI({
    mod_1("ui_1")
  })
  
  output$plot_iris <- renderPlot({
    plot(iris[[input$names_iris]])
  })
  
  observeEvent(input$compare, {
    output$panel <- renderUI({
      mod_2("ui_2")
    })
    output$plot_iris_2 <- renderPlot({
      plot(iris[[input$names_iris_2]])
    })
  })
  
  observeEvent(input$back, {
    output$panel <- renderUI({
      first_design
    })
  })

  
}

shinyApp(ui, server)