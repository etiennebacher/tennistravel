# pkgs
library(shiny)
library(shticky)
longdiv <- function(...){
  div(style = "min-height:100vh;", ...)
}
# ui
ui <- fluidPage(
  #'////////////////////////////////////////
  # head + css
    tags$style(
      'body {
                             background-image: url("bg_image_blurred.png");
                             background-repeat: no-repeat;
                             background-size: cover;
                             background-attachment: fixed;
              }'
    ),
  plotOutput("plot_output"),
  longdiv(),
  p("hello")
  
)


# server
server <- shinyServer(function(input, output){

  output$plot_output <- renderPlot(plot(iris))
  
})


# app
shinyApp(ui, server)