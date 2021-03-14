#' homepage UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_homepage_ui <- function(id){
  ns <- NS(id)
  tagList(
    fullSlideImage(
      img = "www/bg_image_blurred.png",
      center = FALSE,
      menu = NULL,
      textyle::textyle(
        h1("What is the footprint of tennis players?"),
        transition = 0.1,
        duration = 0.1,
        color = "#b9b4b4"
      ),
      cus_p("The ATP (Association of Tennis Professionals) Tour takes place all over the world. We can divide in more or less X periods:"),
      withTags(
        cus_ul(
          cus_li("January: Oceania round (Auckland, Sidney, Brisbane, and most importantly ", cus_b("Melbourne (Australian Open);")),
          cus_li("February: transition round (Dubai, Rotterdam, Mexico, Buenos Aires)"),
          cus_li("March: first US round (Indian Wells, Miami)"),
          cus_li("April - June: clay round (Monte Carlo, Barcelona, Madrid, Rome, and most importantly ", cus_b("Roland Garros;")),
          cus_li("June - July: grass round (Queens, Halle, and most importantly ", cus_b("Wimbledon;")),
          cus_li("July: transition round (Newport, Hamburg, Atlanta);"),
          cus_li("August - September: North American round (Cincinnati, Montréal/Toronto, and most importantly", cus_b("Flushing Meadows (US Open);")),
          cus_li("September - October: Asian round (Beijing, Tokyo, Shanghai);"),
          cus_li("October - November: European round (Basel, Stockholm, Paris-Bercy, and most importantly the", cus_b("Masters in London;"))
        )
      ),
      withTags(
        cus_p("All of these trips imply that tennis players in general have a huge carbon footprint compared to average people. But can we", cus_b("quantify this carbon footprint?"))
      )
    )
  )
}
    
#' homepage Server Function
#'
#' @noRd 
mod_homepage_server <- function(input, output, session){
  ns <- session$ns
  setup_pushbar()
  observeEvent(input$open, {
    pushbar_open(id = "myPushbar")
  })  
}
    
## To be copied in the UI
# mod_homepage_ui("homepage_ui_1")
    
## To be copied in the server
# callModule(mod_homepage_server, "homepage_ui_1")
 
