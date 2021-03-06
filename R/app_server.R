#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function( input, output, session ) {
  sever::sever(
    html = tagList(
      h1("Whoops..."),
      p("You have been disconnected"),
      sever::reload_button("Reload", class = "default")
    ), 
    color = "#fff",
    bg_image = "www/bg_image_blurred.png",
    box = TRUE
  )
  sever::rupture(
    html = tagList(
      h1("Whoops..."),
      p("You have been disconnected because of inactivity"),
      sever::reload_button("Reload", class = "default")
    ), 
    color = "#fff",
    bg_image = "www/bg_image_blurred.png",
    box = TRUE
  )
  callModule(mod_homepage_server, "homepage_ui_1")
  callModule(mod_atp_calendar_server, "atp_calendar_ui_1")
  callModule(mod_player_card_server, "player_card_ui_1")
  callModule(mod_method_server, "method_ui_1")
  callModule(mod_credits_server, "credits_ui_1")
}
