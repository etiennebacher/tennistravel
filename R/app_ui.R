#' The application User-Interface
#' 
#' @param request Internal parameter for `{shiny}`. 
#'     DO NOT REMOVE.
#' @import shiny fullPage pushbar
#' @noRd
app_ui <- function(request) {
  fluidPage(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # List the first level UI elements here 
    fluidRow(
      longdiv(35),
      column(width = 3),
      column(width = 6,
             mod_homepage_ui("homepage_ui_1"),
             mod_atp_calendar_ui("atp_calendar_ui_1")
      ),
      column(width = 3) 
    ),
    fluidRow(
      mod_player_card_ui("player_card_ui_1")
    ),
    fluidRow(
      column(width = 3),
      column(width = 6,
             mod_method_ui("method_ui_1"),
             mod_credits_ui("credits_ui_1")
      ),
      column(width = 3) 
    )
  )
}

#' Add external Resources to the Application
#' 
#' This function is internally used to add external 
#' resources inside the Shiny application. 
#' 
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function(){
  
  add_resource_path(
    'www', app_sys('app/www')
  )
 
  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys('app/www'),
      app_title = 'tennistravel'
    ),
    # Add here other external resources for example, you can add
    # shinya?lert::useShinyalert()
    vov::use_vov(),
    prompter::use_prompt(),
    gotop::use_gotop(
      color = "white",
      opacity = 0.9,
      width = 60,
      appear = 400
    ),
    pushbar::pushbar_deps()
  )
}

