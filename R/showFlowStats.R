#'
#' Display the Graphical User Interface (GUI) of FlowStats
#'
#' @description
#' Calling this functions will show the GUI of FlowStats
#'
#' @return GUI of FlowStats

#' @examples
#'
#'\donttest{
#' showFlowStats()
#' }
#'
#'@importFrom shiny runApp
#'
#' @export
#'
showFlowStats <- function(){
  # Path to FlowStats app
  app_path <- system.file("FlowStats", package = "FlowStats")

  # Run app
  shiny::runApp(app_path, display.mode = "normal")
}








