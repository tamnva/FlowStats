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
#' FlowStats()
#' }
#'
#'@importFrom shiny runApp
#'
#' @export
#'
FlowStatsGUI <- function(){
  # Path to FlowStats app
  appDir <- system.file("FlowStats", package = "FlowStats")

  # Run app
  shiny::runApp(appDir, display.mode = "normal")
}








