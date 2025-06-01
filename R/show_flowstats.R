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
show_flowstats <- function(){
  # Path to FlowStats app
  app_path <- system.file("FlowStats", package = "FlowStats")

  # Run app
  shiny::runApp(app_path, display.mode = "normal")
}








