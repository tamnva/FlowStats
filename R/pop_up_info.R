
#' Pop up text when clicking to the gauge on leaflet map
#'
#' @param gauge_name character, name of the gauge
#'
#' @param gauge_id character, gauge id named after the CAMELS-DE data
#'
#' @param NSE numeric, the performance of the LSTM for the test period for
#' this gauge
#'
#' @param are_skm numeric, drainage area of this gauge in square kilometer
#'
#' @return information about the gauged, organized in a nice way

#' @examples
#'
#' pop_up_info(gauge_name = "Achstetten", gauge_id = "DE110040",
#'             NSE = 0.882, are_skm = 273.0)
#'
#' @export
#'
pop_up_info <- function(gauge_name, gauge_id, NSE, are_skm){
  pop_up_text <- paste(
    sep = "<br/>",
    "<strong><span style='color: #008000;'>Gauge Infomation</span></strong>",
    paste0("Name = ", gauge_name, " (", gauge_id, ")"),
    paste0("NSE = ", round(NSE,2)),
    paste0("Drainage area = ", round(are_skm,0), " (km<sup>2</sup>)"))
  return(pop_up_text)
}
