
#' Aggregate forecast meterological data from ICON-D2 and ICON-DE at forecast
#' time step to daily
#'
#' @param forecast_data data frame of raw forecast meterological data with
#' forecasting time steps for different variables
#'
#' @return forecast_data aggregated to daily time steps
#'
#' @examples
#'
#'\dontrun{
#' # Save downloaded data in this folder
#' data_dir <- "C:/Users/nguyenta/Documents/test"
#'
#' Download for these variables
#' variables <- c("t_2m", "relhum_2m", "tot_prec")
#'
#' # Now download the data (get_forecast_metero function in the R folder)
#' forecast_metadata  <- get_forecast_metero(data_dir, variables)
#'
#' # Get basin shape file
#' basins <- sf::read_sf(file.path(.libPaths(),"FlowStats", "FlowStats","data",
#'                                "de_basins.shp"))
#'
#' forecast_data <- extract_de_forecast(data_dir, forecast_metadata, basins)
#'
#' forecast_data_agg <- aggregate_forecast_metero(forecast_data)
#'
#' }
#' @export
#'
#'

aggregate_forecast_metero <- function(forecast_data){

  # Get the station names
  cols <- tail(colnames(forecast_data), -3)

  # Get forecast variable name
  variables <- unique(forecast_data$variable)

  # Aggregate to daily
  counter <- 0
  for (var in variables){
    counter <- counter + 1

    if (var == "t_2m"){
      temp <- forecast_data %>%
        dplyr::filter(variable == var) %>%
        dplyr::mutate(day = dplyr::case_when(
          forecast_hour <= 24 ~ 1,
          forecast_hour <= 48 ~ 2,
          forecast_hour <= 72 ~ 3,
          forecast_hour <= 96 ~ 4,
          forecast_hour <= 120 ~ 5
        )) %>%
        dplyr::select(!c("variable", "dates", "forecast_hour"))

      t_min <- temp %>%
        dplyr::group_by(day) %>%
        dplyr::summarise(dplyr::across(tidyselect::all_of(cols), min)) %>%
        tibble::add_column(variable = "t_2m_min", .before = 1)

      t_max <- temp %>%
        dplyr::group_by(day) %>%
        dplyr::summarise(dplyr::across(tidyselect::all_of(cols), max)) %>%
        tibble::add_column(variable = "t_2m_max", .before = 1)

      if (counter == 1) {
        daily <- rbind(t_min, t_max)
      } else {
        daily <- rbind(daily, rbind(t_min, t_max))
      }

    } else if(var == "tot_prec") {
      temp <- forecast_data %>%
        dplyr::filter(variable == "tot_prec") %>%
        dplyr::filter(forecast_hour %in% c(24, 48,49, 72, 96, 120)) %>%
        dplyr::select(!c("variable", "dates", "forecast_hour"))

      # daily precipitation from ICON-D2 data
      temp[2,] <- temp[2,] - temp[1,]

      # daily precipitation from ICON-EU data
      temp[4,] <- temp[4,] - temp[3,]
      temp[5,] <- temp[5,] - temp[3,]
      temp[6,] <- temp[6,] - temp[3,]

      # Remove stating of the third day
      temp <- temp[-c(3), ]

      # Make sure tot_pre is positive
      temp[temp < 0] <- 0

      # Add additional info
      temp <- temp %>%
        tibble::add_column(day = c(1:5), .before = 1) %>%
        tibble::add_column(variable = "tot_prec", .before = 1)

      if (counter == 1) {
        daily <- temp
      } else {
        daily <- rbind(daily, temp)
      }

    } else {
      temp <- forecast_data %>%
        dplyr::filter(variable == var) %>%
        dplyr::mutate(day = dplyr::case_when(
          forecast_hour <= 24 ~ 1,
          forecast_hour <= 48 ~ 2,
          forecast_hour <= 72 ~ 3,
          forecast_hour <= 96 ~ 4,
          forecast_hour <= 120 ~ 5
        )) %>%
        dplyr::select(!c("variable", "dates", "forecast_hour"))

      temp <- temp %>%
        dplyr::group_by(day) %>%
        dplyr::summarise(dplyr::across(tidyselect::all_of(cols), mean)) %>%
        tibble::add_column(variable = var, .before = 1)

      if (counter == 1) {
        daily <- temp
      } else {
        daily <- rbind(daily, temp)
      }

    }
  }

  daily$day <- as.Date(forecast_data$dates[1]) + daily$day - 1

  return(daily)
}
