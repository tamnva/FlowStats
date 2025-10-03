#' Extracting meteorological data downloaded from ICON-D2 and ICON-EU to basin level
#'
#' @param data_dir directory of downloaded data from ICON-D2 and ICON-EU files
#'
#' @param  forecast_metadata dataframe object of three columns, variable name,
#' predicted time (UTC), and downloaded file names
#'
#' @param  basins basin shape object
#'
#' @return a data frame of forecasted meterological data at the forecast time
#' step and for different basins
#'
#' @examples
#'\dontrun{
#'
#' data_dir <- tempdir()
#' basins <- sf::read_sf(file.path(.libPaths(),"FlowStats", "FlowStats",
#'                                 "data", "de_basins.shp"))
#' forecast_metadata <- get_forecast_metero(data_dir , variables)
#' extract_de_forecast(data_dir, forecast_metadata, basins)
#'
#' }
#' @export
#'
extract_de_forecast <- function(data_dir, forecast_metadata, basins){

  for (ifile in 1:length(forecast_metadata$file)){

    data <- rdwd::readDWD(file.path(data_dir, forecast_metadata$file[ifile]))

    # If precipitation, only take the last value
    if(forecast_metadata$variable[ifile] == "tot_prec") {
      data <- terra::subset(data, dim(data)[3])
    }

    if (ifile == 1){
      dates <- terra::time(data)
    } else {
      dates <- c(dates, terra::time(data))
    }


    # Get basin average value
    basins <- sf::st_transform(basins, sf::st_crs(data))
    data_mean <- exactextractr::exact_extract(data, basins, fun = 'mean')
    names(data_mean) <- basins$gauge_id

    # Convert to data frame
    if (ifile == 1){
      result <- tibble::as_tibble_row(data_mean)
    } else {
      result <- rbind(result, data_mean)
    }
  }

  result <- result %>%
    tibble::add_column(forecast_hour = forecast_metadata$forecast_hour, .before = 1) %>%
    tibble::add_column(dates = dates, .before = 1) %>%
    tibble::add_column(variable = forecast_metadata$variable, .before = 1)

  return(result)
}


