#' Download ICON-D2 (1-48 hours) and ICON-EU (49-120 hours) meteorological data
#'
#' @param data_dir directory to save downloaded data from ICON-D2 and ICON-EU
#' files
#'
#' @param  variables variable names, could be one or multiple variables (vector),
#' for example c("t_2m", "relhum_2m", "tot_prec"). A list of all variables names
#' can be found here https://opendata.dwd.de/weather/nwp/icon-d2/grib/00/
#'
#'
#' @return a dataframe object of three columns, variable name, predicted time (UTC),
#' and downloaded file names

#' @examples
#'\dontrun{
#'
#' data_dir <- tempdir()
#' variables <- c("t_2m", "relhum_2m", "tot_prec")
#' forecast_files <- get_forecast_metero(data_dir , variables)
#'
#' }
#' @export
#'

get_forecast_metero <- function(data_dir , variables){

  # Model to get prediction meterological data from 0 to 120 hours ahead
  models <- c("icon-d2", "icon-eu")

  # Modelling time step, from ICON-D2 and ICON-EU
  icon_d2_time <- sprintf("%03d", seq(0, 48, 1))
  icon_eu_time  <- sprintf("%03d", c(seq(49, 77, 1), seq(78, 120,3)))

  # Base url from ICON-D2 and ICON-EU
  icon_d2_url <- paste0("https://opendata.dwd.de/weather/nwp/model/grib/00/",
                        "variable/model_germany_regular-lat-lon_single-level_",
                        format(Sys.Date(), "%Y%m%d"),
                        "00_time_2d_variable.grib2.bz2")

  icon_eu_url <- paste0("https://opendata.dwd.de/weather/nwp/model/grib/00/",
                        "variable/model_europe_regular-lat-lon_single-level_",
                        format(Sys.Date(), "%Y%m%d"),
                        "00_time_VARIABLE.grib2.bz2")

  # Save time step, file name, variable name for outputs
  itime <- c()
  ifile <- c()
  var <- c()


  # Now get the data
  for (variable in variables){
    for (model in models){
      if (model == "icon-d2"){
        for (time in icon_d2_time){
          download_url <- gsub("model", model, icon_d2_url)
          download_url <- gsub("variable", variable, download_url)
          download_url <- gsub("time", time, download_url)
          utils::download.file(download_url,
                        file.path(data_dir, basename(download_url)),
                        mode="wb")

          itime <- c(itime, as.integer(time))
          ifile <- c(ifile, basename(download_url))
          var <- c(var, variable)
        }
      } else {
        for (time in icon_eu_time){
          download_url <- gsub("model", model, icon_eu_url)
          download_url <- gsub("variable", variable, download_url)
          download_url <- gsub("VARIABLE", toupper(variable), download_url)
          download_url <- gsub("time", time, download_url)
          utils::download.file(download_url,
                        file.path(data_dir, basename(download_url)),
                        mode="wb")

          itime <- c(itime, as.integer(time))
          ifile <- c(ifile, basename(download_url))
          var <- c(var, variable)
        }
      }
    }
  }

  return(data.frame(variable = var, forecast_hour = itime, file = ifile))
}
