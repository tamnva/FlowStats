#' Get  most recent daily gridded HYDRAS hydrological data (including
#' precipitation, tempearture min and max, humidity) from the Deutsche
#' Wetterdienst
#'
#' @param years integer vector, list of all years that users want to download.
#' Each file of the HYDRAS data contains 1 calendar year. Since the base data
#' already have simulated streamflow from 1980 to 2025. Therefore, users only
#' need to download data from 2025
#'
#' @param data_dir path to save downloaded data, if not given, the temporary path
#' will be used
#'
#' @return a list object, containing information about the downloaded files,
#' years, and path to the downloaded data

#' @examples
#'
#'\dontrun{
#' meterological_data <- get_meterological_data(years = NA, data_dir = NA)
#' }
#'
#' @export
#'

# devtools::document()
get_meterological_data <- function(years = NA, data_dir = NA){

  options(timeout=3600)

  if (is.na(years)) years <- c(2024:lubridate::year(Sys.Date()))
  if (is.na(data_dir)) {
    data_dir <- tempdir()
    message(paste0("Downloaded data is saved to: ", data_dir))
  }

  base_link <- paste0("https://opendata.dwd.de/climate_environment/",
                      "CDC/grids_germany/daily/hyras_de")
  file_name <- "_hyras_1_year_v6-0_de.nc"

  base_link_suffix <- c("precipitation", "air_temperature_min",
                        "air_temperature_max", "humidity")
  file_name_prefix <- c("pr", "tasmin", "tasmax", "hurs")

  for (yr in years){
    for (i in 1:4){
      base_link_update <- file.path(base_link, base_link_suffix[i])
      file_name_update <- paste0(file_name_prefix[i], file_name)
      file_name_update <- gsub("year", yr, file_name_update)

      utils::download.file(file.path(base_link_update, file_name_update),
                    file.path(data_dir, file_name_update), mode="wb")

    }
  }

  output <- list()
  output[["years"]] <- years
  output[["data_dir"]] <- data_dir

  return(output)
}
