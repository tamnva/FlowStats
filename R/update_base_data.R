#' Update existing meterological data with near-real-time (1-day delay)
#' meterological data from Deutsche Wetterdienst
#'
#' @param old_data_file character: path to old data file.
#'
#' @param basins shape file object of basins
#'
#' @param years integer vector, list of all years that users want to download.
#' Each file of the HYDRAS data contains 1 calendar year. Since the base data
#' already have simulated streamflow from 1980 to 2025. Therefore, users only
#' need to download data from 2025
#'
#' @param data_dir path to save downloaded data, if not given, the temporary path
#' will be used
#'
#' @return new data will be added to the old data. IMPORTANT: The old_data_file
#' will be replace

#' @examples
#'
#'\dontrun{
#' old_data_file <- "your_path/time_series.csv"
#' basins <- read_sf("your_path/de_basins.shp")
#' update_data(old_data_file, basins, years = NA, data_dir = NA)
#'
#'
#' }
#' @export
#' @importFrom stats time
#'
update_data <- function(old_data_file, basins, years = NA, data_dir = NA){

  message("Downloading historical data")
  historical_data <- get_meterological_data(years = years, data_dir = data_dir)

  message("Extrating historical data for basins")
  historical_data_basin_extract <- grid_to_basin(historical_data[["years"]],
                                                 historical_data[["data_dir"]],
                                                 basins)
  message("Reading historical data")

  old_data <- readr::read_csv(
    old_data_file,
    col_types = readr::cols(
      time = readr::col_datetime(format = "%Y-%m-%d %H:%M")
      )
  )

  old_data <- subset(old_data, time < historical_data_basin_extract[["date"]][1])

  # Unique gauges
  gauge_id <- unique(old_data$object_id)


  message("Updating data")
  for (gauge in gauge_id){

    message(paste("Updating gauge:", gauge))
    col <- which(colnames(historical_data_basin_extract[["pr"]]) == gauge)

    new_data <- tibble::tibble(
      object_id = gauge,
      time = historical_data_basin_extract[["date"]],
      pr = historical_data_basin_extract[["pr"]][,col],
      tasmin = historical_data_basin_extract[["tasmin"]][,col],
      tasmax = historical_data_basin_extract[["tasmax"]][,col],
      hurs  = historical_data_basin_extract[["hurs"]][,col],
      discharge_spec_obs = NA
      )


    gauge_data <- old_data %>%
      dplyr::filter(object_id == gauge) %>%
      dplyr::bind_rows(new_data) %>%
      dplyr::mutate(time = paste0(time, " 00:00"),
                    pr = round(pr, 2),
                    tasmin  = round(tasmin, 2),
                    tasmax   = round(tasmax, 2),
                    hurs  = round(hurs, 2),
                    discharge_spec_obs = round(discharge_spec_obs, 2),)

    if (gauge == gauge_id[1]){
      data.table::fwrite(gauge_data, file = old_data_file, append = FALSE,
                         quote = FALSE, row.names = FALSE, col.names = TRUE)
    } else {
      data.table::fwrite(gauge_data, file = old_data_file, append = TRUE,
                         quote = FALSE, row.names = FALSE, col.names = FALSE)
    }
  }
}
