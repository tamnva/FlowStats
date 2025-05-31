#' Get historical most recent daily gridded HYDRAS hydrological data (including
#' precipitation, tempearture min and max, humidity) from the Deutsche
#' Wetterdienst
#'
#' @param year integer vector, list of all years that users want to download.
#' Each file of the HYDRAS data contains 1 calendar year. Since the base data
#' already have simulated streamflow up to 2025. Therefore, users should only
#' download data from since 2024 (model need warm-up time so only simulated data
#' after 2024 are used and updated to the base data. By default, if year is not
#' given, the default year is from c(2024:year(Sys.Date()))
#'
#' @param data_dir path to save downloaded data, if not given, the temporary path
#' will be used
#'
#' @return a list object, containing information about the downloaded files,
#' years, and path to the downloaded data

#' @examples
#'
#' download_dwd <- get_de_hist(years=NA, data_dir=NA)
#'
#' @export
#'

grid_to_basins <- function(years, data_dir, basins){

  file_name <- "_hyras_1_year_v6-0_de.nc"
  file_name_prefix <- c("pr", "tasmin", "tasmax", "hurs")

  output <- list()

  for (yr in years){

    for (i in c(1:4)){

      file_name_update <- file.path(
        data_dir, gsub("year", yr, paste0(file_name_prefix[i], file_name)))

      # Get data
      data <- rast(file_name_update)

      if (yr == years[1]) basins <- st_transform(basins, crs(data))

      # Extract data
      data <- exact_extract(data, basins, fun = 'mean')

      data <- t(data.frame(data))
      colnames(data) <- basins$gauge_id

      if (yr == years[1]) {
        output[[file_name_prefix[i]]] <- data

      } else {
        output[[file_name_prefix[i]]] <- rbind(output[[file_name_prefix[i]]], data)
      }
    }
  }

  output[["date"]] <- seq.Date(as.Date(paste0(years[1], "-01-01")),
                            as.Date(paste0(years[1], "-01-01")) +
                              nrow(output[[file_name_prefix[i]]]) - 1,
                            by = "days")


  return(output)
}



#------------------------------------------------------------------------------#
#                              Update data                                     #
#------------------------------------------------------------------------------#
update_data <- function(old_data_file, basins){

  message("Downloading historical data")
  historical_data <- get_de_hist(years=NA, data_dir=NA)

  message("Extrating historical data for basins")
  historical_data_basin_extract <- extract_de_hist(historical_data[["years"]],
                                                   historical_data[["data_dir"]],
                                                   basins)
  message("Reading historical data")
  old_data <- fread(old_data_file)

  library(readr)
  old_data <- read_csv(old_data_file,
                 col_types = cols(time = col_datetime(format = "%Y-%m-%d %H:%M"))
                 )
  old_data <- subset(old_data, time < historical_data_basin_extract[["date"]][1])

  # Unique gauges
  gauge_id <- unique(old_data$object_id)


  message("Updating data")
  for (gauge in gauge_id){

    message(paste("Updating gauge:", gauge))
    col <- which(colnames(historical_data_basin_extract[["pr"]]) == gauge)

    new_data <- tibble(object_id = gauge,
           time = historical_data_basin_extract[["date"]],
           pr = historical_data_basin_extract[["pr"]][,col],
           tasmin = historical_data_basin_extract[["tasmin"]][,col],
           tasmax = historical_data_basin_extract[["tasmax"]][,col],
           hurs  = historical_data_basin_extract[["hurs"]][,col],
           discharge_spec_obs = NA)


    temp <- old_data %>%
      filter(object_id == gauge) %>%
      bind_rows(new_data) %>%
      mutate(time = paste0(time, " 00:00"),
             pr = round(pr, 2),
             tasmin  = round(tasmin, 2),
             tasmax   = round(tasmax, 2),
             hurs  = round(hurs, 2),
             discharge_spec_obs = round(discharge_spec_obs, 2),)

    if (gauge == gauge_id[1]){
      fwrite(temp, file = old_data_file, append = FALSE, quote = FALSE,
             row.names = FALSE, col.names = TRUE)
    } else {
      fwrite(temp, file = old_data_file, append = TRUE, quote = FALSE,
             row.names = FALSE, col.names = FALSE)
      }
  }
}
