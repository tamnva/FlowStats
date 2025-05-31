#' Extract gridded HYDRAS hydrological data and aggregate at the basins levels
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
