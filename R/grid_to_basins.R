#' Extract gridded HYDRAS hydrological data and aggregate at the basins levels
#'
#' @param years integer vector, list of all years of the downloaded HYDRAS data
#'
#' @param data_dir path to save extracted data
#'
#' @param basins shape file object of basins
#'
#' @return a list object, containing extract data for each basin

#' @examples
#'
#'\donttest{
#' basin_data <- grid_to_basin(years = c(2024:2025), data_dir = tempdir(),
#'                            basins)
#' }

#'
#' @export
#'

grid_to_basin <- function(years, data_dir, basins){

  file_name <- "_hyras_1_year_v6-0_de.nc"
  file_name_prefix <- c("pr", "tasmin", "tasmax", "hurs")

  output <- list()

  for (yr in years){

    for (i in c(1:4)){

      file_name_update <- file.path(
        data_dir, gsub("year", yr, paste0(file_name_prefix[i], file_name)))

      # Get data
      data <- terra::rast(file_name_update)

      if (yr == years[1]) basins <- sf::st_transform(basins, sf::st_crs(data))

      # Extract data
      data <- exactextractr::exact_extract(data, basins, fun = 'mean')

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
