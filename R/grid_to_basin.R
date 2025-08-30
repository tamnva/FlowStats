#' Extract gridded HYDRAS hydrological data and aggregate at the basins levels
#'
#' @param years integer vector, list of all years of the downloaded HYDRAS data
#'
#' @param data_dir path to save extracted data
#'
#' @param basins basin shape file from the CAMEL-DE dataset (Loritz et al., 2024)
#'
#' @return a list object, containing extract data for each basin

#' @examples
#'
#'\dontrun{
#'
#' basins <- sf::read_sf(file.path(.libPaths(),"FlowStats", "FlowStats",
#'                                "data", "de_basins.shp"))
#'
#' basin_data <- grid_to_basin(years = c(2024:2025), data_dir = tempdir(),
#'                             basins = basins)
#' }
#'
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

  nrows <- min(nrow(output[["pr"]]),
               nrow(output[["tasmin"]]),
               nrow(output[["tasmax"]]),
               nrow(output[["hurs"]])
               )

  for(var in file_name_prefix){
    output[[var]] <- output[[var]][1:nrows,]
  }

  output[["date"]] <- seq.Date(as.Date(paste0(years[1], "-01-01")),
                            as.Date(paste0(years[1], "-01-01")) +
                              nrow(output[[file_name_prefix[i]]]) - 1,
                            by = "days")


  return(output)
}
