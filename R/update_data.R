
#' Calculate streamflow statistic
#'
#' @param env_path path to the python virtual envionment to run the LSTM model.
#' The python virtual environment can be created using annaconda with the
#' environment file (environment.yml, link to this file is in the example)
#' in the package folder. The LSTM model was constructed using the HydroEcoLSTM
#' package (https://github.com/tamnva/hydroecolstm).
#'
#' @param basins basin shape file from the CAMEL-DE dataset (Loritz et al., 2024)
#'
#' @param forecast logical variable, TRUE meaning that forecast meteorological
#' data will be downloaded, FALSE if use only want to update historical
#' meterological data
#'
#' @return the base data will be updated
#'
#' @examples
#'
#'\dontrun{
#' # The path of Python environment for running the LSTM model
#' # Here is the path to the environment.yml file using annaconda
#' # More information can be found at https://github.com/tamnva/hydroecolstm
#' file.path(.libPaths(), "FlowStats", "FlowStats","data",
#'                             "lstm_data", "environment.yml")
#'
#' env_path <- "C:/Users/nguyenta/AppData/Local/anaconda3/envs/hydro"
#'
#' # Read the basin shape file
#' basins <- sf::read_sf(file.path(.libPaths(),"FlowStats", "FlowStats",
#'                                "data", "de_basins.shp"))
#'
#' # Update the data
#' update_data(env_path, basins, forecast = TRUE)
#'
#' # Here is the updated data file
#' Q_data <- readRDS(file.path(.libPaths(), "FlowStats", "FlowStats","data",
#'                             "lstm_data", "de_sim_discharge.rds"))
#'
#' }
#'
#' @export
#' @importFrom rlang :=

update_data <- function(env_path, basins, forecast){

  lstm_data_dir <- file.path(.libPaths(), "FlowStats",
                             "FlowStats","data", "lstm_data")

  # Getting data from DWD
  message("Downloading meterological data from DWD...")
  historical_data <- get_historical_metero()

  message("Extrating historical data for basins")
  historical_data <- grid_to_basin(historical_data[["years"]],
                                   historical_data[["data_dir"]],
                                   basins)

  message("Combining all meterological data to one file...")
  for(var in c("pr", "tasmin", "tasmax", "hurs")){

    if(!identical(colnames(historical_data[["pr"]]),
                  colnames(historical_data[[var]]))){
      stop("column names of the meterological data are not identical")
    }

    temp <- tibble::as_tibble(historical_data[[var]]) %>%
      dplyr::mutate(time = historical_data[["date"]]) %>%
      tidyr::pivot_longer(cols = tidyselect::starts_with("DE"),
                          names_to = "object_id",
                          values_to = "value") %>%
      dplyr::arrange(object_id, time)

    if (var == "pr"){
      time_series <- tibble::tibble(object_id = temp$object_id,
                                    time = paste0(temp$time, " 00:00"),
                                    !!var := round(temp$value, 2))
    } else {
      time_series <- time_series %>%
        dplyr::mutate(!!var := round(temp$value, 2))
    }

  }

  time_series$discharge_spec_obs <- NA

  message("Downloading forecast meterological data...")

  if (forecast){
    data_dir <- tempdir()
    variables <- c("t_2m", "relhum_2m", "tot_prec")

    forecast_metadata  <- get_forecast_metero(data_dir, variables)
    forecast_data <- extract_de_forecast(data_dir, forecast_metadata, basins)
    forecast_data_agg <- aggregate_forecast_metero(forecast_data)

    forecast_data_agg <- forecast_data_agg %>%
      dplyr::rename(time = day)%>%
      dplyr::mutate(variable = dplyr::case_when(
        variable == "t_2m_min" ~ "tasmin",
        variable == "t_2m_max" ~ "tasmax",
        variable == "tot_prec" ~ "pr",
        variable == "relhum_2m" ~ "hurs")) %>%
      tidyr::pivot_longer(
        cols = tidyselect::starts_with("DE"),
        names_to = "object_id",
        values_to = "value") %>%
      dplyr::group_by(object_id) %>%
      tidyr::pivot_wider(
        names_from = variable,
        values_from = value
      )  %>%
      dplyr::relocate(c("object_id", "time", "pr", "tasmin", "tasmax", "hurs")) %>%
      dplyr::mutate(time = as.Date(time),
             discharge_spec_obs = NA) %>%
      dplyr::arrange(dplyr::desc(object_id), time)

    time_series <- time_series %>%
      dplyr::mutate(time = as.Date(substr(time,1,10)))

    # First predicted date = last historical date + 1
    if (min(forecast_data_agg$time) - max(time_series$time) == 1){
      time_series <- time_series %>%
        dplyr::bind_rows(forecast_data_agg) %>%
        dplyr::arrange(object_id, time)

      # First predicted date = last historical date
    } else if(min(forecast_data_agg$time) - max(time_series$time) == 0) {
      time_series <- time_series %>%
        dplyr::filter(time != max(time_series$time))
    } else {
      message("Gap in historical and predicted data > 1 days.
              Only use historical data")
    }

    time_series <- time_series %>%
      dplyr::mutate(time = paste0(time, " 00:00"))
  }

  # Save data
  data.table::fwrite(time_series %>%
                       dplyr::rename(id = object_id),
                     file.path(lstm_data_dir, "time_series.csv"),
                     quote = FALSE, row.names = FALSE, col.names = TRUE)

  #----------------------------------------------------------------------------#
  #                          Run the LSTM model                                #
  #----------------------------------------------------------------------------#
  message("Modifing the Python script...")
  # Change file path to the python script to the data path
  run_lstm_script <-  readLines(file.path(lstm_data_dir,"main.py"), -1L)
  run_lstm_script[10] <- paste0("lstm_data_dir = '", lstm_data_dir, "'")
  writeLines(run_lstm_script, file.path(lstm_data_dir,"main.py"))

  message("Running the LSTM model...")
  env_path <- file.path(env_path, "python.exe")
  system(paste0(env_path, " ", file.path(lstm_data_dir, "main.py")))

  #----------------------------------------------------------------------------#
  #                         Merge new data to old data                         #
  #----------------------------------------------------------------------------#
  message("Updating the base discharge dataset with new data...")
  de_sim_discharge <- readRDS(
    file.path(lstm_data_dir, "de_sim_discharge.rds")
    ) %>%
    tidyr::pivot_longer(cols = -date,
                        values_to = "q_mm_day",
                        names_to = "gauge_id") %>%
    dplyr::mutate(year = lubridate::year(date),
                  day_of_year = lubridate::yday(date)) %>%
    dplyr::arrange(gauge_id, date)

  de_sim_discharge_update <- data.table::fread(
    file.path(lstm_data_dir, "de_sim_discharge_update.csv")
  ) %>%
    tibble::as_tibble() %>%
    dplyr::rename(gauge_id = id,
                  date = time,
                  q_mm_day = discharge_spec_obs) %>%
    dplyr::mutate(date = as.Date(date),
                  year = lubridate::year(date),
                  day_of_year = lubridate::yday(date)) %>%
    dplyr::filter(date >= as.Date("2025-01-01"))

  de_sim_discharge <- de_sim_discharge %>%
    dplyr::filter(date < de_sim_discharge_update$date[1]) %>%
    dplyr::bind_rows(de_sim_discharge_update) %>%
    dplyr::arrange(gauge_id, date) %>%
    dplyr::mutate(q_mm_day = pmax(q_mm_day, 0))

  saveRDS(de_sim_discharge %>%
            dplyr::select(c(gauge_id, date, q_mm_day)) %>%
            tidyr::pivot_wider(id_cols = date,
                        values_from = q_mm_day,
                        names_from = gauge_id),
          file.path(lstm_data_dir, "de_sim_discharge.rds"))

  message("The streamflow data was sucessfull updated")
}
