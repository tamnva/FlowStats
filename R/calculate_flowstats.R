#' Calculate streamflow statistic
#'
#' @param Q_input dataframe object, simulated or observed streamflow data, must
#' contains the following columns "gauge_id", "date", "q_mm_day"
#'
#' @param n_day_mean smoothing the streamflow data using moving window, so the
#' streamflow of day t is the average streamflow from day t-n-1 to t
#'
#' @param select_period date vector with format "%Y-%m-%d", starting and ending
#' date of of the period for calculating streamflow statistic
#'
#' @param stat_function character, can be one of the following options
#' 'Q_min (selected period)' for minimum, 'Q_mean (selected period)' to calculate
#' mean, 'Q_max (selected period)' to calculate maximum, or 'Q_last_day' to just
#' get Q latest day in the Q_input dataset
#'
#' @param gui logical variable, by default is TRUE to if this function is called
#' inside the shiny serve to display the processing time. Otherwise when users
#' do not use the GUI, just set to FALSE
#'
#' @return a dataframe object, containing streamflow percentiles of the gaugeid

#' @examples
#'
#'\dontrun{
#' # Read data from FlowStats package
#' Q_input <- readRDS(file.path(.libPaths(),"FlowStats", "FlowStats",
#'                             "data", "lstm_data", "de_sim_discharge.rds"))
#'
#'
#' # Period for calculating streamflow statistic
#' select_period <- c(as.Date("2025-05-01"), as.Date("2025-05-31"))
#' n_day_mean <- 10
#' stat_function <- "Mean"
#' gui <- FALSE
#'
#' # Get flow percentiles
#' percentile <- calculate_flowstats(Q_input, select_period,
#'                                   n_day_mean, stat_function, gui)
#'
#' }
#'
#' @export
#'
#' @importFrom stats ecdf

calculate_flowstats <- function(Q_input, select_period , n_day_mean,
                                stat_function, gui, flood_percentile = FALSE){

  # Assign stat_function to input function
  if (!flood_percentile){
    if (stat_function == "Min") {
      stat_function = min
    } else if(stat_function == "Mean") {
      stat_function = mean
    } else if(stat_function == "Max") {
      stat_function = max
    } else {
      stop("Unknown streamflow statistics name: ", stat_function)
    }
  } else {

    if (select_period[2] != select_period[1]){

      message(paste0("For flood percentile, select a single date ",
           "starting and ending dates of the selected period must be ",
           "identical. The starting & ending dates are automatically ",
           "adjusted to ", select_period[1]))

      select_period[2] != select_period[1]

    } else {
      message(paste0("Note: For flood percentile, inputs 'n_day_mean' and ",
                     "'stat_function' are not required/used"))
    }
  }

  # Calculate streamflow statistics
  if ((n_day_mean > 1) & !flood_percentile) {
    Q_input <- Q_input %>%
      dplyr::group_by(gauge_id)  %>%
      dplyr::mutate(q_mm_day = data.table::frollmean(q_mm_day, n_day_mean))
  }

  # Calculate streamflow statistics
  if (!flood_percentile){
    Q_input <- Q_input %>%
      dplyr::filter(day_of_year >= lubridate::yday(select_period[1]),
                    day_of_year <= lubridate::yday(select_period[2])) %>%
      tidyr::drop_na() %>%
      dplyr::group_by(gauge_id, year) %>%
      dplyr::summarise(q_mm_day = stat_function(q_mm_day),
                       .groups = 'drop')
  }

  # Calculate percentile
  if (flood_percentile){
    percentiles <- Q_input %>%
      dplyr::select(gauge_id, q_mm_day, year, date) %>%
      dplyr::group_by(gauge_id) %>%
      dplyr::mutate(Q_min = min(q_mm_day),
                    Q_max = max(q_mm_day)) %>%
      dplyr::filter(date == select_period[1]) %>%
      dplyr::select(!date) %>%
      dplyr::mutate(percentiles = dplyr::case_when(
        q_mm_day == Q_min ~ 0.0,
        q_mm_day == Q_max ~ 100.0
      ))
  } else {
    percentiles <- Q_input %>%
      dplyr::select(gauge_id, q_mm_day, year) %>%
      dplyr::group_by(gauge_id) %>%
      dplyr::mutate(Q_min = min(q_mm_day),
                    Q_max = max(q_mm_day)) %>%
      dplyr::filter(year == lubridate::year(select_period[1])) %>%
      dplyr::mutate(percentiles = dplyr::case_when(
        q_mm_day == Q_min ~ 0.0,
        q_mm_day == Q_max ~ 100.0
      ))
  }

  n_gauges <- nrow(percentiles)
  pb <- txtProgressBar(min = 0, max = n_gauges, style = 3)

  if (gui){
    shiny::withProgress(message = 'Calculating streamflow statistics', value = 0, {
      for (i in 1:n_gauges){

        setTxtProgressBar(pb, i)

        shiny::incProgress(1/n_gauges, detail = paste0(round(i*100/n_gauges,0),"%"))

        if (is.na(percentiles$percentiles[i])){
          temp <- Q_input %>%
            dplyr::filter(gauge_id == percentiles$gauge_id[i]) %>%
            dplyr::summarise(percentiles = 100*ecdf(q_mm_day)(percentiles$q_mm_day[i]))
          percentiles$percentiles[i] <- temp$percentiles
        }

      }})
  } else {
    for (i in 1:n_gauges){

      setTxtProgressBar(pb, i)

      if (is.na(percentiles$percentiles[i])){
        temp <- Q_input %>%
          dplyr::filter(gauge_id == percentiles$gauge_id[i]) %>%
          dplyr::summarise(percentiles = 100*ecdf(q_mm_day)(percentiles$q_mm_day[i]))
        percentiles$percentiles[i] <- temp$percentiles
      }
    }
  }

  close(pb)

  return(percentiles)
}
