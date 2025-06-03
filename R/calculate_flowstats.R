#' Calculate streamflow statistic
#'
#' @param Q_input dataframe object, simulated or observed streamflow data, must
#' contains the following columns "gauge_id", "date", "Q_cms"
#'
#' @param period date vector with format "%Y-%m-%d", starting and ending date of
#' of the period for calculating streamflow statistic
#'
#' @param gaugeid character vector, list of gauge_id which users want to
#' calculate streamflow statistics
#'
#' @param fun character, can be one of the following options
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
#'\dontrun{
#' # Read data from FlowStats package
#' Q_input <- readRDS(file.path(.libPaths(),"FlowStats", "FlowStats",
#'                             "data", "lstm_data", "de_sim_discharge.rds"))
#'
#'
#' # Period for calculating streamflow statistic
#' period <- c(as.Date("2025-05-01"), as.Date("2025-05-31"))
#'
#' # Gauge id
#' gaugeid <- Q_input$gauge_id[1]
#'
#' # How to calculate Q
#' plot_type <- "Daily (by year)"
#'
#' # Get flow percentiles
#' flow_stats <- calculate_flowstats(Q_input, period, gaugeid,
#'                                   fun = "Q_min (selected period)",
#'                                   gui = FALSE)
#' }
#' @export
#'
#' @importFrom stats ecdf quantile
#' @importFrom utils tail

calculate_flowstats <- function(Q_input, period, gaugeid, fun, gui = TRUE){

  if(fun == "Q_min (selected period)"){
    assign("agg_func", min)
  } else if (fun == "Q_mean (selected period)") {
    assign("agg_func", mean)
  } else if (fun == "Q_max (selected period)") {
    assign("agg_func", max)
  } else {
    assign("agg_func", sum)
  }

  if(fun == "Q_last_day"){
    Q_input_aggregate <- Q_input %>% dplyr::rename(`Q_cms_aggregate` = Q_cms)
    Q_input_year <- Q_input_aggregate %>% dplyr::filter(date == period[1])

  } else {
    Q_input_aggregate <- Q_input %>%
      dplyr::mutate(day_of_year = lubridate::yday(date),
                    year = lubridate::year(date)) %>%
      dplyr::filter(day_of_year >= lubridate::yday(period[1]),
                    day_of_year <= lubridate::yday(period[2])) %>%
      dplyr::group_by(gauge_id, year)  %>%
      dplyr::summarise(Q_cms_aggregate = agg_func(Q_cms),
                       .groups = 'drop')

    Q_input_year <- Q_input_aggregate %>%
      dplyr::filter(year == lubridate::year(period[1]))
  }

  percentiles <- tibble::tibble(gauge_id = gaugeid, percentiles = NA)
  ngauges <- length(gaugeid)


  if (gui){
    shiny::withProgress(message = 'Calculating streamflow statistics', value = 0, {

      for (i in 1:ngauges){

        shiny::incProgress(1/ngauges, detail = paste0(round(i*100/ngauges,0),"%"))

        iloc <- which(Q_input_year$gauge_id == gaugeid[i])

        temp <- Q_input_aggregate %>%
          dplyr::filter(gauge_id == gaugeid[i]) %>%
          dplyr::summarise(percentiles = 100*ecdf(Q_cms_aggregate)(
            Q_input_year$Q_cms_aggregate[iloc]))

        min_max <- Q_input_aggregate %>%
          dplyr::filter(gauge_id == gaugeid[i]) %>%
          dplyr::summarise(min = min(Q_cms_aggregate),
                           max = max(Q_cms_aggregate))

        if (Q_input_year$Q_cms_aggregate[iloc] == min_max$min){
          percentiles$percentiles[i] = 0.0
        } else if (Q_input_year$Q_cms_aggregate[iloc] == min_max$max){
          percentiles$percentiles[i] = 100.0
        } else{
          percentiles$percentiles[i] <- temp$percentiles
        }

      }})
  } else {

    for (i in 1:ngauges){

      iloc <- which(Q_input_year$gauge_id == gaugeid[i])

      temp <- Q_input_aggregate %>%
        dplyr::filter(gauge_id == gaugeid[i]) %>%
        dplyr::summarise(percentiles = 100*ecdf(Q_cms_aggregate)(
          Q_input_year$Q_cms_aggregate[iloc]))

      min_max <- Q_input_aggregate %>%
        dplyr::filter(gauge_id == gaugeid[i]) %>%
        dplyr::summarise(min = min(Q_cms_aggregate),
                         max = max(Q_cms_aggregate))

      if (Q_input_year$Q_cms_aggregate[iloc] == min_max$min){
        percentiles$percentiles[i] = 0.0
      } else if (Q_input_year$Q_cms_aggregate[iloc] == min_max$max){
        percentiles$percentiles[i] = 100.0
      } else{
        percentiles$percentiles[i] <- temp$percentiles
      }

    }
  }

  return(percentiles)
}
