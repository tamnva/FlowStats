#' Plot streamflow statistic for one gauge
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
#' @return ggplot object

#' @examples
#'\dontrun{
#' # Read data from FlowStats package
#' Q_input <- readRDS(file.path(.libPaths(),"FlowStats", "FlowStats",
#'                             "data", "lstm_data", "de_sim_discharge.rds"))
#'
#' # Period for calculating streamflow statistic
#' period <- c(as.Date("2025-05-01"), as.Date("2025-05-31"))
#'
#' # Gauge id
#' gaugeid <- Q_input$gauge_id[1]
#'
#' # How to calculate Q
#' fun <- "Q_mean (selected period)"
#'
#' # Get flow percentiles
#' plt_gauge_stats <- plot_gauge_stats(Q_input, period, gaugeid, fun)
#' }
#' @export
#' @importFrom ggplot2 ggplot geom_point geom_line labs theme_bw theme aes element_text
#' @importFrom dplyr mutate filter group_by summarise

plot_gauge_stats <- function(Q_input, period, gaugeid, fun){

  Q_input <- Q_input %>% filter(gauge_id == gaugeid)

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
    Q_input_aggregate <- Q_input %>% dplyr::rename(Q_cms_aggregate = Q_cms,
                                                   year = date)
  } else {
    Q_input_aggregate <- Q_input %>%
      mutate(day_of_year = lubridate::yday(date),
                    year = lubridate::year(date)) %>%
      filter(day_of_year >= lubridate::yday(period[1]),
                    day_of_year <= lubridate::yday(period[2])) %>%
      group_by(gauge_id, year)  %>%
      summarise(Q_cms_aggregate = agg_func(Q_cms),
                       .groups = 'drop')
  }

  Q_current_year <- tail(Q_input_aggregate, 1)

  color <- c("#420b2c", "#D01C8B", "#F1B6DA", "#D0EBAB",
             "#9CCE64","#276419", "#023903")

  pcolor <- colorBin(palette = color,
                     bins = c(0, 0.01, 10, 25, 75, 90, 99.99, 100))


  isort <- sort(Q_input_aggregate$Q_cms_aggregate, index.return=TRUE)
  Q_input_aggregate <- Q_input_aggregate[isort$ix,]
  Q_input_aggregate$percentile <- 100*round(c(1:nrow(Q_input_aggregate))/(
    nrow(Q_input_aggregate) + 1), 4)

  if (Q_current_year$Q_cms_aggregate <=
      Q_input_aggregate$Q_cms_aggregate[1]){

    Q_current_year$percentile <- Q_input_aggregate$percentile[1]
    pcolor_current_year <- "#420b2c"

  } else if (Q_current_year$Q_cms_aggregate >=
             Q_input_aggregate$Q_cms_aggregate[nrow(Q_input_aggregate)]){

    Q_current_year$percentile <-
      Q_input_aggregate$percentile[nrow(Q_input_aggregate)]
    pcolor_current_year <- "#023903"

  } else {

    Q_current_year$percentile <- round(100*ecdf(Q_input_aggregate$Q_cms_aggregate)(
      Q_current_year$Q_cms_aggregate), 2)
    pcolor_current_year <- pcolor(Q_current_year$percentile)
  }


  plt <- ggplot(Q_input_aggregate,
                aes(x = Q_cms_aggregate, y = percentile, label = year)) +
    geom_point(alpha = 0.5, size = 1) +
    geom_line(alpha = 0.5, linewidth = 0.5) +
    geom_point(data = Q_current_year, aes(x = Q_cms_aggregate, y = percentile),
               color=pcolor_current_year, size = 2) +
    labs(x = fun, y = "Non-exceedance probability (%)") +
    theme_bw() +
    theme(axis.title=element_text(size=8))

  return(plt)
}
