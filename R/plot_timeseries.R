#' Plot streamflow statistic for one gauge
#'
#' @param Q_input dataframe object, simulated or observed streamflow data, must
#' contains the following columns "gauge_id", "date", "Q_cms"
#'
#' @param gaugeid character, selected gauge_id
#'
#' @param plot_type character, can be one of the following options
#' 'Daily (by year)' for minimum, 'Q_mean (selected period)' to calculate
#' mean, 'Q_max (selected period)' to calculate maximum, or 'Q_last_day' to just
#' get Q latest day in the Q_input dataset
#'
#' @param log_y 1 for log_y axis and 0 for normal axis
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
#' plot_type <- "Daily (by year)"
#'
#' # Get flow percentiles
#' plt_timeseries <- plot_timeseries(Q_input, gaugeid, plot_type, log_y)
#' }
#' @export
#'

plot_timeseries <- function(Q_input, gaugeid, plot_type, log_y){

  Q_gauge_id <- Q_input %>%
    dplyr::filter(gauge_id == gaugeid)

  # Plot daily
  if (plot_type == "Daily (by year)"){
    Q_daily_stat <- Q_gauge_id %>%
      dplyr::mutate(year = lubridate::year(date),
                    month = lubridate::month(date),
                    day = lubridate::day(date)) %>%
      dplyr::group_by(month, day) %>%
      dplyr::summarise(
        Q_min = min(Q_cms),
        `Q_10%` = quantile(Q_cms, c(0.05)),
        `Q_25%` = quantile(Q_cms, c(0.25)),
        `Q_75%` = quantile(Q_cms, c(0.75)),
        `Q_90%` = quantile(Q_cms, c(0.95)),
        Q_max = max(Q_cms),
        .groups = 'drop'
      )
  } else if(plot_type == "Daily"){
    plt <- ggplot2::ggplot(Q_gauge_id, ggplot2::aes(x = date, y = Q_cms)) +
      ggplot2::geom_line(color = "#276419", linewidth = 0.3, alpha = 0.8) +
      ggplot2::labs(y = "Q (cms)", x = " ") +
      ggplot2::theme_bw() +
      ggplot2::theme(axis.title = ggplot2::element_text(size = 9))
  } else if (plot_type == "Daily cumsum (by year)"){
    Q_daily_stat_cumsum <- Q_gauge_id %>%
      dplyr::mutate(year = lubridate::year(date),
                    month = lubridate::month(date),
                    day = lubridate::day(date)) %>%
      dplyr::group_by(year) %>%
      dplyr::mutate(Q_cms = cumsum(Q_cms))  %>%
      dplyr::group_by(month, day) %>%
      dplyr::summarise(
        Q_min = min(Q_cms),
        `Q_10%` = quantile(Q_cms, c(0.05)),
        `Q_25%` = quantile(Q_cms, c(0.25)),
        `Q_75%` = quantile(Q_cms, c(0.75)),
        `Q_90%` = quantile(Q_cms, c(0.95)),
        Q_max = max(Q_cms),
        .groups = 'drop'
      )
  }

  # Select data of current year
  current_year <- lubridate::year(tail(Q_gauge_id$date, 1))
  date <- seq.Date(as.Date(paste0(current_year, "-01-01")),
                   as.Date(paste0(current_year, "-12-31")), by = "days")

  if (length(date) == 365) {
    if (plot_type == "Daily (by year)"){
      Q_daily_stat = Q_daily_stat[-c(60),]
    } else if (plot_type == "Daily cumsum (by year)"){
      Q_daily_stat_cumsum = Q_daily_stat_cumsum[-c(60),]
    }
  }

  if (plot_type == "Daily (by year)"){
    Q_daily_stat <- Q_daily_stat %>% dplyr::mutate(date = date, .before = 1)

    plt <- ggplot2::ggplot(Q_daily_stat, ggplot2::aes(x = date)) +
      ggplot2::geom_ribbon(ggplot2::aes(ymin = Q_min, ymax = `Q_10%`),
                           fill = "#D01C8B", alpha = 0.6) +
      ggplot2::geom_ribbon(ggplot2::aes(ymin = `Q_10%`, ymax = `Q_25%`),
                           fill = "#F1B6DA", alpha = 0.6) +
      ggplot2::geom_ribbon(ggplot2::aes(ymin = `Q_25%`, ymax = `Q_75%`),
                           fill = "#D0EBAB", alpha = 0.6) +
      ggplot2::geom_ribbon(ggplot2::aes(ymin = `Q_75%`, ymax = `Q_90%`),
                           fill = "#9CCE64", alpha = 0.6) +
      ggplot2::geom_ribbon(ggplot2::aes(ymin = `Q_90%`, ymax = Q_max),
                           fill = "#276419", alpha = 0.6) +
      ggplot2::geom_line(
        data = Q_gauge_id %>%
          dplyr::filter(lubridate::year(date) == current_year) %>%
          dplyr::rename(`Q_current_year` = Q_cms),
        ggplot2::aes(x = date, y = `Q_current_year`), color = "blue") +
      ggplot2::labs(y = "Q (cms)", x = " ") +
      ggplot2::theme_bw() +
      ggplot2::theme(axis.title = ggplot2::element_text(size = 9))


  } else if (plot_type == "Daily cumsum (by year)"){
    Q_daily_stat_cumsum <- Q_daily_stat_cumsum %>%
      dplyr::mutate(date = date, .before = 1)

    plt <- ggplot2::ggplot(Q_daily_stat_cumsum, ggplot2::aes(x = date)) +
      ggplot2::geom_ribbon(ggplot2::aes(ymin = Q_min, ymax = `Q_10%`,
                               fill = "Much below normal"), alpha = 0.6) +
      ggplot2::geom_ribbon(ggplot2::aes(ymin = `Q_10%`, ymax = `Q_25%`,
                               fill = "Below normal"), alpha = 0.6) +
      ggplot2::geom_ribbon(ggplot2::aes(ymin = `Q_25%`, ymax = `Q_75%`,
                               fill = "Normal"), alpha = 0.6) +
      ggplot2::geom_ribbon(ggplot2::aes(ymin = `Q_75%`, ymax = `Q_90%`,
                               fill = "Above normal"), alpha = 0.6) +
      ggplot2::geom_ribbon(ggplot2::aes(ymin = `Q_90%`, ymax = Q_max,
                               fill = "Much above normal"), alpha = 0.6) +
      ggplot2::geom_line(
        data = Q_gauge_id %>%
          dplyr::filter(lubridate::year(date) == current_year) %>%
          dplyr::rename(`Q_current_year` = Q_cms),
        ggplot2::aes(x = date, y = cumsum(`Q_current_year`)), color = "blue") +
      ggplot2::scale_fill_manual(
        values =  c("Much below normal" = "#D01C8B",
                    "Below normal" = "#F1B6DA" ,
                    "Normal" = "#D0EBAB",
                    "Above normal" = "#9CCE64",
                    "Much above normal" = "#276419")) +
      ggplot2::labs(y = "Q (cms)", x = " ", fill = "Color legend") +
      ggplot2::theme_bw() +
      ggplot2::theme(legend.position = "none",
                     axis.title = ggplot2::element_text(size = 9))

  }

  # Log y axis if select
  if (log_y == 1) plt <- plt + ggplot2::scale_y_log10()

  return(plt)
}
