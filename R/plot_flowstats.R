#' Plot streamflow statistic for one gauge
#'
#' @param Q_input dataframe object, simulated or observed streamflow data, must
#' contains the following columns "gauge_id", "date", "q_mm_day". IMPORTANT, this
#' data frame should have only one gauge id
#'
#' @param select_period date vector with format "%Y-%m-%d", starting and ending date of
#' of the period for calculating streamflow statistic
#'
#' @param n_day_mean integer, number of days for calculating the moving average
#'
#' @param percentile_class percentile of streamflow should be calculated to
#' higlight all ranges of flows "All" or only for "Drought" or only for "Flood"
#'
#' @param stat_function character, could be "Min", "Max", or "Median"
#'
#' @return ggplot object

#' @examples
#'\dontrun{
#' select_period <- c(as.Date("2025-05-01"), as.Date("2025-05-31"))
#' stat_function <- "Mean"
#' n_day_mean <- 1
#' percentile_class <- "All"
#' }
#' @export
#' @importFrom ggplot2 ggplot geom_point geom_line labs theme_bw theme aes element_text

plot_flowstats <- function(Q_input, select_period, n_day_mean, stat_function,
                           percentile_class){

  # Assign stat_function to input function
  if (stat_function == "Min") {
    stat_function = min
  } else if(stat_function == "Mean") {
    stat_function = mean
  } else if(stat_function == "Max") {
    stat_function = max
  } else {
    stop("Unknown streamflow statistics name: ", stat_function)
  }

  # Calculate streamflow statistics
  Q_input <- Q_input %>%
    dplyr::mutate(q_mm_day = data.table::frollmean(q_mm_day, n_day_mean)) %>%
    dplyr::filter(day_of_year >= lubridate::yday(select_period[1]),
                  day_of_year <= lubridate::yday(select_period[2])) %>%
    tidyr::drop_na() %>%
    dplyr::group_by(year) %>%
    dplyr::summarise(q_mm_day = stat_function(q_mm_day),
                     .groups = 'drop')

  Q_current_year <- Q_input %>%
    dplyr::filter(year == lubridate::year(select_period[1]))

  # Define the plot color
  if (percentile_class == "All"){
    color <- c("#420b2c", "#D01C8B", "#F1B6DA", "#D0EBAB",
               "#9CCE64","#276419", "#023903")
    pcolor <- colorBin(palette = color,
                       bins = c(0, 0.01, 10, 25, 75, 90, 99.99, 100))
  } else if (percentile_class == "Drought"){
    color <- c("#420b2c", "#841859","#D01C8B", "#F1B6DA", "#ffffff")
    pcolor <- colorBin(palette = color,bins = c(0, 0.0001, 5, 10, 25, 100))
  } else {
    color <- c("#ffffff",  "#9CCE64", "#023903")
    pcolor <- colorBin(palette = color,bins = c(0, 95, 99, 100))
  }


  isort <- sort(Q_input$q_mm_day, index.return=TRUE)
  Q_input <- Q_input[isort$ix,]
  Q_input$percentile <- 100*round(c(1:nrow(Q_input))/(
    nrow(Q_input) + 1), 4)


  if (Q_current_year$q_mm_day <= Q_input$q_mm_day[1]){
    Q_current_year$percentile <- Q_input$percentile[1]
    pcolor_current_year <- "#420b2c"

  } else if (Q_current_year$q_mm_day >= Q_input$q_mm_day[nrow(Q_input)]){
    Q_current_year$percentile <- Q_input$percentile[nrow(Q_input)]
    pcolor_current_year <- "#023903"
  } else {
    Q_current_year$percentile <- round(100*ecdf(Q_input$q_mm_day)(
      Q_current_year$q_mm_day), 2)
    pcolor_current_year <- pcolor(Q_current_year$percentile)
  }


  plt <- ggplot(Q_input,
                aes(x = q_mm_day, y = percentile, label = year)) +
    geom_point(alpha = 0.5, size = 1) +
    geom_line(alpha = 0.5, linewidth = 0.5) +
    geom_point(data = Q_current_year, aes(x = q_mm_day, y = percentile),
               color=pcolor_current_year, size = 2) +
    labs(x = "Q aggregate (mm/day)", y = "Non-exceedance probability (%)") +
    theme_bw() +
    theme(axis.title=element_text(size=8))

  return(plt)
}
