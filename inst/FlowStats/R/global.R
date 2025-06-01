library(leaflet)
library(FlowStats)
library(ggplot2)
library(shiny)

# Read shape files of gauges and basins
stations <- sf::read_sf(file.path("data", "de_stations.shp"))
basins <- sf::read_sf(file.path("data", "de_basins.shp"))

# Read simulated streamflow from LSTM
Q_data <- readRDS(file.path("data", "lstm_data", "de_sim_discharge.rds"))

# Default gauges coloring scheme
color <- c("#F1B6DA", "#B8E186",  "#4D9221", "#276419")
plabels <- c("Unsatisfactory (NSE < 0.5)",
             "Satisfactory (0.5 ≤ NSE < 0.65)",
             "Good (0.65 ≤ NSE < 0.75)",
             "Very good (0.75 ≤ NSE < 1)")
pcolor <- colorBin(palette = color, bins = c(0.0, 0.5, 0.65, 0.75, 1.0))
pcolor <- pcolor(ifelse(stations$NSE < 0, 0, stations$NSE))
ptitle <- "NSE"

#------------------------------------------------------------------------------#
#                                Plot streamflow                               #
# Streamflow classification follows  USGS                                      #
# https://waterwatch.usgs.gov/index.php?id=ww_past                             #
#------------------------------------------------------------------------------#

plot_streamflow <- function(input_data, gaugeid, plot_type, log_y){
  Q_gauge_id <- input_data %>%
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
    plt <- ggplot(Q_gauge_id, aes(x = date, y = Q_cms)) +
      geom_line(color = "#276419", linewidth = 0.3, alpha = 0.8) +
      labs(y = "Q (cms)", x = " ") +
      theme_bw() +
      theme(axis.title = element_text(size = 9))
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

    plt <- ggplot(Q_daily_stat, aes(x = date)) +
      geom_ribbon(aes(ymin = Q_min, ymax = `Q_10%`),
                  fill = "#D01C8B", alpha = 0.6) +
      geom_ribbon(aes(ymin = `Q_10%`, ymax = `Q_25%`),
                  fill = "#F1B6DA", alpha = 0.6) +
      geom_ribbon(aes(ymin = `Q_25%`, ymax = `Q_75%`),
                  fill = "#D0EBAB", alpha = 0.6) +
      geom_ribbon(aes(ymin = `Q_75%`, ymax = `Q_90%`),
                  fill = "#9CCE64", alpha = 0.6) +
      geom_ribbon(aes(ymin = `Q_90%`, ymax = Q_max),
                  fill = "#276419", alpha = 0.6) +
      geom_line(data = Q_gauge_id %>%
                  dplyr::filter(lubridate::year(date) == current_year) %>%
                  dplyr::rename(`Q_current_year` = Q_cms),
                aes(x = date, y = `Q_current_year`), color = "blue") +
      labs(y = "Q (cms)", x = " ") +
      theme_bw() +
      theme(axis.title = element_text(size = 9))


  } else if (plot_type == "Daily cumsum (by year)"){
    Q_daily_stat_cumsum <- Q_daily_stat_cumsum %>%
      dplyr::mutate(date = date, .before = 1)

    plt <- ggplot(Q_daily_stat_cumsum, aes(x = date)) +
      geom_ribbon(aes(ymin = Q_min, ymax = `Q_10%`, fill = "Much below normal"),
                  alpha = 0.6) +
      geom_ribbon(aes(ymin = `Q_10%`, ymax = `Q_25%`, fill = "Below normal"),
                  alpha = 0.6) +
      geom_ribbon(aes(ymin = `Q_25%`, ymax = `Q_75%`, fill = "Normal"),
                  alpha = 0.6) +
      geom_ribbon(aes(ymin = `Q_75%`, ymax = `Q_90%`, fill = "Above normal"),
                  alpha = 0.6) +
      geom_ribbon(aes(ymin = `Q_90%`, ymax = Q_max, fill = "Much above normal"),
                  alpha = 0.6) +
      geom_line(data = Q_gauge_id %>%
                  dplyr::filter(lubridate::year(date) == current_year) %>%
                  dplyr::rename(`Q_current_year` = Q_cms),
                aes(x = date, y = cumsum(`Q_current_year`)), color = "blue") +
      scale_fill_manual(values =  c("Much below normal" = "#D01C8B",
                                    "Below normal" = "#F1B6DA" ,
                                    "Normal" = "#D0EBAB",
                                    "Above normal" = "#9CCE64",
                                    "Much above normal" = "#276419")) +
      labs(y = "Q (cms)", x = " ", fill = "Color legend") +
      theme_bw() +
      theme(legend.position = "none",
            axis.title = element_text(size = 9))

  }

  # Log y axis if select
  if (log_y == 1) plt <- plt + scale_y_log10()

  return(plt)
}

#------------------------------------------------------------------------------#
#                     Calculate Q statistics                                   #
#------------------------------------------------------------------------------#
calculate_flowstats <- function(Q_input, period, gaugeid, fun){

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


  withProgress(message = 'Calculating streamflow statistics', value = 0, {

    for (i in 1:ngauges){

      incProgress(1/ngauges, detail = paste0(round(i*100/ngauges,0),"%"))

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

  return(percentiles)
}

#------------------------------------------------------------------------------#
#                                 Plot Q statistics                            #
#------------------------------------------------------------------------------#

plot_flowstats <- function(Q_input, period, gaugeid, fun){

  Q_input <- Q_input %>% dplyr::filter(gauge_id == gaugeid)

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
      dplyr::mutate(day_of_year = lubridate::yday(date),
                    year = lubridate::year(date)) %>%
      dplyr::filter(day_of_year >= lubridate::yday(period[1]),
                    day_of_year <= lubridate::yday(period[2])) %>%
      dplyr::group_by(gauge_id, year)  %>%
      dplyr::summarise(Q_cms_aggregate = agg_func(Q_cms),
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
