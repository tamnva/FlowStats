library(leaflet)
library(FlowStats)
library(ggplot2)
library(shiny)

# Read shape files of gauges and basins
stations <- sf::read_sf(file.path("data", "de_stations.shp"))
basins <- sf::read_sf(file.path("data", "de_basins.shp"))

# Read simulated streamflow from LSTM
Q_data <- readRDS(file.path("data", "lstm_data", "de_sim_discharge.rds")) %>%
  tidyr::pivot_longer(cols = -date, values_to = "q_mm_day", names_to = "gauge_id") %>%
  dplyr::mutate(year = lubridate::year(date),
                day_of_year = lubridate::yday(date)) %>%
  dplyr::arrange(gauge_id, date)

last_day <- tail(Q_data$date, 1)

# Default gauges coloring scheme
color <- c("#F54E75" ,"#F1B6DA" ,"#B8E186", "#276419")
plabels <- c("Unsatisfactory (NSE < 0.5)",
             "Satisfactory (0.5 ≤ NSE < 0.65)",
             "Good (0.65 ≤ NSE < 0.75)",
             "Very good (0.75 ≤ NSE < 1)")
pcolor <- colorBin(palette = color, bins = c(0.0, 0.5, 0.65, 0.75, 1.0))
pcolor <- pcolor(ifelse(stations$NSE < 0, 0, stations$NSE))
ptitle <- "NSE"

