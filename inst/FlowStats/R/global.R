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
