

function(input, output, session) {

  #----------------------------------------------------------------------------#
  #                                Background map                              #
  #----------------------------------------------------------------------------#
  output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$OpenTopoMap, group = "OpenTopoMap") %>%
      addTiles(group = "OpenStreetMap") %>%
      addProviderTiles(providers$Esri.WorldImagery,group = "WorldImagery") %>%
      addLayersControl(
        baseGroups = c("OpenStreetMap", "OpenTopoMap", "WorldImagery"),
        overlayGroups = c("Subbasin", "Station", "Main basin"),
        options = layersControlOptions(position = "bottomleft")
      )  %>%
      addCircleMarkers(data = stations,
                       radius = 3,
                       group = "Station",
                       fillColor = pcolor,
                       fillOpacity = 0.8,
                       stroke = FALSE,
                       popup = ~ pop_up_info(gauge_name, gauge_id, NSE, are_skm),
                       layerId = ~gauge_id
      ) %>%
      addLegend(position = "bottomleft",
                colors = color,
                title = ptitle,
                labels = plabels,
                opacity = 1)  %>%
      setView(lng = 9, lat = 50, zoom = 4)
  })

  #----------------------------------------------------------------------------#
  #                                User guide                                  #
  #----------------------------------------------------------------------------#
  output$user_guide <- renderText({
    HTML(readLines("data/html/user_guide.html",
                   warn = FALSE) |> paste(collapse = "\n"))
  })

  # Visualize gauge by NSE
  observeEvent(input$gauge_visual_by_NSE, {

    if (input$gauge_visual_by_NSE == 1){
      leafletProxy("map") %>%
        clearShapes() %>%
        addCircleMarkers(data = stations,
                         radius = 3,
                         group = "Station",
                         fillColor = pcolor,
                         fillOpacity = 0.8,
                         stroke = FALSE,
                         popup = ~ pop_up_info(gauge_name, gauge_id, NSE, are_skm),
                         layerId = ~gauge_id
        ) %>%
        clearControls() %>%
        addLegend(position = "bottomleft",
                  colors = rev(color),
                  title = ptitle,
                  labels = rev(plabels),
                  opacity = 1)
    } else {
      leafletProxy("map") %>%
        clearShapes() %>%
        addCircleMarkers(data = stations,
                         radius = 3,
                         group = "Station",
                         fillColor = "#492050",
                         fillOpacity = 0.6,
                         stroke = FALSE,
                         popup = ~ pop_up_info(gauge_name, gauge_id, NSE, are_skm),
                         layerId = ~gauge_id
        ) %>%
        clearControls()
    }


  })

  #----------------------------------------------------------------------------#
  #                      Visualize all gauges                                  #
  #----------------------------------------------------------------------------#
  observeEvent(input$visualize_gauge, {

    if(input$station_visual == "Q_last_day"){
      date_range <- c(tail(Q_data, 1)$date, tail(Q_data, 1)$date)
    } else {
      date_range <- input$date_range
    }

    q_percentiles <- calculate_flowstats(Q_data, date_range,
                                         stations$gauge_id,
                                         input$station_visual)

    ptitle <- "Streamflow classification"

    if (input$station_visual == "Q_last_day"){
      if (input$percentile_class == "All"){
        color <- c("#420b2c", "#D01C8B", "#F1B6DA", "#D0EBAB",
                   "#9CCE64","#276419", "#023903")

        # Due to uncertainty in simulation, percentiles < 0.1% and > 99.9% are
        # lowest and highest values
        pcolor <- colorBin(palette = color,
                           bins = c(0, 0.01, 10, 25, 75, 90, 99.99, 100))
        pcolor <- pcolor(q_percentiles$percentiles)
        plabels <- c("Lowest","Much below normal", "Below normal", "Normal",
                     "Above normal", "Much above normal", "Highest")

      } else if(input$percentile_class == "Flood"){
        color <- c("#ffffff",  "#9CCE64", "#276419")
        pcolor <- colorBin(palette = color,bins = c(0, 95, 99, 100))
        pcolor <- pcolor(q_percentiles$percentiles)
        ptitle <- "Streamflow classification"
        plabels <- c("No flood", "Servere hydrologic flood", "Extreme hydrologic flood")

      } else {
        color <- c("#420b2c", "#841859","#D01C8B", "#F1B6DA", "#ffffff")
        pcolor <- colorBin(palette = color,bins = c(0, 0.1, 5, 10, 25, 100))
        pcolor <- pcolor(q_percentiles$percentiles)
        plabels <- c("Extreme hydrologic drought",
                     "Servere hydrologic drought",
                     "Moderate hydrologic drought",
                     "Below normal",
                     "No drought")
      }
    } else {
      color <- c("#420b2c", "#D01C8B", "#F1B6DA", "#D0EBAB",
                 "#9CCE64","#276419", "#023903")

      # Due to uncertainty in simulation, percentiles < 0.01% and > 99.99% are
      # lowest and highest values

      pcolor <- colorBin(palette = color,
                         bins = c(0, 0.01, 10, 25, 75, 90, 99.99, 100))
      pcolor <- pcolor(q_percentiles$percentiles)
      plabels <- c("Lowest","Much below normal", "Below normal", "Normal",
                   "Above normal", "Much above normal", "Highest")
    }


    leafletProxy("map") %>%
      clearShapes() %>%
      addCircleMarkers(data = stations,
                 radius = 3,
                 group = "Station",
                 fillColor = pcolor,
                 fillOpacity = 0.8,
                 stroke = FALSE,
                 popup = ~ pop_up_info(gauge_name, gauge_id, NSE, are_skm),
                 layerId = ~gauge_id
      ) %>%
      clearControls() %>%
      addLegend(position = "bottomleft",
                colors = rev(color),
                title = ptitle,
                labels = rev(plabels),
                opacity = 1)
  })


  # Show a popup at the given location
  showZipcodePopup <- function(gauge_id, lat, lng) {

    content <- as.character(tagList(
      tags$h5("Gauge ID. = ", gauge_id),
      sprintf("Basin area (square km): %s", gauge_id), tags$br()
    ))

    leafletProxy("map") %>% addPopups(lng, lat, content, layerId = gauge_id)
  }

  #---------------------------------------------------Show basins when map click
  observeEvent(input$map_marker_click, {

    if (!is.null(input$map_marker_click$id)){
      leafletProxy("map") %>%
        addPolygons(
          data = subset(basins, gauge_id == input$map_marker_click$id),
          group = "Subbasin",
          stroke = TRUE,
          weight = 2,
          layerId = "basin_shape_id")}
    })

  #-----------------------------------------------------Show plot when map click
  observe({

    req(input$date_range)
    req(input$station_visual)
    req(input$map_marker_click)

    if (!is.null(input$map_marker_click$id) &
                 input$navset == "All gauges" ){
        plt <- plot_flowstats(Q_data, input$date_range, input$map_marker_click$id,
                              input$station_visual)
        output$plot_spatial <- plotly::renderPlotly({plotly::ggplotly(plt)})
    }
  })

  #-----------------------------------------------------Show plot when map click
  observe({

    req(input$map_marker_click)
    req(input$plot_type)
    req(!is.null(input$log_y))

    if (input$navset == "Single gauge"){
      if (!is.null(input$map_marker_click$id)){
        plt <- plot_streamflow(Q_data, input$map_marker_click$id,
                               input$plot_type, input$log_y)
        output$input_data <- plotly::renderPlotly({plotly::ggplotly(plt)})
      }
    }

  })
}
