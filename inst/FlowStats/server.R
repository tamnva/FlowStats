

function(input, output, session) {

  # Stop the app when user close the browser
  session$onSessionEnded(function(){
    shiny::stopApp()
  })

  #----------------------------------------------------------------------------#
  #                                Background map                              #
  #----------------------------------------------------------------------------#
  output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$CartoDB.PositronNoLabels,
                       group = "CartoDBPositronNolabel") %>%
      addProviderTiles(providers$CartoDB.Positron,
                       group = "CartoDBPositron") %>%
      addProviderTiles(providers$OpenTopoMap,
                       group = "OpenTopoMap") %>%
      addTiles(group = "OpenStreetMap") %>%
      addProviderTiles(providers$Esri.WorldImagery,
                       group = "WorldImagery") %>%
      addLayersControl(
        baseGroups = c("CartoDBPositronNolabel", "CartoDBPositron",
                       "OpenStreetMap", "OpenTopoMap", "WorldImagery"),
        overlayGroups = c("Gauge"),
        options = layersControlOptions(position = "bottomleft")
      )  %>%
      addCircleMarkers(data = stations,
                       radius = 3,
                       group = "Gauge",
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
      setView(lng = 9, lat = 50, zoom = 5)
  })

  #----------------------------------------------------------------------------#
  #                                User guide                                  #
  #----------------------------------------------------------------------------#
  output$user_guide <- renderText({
    HTML(readLines(file.path('data', 'html', 'user_guide.html'),
                   warn = FALSE) |> paste(collapse = "\n"))
  })

  # Visualize gauge by NSE
  observeEvent(input$gauge_visual_by_NSE, {

    if (input$gauge_visual_by_NSE == 1){
      leafletProxy("map") %>%
        clearShapes() %>%
        addCircleMarkers(data = stations,
                         radius = 4,
                         group = "Gauge",
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
                         radius = 4,
                         group = "Gauge",
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
  #                                Update data                                #
  #----------------------------------------------------------------------------#
  observeEvent(input$py_venv, {

    venv <- trimws(input$py_venv)

    if (!dir.exists(venv)){
      output$py_venv_check <- renderText("Input dir does NOT exist")
    } else {
      if (dir.exists(file.path(venv, "Scripts")) |
          dir.exists(file.path(venv, "bin"))){
        output$py_venv_check <- renderText(" ")
      } else {
        output$py_venv_check <- renderText("Input directory is NOT a python virtual environment")
      }
    }
  })

  observeEvent(input$update_data, {

    venv <- trimws(input$py_venv)
    if (dir.exists(file.path(venv, "Scripts")) |
        dir.exists(file.path(venv, "bin"))){
      update_data(venv, basins, input$forecast_data)
    } else {
      showModal(modalDialog(
        title = "Update data",
        "Input python virtual environment error (please see message above)",
        easyClose = TRUE))
    }
  })


  #----------------------------------------------------------------------------#
  #                      Visualize all gauges                                  #
  #----------------------------------------------------------------------------#
  observeEvent(input$visualize_gauge, {

    showModal(modalDialog(
      title = "Apply to all gauges",
      HTML(readLines(file.path('data', 'html', 'message_plot_all_gauges.html'),
                     warn = FALSE) |> paste(collapse = "\n")),
      easyClose = TRUE
    ))

    q_percentiles <- calculate_flowstats(Q_data, input$select_period,
                                         input$n_day_mean,
                                         input$stat_function, TRUE) %>%
      dplyr::select(gauge_id, percentiles)

    q_percentiles <- dplyr::left_join(stations, q_percentiles, by = "gauge_id")
    ptitle <- "Streamflow classification"


    if (input$percentile_class == "All"){
      color <- c("#420b2c", "#D01C8B", "#F1B6DA", "#D0EBAB",
                 "#9CCE64","#276419", "#023903")

      # Due to uncertainty in simulation, percentiles < 0.1% and > 99.9% are
      # lowest and highest values
      pcolor <- colorBin(palette = color,
                         bins = c(0, 1, 10, 25, 75, 90, 99, 100))
      pcolor <- pcolor(q_percentiles$percentiles)
      plabels <- c("Lowest","Much below normal", "Below normal", "Normal",
                   "Above normal", "Much above normal", "Highest")

    } else if(input$percentile_class == "Flood"){
      color <- c("#ffffff",  "#9CCE64", "#023903")
      pcolor <- colorBin(palette = color,bins = c(0, 95, 99, 100))
      pcolor <- pcolor(q_percentiles$percentiles)

      ptitle <- "Streamflow classification"
      plabels <- c("No flood", "Severe hydrologic flood",
                   "Extreme hydrologic flood")

    } else {

      # In the function to calculate q_percentile I set percentile = 0 for min value
      color <- c("#420b2c", "#841859","#D01C8B", "#F1B6DA", "#ffffff")
      pcolor <- colorBin(palette = color,bins = c(0, 0.0001, 5, 10, 25, 100))
      pcolor <- pcolor(q_percentiles$percentiles)

      plabels <- c("Extreme hydrologic drought",
                   "Severe hydrologic drought",
                   "Moderate hydrologic drought",
                   "Below normal",
                   "No drought")
    }



    leafletProxy("map") %>%
      clearShapes() %>%
      addCircleMarkers(data = stations,
                 radius = 4,
                 group = "Gauge",
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
          stroke = TRUE,
          weight = 2,
          layerId = "basin_shape_id")}
    })

  #-----------------------------------------------------Show plot when map click
  observe({

    req(input$select_period)
    req(input$n_day_mean)
    req(input$stat_function)
    req(input$percentile_class)
    req(input$map_marker_click)

    # Save project setting
    spsComps::shinyCatch(
      if (!is.null(input$map_marker_click$id) &
          input$navset == "All gauges" ){

        plt <- plot_flowstats(Q_data %>%
                                dplyr::filter(gauge_id == input$map_marker_click$id),
                              input$select_period, input$n_day_mean,
                              input$stat_function, input$percentile_class)

        output$gauge_spatial_plot <- plotly::renderPlotly({plotly::ggplotly(plt)})
      },
      blocking_level = "error"
    )


  })

  #-----------------------------------------------------Show plot when map click
  observe({

    req(input$map_marker_click)
    req(input$plot_type)
    req(!is.null(input$log_y))

    if (input$navset == "Single gauge"){
      if (!is.null(input$map_marker_click$id)){
        plt <- plot_timeseries(Q_data, input$map_marker_click$id,
                               input$plot_type, input$log_y)
        output$input_data <- plotly::renderPlotly({plotly::ggplotly(plt)})
      }
    }

  })

  #----------------------------------------------------------Explain update data
  observe({

    req(input$help_update_data)

    showModal(modalDialog(
      title = "Update data (OPTIONAL)",
      HTML(readLines(file.path('data', 'html', 'update_data.html'),
                     warn = FALSE) |> paste(collapse = "\n")),
      easyClose = TRUE
    ))
  })

  #------------------------------------------------Explain the single gauge plot
  observe({
    req(input$plot_explanation)
    showModal(modalDialog(
      title = "Plot explanation",
      HTML(readLines(file.path('data', 'html', 'plot_explanation.html'),
                     warn = FALSE) |> paste(collapse = "\n")),
      easyClose = TRUE
    ))
  })


  #---------------------------------------------------Explain the all gauge plot
  observe({
    req(input$gauge_plot_explanation)

    showModal(modalDialog(
      title = "Plot explanation",
      HTML(readLines("data/html/gauge_color_explanation.html",
                     warn = FALSE) |> paste(collapse = "\n")),
      easyClose = TRUE
    ))
  })

}
