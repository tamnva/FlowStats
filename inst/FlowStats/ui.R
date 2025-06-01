

navbarPage(
  title = "FlowStats", id="nav",

  tabPanel(
    title = "Interactive map",

    div(
      class="outer",
      tags$head(includeCSS("styles.css")),

      leafletOutput("map", width="100%",  height="100%"),

      #-----------------------------------------------------------Absolute panel
      absolutePanel(

        class = "panel panel-default", fixed = FALSE, draggable = TRUE,
        top = 18, left = "auto", right = 10, bottom = "auto", width = 420,
        height = "auto", cursor = "auto", style = "overflow: auto;",

        bslib::navset_card_underline(
          id = "navset", title = NULL,

          #-----------------------------------------------------User guide panel
          bslib::nav_panel(
            title = "Guide", selected = TRUE,

            column(width = 12,
                   htmlOutput("user_guide"),
                   checkboxInput('gauge_visual_by_NSE',
                                 'Visualize gauges by NSE', value = 1))
            ),

          #--------------------------------------------------------Update Q data
          bslib::nav_panel("Update data"

          ),

          #-------------------------------------------------Visualize all gauges
          bslib::nav_panel(
            title = "All gauges",

            column(
              width = 12,

              selectInput(
                "station_visual",
                "Select streamflow (Q) value",
                list("Q_mean (selected period)",
                     "Q_min (selected period)",
                     "Q_max (selected period)",
                     "Q_last_day")
                ),

              conditionalPanel(
                condition = "input.station_visual.includes('Q_last_day')",

                selectInput("percentile_class",
                            "Q percentile classes",
                            list("All", "Flood","Drought"))
                           ),

              conditionalPanel(
                condition = "input.station_visual != 'Q_last_day'",

                dateRangeInput(
                  "date_range", "Select period",
                  min = paste0(lubridate::year(tail(Q_data, 1)$date), "-01-01"),
                  max = tail(Q_data, 1)$date,
                  start = paste0(lubridate::year(tail(Q_data, 1)$date), "-01-01"),
                  end = tail(Q_data, 1)$date),
                ),


            column(width = 12, plotly::plotlyOutput("plot_spatial", height = 200))),

            tags$div(style="margin-bottom:20px; margin-top:10px;",
                     column(width = 12,
                            actionButton('visualize_gauge',
                                         'Click to apply to all gauges'))
            ),),

          #-----------------------------------------------Visualize single gauge
          bslib::nav_panel(
            "Single gauge",

            column(
              width = 12,
              selectInput(
                "plot_type", "Select plot type ",
                list("Daily", "Daily (by year)", "Daily cumsum (by year)")
                ),

              checkboxInput('log_y', 'Log y-axis', value = 0),

              plotly::plotlyOutput("input_data", height = 220)
            ),
          ),
        ),
      ),
    ),
  ),
)
