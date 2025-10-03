

navbarPage(
  title = "FlowStats", id="nav",

  tabPanel(
    title = "Interactive map",

    div(
      class="outer",
      tags$head(includeCSS("styles.css")),

      shinybusy::add_busy_spinner(spin = "radar", position = c("bottom-right"),
                                  margins = c(100, 100)),

      leafletOutput("map", width="100%",  height="100%"),

      #-----------------------------------------------------------Absolute panel
      absolutePanel(

        class = "panel panel-default", fixed = FALSE, draggable = FALSE,
        top = 18, left = "auto", right = 10, bottom = "auto", width = 400,
        height = "auto", cursor = "auto", style = "overflow: auto;",

        bslib::navset_card_underline(
          id = "navset",
          title = NULL,

          #-----------------------------------------------------User guide panel
          bslib::nav_panel(
            title = "Guide", selected = TRUE,
            tags$hr(class = "custom-line"), h4(),
            column(width = 12,
                   htmlOutput("user_guide"),
                   checkboxInput('gauge_visual_by_NSE',
                                 'Visualize gauges by NSE', value = 1))
          ),

          #--------------------------------------------------------Update Q data
          bslib::nav_panel(
            title = "Update data",
            tags$hr(class = "custom-line"),
            h4(),
            column(width = 12, actionButton('help_update_data','Help',
                                            width = '100%')),

            h4(),
            column(width = 12,
                   textInput("py_venv",
                             "Provide Python virtual environment",
                             "C:/Users/nguyenta/AppData/Local/anaconda3/envs/hydro",
                             width = "100%"
                   ),
                   h4(),
                   span(textOutput("py_venv_check"), style="color:red"),

                   h4(),
                   checkboxInput("forecast_data",
                                 "Get forecast meterological data (optional)", FALSE),

                   h4(),
                   actionButton("update_data",
                                "Update data"),
                   h4()),

          ),

          #-----------------------------------------------Visualize single gauge
          bslib::nav_panel(
            "Single gauge",
            tags$hr(class = "custom-line"), h4(),

            column(
              width = 12,
              selectInput(
                "plot_type", "Select plot type",
                list("Daily", "Daily (by year)", "Daily cumsum (by year)")
              ),

              checkboxInput('log_y', 'Log y-axis', value = 0),

              h5("Click to the gauge location on leaflet to see plot"),

              plotly::plotlyOutput("input_data", height = 250),

              tags$div(style="margin-bottom:30px; margin-top:10px;",
                       actionButton('plot_explanation',
                                    'Plot explanation')
              ),

            ),
          ),

          #-------------------------------------------------Visualize all gauges
          bslib::nav_panel(
            title = "All gauges",
            tags$hr(class = "custom-line"), h4(),
            column(width = 12, actionButton('gauge_plot_explanation','Help',
                                            width = '100%')),

            tags$div(style="margin-bottom:20px; margin-top:10px;",
                     column(width = 7,
                            dateRangeInput(
                              "select_period", "Selected period",
                              min = paste0(lubridate::year(last_day), "-01-01"),
                              max = last_day,
                              start = paste0(lubridate::year(last_day), "-01-01"),
                              end = last_day)),
                     column(width = 5,
                            numericInput("n_day_mean", "N-day mean", value = 1,
                                         min = 1,max = 366, step = 1)),

                     column(width = 7,
                            selectInput("stat_function", "Streamflow statistic",
                                        list("Mean", "Min","Max"))),
                     column(width = 5,
                            selectInput("percentile_class", "Q percentile classes",
                                        list("All", "Flood","Drought"))),
                     column(width = 12,
                            plotly::plotlyOutput("gauge_spatial_plot",
                                                 height = 250)),
                     column(width = 6,
                            actionButton('visualize_gauge',
                                         'Click to apply to all gauges')),
            ),
          ),
        ),
      ),
    ),
  )
)
