library(shiny)
library(plotly)
library(leaflet)
library(leaflet.extras)
library(leaflet.extras2)
library(jsonlite)
library(httr)
library(httr2)
library(base64enc)
library(R6)
library(shinyjs)
library(shinydashboard)
library(logger)  # Add logging library

source("wtss/wtss_client.R")

# Set up logging configuration
log_dir <- "logs"
if (!dir.exists(log_dir)) {
  dir.create(log_dir)
}
log_file <- file.path(log_dir, paste0("wtss_app_", format(Sys.time(), "%Y%m%d"), ".log"))
log_threshold(TRACE)
log_appender(appender_file(log_file))

# Initialize the WTSS client
wtss_inpe <- "https://data.inpe.br/bdc/wtss/v4/"
client <- WTSSClient$new(base_url = wtss_inpe)

# Get available collections for the dropdown
capabilities <- client$get_capabilities()
available_collections <- sapply(capabilities$available_collections, function(x) x$name)

# UI Definition
ui <- fluidPage(
  useShinyjs(),
  tags$head(
    tags$style(HTML("
      .sidebar { 
        height: calc(100vh - 20px);
        overflow-y: auto;
        padding: 15px;
        background-color: #f8f9fa;
        position: fixed;
        width: 300px;
        left: 15px;
        top: 10px;
        border-radius: 5px;
        box-shadow: 0 0 10px rgba(0,0,0,0.1);
      }
      .map-container {
        height: 100vh;
        margin-left: 330px;
        padding: 0;
      }
      #plotPanel {
        z-index: 1000;
      }
      .well {
        background-color: white;
        border: none;
        box-shadow: none;
      }
    "))
  ),
  # Sidebar with form
  div(class = "sidebar",
    wellPanel(
      h2("WTSS Explorer", style = "margin-top: 0; color: #2c3e50;"),
      hr(),
      # Product Selection
      selectInput("product", "Select Satellite Product",
                 choices = available_collections,
                 selected = NULL),
      # Summarise Geometry Checkbox
      checkboxInput("summariseGeometry", "Summarise Geometry", value = FALSE),
      # Dynamic Bands Selection
      uiOutput("bandSelector"),
      # Date Range Selection
      dateRangeInput("dateRange", "Select Date Range",
                    start = Sys.Date() - 365,
                    end = Sys.Date(),
                    format = "yyyy-mm-dd"),
      # Submit Button
      actionButton("getData", "Get Time Series",
                  class = "btn-primary btn-block",
                  style = "margin-top: 20px;")
    )
  ),
  # Main panel with map
  div(class = "map-container",
    leafletOutput("map", height = "100%"),
    # Plot panel
    div(
      id = "plotPanel",
      style = "display: none;",  # Initially hidden
      absolutePanel(
        class = "panel panel-default",
        fixed = TRUE,
        draggable = TRUE,
        top = 60,
        right = 20,
        width = "500px",
        height = "400px",
        style = "background-color: white; padding: 15px; border-radius: 5px; box-shadow: 0 0 10px rgba(0,0,0,0.1);",
        plotlyOutput("timeSeries", width = '100%', height = '350px'),
        actionButton("closePlot", "Close", class = "btn-sm btn-danger")
      )
    )
  )
)

# Server Definition
server <- function(input, output, session) {
  # Log application start
  log_info("WTSS Explorer application started")
  
  # Reactive values to store drawn features and selected data
  rv <- reactiveValues(
    drawnFeatures = NULL,
    timeSeriesData = NULL
  )
  
  # Dynamic band selector based on selected product
  output$bandSelector <- renderUI({
    req(input$product)
    log_info("User selected product: {input$product}")
    collection_info <- client$get_collection_info(input$product)
    band_names <- sapply(collection_info$bands, function(x) x$name)
    
    # Allow single selection if summariseGeometry is checked
    selectInput("bands", "Select Band/Index",
                choices = band_names,
                selected = band_names[1],
                multiple = !input$summariseGeometry)
  })
  
  # Initialize the map
  output$map <- renderLeaflet({
    log_info("Initializing map view")
    leaflet() %>%
      addTiles() %>%
      addProviderTiles(providers$OpenStreetMap, group = "OpenStreetMap") %>%
      addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>%
      addLayersControl(
        baseGroups = c("OpenStreetMap", "Satellite"),
        options = layersControlOptions(collapsed = TRUE)
      ) %>%
      addDrawToolbar(
        targetGroup = 'Features',
        editOptions = editToolbarOptions(),
        polylineOptions = FALSE,
        rectangleOptions = TRUE,
        circleOptions = TRUE,
        markerOptions = TRUE,
        polygonOptions = TRUE
      ) %>%
      setView(lng = -51.9253, lat = -14.2350, zoom = 4)
  })
  
  # Handle drawn features
  observeEvent(input$map_draw_new_feature, {
    feature_type <- input$map_draw_new_feature$properties$feature_type
    log_info("User drew a new {feature_type} on the map")
    rv$drawnFeatures <- input$map_draw_new_feature
  })
  
  # Handle deleted features
  observeEvent(input$map_draw_deleted_features, {
    log_info("User deleted drawn features from the map")
    rv$drawnFeatures <- NULL
    hide("plotPanel")
  })
  
  # Log band selection changes
  observeEvent(input$bands, {
    log_info("User selected bands: {paste(input$bands, collapse=', ')}")
  })
  
  # Log date range changes
  observeEvent(input$dateRange, {
    log_info("User selected date range: {input$dateRange[1]} to {input$dateRange[2]}")
  })
  
  # Get Time Series Data
  observeEvent(input$getData, {
    req(rv$drawnFeatures, input$product, input$bands, input$dateRange)
    log_info("User requested time series data for product: {input$product}")
    
    # Convert the drawn feature to the required format
    feature_type <- rv$drawnFeatures$properties$feature_type
    coordinates <- rv$drawnFeatures$geometry$coordinates
    
    # Prepare geometry based on feature type
    if (feature_type == "polygon") {
      geom <- list(
        type = "Polygon",
        coordinates = coordinates
      )
    } else if (feature_type == "marker") {
      geom <- list(
        type = "Point",
        coordinates = coordinates
      )
    } else if (feature_type == "circle") {
      center <- coordinates
      radius <- rv$drawnFeatures$properties$radius
      # Convert circle to polygon points
      points <- lapply(seq(0, 360, by = 10), function(angle) {
        x <- center[[1]] + radius * cos(angle * pi / 180)
        y <- center[[2]] + radius * sin(angle * pi / 180)
        c(x, y)
      })
      # Close the polygon
      points[[length(points) + 1]] <- points[[1]]
      geom <- list(
        type = "Polygon",
        coordinates = list(points)
      )
    }
    # Get time series data
    tryCatch({
      log_info("Requesting time series data from WTSS service")

      if (input$summariseGeometry) {
        # Use summarize_get for summarized geometry
        summary_data <- client$summarize_get(
          collectionId = input$product,
          geom = geom,
          attributes = input$bands,
          start_datetime = paste0(format(input$dateRange[1], "%Y-%m-%d"), "T00:00:00Z"),
          end_datetime = paste0(format(input$dateRange[2], "%Y-%m-%d"), "T00:00:00Z")
        )
        
        # Access the values dynamically
        band_name <- input$bands  # Get the dynamic band name
        
        df <- parseSummaryDataToDF(summary_data)


        rv$timeSeriesData <- data.frame(
          date = df[['date']], 
          max = df[[paste0(band_name, '_max')]],
          min = df[[paste0(band_name, '_min')]],
          avg = df[[paste0(band_name, '_mean')]]
        )

        plot <- plot_ly(data = rv$timeSeriesData, x = ~date) %>%
          add_trace(y = ~max, name = "Max", type = "scatter", mode = "lines") %>%
          add_trace(y = ~min, name = "Min", type = "scatter", mode = "lines") %>%
          add_trace(y = ~avg, name = "Average", type = "scatter", mode = "lines") %>%
          layout(title = "Summarized Time Series Data",
                xaxis = list(title = "Date"),
                yaxis = list(title = "Values"))

      } else {

        response <- client$timeseries_get(
          collectionId = input$product,
          geom = geom,
          attributes = input$bands,
          start_datetime = paste0(format(input$dateRange[1], "%Y-%m-%d"), "T00:00:00Z"),
          end_datetime = paste0(format(input$dateRange[2], "%Y-%m-%d"), "T00:00:00Z")
        )
        
        # Parse and store the data
        rv$timeSeriesData <- parseGetTimeSeriesToDF(response)

        plot <- plot_ly()
        for(band in input$bands) {
          plot <- plot %>% add_trace(
            data = rv$timeSeriesData,
            x = ~date,
            y = as.formula(paste0("~", band)),
            name = band,
            type = "scatter",
            mode = "lines"
          )
        }

      }

      log_info(paste0("Successfully retrieved and parsed time series data", ", columns:", paste(names(rv$timeSeriesData), collapse=';')))
      
      # Show the plot panel
      show("plotPanel")
      log_info("Displaying time series plot")

      output$timeSeries <- renderPlotly({
        plot
      })

    }, error = function(e) {
      log_error("Error getting time series data: {e$message}")
      showNotification(
        paste("Error getting time series data:", e$message),
        type = "error"
      )
    })
  })
  
  # Close plot panel
  observeEvent(input$closePlot, {
    log_info("User closed the time series plot")
    hide("plotPanel")
  })
  
  # Log when session ends
  session$onSessionEnded(function() {
    log_info("User session ended")
  })
}

# Run the application
shinyApp(ui = ui, server = server) 