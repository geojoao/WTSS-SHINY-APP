library(httr)
library(jsonlite)
library(R6)
library(logger)

# Helper function to log request details
log_request_details <- function(method, url, params = NULL, payload = NULL) {
  log_info("API Request - Method: {method}, URL: {url}")
  if (!is.null(params)) {
    params_str <- jsonlite::toJSON(params, auto_unbox = TRUE)
    log_info("Request Parameters: {params_str}")
  }
  if (!is.null(payload)) {
    payload_str <- jsonlite::toJSON(payload, auto_unbox = TRUE)
    log_info("Request Payload: {payload_str}")
  }
}

# Define the WTSSClient
WTSSClient <- R6::R6Class(
  "WTSSClient",
  public = list(
    base_url = NULL,

    # Initialize with production or development base url
    initialize = function(base_url = "http://brazildatacube.dpi.inpe.br/dev/wtss") {
      self$base_url <- base_url
      log_info("WTSS Client initialized with base URL: {base_url}")
    },

    # Get detailed information about a specific collection
    get_collection_info = function(collection_id) {
      url <- paste0(self$base_url, "/", collection_id)
      log_request_details("GET", url)
      
      response <- GET(url)
      
      if (response$status_code == 200) {
        log_info("Successfully retrieved collection info for: {collection_id}")
        collection <- content(response, as = "parsed")
        
        # Create a formatted output
        info <- list()
        
        # Basic collection information
        info$name <- collection$name
        info$version <- collection$version
        info$description <- collection$description
        info$title <- collection$title
        
        # Data range information
        info$temporal_coverage <- list(
          start = collection$timeline[[1]],
          end = collection$timeline[[length(collection$timeline)]],
          total_dates = length(collection$timeline)
        )
        
        # Band information
        info$bands <- lapply(collection$bands, function(band) {
          list(
            name = band$name,
            common_name = band$common_name,
            data_type = band$data_type,
            scale = band$scale,
            nodata = band$nodata,
            valid_range = c(band$min_value, band$max_value),
            resolution = c(x = band$resolution_x, y = band$resolution_y)
          )
        })
        
        # Spatial coverage
        info$spatial_extent <- collection$extent
        info$projection <- collection$`bdc:crs`
        info$grid_size <- collection$raster_size
        
        return(info)
      } else {
        log_error("Failed to get collection info - Status: {response$status_code}")
        stop("Error: ", response$status_code)
      }
    },

    # Get capabilities in a more readable format
    get_capabilities = function() {
      url <- paste0(self$base_url, "/")
      log_request_details("GET", url)
      
      # Get the landing page content
      landing_page <- self$get_landing_page()
      log_info("Successfully retrieved capabilities")
      
      # Create a list to store the formatted output
      capabilities <- list()
      
      # Add WTSS version
      capabilities$wtss_version <- landing_page$wtss_version
      
      # Process collections
      collections <- list()
      for (link in landing_page$links) {
        if (link$rel == "data") {
          collections[[length(collections) + 1]] <- list(
            name = link$title,
            href = link$href,
            type = link$type
          )
        } else if (link$rel == "service-doc") {
          capabilities$documentation <- list(
            title = link$title,
            href = link$href,
            type = link$type
          )
        }
      }
      
      capabilities$available_collections <- collections
      capabilities$collection_count <- length(collections)
      
      return(capabilities)
    },

    # Landing page endpoint: GET /
    get_landing_page = function() {
      url <- paste0(self$base_url, "/")
      log_request_details("GET", url)
      
      response <- GET(url)
      if (response$status_code == 200) {
        log_info("Successfully retrieved landing page")
        return(content(response, as = "parsed"))
      } else {
        log_error("Failed to get landing page - Status: {response$status_code}")
        stop("Error: ", response$status_code)
      }
    },

    # Summarize using GET: GET /{collectionId}/summarize
    summarize_get = function(collectionId, geom, attributes, start_datetime = NULL,
                             end_datetime = NULL, aggregations = NULL, applyAttributeScale = TRUE,
                             pixelCollisionType = "center", qa = NULL, masked = NULL) {
      url <- paste0(self$base_url, "/", collectionId, "/summarize")
      
      query_params <- list(
        geom = jsonlite::toJSON(geom, auto_unbox = TRUE),
        attributes = paste(attributes, collapse = ","),
        start_datetime = start_datetime,
        end_datetime = end_datetime,
        applyAttributeScale = applyAttributeScale,
        pixelCollisionType = pixelCollisionType
      )
      
      if (!is.null(aggregations)) {
        query_params$aggregations <- paste(aggregations, collapse = ",")
      }
      
      log_request_details("GET", url, query_params)
      
      response <- GET(url, query = query_params)
      if (response$status_code == 200) {
        log_info("Successfully retrieved summary for collection: {collectionId}")
        return(content(response, as = "parsed"))
      } else {
        log_error("Failed to get summary - Status: {response$status_code}")
        stop("Error: ", response$status_code)
      }
    },

    # Summarize using POST: POST /{collectionId}/summarize
    summarize_post = function(collectionId, payload) {
      url <- paste0(self$base_url, "/", collectionId, "/summarize")
      log_request_details("POST", url, payload = payload)
      
      response <- POST(url,
                       body = payload,
                       encode = "json",
                       add_headers(`Content-Type` = "application/json"))
      if (response$status_code == 200) {
        log_info("Successfully posted summary for collection: {collectionId}")
        return(content(response, as = "parsed"))
      } else {
        log_error("Failed to post summary - Status: {response$status_code}")
        stop("Error: ", response$status_code)
      }
    },

    # Time Series using GET: GET /{collectionId}/timeseries
    timeseries_get = function(collectionId, geom, attributes, start_datetime = NULL,
                              end_datetime = NULL, applyAttributeScale = TRUE,
                              pixelCollisionType = "center", pagination = NULL) {
      url <- paste0(self$base_url, "/", collectionId, "/timeseries")
      
      query_params <- list(
        geom = jsonlite::toJSON(geom, auto_unbox = TRUE),
        attributes = paste(attributes, collapse = ","),
        start_datetime = start_datetime,
        end_datetime = end_datetime,
        applyAttributeScale = applyAttributeScale,
        pixelCollisionType = pixelCollisionType
      )
      
      if (!is.null(pagination)) {
        query_params$pagination <- pagination
      }
      
      log_request_details("GET", url, query_params)
      
      response <- GET(url, query = query_params)
      
      if (response$status_code == 200) {
        log_info("Successfully retrieved time series for collection: {collectionId}")
        return(content(response, as = "parsed"))
      } else {
        log_error("Failed to get time series - Status: {response$status_code}")
        stop("Error: ", response$status_code)
      }
    },

    # Time Series using POST: POST /{collectionId}/timeseries
    timeseries_post = function(collectionId, payload) {
      url <- paste0(self$base_url, "/", collectionId, "/timeseries")
      log_request_details("POST", url, payload = payload)
      
      response <- POST(url,
                       body = payload,
                       encode = "json",
                       add_headers(`Content-Type` = "application/json"))
      if (response$status_code == 200) {
        log_info("Successfully posted time series for collection: {collectionId}")
        return(content(response, as = "parsed"))
      } else {
        log_error("Failed to post time series - Status: {response$status_code}")
        stop("Error: ", response$status_code)
      }
    }
  )
)

# Helper function to parse time series response to data frame
parseGetTimeSeriesToDF <- function(json_output) {
  # Parse the JSON output
  res <- json_output
  
  # Check if 'results' field is present
  if (!("results" %in% names(res))) {
    stop("Unexpected response structure: 'results' field missing.")
  }
  
  # Extract the first result (assuming only one result is present)
  result <- res$results[[1]]
  
  # Extract timeline and values
  timeline <- unlist(result$time_series$timeline)
  values <- result$time_series$values
  
  # Create a data frame with the timeline
  df <- data.frame(date = as.Date(timeline), stringsAsFactors = FALSE)
  
  # Add each value vector to the data frame
  for (key in names(values)) {
    df[[key]] <- unlist(values[[key]])
  }
  
  return(df)
}
# Function to parse summary data into a dataframe for all bands
parseSummaryDataToDF <- function(summary_data) {
  # Extract the timeline
  dates <- unlist(summary_data$results$timeline)
  
  # Ensure dates are in the correct format
  dates <- as.Date(dates, format = "%Y-%m-%d")  # Adjust format if necessary
  
  # Initialize a list to hold the data
  data_list <- list(date = dates)
  
  # Loop through each band in the summary data
  for (band_name in names(summary_data$results$values)) {
    # Extract statistics for the current band
    band_stats <- summary_data$results$values[[band_name]]
    
    # Loop through each statistic and add to the data list
    for (stat_name in names(band_stats)) {
      # Create a dynamic column name
      column_name <- paste0(band_name, "_", stat_name)
      # Add the statistic to the data list
      data_list[[column_name]] <- unlist(band_stats[[stat_name]])
    }
  }
  
  # Convert the list to a dataframe
  df <- as.data.frame(data_list)
  
  return(df)
}

# verify if the file is the main file
if (sys.nframe() == 0) {
  # Initialize the WTSS client
  wtss_inpe <- "https://data.inpe.br/bdc/wtss/v4/"
  client <- WTSSClient$new(base_url = wtss_inpe) 
}
