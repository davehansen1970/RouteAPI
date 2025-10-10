library(httr2)
library(tidyverse)

# board <- board_s3(
#   "willy2",
#   region = "us-east-1",
#   access_key = Sys.getenv("S3_WILLY_KEY"),
#   secret_access_key = Sys.getenv("S3_WILLY_SECRET")
# )

full_routes_pre <- read_csv("data/data_raw.csv",
                            col_names = TRUE,
                            col_types = "ccciccc?")

# a list of origins and destinations, using Google's PlaceID format.
# PlaceIDs can be interactively obtained here: https://developers.google.com/maps/documentation/places/web-service/place-id
OD <- list(
  "JND_at_North_Shore" = "EjJKb2huIE5vbGVuIERyICYgTiBTaG9yZSBEciwgTWFkaXNvbiwgV0kgNTM3MDMsIFVTQSJmImQKFAoSCXu5mjo7UwaIEQoMsRKsOd_lEhQKEgl7uZo6O1MGiBEKDLESrDnf5RoUChIJjfKfA-BSBogRYsWXJvaejFUaFAoSCXuIPEYlUwaIEXg83UWw3DMxIgoNpX6rGRXEzrjK",
  "Olbrich_boat_launch" = "ChIJ5TFo0_BTBogRiLW3n_CHw1g",
  "Willy_at_Ingersoll" = "EjZXaWxsaWFtc29uIFN0ICYgUyBJbmdlcnNvbGwgU3QsIE1hZGlzb24sIFdJIDUzNzAzLCBVU0EiZiJkChQKEgl5M0JHcVMGiBGPw2HWAUYh8hIUChIJeTNCR3FTBogRj8Nh1gFGIfIaFAoSCU95EHRxUwaIEWK02CjjGAaNGhQKEgnVLyMUcVMGiBGluqGTTsODeCIKDeLLrRkVHs-7yg",
  "E_Wash_at_Milwaukee" = "EjdFIFdhc2hpbmd0b24gQXZlICYgTWlsd2F1a2VlIFN0LCBNYWRpc29uLCBXSSA1MzcwNCwgVVNBImYiZAoUChIJUc7xKnFUBogRwRIyIhvYO_MSFAoSCVHO8SpxVAaIEcESMiIb2DvzGhQKEglb_GtKEVQGiBGDTq5ArHySmxoUChIJ384s9rVWBogRKuqY4yFj-BEiCg2_lLAZFXd5vso",
  "E_Wash_at_First" = "EjVFIFdhc2hpbmd0b24gQXZlICYgTiBGaXJzdCBTdCwgTWFkaXNvbiwgV0kgNTM3MDQsIFVTQSJmImQKFAoSCeMnhbmBUwaIEYkYcoZtUER5EhQKEgnjJ4W5gVMGiBGJGHKGbVBEeRoUChIJW_xrShFUBogRg06uQKx8kpsaFAoSCcmDLdWBUwaIEbdQz9fubXyjIgoNklqvGRUY4LzK",
  "Wilson_at_Willy" = "EjNFIFdpbHNvbiBTdCAmIFdpbGxpYW1zb24gU3QsIE1hZGlzb24sIFdJIDUzNzAzLCBVU0EiZiJkChQKEgnhym9La1MGiBGcY9ohN_QfmhIUChIJ4cpvS2tTBogRnGPaITf0H5oaFAoSCfVVbLltUwaIEeqhwVpfun4hGhQKEglPeRB0cVMGiBFitNgo4xgGjSIKDdbmrBkVlla6yg",
  "Eastwood_at_Winnebago" = "EjJFYXN0d29vZCBEciAmIFdpbm5lYmFnbyBTdCwgTWFkaXNvbiwgV0kgNTM3MDQsIFVTQSJmImQKFAoSCeNrPLqDUwaIEffZcIG93NP9EhQKEgnjazy6g1MGiBH32XCBvdzT_RoUChIJ9SfChYZTBogRf5DI8zHQBHYaFAoSCVkZZw6HUwaIESCTKdLOYzU9IgoNnNiuGRW5G73K",
  "JND_at_Rimrock" = "EjJKb2huIE5vbGVuIERyICYgUmltcm9jayBSZCwgTWFkaXNvbiwgV0kgNTM3MTMsIFVTQSJmImQKFAoSCfv79lbvUgaIEd-bxOlXat-ZEhQKEgn7-_ZW71IGiBHfm8TpV2rfmRoUChIJjfKfA-BSBogRYsWXJvaejFUaFAoSCUkcKUuVUgaIEf66KRm-lS1iIgoN02eoGRXbjLrK"
)


# main function to obtain route
get_route <- function(origin, destination, intermediate = NULL) {
  #Define the request body as a list, depending on whether an intermediate is given
  if (is.null(intermediate)) {
    request_body <- list(
      origin = list(placeId = OD[[origin]]),
      destination = list(placeId = OD[[destination]]),
      travelMode = "DRIVE",
      routingPreference = "TRAFFIC_AWARE",
      computeAlternativeRoutes = FALSE,
      languageCode = "en-US",
      units = "IMPERIAL"
    )
  } else {
    request_body <- list(
      origin = list(placeId = OD[[origin]]),
      destination = list(placeId = OD[[destination]]),
      intermediates = list(placeId = OD[[intermediate]]),
      travelMode = "DRIVE",
      routingPreference = "TRAFFIC_AWARE",
      computeAlternativeRoutes = FALSE,
      languageCode = "en-US",
      units = "IMPERIAL"
    )
  }
  
  
  # Built the API request
  response <-
    request("https://routes.googleapis.com/directions/v2:computeRoutes") |>
    req_method("POST")  |>
    req_headers(
      "Content-Type" = "application/json",
      "X-Goog-Api-Key" = Sys.getenv("ROUTES_API_KEY"),
      "X-Goog-FieldMask" = "routes.duration,routes.description,routes.staticDuration,routes.distanceMeters,routes.polyline.encodedPolyline"
    ) %>%
    req_body_json(request_body) |>
    req_perform()
  
  # Parse the response
  result <- response %>% resp_body_json()
  
  routes <- result$routes[[1]]
  # Return the results as named list
  return(
    tibble(
      origin = origin,
      destination = destination,
      intermediate = ifelse(is.null(intermediate), NA_character_, intermediate),
      distance = routes$distanceMeters,
      duration = routes$duration,
      static_duration = routes$staticDuration,
      polyline = routes$polyline$encodedPolyline,
      request_time = Sys.time()
    )
  )
}

rimrock_hairball <- get_route(origin = "JND_at_Rimrock",
                              destination = "Wilson_at_Willy",
                              intermediate = "JND_at_North_Shore")
hairball_rimrock <- get_route(origin = "Wilson_at_Willy",
                              destination = "JND_at_Rimrock",
                              intermediate = "JND_at_North_Shore")

full_routes <- bind_rows(
  full_routes_pre,
  rimrock_hairball,
  hairball_rimrock
)

full_routes |> write_csv(file = "data/data_raw.csv")
