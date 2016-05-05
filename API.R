library(httr)
library(httpuv)
library(jsonlite)
library(base64enc)
library(dplyr)

consumerKey = "1sHLetbrWRuoOWbAHeK2JN6IATsrOwE1"

HitMap_hotels <- function(checkIn, checkOut) {
  scheme <- "http"
  host <- "api.sandbox.amadeus.com/v1.2"
  
  # Build API URL.
  url <- paste0(scheme, "://", host, "/hotels/search-box?south_west_corner=37.70827%2C%20-122.523994&north_east_corner=37.815615%2C%20-122.35302&check_in=", checkIn, "&check_out=", checkOut, "&number_of_results=150&apikey=", consumerKey)
  
  # Make request.
  results <- data.frame(jsonlite::fromJSON(url, flatten = TRUE))
  
  # Get Json Result
  return(results)
}

#Get request Examples
#result <- GET("http://api.sandbox.amadeus.com/v1.2/hotels/search-circle?latitude=37.793344&longitude=-122.403101&radius=50&check_in=2016-04-28&check_out=2016-04-30&number_of_results=50&apikey=1sHLetbrWRuoOWbAHeK2JN6IATsrOwE1")


