## Google API Geocode & Distance Call
## Used for submitting http requests to the API
library(RCurl)

## Used for processing the results returned in JSON format
library(RJSONIO)

## A function to construct a URL for querying Google's geocoding API
## At a minimum, the API requires two parameters, reflected in this function
## 1. A text address
## 2. The format of the data to be returned. JSON by default. Option is XML
construct.geocode.url <- function(u_address, return.call = "json") {
  ## The base API URL
  root <- "https://maps.googleapis.com/maps/api/geocode/"
  ## Join the components of the url together
  u <- paste(root, return.call, "?address=", u_address, sep = "")
  ## Return an encoded URL string
  return(URLencode(u))
}

## A function to construct a URL for querying Google's distance matrix API
## At a minimum, the API requires three parameters
## A 'from' address, 
## A 'to' address, and  
## The format to be returned. JSON by default. Option is XML
## There are optional parameters, which may be useful
## Units (metric, imperial. Default metric)
## Mode of transport (driving, walking, bicycling, transit. Default driving)
construct.distance.url <- function(u_from, u_to, return.call = "json", u_units = "metric", u_mode = "driving") {
  ## The base API URL
  root <- "https://maps.googleapis.com/maps/api/distancematrix/"
  ## Join the components of the url together
  u <- paste(root, return.call, "?origins=", u_from, "&destinations=", u_to, "&mode=", u_mode, "&units=", u_units, sep = "")
  ## Return an encoded URL string
  return(URLencode(u))
}

## This function queries Google's geocoding API
gGeoCode <- function(address, return.type = "json") {
  ## Call the function we made earlier to build a URL string
  u <- construct.geocode.url(address, return.type)
  ## Request information from the API
  doc <- getURL(u)
  ## Parse the JSON into a structured format
  x <- fromJSON(doc,simplify = FALSE)
  ## Check the response status from the API
  if(x$status=="OK") {
    ## Extract the latitude and longitude coordinates
    lat <- x$results[[1]]$geometry$location$lat
    lng <- x$results[[1]]$geometry$location$lng
    return(paste(lat, lng, sep = ", "))
  } else {
    ## Return NAs
    return(paste(NA, NA, sep = ", "))
  }
}

## When given an origin and destination, this function will return the distance and time between the locations according to the mode of transport specified.
## Ths function queries Google's distance matrix API
gDistanceTime <- function(from, to, return.type = "json", units = "metric", mode = "driving") {
  ## Call the function to construct a distance matrix URL
  u <- construct.distance.url(from, to, return.type, units, mode)
  ## Request information from the API
  doc <- getURL(u)
  ## Parse the JSON into a structured format
  x <- fromJSON(doc,simplify = FALSE)
  ## Check the response status from the API  
  if(x$status=="OK") {
    # Extract the distance (m) and time (seconds)
    txt.distance <- x$rows[[1]]$elements[[1]]$distance$text
    txt.time <- x$rows[[1]]$elements[[1]]$duration$text
    val.distance <- x$rows[[1]]$elements[[1]]$distance$value
    val.time <- x$rows[[1]]$elements[[1]]$duration$value
    return(paste(txt.distance, " (", val.distance, "m) and ", txt.time, " (", val.time, "s)", sep = ""))
  } else {
    ## Return NAs
    return(c(NA,NA))
  }
}

fromAddr <- "7474 Gannon Saint Louis, MO 63110"
fromCoord <- gGeoCode(fromAddr)
print(fromCoord)

toAddr <- "5225 MIDAMERICA PLZ Saint Louis, MO 63129"
toCoord <- gGeoCode(toAddr)
print(toCoord)

toAddr2 <- "4921 PARKVIEW PL Saint Louis, MO 63110"
toCoord <- gGeoCode(toAddr)
print(toCoord)

## Return driving distance and time 
gDistanceTime(fromAddr, toAddr, return.type = "json", mode = "driving")
gDistanceTime(fromAddr, toAddr2, return.type = "json", mode = "driving")
