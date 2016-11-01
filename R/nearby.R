## The nearby function - aim is to make it easier to
## get locations when you don't have a street address
#' @export
#' @examples 
#' \dontrun{ # Server response can be slow; this cuts down check time.
#' register_google(key=APIkey)
#' adelaide.cbd <- geocode("Victoria Square, Adelaide, Australia")
#' h<-nearbyplaces(as.numeric(adelaide.cbd), "hospital", "Royal Adelaide Hospital Emergency Department", radius=2000)
#' 
#' For this project I might be better off manually looking up placeIDs
#' }
nearbyplaces <- function(location,  placetype, placename, radius=50000,
                         output = c("latlon", "latlona", "more", "all"),
                         messaging = FALSE,
                         urlonly = FALSE,
                         override_limit = FALSE,
                         ext = "com", inject = "", ...
)
{
  stopifnot(is.numeric(location) && length(location) == 2)
  stopifnot(is.logical(messaging))
  output   <- match.arg(output)

  type <- placetype
  name <- URLencode(placename, reserved=TRUE)
  location <- paste(rev(location), collapse = ',')

  posturl <- paste(
    fmteq(location), fmteq(radius), fmteq(type), fmteq(name),
    sep = "&"
  )
  
  ## Only uses api key
  if (has_goog_key()) {
    key <- goog_key()
    posturl <- paste(posturl, fmteq(key), sep = "&")
  }
  
  # add to url
  url_string <- paste0(
    sprintf("https://maps.googleapis.%s/maps/api/place/nearbysearch/json?", ext),
    posturl
  )
  if (urlonly)return(url_string)
  
  connect <- url(url_string); on.exit(close(connect), add = TRUE)
  rgc <- fromJSON(paste(readLines(connect), collapse = ''))
  if(output == "all") return(rgc)
  
  return(rgc)
}