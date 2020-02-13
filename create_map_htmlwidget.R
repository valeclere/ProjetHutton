library(htmlwidgets)
library(leaflet)


mapwid <- htmlwidgets::createWidget(
  'leaflet',
  structure(
    list(options = options),
    leafletData = data
  ),
  width = 650, height = 400,
  sizingPolicy = htmlwidgets::sizingPolicy(
    defaultWidth = '100%',
    defaultHeight = 400,
    padding = 0,
    browser.fill = TRUE
  ),
  preRenderHook = function(widget) {
    if (!is.null(widget$jsHooks$render)) {
      widget$jsHooks$render <- lapply(widget$jsHooks$render, function(hook) {
        if (is.list(hook)) {
          hook$code <- sprintf(hookWrapperTemplate, paste(hook$code, collapse = "\n"))
        } else if (is.character(hook)) {
          hook <- sprintf(hookWrapperTemplate, paste(hook, collapse = "\n"))
        } else {
          stop("Unknown hook class ", class(hook))
        }
        hook
      })
    }
    widget
  }
)

# map2 <- leafletMap(outputId = "mymap",
#            width = "400px", 
#            height = "400px",
#            initialTileLayer = 'https://maps.googleapis.com/maps/api/js?v=3.exp&key=AIzaSyAQvaBc5_RruTllCvOxy3i9YNFYlaDzaJ8')
# 
# leafl

# mapwid
###############################
# dispatch()
# invokeMethod()
# getMapData()
###############################
# dispatch <- function(map,
#                      funcName,
#                      leaflet = stop(paste(funcName, "requires a map proxy object")),
#                      leaflet_proxy = stop(paste(funcName, "does not support map proxy objects"))
# ) {
#   if (inherits(map, "leaflet"))
#     return(leaflet)
#   else if (inherits(map, "leaflet_proxy"))
#     return(leaflet_proxy)
#   else
#     stop("Invalid map parameter")
# }
##
# invokeMethod <- function(map, data, method, ...) {
#   if (crosstalk::is.SharedData(data)) {
#     map$dependencies <- c(map$dependencies, crosstalk::crosstalkLibs())
#     data <- data$data()
#   } else {
#     NULL
#   }
#   args = evalFormula(list(...), data)
#   
#   dispatch(map,
#            method,
#            leaflet = {
#              x = map$x$calls
#              if (is.null(x)) x = list()
#              n = length(x)
#              x[[n + 1]] = list(method = method, args = args)
#              map$x$calls = x
#              map
#            },
#            leaflet_proxy = {
#              invokeRemote(map, method, args)
#              map
#            }
#   )
# }
##
###############################
# getMapData <- function(map) {
#   attr(map$x, "leafletData", exact = TRUE)
# }
###############################
# tileOptions
#################################
# https://github.com/rstudio/leaflet/blob/d6918ce3789a1742dbde80eca84c1fc860a66faf/R/layers.R#L151-L167
##################################
addKillianTiles <- function(
  map,
  attribution = NULL,
  layerId = NULL,
  group = NULL,
  options = tileOptions()
) {
  options$attribution = attribution
  # if (missing(urlTemplate) && is.null(options$attribution))
  #   options$attribution = paste(
  #     '&copy; <a href="http://openstreetmap.org">OpenStreetMap</a>',
  #     'contributors, <a href="http://creativecommons.org/licenses/by-sa/2.0/">CC-BY-SA</a>'
  #   )
  invokeMethod(map, getMapData(map), 'addGoogleLayer', layerId, group,
               options)
}


