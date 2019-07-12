
library("rgdal")
library("lubridate")
library("plyr")
library("dplyr")
library("ggplot2")
library("leaflet")
library("stringr")
library("RColorBrewer")

add_gpx_to_leaflet <- function(GPX_file, m = NULL, color = "blue", cumbbox = NULL,
                               hike_start = NULL, hike_end = NULL, limit_map = FALSE,
                               height = "700px", width = NULL) {
#   if (limit_map) {
#     # get coordinates so I can limit leaflet map
#     wp <- readOGR(GPX_file, layer = "track_points") # returns SpatialPointsDataFrame object
#     wpdf <- as.data.frame(wp)
#     # e.g., hike_start <- ymd_hms("2014-10-15 04:29:07 UTC")
#     if (!is.null(hike_start)) wpdf <- subset(wpdf, wp$time >= hike_start)
#     if (!is.null(hike_end)) wpdf <- subset(wpdf, wp$time <= hike_end)
#     
#     lat_max <- max(wpdf$coords.x2)
#     long_max <- max(wpdf$coords.x1)
#     lat_min <- min(wpdf$coords.x2)
#     long_min <- min(wpdf$coords.x1)
#   }
  # changed June 12th to no longer accumulate bbox here. Do it in map_trip.f
  track <- readOGR(GPX_file, layer = "tracks", verbose = FALSE)
  if (is.null(m)) {
    m <- leaflet(height = height, width = width) %>%
      # Add tiles
      # addProviderTiles("Thunderforest.Landscape", group = "Topographical") %>%    # old code before Thunderforest API
      addTiles("https://{s}.tile.thunderforest.com/landscape/{z}/{x}/{y}.png?apikey=452dc06e5d1947b7b4e893535d0e6b36", group = "Topographical") %>%
      addTiles("https://{s}.tile.thunderforest.com/outdoors/{z}/{x}/{y}.png?apikey=452dc06e5d1947b7b4e893535d0e6b36", group = "Outdoors") %>%
      addProviderTiles("OpenStreetMap.Mapnik", group = "Road map") %>%
      addProviderTiles("Esri.WorldImagery", group = "Satellite")
      
  }
  else {print("In add_gpx_to_leaflet.R and map is not NULL.")}
  m <-  addPolylines(m, data=track, group='Hiking routes', color = color)
  #if (limit_map) m <- fitBounds(m, long_min, lat_min, long_max, lat_max)
  list(map = m, this_bbox = bbox(track))
}

# test: m <- add_gpx_to_leaflet("/Users/john_imac/Dropbox/Mapping Info-iDisk/Track Archive/2014 Pyrenees/Track_2014-10-19 ESPOT1.gpx", height = "200px")

# test: m <- add_gpx_to_leaflet("/Users/johng/Dropbox/Mapping Info-iDisk/Track Archive/Pyrenees/Track_2014-10-20 ESPOT2.gpx")

# test: GPX_file <- "/Users/johng/Dropbox/Mapping Info-iDisk/Track Archive/2014 Pyrenees/Track_2014-10-20 ESPOT2.gpx"

# test: m <- add_gpx_to_leaflet("~/Dropbox/Mapping Info-iDisk/Track Archive/Pyrenees/Track_2014-10-19 ESPOT1.gpx")
