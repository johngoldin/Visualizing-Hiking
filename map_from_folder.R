
library("rgdal")
library("lubridate")
library("plyr")
library("dplyr")
library("ggplot2")
library("leaflet")
#library("exif")
library("stringr")

gpx_folder <- "~/Dropbox/Mapping Info-iDisk/Track Archive/Bryce/"
# map_from_folder("~/Dropbox/Mapping Info-iDisk/Track Archive/Yosemite/")
# pw_south_map <- map_from_folder("~/Dropbox/Mapping Info-iDisk/Track Archive/PW-south/")
# pw_north_map <- map_from_folder("~/Dropbox/Mapping Info-iDisk/Track Archive/PW-north/")
# SWCP_map <- map_from_folder("~/Dropbox/Mapping Info-iDisk/Track Archive/SWCP/")
# east_rock_map <- map_from_folder("~/Dropbox/Mapping Info-iDisk/Track Archive/East Rock/")
# andalusia_map <- map_from_folder("~/Dropbox/Mapping Info-iDisk/Track Archive/Andalusia/")
# pyrenees_map <- map_from_folder("~/Dropbox/Mapping Info-iDisk/Track Archive/Pyrenees/")

# combined <- map_from_folder(c("2013 Pennine Way", "2014 Pennine Way", "2015 SWCP"))

map_from_folder <- function (gpx_folder, folder_label = "Hiking Trails", base_path = "~/Dropbox/Mapping Info-iDisk/Track Archive/") {
  # gpx_folder is either a single folder name or a vector of folders
  # map all of the tracks in gpx_folder. If it's a list, maps the tracks in all of tose folders.
  if (!is.vector(gpx_folder)) gpx_folder <- as.vector(gpx_folder)
  m <- NULL
  for(j in 1:length(gpx_folder)) {
    if (!str_detect(gpx_folder[j], "/")) gpx_folder[j] <- paste(base_path, gpx_folder[j], "/", sep = "")
    # need path.expand because readOGR doesn't expand ~ in file path
    gpx_files <- path.expand(paste(gpx_folder[j], list.files(gpx_folder[j]), sep =""))
    gpx_files <- subset(gpx_files, str_detect(gpx_files, "\\.gpx"))
    gpx_files <- gpx_files[order(gpx_files)]
    
    for(i in 1:length(gpx_files)) {
      m <- add_gpx_to_leaflet(gpx_files[i], m, brewer.pal(3, "YlOrRd")[(i %% 2) + 2])
    }
    if (is.null(m)) {
      print("gpx files not found.")
      return(NULL)
    }
  }
  # Layers control
  m <- addLayersControl(m,
    # baseGroups = c("Topographical", "Road map", "Satellite"),
    baseGroups = c("Landscape", "Satellite", "Terrain", "Road Map"),

    overlayGroups = c("Hiking routes"),
    #overlayGroups = c("Hiking routes", "Photo markers"),
    options = layersControlOptions(collapsed = FALSE))
  
  # 
  # addLegend(position = 'bottomleft',opacity = 0.4, 
  #           colors = c('blue', 'red'), 
  #           labels = c('Monterosso-Corniglia (23/09)',
  #                      'Corniglia-Vernazza (24/09)'),
  #           title = 'Hikes Italy, Cinque Terre') %>%
  m
}
  