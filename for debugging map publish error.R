library("rgdal")
library("lubridate")
library("plyr")
library("dplyr")
library("ggplot2")
library("leaflet")
library("stringr")
library("RColorBrewer")
library("httr")
library("jsonlite")
load(file = "flickr_values.RData") # to get user_id and api_key for flickr calls

spain <- map_trip(NULL, "2014 Pyrenees", use_api_key = api_key, use_user_id = user_id,
                  album_name = "2014 Catalonia",
                  add_camera_gps = FALSE, adjust_camera_time = -60 * 60)
spain_df <- data_frame(area = c("Spain"), trip = c("Pyrenees"), year = c(2014),
                       bbox11 = spain$trip_bbox[1, 1], bbox12 = spain$trip_bbox[1, 2],
                       bbox21 = spain$trip_bbox[2, 1], bbox22 = spain$trip_bbox[2, 2])

# load("spain before add photos.RData")
# spain$map <- map_photos(spain$map, "Other Spain", use_api_key = api_key, use_user_id = user_id) %>%
#   addLayersControl(baseGroups = c("Topographical"),
#                    overlayGroups = c("Hiking routes", "Photo markers"),
#                    options = layersControlOptions(collapsed = FALSE)) 
save(spain, spain_df, file = "spain debug leaflet.RData")