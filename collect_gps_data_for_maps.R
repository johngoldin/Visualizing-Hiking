
library(shiny)
library(leaflet)
library("rgdal")
library("lubridate")
library("plyr")
library("dplyr")
#library("ggplot2")
library(RColorBrewer)
library("stringr")
library("purrr")
library("httr")
library("jsonlite")
load(file = "flickr_values.RData") # to get user_id and api_key for flickr calls

source("combine_gpx.R")
source("photoIcon.R")
source("flickr_calls.R")
source("collect_photos_from_album.R")
source("add_trips_to_map.R") # need this to get pad_description()

trips_df <- data_frame(trip = c("Hadrians Wall",         "Coast to Coast",        "Pennine Way",     "Pennine Way",    
                               "SWCP", "Wales",                 "Cotswolds",             "Amsterdam",            
                               "Bryce",                 "Grand Canyon",          "Grand Canyon",          "Yosemite",             
                               "Tucson",                "Phoenix",               "Pyrenees",              "Cabo de Gata",         
                               "Andorra",               "Cadaques",              "Madrid",                "Andalusia"),
                       year = c(2011, 2012, 2014, 2013, 2015, 2012, 2012, 2011, 2013, 2016, 2013, 2013, 2014, 2015, 2014, 2015, 2014, 2014, 2015, 2015),
                       area = c("England", "England", "England", "England", "England", "England", "England", "England", "USA",     "USA",    
                        "USA",     "USA",     "USA",     "USA",     "Spain",   "Spain",   "Spain",   "Spain",   "Spain",   "Spain"),
                       bbox11 = vector("double", 20), bbox12 = vector("double", 20),  
                       bbox21 = vector("double", 20), bbox22 = vector("double", 20),
                       gpx_name = vector("character", 20),
                       album_name = vector("character", 20),
                       add_camera_gps = FALSE,
                       adjust_camera_time = c(5 , -1 , 0 , -1 ,  -1 ,  -1 , 4 , 5 ,
                                             -1 ,  7 , 4 , 7 , 7 , 6 ,
                                              -1 , -2 , -1 , -1 , -2 , -2 )
)
trips_df$adjust_camera_time <- trips_df$adjust_camera_time * 60 * 60 # translate from hours to seconds
trips_df$gpx_name = paste(trips_df$year, trips_df$trip)
trips_df$album_name = paste(trips_df$year, trips_df$trip)
trips_df$album_name[trips_df$trip == "Coast to Coast"] <- "Coast to Coast"
trips_df$album_name[trips_df$trip == "South West Coast Path"] <- "2015 SWCP"
trips_df$album_name[trips_df$album_name == "2013 Bryce"] <- "2013 Utah"
trips_df$album_name[trips_df$album_name == "2013 Grand Canyon"] <- "2013 Utah"
trips_df$trip[(trips_df$trip == "Pennine Way") & (trips_df$year == 2013)] <- "Pennine Way south"
trips_df$trip[(trips_df$trip == "Pennine Way") & (trips_df$year == 2014)] <- "Pennine Way north"
# This last trip was saved via myTracks at a rate one point per second (17,775 points) so loads very slowly
# I neeed to deal with this as a separate issue
# all_england <- map_trip(all_england, "2015 SWCP last day", album = "2015 SWCP", 
#                         use_api_key = api_key, use_user_id = user_id, 
#                         adjust_camera_time = -60 * 60)  #Save the bounding box after these three trips:
# swcp_bbox <- revise_bbox(swcp_bbox, england_trips$trip_bbox)

# Next we have to get the gps info for the trips in trips_df
trips_list <- vector("list", length(trips_df$trip))
trip_photos_list <- vector("list", length(trips_df$trip))
colors_list <- vector("list", length(trips_df$trip))
for (i in seq_along(trips_df$trip)) {
  cat("Loop %f  %s\n", i,trips_df$gpx_name[i] )
  # this is what combine_gpx returns:
  # list(day_names = file_names, trip_photos = trip_photos, 
  #      traces_list = traces_list, traces_color = traces_color, trip_bbox = trip_bbox)
    a_trip <- combine_gpx(trips_df$gpx_name[i], adjust_camera_time = trips_df$adjust_camera_time[i],
                      album_nam = trips_df$album_name[i],
                      use_api_key = api_key, use_user_id = user_id,
                      area = trips_df$area[i])
    trips_list[[i]] <- a_trip$traces_list
    colors_list[[i]] <- a_trip$traces_color
    trip_photos_list[[i]] <- a_trip$trip_photos
    trips_df$bbox11[i] <- a_trip$trip_bbox[1, 1]
    trips_df$bbox12[i] <- a_trip$trip_bbox[1, 2]
    trips_df$bbox21[i] <- a_trip$trip_bbox[2, 1]
    trips_df$bbox22[i] <- a_trip$trip_bbox[2, 2]
}
trips_df$bbox11[trips_df$trip == "Coast to Coast"] <- -3.626556
trips_df$bbox12[trips_df$trip == "Coast to Coast"] <- -1.738667

trip_photos_df <- bind_rows(trip_photos_list) %>%
  select(id, secret, title, description, datetaken,
                        latitude, longitude, url_m, height_m, width_m,
                        url_s, height_s, width_s, album_id, include, lng, lat, area) %>%
  filter(include)

other_photos <- map2(c("Other USA", "Other UK", "Other Spain"), c("USA", "England", "Spain"), collect_photos_from_album, use_api_key = api_key, use_user_id = user_id)
other_photos_df <- bind_rows(other_photos) %>%
  filter(!(id %in% trip_photos_df$id), !is.na(longitude), !is.na(latitude))
other_photos_df$lat <- as.numeric(other_photos_df$latitude)
other_photos_df$lng <- as.numeric(other_photos_df$longitude)


all_photos_df <- bind_rows(trip_photos_df, other_photos_df)
all_photos_df$description <- map_chr(all_photos_df$description, pad_description)
#  photos_in_album_url(user_id, photos_df$album_id, photos_df$id),
# Next apply photos_in_album_url function to album_id and id to get url popup.
all_photos_df$photo_in_album <- map2_chr(all_photos_df$album_id, all_photos_df$id, photos_in_album_url, user_id)


save(trips_df, trips_list, all_photos_df, colors_list, file = "trace and photo info for trips.RData")
file.remove("saved_map.RData")  # need to remove the cached map

