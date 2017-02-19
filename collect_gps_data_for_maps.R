# collect_gps_data_for_maps.R

# To add a new trip, I need to edit and run this file before gong to app.R to 
# create the new Shiny app.

# To check what value might work for the adjustment for the camera time, use
# camera_gps_time_difference("2016 Phoenix", "2016-12-26 GRANITE MTN WALK.GPX", filter_by_time = FALSE) 


library("rgdal")
library(sp)
library("httr")
library("jsonlite")
library(tidyverse)
# ggplot2, for data visualisation.
# dplyr, for data manipulation.
# tidyr, for data tidying.
# readr, for data import.
# purrr, for functional programming.
# tibble, for tibbles, a modern re-imagining of data frames.
library(RColorBrewer)
library("stringr")
library("lubridate")
library(shiny)
library(leaflet)
load(file = "flickr_values.RData") # to get user_id and api_key for flickr calls

source("combine_gpx.R")
source("photoIcon.R")
source("flickr_calls.R")
source("collect_photos_from_album.R")
source("add_trips_to_map.R") # need this to get pad_description()

saved_trip_data <- TRUE
# If you need to re-run the data for a trip, make sure the trip file is removed from 
# the folder saved_trip_data.  The trips are saved because it makes re-running things
# much, much faster. 

ntrips <- 24 # all of the lists below must be this long
trips_df <- data_frame(trip = c("Hadrians Wall",         "Coast to Coast",        "Pennine Way",     "Pennine Way",    
                               "SWCP", "Wales",                 "Cotswolds",             "Amsterdam",            
                               "Bryce",                 "Grand Canyon",          "Grand Canyon",          "Yosemite",             
                               "Tucson",                "Phoenix",               "Phoenix", "Florida", "Pyrenees",              "Cabo de Gata",         
                               "Andorra",               "Cadaques",              "Madrid",                "Andalusia", "Amalfi Coast", "Rome"),
                       year = c(2011, 2012, 2014, 2013, 2015, 2012, 2012, 2011, 2013, 2016, 2013, 2013, 2014, 2015, 2016, 2016, 2014, 2015, 2014, 2014, 2015, 2015, 2016, 2016),
                       area = c("England", "England", "England", "England", "England", "England", "England", "England", "USA",     "USA",    
                        "USA",     "USA",     "USA",     "USA",     "USA", "USA", "Spain",   "Spain",   "Spain",   "Spain",   "Spain",   "Spain", "Italy", "Italy"),
                       bbox11 = vector("double", ntrips), bbox12 = vector("double", ntrips),  
                       bbox21 = vector("double", ntrips), bbox22 = vector("double", ntrips),
                       gpx_name = vector("character", ntrips),
                       album_name = vector("character", ntrips),
                       add_camera_gps = FALSE,
                       adjust_camera_time = c(5 , -1 , 0 , -1 ,  -1 ,  -1 , 4 , 5 ,
                                             -1 ,  7 , 4 , 7 , 7 , 6 , 7, 5,
                                              -1 , -2 , -1 , -1 , -2 , -2, -1, -2)
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
trips_df$album_name[trips_df$album_name == "2016 Rome"] <- "2016 Italy"
trips_df$album_name[trips_df$album_name == "2016 Amalfi Coast"] <- "2016 Italy"

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
  # this is what combine_gpx returns:
  # list(day_names = file_names, trip_photos = trip_photos, 
  #      traces_list = traces_list, traces_color = traces_color, trip_bbox = trip_bbox)
  if (saved_trip_data && file.exists(paste0("saved_trip_data/", trips_df$gpx_name[i], ".RData"))) {
    cat(sprintf("Loading from saved file: %3.f  %s\n", i,trips_df$gpx_name[i] ))
    load(paste0("saved_trip_data/", trips_df$gpx_name[i], ".RData"))
  } else {
    cat(sprintf("Creating map data for trip: %f  %s\n", i,trips_df$gpx_name[i] ))
    a_trip <- combine_gpx(trips_df$gpx_name[i], adjust_camera_time = trips_df$adjust_camera_time[i],
                          album_nam = trips_df$album_name[i],
                          use_api_key = api_key, use_user_id = user_id,
                          area = trips_df$area[i])
    save(a_trip, file = paste0("saved_trip_data/", trips_df$gpx_name[i], ".RData"))
  }
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

