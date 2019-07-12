# add_trips_to_map.R 
# This may be the only sourced file that we need in the app. It loads both trip traces
# and photos.

########################################################################################################################
#
# IMPORTANT!
# If you make changes here, need to run:  
# file.remove("saved_map.RData") 
# Otherwise, won't see changes because app will skipthis and rely on saved_map.RData instead.
#


# Here's the data I have to work with:

# trips_list[[i]] <- a_trip$traces_list
# colors_list[[i]] <- a_trip$traces_color
# trip_photos_list[[i]] <- a_trip$trip_photos
# trips_df$bbox11[i] <- a_trip$trip_bbox[1, 1]
# trips_df$bbox12[i] <- a_trip$trip_bbox[1, 2]
# trips_df$bbox21[i] <- a_trip$trip_bbox[2, 1]
# trips_df$bbox22[i] <- a_trip$trip_bbox[2, 2]

# fix popup HRML so there's a line break before the description
pad_description <- function(d) {
  if (is.na(d)) return("")
  if (d == "") return("")
  if (d == " ") return("")
  paste("<br/>", d, "<br/>", sep = "")
}

addMyProviders <- function(m) {
  print("in my providers")
  m %>%
    # addTiles("https://{s}.tile.thunderforest.com/outdoors/{z}/{x}/{y}.png?apikey=452dc06e5d1947b7b4e893535d0e6b36", group = "Terrain") %>%
    # addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>%
    # addProviderTiles(providers$OpenTopoMap, group = "Open Topo") %>%
    # addProviderTiles(providers$OpenStreetMap.Mapnik, group = "Road Map")  
    
    addProviderTiles(providers$OpenTopoMap, group = "Topographical") %>%
    # addTiles("https://{s}.tile.thunderforest.com/landscape/{z}/{x}/{y}.png?apikey=452dc06e5d1947b7b4e893535d0e6b36", group = "Topographical") %>%
    addProviderTiles(providers$Thunderforest.Outdoors, group = "Outdoors",
                     options = providerTileOptions(apikey = "452dc06e5d1947b7b4e893535d0e6b36")) %>%
    addProviderTiles("Esri.WorldImagery", group = "Satellite") %>%
    addProviderTiles("OpenStreetMap.Mapnik", group = "Road map") %>%
    addLayersControl(baseGroups = c("Outdoors", "Topographical", "Satellite", "Road map"),    
                     overlayGroups = c("Hiking routes", "Photo markers"),
                     options = layersControlOptions(collapsed = FALSE)) %>%
    addScaleBar(position = c("topleft"))
  
}
#test:    m = add_lines_for_trip(m = NULL, trip = trips_list[[1]], trip_colors = colors_list[[1]])
#test:   track <- readOGR("/Users/johng/Dropbox/Mapping Info-iDisk/Track Archive/2014 Pyrenees/Track_2014-10-20 ESPOT2.gpx", layer = "tracks", verbose = FALSE)
add_lines_for_trip <- function(m = NULL, trip, trip_colors) {
  if (is.null(m)) {
    m <- leaflet(height = "700px", width = NULL) %>%
      addMyProviders()
      # Add tiles
      # addProviderTiles(providers$Thunderforest.Landscape, group = "Landscape") %>%
      # addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>%
      # addProviderTiles(providers$Stamen.Terrain, group = "Terrain") %<%
      # addProviderTiles(providers$OpenStreetMap.Mapnik, group = "Road map")  
  }
  for (k in seq_along(trip)) {
    print(paste("add_lines_for_trip", k, trip_colors[k], trip[[k]]@data[["name"]]))
    m <-  addPolylines(m, data=trip[[k]], group='Hiking routes', color = trip_colors[k])
  }
  m
}

# pick_area <- trips_df$area == "USA"
#test:  m <- add_trips_to_map(NULL, trips_list, colors_list)
add_trips_to_map <- function(m = NULL, trips_list = NULL, colors_list = NULL, pick_area = NULL) {
  # print(length(trips_list))
  if (!is.null(pick_area)) {
    colors_list <- colors_list[pick_area]
    trips_list <- trips_list[pick_area]
  }
  for (k in seq_along(trips_list)) {
    # use this to get name of trip name (internal to gpx file): trips_list[[k]][[1]]@data[["name"]]
    # print(paste(trips_list[[k]][[1]]@data[["name"]], "color:", colors_list[[k]]))
    # print(paste("trip", k, length(trips_list[[k]]), !is.null(trips_list[[k]])))
    if (!is.null(trips_list[[k]])) {
      m <- add_lines_for_trip(m, trips_list[[k]], colors_list[[k]])
    }
  }
  m
}


#test   xx <- add_photos_to_map(all_photos_df, m)
#test   xx <- add_photos_to_map(all_photos_df, m)
#specific test:  abc <- add_photos_to_map(xx$trip_photos, m)
#test    add_photos_to_map(xx, NULL, "2016 Amalfi Coast")
add_photos_to_map <- function(photos_df, m = null, select_area = NULL) {
  if (is.null(m)) {
    m <- leaflet(height = "700px", width = NULL) %>%
      addMyProviders()
      # addProviderTiles(providers$Thunderforest.Landscape, group = "Landscape") %>%
      # addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>%
      # addProviderTiles(providers$Stamen.Terrain, group = "Terrain") %<%
      # addProviderTiles(providers$OpenStreetMap.Mapnik, group = "Road map")  
    }
  photos_df <- filter(photos_df, !is.na(lng), !is.na(lat), include)
  if (!is.null(select_area)) photos_df <- filter(photos_df, area == select_area)

  # photo_in_album is created by photos_in_album_url().  
  # e.g., xx$trip_photos$photo_in_album <- map2_chr(xx$trip_photos$album_id, xx$trip_photos$id, photos_in_album_url, user_id)
  m <- addMarkers(m, lng=photos_df$lng, lat= photos_df$lat,  
                  popup = sprintf("<a href=\"%s\ \" target=\"_blank\">
                                  <IMG SRC=\"%s\" ALT=\"%s\" WIDTH=%s HEIGHT=%s>%s </a>", 
                                  photos_df$photo_in_album,
                                  photos_df$url_s, 
                                  "Click to go to photo in Flickr",
                                  photos_df$width_s,
                                  photos_df$height_s,
                                  photos_df$description),  
                  icon = photoIcon, # function providing custom marker-icons
                  group='Photo markers')
  m
}
