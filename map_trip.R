#library("rgeos")

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#
#   You probably want to be in combine_gpx.R rather than here
#
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

filter_photos <- function(photos, hikes)
  # find photos during the hike
{
  by_hike <- group_by(hikes, day) %>% summarise(start_time = min(time, na.rm = TRUE), end_time = max(time, na.rm = TRUE))
  photos$day <- NA
  #for(i in 1:length())
}

#test:  find_nearest_point(long=coordinates(wp)[505,1], lat=coordinates(wp)[505,2], wp)
find_nearest_point <- function(long, lat, wp) {
  # wp must be SpatialPointsDataFrame
  apoint <- SpatialPoints(data.frame(long = c(long), lat = c(lat)))
  thedist <- spDistsN1(coordinates(wp), apoint, longlat = TRUE)
  which.min(thedist)
}


revise_bbox <- function(bbox1, bbox2) {
  # revise bbox1 bbox based on bbox2
  if (is.null(bbox2)) return(bbox1)
  if (is.null(bbox1)) return(bbox2)
  if (bbox2[1, 1] < bbox1[1, 1]) bbox1[1, 1] <- bbox2[1, 1]
  if (bbox2[2, 1] < bbox1[2, 1]) bbox1[2, 1] <- bbox2[2, 1]
  if (bbox2[1, 2] > bbox1[1, 2]) bbox1[1, 2] <- bbox2[1, 2]
  if (bbox2[2, 2] > bbox1[2, 2]) bbox1[2, 2] <- bbox2[2, 2]
  bbox1
}

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#
#   You probably want to be in combine_gpx.R rather than here
#
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

# for each trip, return a list with map, hikes data.frame
# note that for this test, add_camera_gps = TRUE so that I can see a separate icon for the unadjusted time.
# test:  xx <- map_trip(NULL, "Pennine Way Test", album_name = "2013 Pennine Way", use_api_key = api_key, use_user_id = user_id, add_camera_gps = TRUE, adjust_camera_time = 0 * 60 * 60)
# xx <- map_trip(NULL, "2016 Italy", album_name = "2016 Italy", use_api_key = api_key, use_user_id = user_id, add_camera_gps = FALSE, adjust_camera_time = -1 * 60 * 60)
# xx <- map_trip(NULL, "2016 Rome", album_name = "2016 Italy", use_api_key = api_key, use_user_id = user_id, add_camera_gps = FALSE, adjust_camera_time = -2 * 60 * 60)
#  Try xx$map to see the test map
map_trip <- function(trip,
                     trip_name, 
                     album_name = NULL,
                     mark_photos = TRUE, 
                     base_path = "~/Dropbox/Mapping Info-iDisk/Track Archive/",
                     map_photos = TRUE,
                     show_description = TRUE,
                     small_pictures = TRUE,
                     adjust_camera_time = 0, # adjustment because gpx is zulu, not localtime zone
                     add_camera_gps = FALSE, # map marker based on camera gps location
                     use_api_key = NULL, use_user_id = NULL) {
  map <- NULL
  trip_bbox <- NULL
  if (!is.null(trip)) map <- trip$map
  if (is.null(album_name)) album_name <- trip_name
  base_path <- path.expand(base_path)
  # if "ignore" appears in the file name it will be ignored
  file_names <- list.files(paste(base_path, trip_name, "/", sep = ""))
  file_names <- file_names[!str_detect(file_names, regex("Ignore", ignore_case = TRUE))]
  gpx_files <- paste(base_path, trip_name, "/", file_names, sep = "")
  # (?i:) makes case insensitive.   http://www.regular-expressions.info/modifiers.html
  # str_detect("Ignore", "(?i:ignore)")
  gpx_files <- subset(gpx_files, str_detect(gpx_files, "\\.(?i:gpx)"))   
  gpx_files <- gpx_files[order(gpx_files)]
  if (map_photos & !is.null(use_api_key) & !is.null(use_user_id)) {
    trip_photos <- photos_from_trip(api_key = use_api_key, user_id = use_user_id, album_name)
    if (class(trip_photos) != "data.frame") {
      add_photos <- FALSE
    }
    else trip_photos$datetaken <- trip_photos$datetaken + adjust_camera_time
  }
  hikes <- NULL
  if (!is.null(trip)) hikes <- trip$hikes
  if (length(gpx_files) == 0) {
    print("gpx_files is empty")
    return(NULL)
  }
  color_index <- 0
  for(i in 1:length(gpx_files)) {
    wp <- readOGR(gpx_files[i], layer = "track_points", verbose = FALSE) # returns SpatialPointsDataFrame object
    dist = cumsum(c(0, spDists(wp, segments=TRUE)))
    wp$time <- ymd_hms(wp$time)   # convert timestamps to datetime-objects
    wp$start <- min(wp$time)
    wp$hours <- as.numeric(difftime(wp$time, wp$start, units = "hours"))
    longitude = coordinates(wp)[, 1]
    latitude = coordinates(wp)[, 2]
    wp_df <- as.data.frame(wp)
    hike <- data_frame( distance = dist, hours = wp_df$hours, elev = wp_df$ele, 
                        longitude = longitude, latitude = latitude,
                        time = wp_df$time, trip_name = trip_name,
                        miles = dist * 0.621371,
                        day_name = str_replace(file_names[i], "\\.gpx", "")) 
    if (is.null(hikes)) hikes <- hike
    else hikes <- bind_rows(hike, hikes)
    cat(paste("file:",i, file_names[i]))
    cat(sprintf(" %s %s ", as.character(min(wp_df$time)), as.character(max(wp_df$time))))
    if (!is.na((str_locate(file_names[i], regex("Drive", ignore_case = TRUE))[1, 1]))) {
      cat(" contains Drive. \n")
      added_track <- add_gpx_to_leaflet(gpx_files[i], map, "lightyellow", cumbbox = NULL)
      track_bbox <- NULL  # don't add to bbox if this is a Drive
    }
    else {
      cat(" does not contain Drive. \n")
      color_index <- color_index + 1
      added_track <- add_gpx_to_leaflet(gpx_files[i], map, brewer.pal(3, "YlOrRd")[(i %% 2) + 2])
      track_bbox <- added_track$this_bbox
    }
    map <- added_track$map
    if (is.null(trip_bbox)) trip_bbox <- track_bbox
    else if (!is.null(track_bbox)) trip_bbox <- revise_bbox(trip_bbox, track_bbox)
    if (map_photos & !is.null(use_api_key) & !is.null(use_user_id) & !is.null(trip_photos)) {
      day_photos <- dplyr::filter(trip_photos, 
                           datetaken >= min(wp_df$time), 
                           datetaken <= max(wp_df$time))
      if (length(day_photos$datetaken) > 0) {
        for (j in 1:length(day_photos$datetaken)) {
          #locate photo based on time and then place on map
          photo_position <- which.min(abs(wp_df$time - day_photos$datetaken[j]))
          if (show_description) description <- str_c("<br/>", day_photos$description[j], "<br/>")
          else description <- ""
          
          if (small_pictures == FALSE) {
            map <- addMarkers(map, lng=wp_df$coords.x1[photo_position], lat=wp_df$coords.x2[photo_position],  
                              popup=sprintf("<IMG SRC=\"%s\" ALT=\"%s\" WIDTH=%s HEIGHT=%s>%s", 
                                            day_photos$url_m[j], 
                                            "Trip photo",
                                            day_photos$width_m[j],
                                            day_photos$height_m[j],
                                            description),  
                              icon = photoIcon, # function providing custom marker-icons
                              group='Photo markers')
          } else {
#    photos_in_album_url(user_id, some_photos$album_id[1], some_photos$id[1])
#             <a href="http://www.google.com" target="_blank">
#               <img width="220" height="250" border="0" align="center"  src=""/>
#                 </a>
# URL of photo in album:
# https://www.flickr.com/photos/99418994@N00/22042568849/in/album-72157657606414684/
                map <- addMarkers(map, lng=wp_df$coords.x1[photo_position], lat=wp_df$coords.x2[photo_position],  
                              popup=sprintf("<a href=\"%s\ \" target=\"_blank\">
                                            <IMG SRC=\"%s\" ALT=\"%s\" WIDTH=%s HEIGHT=%s>%s </a>", 
                                            photos_in_album_url(day_photos$album_id[j], day_photos$id[j], user_id),
                                            day_photos$url_s[j], 
                                            "Click to go to photo in Flickr",
                                            day_photos$width_s[j],
                                            day_photos$height_s[j],
                                            description),  
                              icon = photoIcon, # function providing custom marker-icons
                              group='Photo markers')
          }
          if (!is.na(day_photos$longitude[j]) & add_camera_gps) {
#             map <- addMarkers(map, lng=day_photos$longitude[j], lat=day_photos$latitude[j],  
#                               popup=sprintf("<IMG SRC=\"%s\" ALT=\"%s\" WIDTH=%s HEIGHT=%s>", 
#                                             day_photos$url_m[j], 
#                                             "Trip photo",
#                                             day_photos$width_m[j],
#                                             day_photos$height_m[j]),  
#                               group='Camera GPS')
            np <- find_nearest_point(as.numeric(day_photos$longitude[j]), 
                                     as.numeric(day_photos$latitude[j]), wp)
            map <- addMarkers(map, lng=day_photos$longitude[j], lat=day_photos$latitude[j],
                              icon = photoIcon, # function providing custom marker-icons
                              popup=sprintf("%s %s <br/>photo %s %s<br>gps %s<br/>  %10.6f %10.6f",
                                            as.character(day_photos$datetaken[j]),
                                            day_photos$title[j],
                                            day_photos$longitude[j],
                                            day_photos$latitude[j],
                                            as.character(wp_df$time[np]),
                                            wp_df$coords.x1[np],
                                            wp_df$coords.x2[np]),
                              group='Camera GPS')
          }
        }
      }
    } 
  }
  # I have change this so that map is a cumulative map, but trip_bbox is for this trip only.
  list(day_names = file_names, map = map, hikes = hikes, trip_bbox = trip_bbox)
}

# the purpose of map_photos is to place icons for photos that have GPS info but are not on a trail
# test:  map_photos(NULL, "Other Canyons", use_api_key = api_key, use_user_id = user_id, show_description = TRUE)
# test:  map_photos(NULL, "2013 Yosemite", exclude_route_photos = TRUE, wp = yosemite$hikes, adjust_camera_time = 7 * 60 * 60, use_api_key = api_key, use_user_id = user_id, show_description = TRUE)

map_photos <- function(map, album_name, use_api_key = NULL, use_user_id = NULL, 
                       exclude_route_photos = FALSE, wp = NULL, 
                       adjust_camera_time = 0, # adjustment because gpx is zulu, not localtime zone
                       small_pictures = TRUE,
                       show_description = TRUE) {
  trip_name <- album_name
  trip_photos <- photos_from_trip(api_key = use_api_key, user_id = use_user_id, album_name)
  if (is.null(trip_photos)) return(map)
  if (class(trip_photos) != "data.frame") {
    return(NULL)
  }
  if (length(trip_photos$datetaken) > 0) {
    # if exclude_route_photos == TRUE, exclude any photos on the start or end date of route
    if (exclude_route_photos) {
      trip_photos$include <- TRUE
      trip_photos$datetaken <- trip_photos$datetaken + adjust_camera_time
      #       base_path <- path.expand(base_path)
      #       file_names <- list.files(paste(base_path, trip_name, "/", sep = ""))
      #       if (length(file_names > 0)) {
      #         gpx_files <- paste(base_path, trip_name, "/", file_names, sep = "")
      #         # (?i:) makes case insensitive.   http://www.regular-expressions.info/modifiers.html
      #         gpx_files <- subset(gpx_files, str_detect(gpx_files, "\\.(?i:gpx)"))   
      #       }
      #       if (length(gpx_files) > 0) for(i in 1:length(gpx_files)) {
      #         wp <- readOGR(gpx_files[i], layer = "track_points", verbose = FALSE) # returns SpatialPointsDataFrame object
      #         wp$time <- ymd_hms(wp$time)   # convert timestamps to datetime-objects
      #         start_time <- min(wp$time)
      #         end_time <- max(wp$time)
      #         trip_photos$include <- ifelse((trip_photos$datetaken >= start_time) &
      #                                         (trip_photos$datetaken <= end_time), FALSE, trip_photos$include)
      #       }
      # # Instead of doing the above code to get the start and end time by track, this version
      # # uses wp (as in yosemite$hikes) to get the times.
      hiking_times <- summarise(group_by(wp, day_name), start_time = min(time), end_time = max(time))
      if (length(hiking_times$day_name) > 0) for (i in 1:length(hiking_times$day_name)) {
        trip_photos$include <- ifelse((trip_photos$datetaken >= hiking_times$start_time[i]) &
                                        (trip_photos$datetaken <= hiking_times$end_time[i]), FALSE, trip_photos$include)
      }
      trip_photos <- subset(trip_photos, trip_photos$include)
    }
    if (is.null(map)) {
      print(paste("In map_trip.R, about to create map with leaflet  with debugging code map_trip.R."))
      map <- leaflet() %>%
        addMyProviders()
      # addProviderTiles("Stamen.TonerLite",
      #                    options = providerTileOptions(noWrap = TRUE), group = "none")
                         
        #addProviderTiles("Thunderforest.Landscape", group = "Topographical") %>%
        #addProviderTiles("OpenStreetMap.Mapnik", group = "Road map") %>%
        #addProviderTiles("Esri.WorldImagery", group = "Satellite") 
    }
    else {print("In map_trip.R and map is not NULL.")}
    # instead of testing for length, could do j in str_length(trip_photos$datataken)
    if (length(trip_photos$datetaken) > 0) for (j in 1:length(trip_photos$datetaken)) 
      if (!is.na(trip_photos$longitude[j]) & !is.na(trip_photos$latitude[j])) {
        if (show_description) description <- str_c("<br/>", trip_photos$description[j], "<br/>")
        else description <- ""
        if (small_pictures == FALSE) {
          map <- addMarkers(map, lng=as.numeric(trip_photos$longitude[j]), 
                            lat=as.numeric(trip_photos$latitude[j]),  
                            popup=sprintf("<IMG SRC=\"%s\" ALT=\"%s\" WIDTH=%s HEIGHT=%s>%s", 
                                          trip_photos$url_m[j], 
                                          "Trip photo",
                                          trip_photos$width_m[j],
                                          trip_photos$height_m[j],
                                          description),  
                            icon = photoIcon, # function providing custom marker-icons
                            group='Photo markers')
        } else {
          map <- addMarkers(map, lng=as.numeric(trip_photos$longitude[j]), 
                            lat=as.numeric(trip_photos$latitude[j]),   
                            popup=sprintf("<a href=\"%s\ \" target=\"_blank\">
                                          <IMG SRC=\"%s\" ALT=\"%s\" WIDTH=%s HEIGHT=%s>%s</a>", 
                                          photos_in_album_url(trip_photos$album_id[j], trip_photos$id[j], user_id),
                                          trip_photos$url_s[j], 
                                          "Click to go to photo in Flickr",
                                          trip_photos$width_s[j],
                                          trip_photos$height_s[j],
                                          description),  
                            icon = photoIcon, # function providing custom marker-icons
                            group='Photo markers')
        }
      }
  }
  map
}


