
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

# for each trip, return a list with map, hikes data.frame
# note that for this test, add_camera_gps = TRUE so that I can see a separate icon for the unadjusted time.
# test:  xx <- combine_gpx("Pennine Way Test", album_name = "2013 Pennine Way", area = "England", use_api_key = api_key, use_user_id = user_id, add_camera_gps = TRUE, adjust_camera_time = 0 * 60 * 60)
# test:  xx <- combine_gpx("2013 Pennine Way", album_name = "2013 Pennine Way", use_api_key = api_key, use_user_id = user_id, add_camera_gps = TRUE, adjust_camera_time = 0 * 60 * 60)
# test:  xx <- combine_gpx("2016 Grand Canyon", album_name = "2016 Grand Canyon", use_api_key = api_key, use_user_id = user_id, add_camera_gps = TRUE, adjust_camera_time = 0 * 60 * 60)
# test:  xx <- combine_gpx("2018 Greece", album_name = "2018 Greece", use_api_key = api_key, use_user_id = user_id, add_camera_gps = TRUE, adjust_camera_time = -2 * 60 * 60)
# test:  xx <- combine_gpx("2016 Amalfi Coast", album_name = "2016 Italy", use_api_key = api_key, use_user_id = user_id, add_camera_gps = TRUE, adjust_camera_time = -2 * 60 * 60)
combine_gpx <- function(trip_name, 
                     album_name = NULL,
                     mark_photos = TRUE, 
                     base_path = "~/Dropbox/Mapping Info-iDisk/Track Archive/",
                     map_photos = TRUE,
                     show_description = TRUE,
                     small_pictures = TRUE,
                     adjust_camera_time = 0, # adjustment because gpx is zulu, not localtime zone
                     add_camera_gps = FALSE, # map marker based on camera gps location
                     all_tagged_photos = TRUE, # include geo-tagged photos even if not in trace time range
                     use_api_key = NULL, use_user_id = NULL,
                     area = NA_character_) {
  trip_bbox <- NULL
  if (is.null(album_name)) album_name <- trip_name
  base_path <- path.expand(base_path)
  file_names <- list.files(paste(base_path, trip_name, "/", sep = ""))
  file_names <- file_names[(!str_detect(file_names, regex("Ignore", ignore_case = TRUE))) &
                             str_detect(file_names, regex("\\.gpx", ignore_case = TRUE))]
  gpx_files <- paste(base_path, trip_name, "/", file_names, sep = "")
  # (?i:) makes case insensitive.   http://www.regular-expressions.info/modifiers.html
  gpx_files <- subset(gpx_files, str_detect(gpx_files, "\\.(?i:gpx)"))   
  gpx_files <- gpx_files[order(gpx_files)]
  if (map_photos & !is.null(use_api_key) & !is.null(use_user_id)) {
    trip_photos <- photos_from_trip(api_key = use_api_key, user_id = use_user_id, album_name)
    if (class(trip_photos) != "data.frame") {
      add_photos <- FALSE
      print(paste("Album", album_name, "not found.", trip_name))
    }
    else {
      trip_photos$datetaken <- trip_photos$datetaken + adjust_camera_time
      trip_photos$include <- FALSE
      trip_photos$lng <- NA_real_
      trip_photos$lat <- NA_real_
      trip_photos$area <- area
      trip_photos$longitude <- as.numeric(trip_photos$longitude)
      trip_photos$latitude <- as.numeric(trip_photos$latitude)
      if (all_tagged_photos) {
        trip_photos$include <- !is.na(trip_photos$longitude) & !is.na(trip_photos$latitude)
      }
      add_photos <- TRUE
      titles <- !str_detect(trip_photos$title, "(^[A-Z][A-Z][A-Z].+)|(^Photo [0-9].+)|(^$)|(photo)")
      trip_photos$description[titles] <- str_c(trip_photos$title[titles], trip_photos$description[titles], sep = " ")
    }
  }
  if (length(gpx_files) == 0) {
    print("gpx_files is empty")
    stop(paste("No gpx files found for", trip_name))
    return(NULL)
  }
  color_index <- 0
  n <- length(gpx_files)
  hikes_list <- vector("list", n)
  traces_list <- vector("list", n)
  traces_color <- vector("character", n)
  # hikes <- tibble( distance = double(length = n), hours =  double(length = n), elev =  double(length = n), 
  #                      longitude =  double(length = n), latitude =  double(length = n),
  #                      time =  double(length = n), trip_name = character(length = n),
  #                      miles =  double(length = n),
  #                      day_name = character(length = n)) 
  for(i in seq_along(gpx_files)) {
    # special case: 10-12-2015 Padstow to Porthcothan.gpx [last file saved via myTracks but seems to be same time adjust]
    wp <- readOGR(gpx_files[i], layer = "track_points", verbose = FALSE) # returns SpatialPointsDataFrame object
    dist = cumsum(c(0, spDists(wp, segments=TRUE)))
    wp$time <- ymd_hms(wp$time)   # convert timestamps to datetime-objects
    if (gpx_files[i] == "10-12-2015 Padstow to Porthcothan.gpx") {
      wp$time <- wp$time + adjust_camera_time
    }
    wp$start <- min(wp$time)
    wp$hours <- as.numeric(difftime(wp$time, wp$start, units = "hours"))
    longitude = coordinates(wp)[, 1]
    latitude = coordinates(wp)[, 2]
    wp_df <- as.data.frame(wp)
    # each element of hikes_list has the readOGR nonverbose info for this a hike (info to place photos)
    hikes_list[[i]] <- tibble( distance = dist, 
                                   hours = wp_df$hours, elev = wp_df$ele, 
                                   longitude = longitude, latitude = latitude,
                                   time = ymd_hms(wp_df$time), # convert timestamps to datetime-objects
                                   trip_name = trip_name,
                                   miles = dist * 0.621371,
                                   day_name = str_replace(file_names[i], "\\.gpx", "")) 
    # next get readOGR again, only this time in format that will be added to a map via
    # m <-  addPolylines(m, data=traces_list[[i]], group='Hiking routes', color = traces_color[[i]])
    a_track <- readOGR(gpx_files[i], layer = "tracks", verbose = FALSE)
    traces_list[[i]] <- a_track
    if (!is.na((str_locate(file_names[i], regex("(Drive|Ferry|Bus Trip|Bus Ride|Taxi|Airplane|Car Ride)", ignore_case = TRUE))[1, 1]))) {
      if (!is.na((str_locate(file_names[i], regex("(Ferry|Airplane)", ignore_case = TRUE))[1, 1]))) {
        traces_color[[i]] <- "darkgray"
      } else traces_color[[i]] <- "darkgray"
      cat(paste0(file_names[i], " color: ", traces_color[[i]], "\n"))
      # cat(" contains Drive or Ferry or Taxi or Bus Ride. \n")
      #added_track <- add_gpx_to_leaflet(gpx_files[i], map, "lightyellow", cumbbox = NULL)
      track_bbox <- NULL  # don't add to bbox if this is a Drive
    }
    else {
      cat(paste0(file_names[i], " does not contain Drive. \n"))
      color_index <- color_index + 1
      traces_color[[i]] <- brewer.pal(3, "YlOrRd")[(color_index %% 2) + 2]
      #added_track <- add_gpx_to_leaflet(gpx_files[i], map, brewer.pal(3, "YlOrRd")[(i %% 2) + 2])
      track_bbox <- bbox(traces_list[[i]])
    }
    if (is.null(trip_bbox)) trip_bbox <- track_bbox
    else if (!is.null(track_bbox)) trip_bbox <- revise_bbox(trip_bbox, track_bbox)
    
    if (map_photos & add_photos & !is.null(use_api_key) & !is.null(use_user_id) & !is.null(trip_photos)) {
      min_time <- min(wp_df$time)
      max_time <- max(wp_df$time)
      for (j in seq_along(trip_photos$datetaken)) {
        # If photo within time range for this walk, locate photo based on time and then place on map
        if ((trip_photos$datetaken[j] >= min_time) & (trip_photos$datetaken[j] <= max_time)) {
          photo_position <- which.min(abs(wp_df$time - trip_photos$datetaken[j]))
          if (show_description) description <- trip_photos$description[j]
          else description <- ""
          trip_photos$include[j] <- TRUE
          trip_photos$lng[j] <- wp_df$coords.x1[photo_position]
          trip_photos$lat[j] <- wp_df$coords.x2[photo_position]
          #if (!is.null(area) & (typeof(area) != "character")) print(paste("Oops", j, area))
          trip_photos$area[j] <- area
          # now save what we need to add photos later
          # np <- find_nearest_point(as.numeric(trip_photos$longitude[j]), 
          #                          as.numeric(trip_photos$latitude[j]), wp)
          # map <- addMarkers(map, lng=day_photos$longitude[j], lat=day_photos$latitude[j],
          #                   icon = photoIcon, # function providing custom marker-icons
          #                   popup=sprintf("%s %s <br/>photo %s %s<br>gps %s<br/>  %10.6f %10.6f",
          #                                 as.character(day_photos$datetaken[j]),
          #                                 day_photos$title[j],
          #                                 day_photos$longitude[j],
          #                                 day_photos$latitude[j],
          #                                 as.character(wp_df$time[np]),
          #                                 wp_df$coords.x1[np],
          #                                 wp_df$coords.x2[np]),
          #                   group='Camera GPS')
        }
      }
    }
  } 
  if (!is.null(trip_photos)) {
    # i dont think I need this here because I moved the de-dupping to collect_gps_data_for_maps
    # gps_lat_long <- trip_photos %>% 
    #   group_by(id, secret) %>%
    #   summarise(lng = max(lng, na.rm = TRUE), lat = max(lat, na.rm = TRUE)) %>%   # max of NULL returns -Inf
    #   mutate(lng = ifelse(lng == -Inf, NA, lng), lat = ifelse(lat == -Inf, NA, lat))
    # trip_photos <- trip_photos %>%
    #   select(id, secret, title, description, datetaken,
    #                     url_m, height_m, width_m, latitude, longitude, # removed lng, lat, 
    #                     url_s, height_s, width_s, album_id, include, area) %>%
    #   left_join(gps_lat_long, by = c("id", "secret")) %>%
    #   mutate(lng = ifelse(is.na(lng), longitude, lng), 
    #          lat = ifelse(is.na(lat), latitude, lat)) %>%
    #   # datetaken gets adjusted differently for some photo albums, e.g., 2016 Italy different for Rome & Amalfi
    #   select(-longitude, -latitude, -datetaken) %>%
    #   unique()
    trip_photos <- trip_photos %>%
      select(id, secret, title, description, datetaken,
                        url_m, height_m, width_m, latitude, longitude, lng, lat,
                        url_s, height_s, width_s, album_id, include, area) 
  }
  # At this point, trip_bbox is the bounding box for this trip
  # trip_photos is tibble of photos for this trip
  list(day_names = file_names, trip_photos = trip_photos, 
       traces_list = traces_list, traces_color = traces_color, trip_bbox = trip_bbox)
}

# test:  xx <- combine_gpx("2015 SWCP", album_name = "2015 SWCP", use_api_key = api_key, use_user_id = user_id, add_camera_gps = TRUE, adjust_camera_time = -1 * 60 * 60, area = "England")
# m <- addPolylines(m, data = xx$traces_list[[11]], group = "Hiking routes", color = "blue")
# xx$trip_photos$photo_in_album <- map2_chr(xx$trip_photos$album_id, xx$trip_photos$id, photos_in_album_url, user_id)
# xx$trip_photos$description <- map_chr(xx$trip_photos$description, pad_description)
# m2 <- add_photos_to_map(xx$trip_photos, m = m, select_area = "England")

