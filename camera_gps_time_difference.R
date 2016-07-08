
#test:  camera_gps_time_difference("2014 Pennine Way", "06-12-2014 Hawes to Keld.gpx") # 372 seconds
#test:  camera_gps_time_difference("2014 Pennine Way", "06-16-2014 Dufton to Garrigill.gpx") # 260 sec
#test:  camera_gps_time_difference("2013 Pennine Way", "06-24-2013 Malham.gpx") # -3550 seconds
#test:  camera_gps_time_difference("2015 SWCP", "10-10-2015 Tintagel to Port Isaac.gpx") # -3501 seconds
#test:  camera_gps_time_difference("2013 Bryce", "Track_BRYCE08-16 095810.gpx", album = "2013 Utah") # -3601 seconds
#test:  camera_gps_time_difference("2013 Grand Canyon", "Track_GRAND08-19 131712.gpx", album = "2013 Utah", filter_by_time = FALSE) # +4.09 hours
#test:  camera_gps_time_difference("2014 Pyrenees", "Track_2014-10-17 SOLDEU.gpx", album = "2014 Catalonia", filter_by_time = FALSE) 
#test:  camera_gps_time_difference("2014 Pyrenees", "Track_2014-10-19 ESPOT1.gpx", album = "2014 Catalonia", filter_by_time = FALSE) 
#test:  camera_gps_time_difference("2012 Cotswolds", "19-JUN-2012 Woton-under-Edge circle route.GPX", filter_by_time = FALSE) # 4 hours
#test:  camera_gps_time_difference("2011 Hadrians Wall", "16 SEP 2011 Hadrians Wall.GPX", filter_by_time = FALSE)  # 5 hours, although result says 4
#test:  camera_gps_time_difference("2012 Wales", "23-JUN-2012 St Davids to Trefin.GPX", filter_by_time = FALSE) # -1 hour
#test:  camera_gps_time_difference("2014 Tucson", "Track_WESSON 12232014.gpx", filter_by_time = FALSE) # 7 hours
#test:  camera_gps_time_difference("2015 Phoenix", "Track_2015-12-23 BLACK ROCK.gpx", filter_by_time = FALSE) # 6 hours
#test:  camera_gps_time_difference("2011 Amsterdam", "18 SEP 2011 Amsterdam.gpx", filter_by_time = FALSE) 
#test:  camera_gps_time_difference("2015 SWCP", "10-12-2015 Padstow to Porthcothan.gpx", filter_by_time = FALSE) 
#test:  camera_gps_time_difference("2015 Madrid", "04-20-2015 Madrid.gpx", filter_by_time = FALSE) 
#test:  camera_gps_time_difference("2015 Cabo de Gata", "Track_A015-04-18 143249.gpx", filter_by_time = FALSE) 
#test:  camera_gps_time_difference("2013 Yosemite", "Track_GLACIER PT 160104.gpx", filter_by_time = FALSE) 
#test:  camera_gps_time_difference("2013 Yosemite", "Track_MARIPOSA09 135136.gpx", filter_by_time = FALSE) 


camera_gps_time_difference <- function(trip_name, GPX_file, use_api_key = api_key,
                                       use_user_id = user_id,
                                       filter_by_time = TRUE,
                                       album = NULL,
                                       base_path = "~/Dropbox/Mapping Info-iDisk/Track Archive/") {
  base_path <- path.expand(base_path)
  GPX_file <- paste(base_path, trip_name, "/", GPX_file, sep = "")
  if (is.null(album)) album <- trip_name
  trip_photos <- photos_from_trip(api_key = use_api_key, user_id = use_user_id, album)
  wp <- readOGR(GPX_file, layer = "track_points") # returns SpatialPointsDataFrame object
  wp$time <- ymd_hms(wp$time)   # convert timestamps to datetime-objects
  wpx <- as.data.frame(wp)
  if (filter_by_time) day_photos <- dplyr::filter(trip_photos, 
                              datetaken >= min(wpx$time), 
                              datetaken <= max(wpx$time),
                              !is.na(longitude))
  # use this broader selection
  else day_photos <- dplyr::filter(trip_photos, 
                              yday(datetaken) == yday(mean(wpx$time)), 
                              !is.na(longitude))
  camera_adjust <- 0
  if (length(day_photos$datetaken) > 0) {
    for (j in 1:length(day_photos$datetaken)) {
      
      np <- find_nearest_point(as.numeric(day_photos$longitude[j]), 
                               as.numeric(day_photos$latitude[j]), wp)
      cat(sprintf("%s %s photo %s %s\ngps %s %10.6f %10.6f time dif=%s \n", 
              as.character(day_photos$datetaken[j]), 
              day_photos$title[j],
              day_photos$longitude[j],
              day_photos$latitude[j],
              as.character(wpx$time[np]),
              wpx$coords.x1[np],
              wpx$coords.x2[np], as.character(wpx$time[np] - day_photos$datetaken[j])))
      camera_adjust <- camera_adjust + (wpx$time[np] - day_photos$datetaken[j])
    }
  }
  if (length(day_photos$datetaken) == 0) return(NULL)
  camera_adjust / length(day_photos$datetaken)
}