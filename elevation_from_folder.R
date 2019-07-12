
# elevation_from_folder("~/Dropbox/Mapping Info-iDisk/Track Archive/PW-south/")
# elevation_from_folder("~/Dropbox/Mapping Info-iDisk/Track Archive/2014 Pyrenees/")
# p <- elevation_from_folder("~/Dropbox/Mapping Info-iDisk/Track Archive/SWCP/")

get_gpx_points <- function(gpx_folder) {
  # need path.expand because readOGR doesn't expand ~ in file path
  gpx_files <- path.expand(paste(gpx_folder, list.files(gpx_folder), sep =""))
  gpx_files <- subset(gpx_files, str_detect(gpx_files, "\\.gpx"))
  hikes <- NULL
  hike_cutoff <- NULL
  for(i in 1:length(gpx_files)) {
    wp <- readOGR(gpx_files[i], layer = "track_points") # returns SpatialPointsDataFrame object
    dist = cumsum(c(0, spDists(wp, segments=TRUE)))
    wp$time <- ymd_hms(wp$time)   # convert timestamps to datetime-objects
    wp$start <- min(wp$time)
    wp$hours <- as.numeric(difftime(wp$time, wp$start, units = "hours"))
    wp <- as.data.frame(wp)
    hike <- tibble( distance = dist, hours = wp$hours, elev = wp$ele, time = wp$time) 
    #     time_step <- c(wp$hours[2:length(wp$hours)] - wp$hours[1:length(wp$hours) -1], NA)
    #     cutoff <- min(which(time_step > 1), na.rm = TRUE)
    #     if (cutoff >= length(wp$hours)) cutoff <- NA
    #     if (!is.na(cutoff) & (cutoff < length(wp$hours))) cutoff <- wp$hours[cutoff]
    #     print(sprintf("day: %i which(time_step > 1: %f cutoff: %f ", i, min(which(time_step > 1), na.rm = TRUE), cutoff))
    hike$day <- gpx_files[i]
    hike$miles <- hike$dist * 0.621371
    if (is.null(hikes)) hikes <- hike
    else hikes <- bind_rows(hike, hikes)
    #     if (is.null(hike_cutoff)) hike_cutoff <- data.frame(cutoff = c(cutoff), day = i)
    #     else hike_cutoff <- bind_rows(hike_cutoff, data.frame(cutoff = c(cutoff), day = i))
  }
  if (is.null(hikes)) {
    print("gpx files not found.")
    return(NULL)
  }
  hikes  
}

elevation_from_folder <- function (gpx_folder) {
  hikes <- get_gpx_points(gpx_folder)
  p <- ggplot(as.data.frame(hikes), # convert to regular dataframe
              aes(x=miles, y=elev)) 
  p <- p + geom_point() + labs(x='Distance (miles)', y='Elevations (meters)')
  p + facet_wrap(~ day, ncol = 1) #+ geom_vline(data = hike_cutoff, xintercept = cutoff)
}

elevation_from_hikes <- function (hikes) {
  p <- ggplot(hikes, # convert to regular dataframe
              aes(x=miles, y=elev)) 
  p <- p + geom_point() + labs(x='Distance (miles)', y='Elevations (meters)')
  if (dim(table(hikes$trip_name)) ==  1) {
    p + facet_wrap(~ day_name, ncol = 1) #+ geom_vline(data = hike_cutoff, xintercept = cutoff)
  } else {
    p + facet_wrap(~ trip_name * day_name, ncol = 1) #+ geom_vline(data = hike_cutoff, xintercept = cutoff)
  }
}

distance_by_day <- function(hikes) {
  group_by(hikes, trip_name, day_name) %>% summarise(dist = max(miles))
}

