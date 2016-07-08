# Based on http://mhermans.net/hiking-gpx-r-leaflet.html   Maarten Hermans

library("rgdal")
library("lubridate")
library("plyr")
library("dplyr")
library("ggplot2")
library("leaflet")
library("exif")
library("Rflickr")
library("RColorBrewer")
library("stringr")
# installed RFlickr via instal_url("http://www.omegahat.org/Rflickr/Rflickr_0.2-1.tar.gz")
# see http://www.omegahat.org/Rflickr/

# for information on sp classes, see https://cran.r-project.org/web/packages/sp/vignettes/intro_sp.pdf
# installed exiftool via http://www.sno.phy.queensu.ca/~phil/exiftool/

GPX_file <- "/Users/johng/Dropbox/Mapping Info-iDisk/Track Archive/SWCP/Track_2015-10-01 COMBE MARTIN.gpx"
GPX_file <- "/Users/johng/Dropbox/Mapping Info-iDisk/Track Archive/SWCP/Track_2015-09-30 LYNTON.gpx"
GPX_file <- "/Users/johng/Dropbox/Mapping Info-iDisk/Track Archive/Andalusia/Track_A015-04-18 143249.gpx"
GPX_file <- "/Users/johng/Dropbox/Mapping Info-iDisk/Track Archive/PW-north/Track_PW-06-19 084640 copy2.gpx"

wp <- readOGR(GPX_file, layer = "track_points") # returns SpatialPointsDataFrame object
# OGR data source with driver: GPX 
# Source: "/Users/johng/Dropbox/Mapping Info-iDisk/Track Archive/Track_2014-10-17 SOLDEU.gpx", layer: "track_points"
# with 3153 features
# It has 26 fields
head(wp[,c('ele', 'time')])
max(wp$ele) - min(wp$ele) # height climbed in meters
hike.dists <- spDists(wp, segments=TRUE)
sum(hike.dists) # about 11.8km hike

wp$time <- ymd_hms(wp$time)   # convert timestamps to datetime-objects
wp <- as.data.frame(wp)
wp$start <- min(wp$time)
wp$hours <- as.numeric(difftime(wp$time, wp$start, units = "hours"))

wp$time_step <- c(wp$hours[2:length(wp$hours)] - wp$hours[1:length(wp$hours) -1], NA)
cutoff <- min(which(wp$time_step > .1))

# hike_start <- ymd_hms("2014-10-17 08:29:07 UTC")
# hike_start <- ymd_hms("2014-10-15 04:29:07 UTC")
# wp_original <- wp
# wp <- subset(wp_original, wp_original$time >= hike_start)

p <- ggplot(wp, # convert to regular dataframe
            aes(x=hours, y=ele))  + geom_vline(xintercept = wp$hours[cutoff])
p + geom_point() + labs(x='Hiking time', y='Elevations (meters)')
# ah! this shows me that there are some old points incuded that pre-date the hike

# get coordinates so I can limit leaflet map
wpdf <- as.data.frame(wp)
lat_max <- max(wpdf$coords.x2)
long_max <- max(wpdf$coords.x1)
lat_min <- min(wpdf$coords.x2)
long_min <- min(wpdf$coords.x1)


track <- readOGR(GPX_file, layer = "tracks", verbose = FALSE)
# fyi:  dim(coordinates(track)[[1]][[1]])   matrix containing coordinates
# leaflet() %>% addTiles() %>% addPolylines(data=track) %>% 
#   fitBounds(long_min, lat_min, long_max, lat_max)
  
  
#  m <- leaflet() %>% fitBounds(long_min, lat_min, long_max, lat_max) %>%
  m <- leaflet() %>% 
    
    # Add tiles
    addProviderTiles("Thunderforest.Landscape", group = "Topographical") %>%
    addProviderTiles("OpenStreetMap.Mapnik", group = "Road map") %>%
    addProviderTiles("Esri.WorldImagery", group = "Satellite") %>%
    
    addLegend(position = 'bottomright',opacity = 0.4, 
              colors = 'blue', 
              labels = 'Soldieu',
              title = 'Hike Andorra') %>%
    
    # Layers control
    addLayersControl(position = 'bottomright',
                     baseGroups = c("Topographical", "Road map", "Satellite"),
                     overlayGroups = c("Hiking routes", "Photo markers"),
                     options = layersControlOptions(collapsed = FALSE)) %>%
    
    addPolylines(data=track, color='blue', group='Hiking routes') 
  
  m
  
  exif_datetime <- function(path) {
    # read out the picture-taken datetime for a file using exiftool
    
    exif_cmd <- 'exiftool -T -r -DateTimeOriginal '  
    cmd <- paste(exif_cmd, '"', path, '"', sep='')
    exif_timestamp <- system(cmd, intern = TRUE) # execute exiftool-command
    
    exif_timestamp
  }
  
  photo_timestamp <- exif_datetime('IMG_1790.jpg')

  photo_exif <- read_exif('IMG_1790.jpg')
photo_timestamp <- ymd_hms(photo_exif$origin_timestamp) - 60*60

wp_position <- which.min(abs(wp$time - photo_timestamp))
wpd <- as.data.frame(wp)
wp_match <- wpd[wp_position,
                c('time', 'track_seg_point_id', 'coords.x1', 'coords.x2', 'ele')]
wp_match

photoIcon <- makeIcon(
  iconAnchorX = 12, iconAnchorY = 12, # center middle of icon on track,
  # instead of top corner  
  iconUrl = "https://www.mapbox.com/maki/renders/camera-12@2x.png"
)

m <- addMarkers(m, lng=wp_match$coords.x1, lat=wp_match$coords.x2,  
                popup=photo_timestamp, # use the timestamp as popup-content 
                icon = photoIcon, # function providing custom marker-icons
                group='Photo markers')

m <- addMarkers(m, lng=wp_match$coords.x1, lat=wp_match$coords.x2,  
                popup=sprintf("<IMG SRC=\"%s\" ALT=\"%s\" WIDTH=640 HEIGHT=640>", photo_url, "some text"), # use the timestamp as popup-content 
                icon = photoIcon, # function providing custom marker-icons
                group='Photo markers')
m
#stack overflow replacing coordinates: http://stackoverflow.com/questions/29311072/change-or-replace-coordinates-of-a-spatiallinesdataframe
# coordinates are here:  coordinates(track)[[1]][[1]]
xx <- coordinates(track)[[1]][[1]]
track@lines[[1]]@Lines[[1]]@coords[] <- xx

# let's see if we can trim some plots



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# From flickr email: The App Garden
# 
# Create an App API Documentation Feeds	What is the App Garden?
# Done! Here's the API key and secret for your new app:
# hiking route display
# 
# Key:
# api_key <- "6a0b624f6d3239c8e7466d7031aa9ddd"
# 
# Secret:
# secret <- "da67697e6d638579"
# Edit app details - Edit auth flow for this app - View all Apps by You 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

# from: http://timelyportfolio.github.io/rCharts_Rflickr/iso_httr.html
flickr.app <- oauth_app("r to flickr",api_key,secret)
flickr.endpoint <- oauth_endpoint(
  request = "https://www.flickr.com/services/oauth/request_token"
  , authorize = "https://www.flickr.com/services/oauth/authorize"
  , access = "https://www.flickr.com/services/oauth/access_token"
)
tok <- oauth1.0_token(
  flickr.endpoint
  , flickr.app
  , cache = F
)
user_id <- "99418994@N00"
photo_id <- "22041641268"
save(user_id, api_key, file = "flickr_values.RData")

https://www.flickr.com/photos/99418994@N00/22041641268/in/album-72157657606414684/
  https://www.flickr.com/photos/99418994@N00/22041641268

#https://api.flickr.com/services/rest/?method=flickr.photos.getInfo&api_key=5efda1b757c132fbeec161c3575a59c8&photo_id=22041641268&secret=da67697e6d638579&format=json&auth_token=72157661586322563-023b9240a154c4ef&api_sig=5bd02ce84fa1ba04847db5d811e78bad
xx <- GET(url=sprintf(
  "https://api.flickr.com/services/rest/?method=flickr.photos.getInfo&api_key=%s&photo_id=%s&secret=%s&format=json&nojsoncallback=1"
  , api_key
  , photo_id
  , secret
  , tok$credentials$oauth_token
)
) %>% content(as = "text")
xy <- fromJSON(xx)  # check that xy$stat == "ok"
photo <- xy$photo
farm_id <- photo$farm
server_id <- photo$server
photo_secret <- photo$secret
# date taken:  photo$dates$taken
# Size Suffixes
# 
# The letter suffixes are as follows:
#   s	small square 75x75
# q	large square 150x150
# t	thumbnail, 100 on longest side
# m	small, 240 on longest side
# n	small, 320 on longest side
# -	medium, 500 on longest side
# z	medium 640, 640 on longest side
# c	medium 800, 800 on longest side†
# b	large, 1024 on longest side*
#   h	large 1600, 1600 on longest side†
# k	large 2048, 2048 on longest side†
# o	original image, either a jpg, gif or png, depending on source format

# https://farm{farm-id}.staticflickr.com/{server-id}/{id}_{secret}_[mstzb].jpg
# https://farm6.staticflickr.com/5636/22041641268_da67697e6d638579_m.jpg
# https://farm1.staticflickr.com/2/1418878_1e92283336_m.jpg
create_photo_url <- function(flickr_size){
photo_url <- sprintf(
  "https://farm%s.staticflickr.com/%s/%s_%s_%s.jpg"
  , farm_id
  , server_id
  , photo_id
  , photo_secret
  , flickr_size
)
}
addMarkers(m, lng=wp_match$coords.x1, lat=wp_match$coords.x2,  
           popup=sprintf("<IMG SRC=\"%s\" ALT=\"%s\" WIDTH=240 HEIGHT=240>", 
                         create_photo_url("m"), "some text"), # use the timestamp as popup-content 
           icon = photoIcon, # function providing custom marker-icons
           group='Photo markers')
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# A COMPLETE EXAMPLE FROM THE ARTICLE:
# Map 3 for Aosta
# ---------------
# m.aosta <- m.base %>% 
#   
#   # Add legend
#   addLegend(position = 'topright',opacity = 0.4, 
#             colors = c('blue', 'red'), 
#             labels = c('Cogne-Lillaz (30/09)', 
#                        'Gimillan-Grausson (01/10)'),
#             title = 'Hikes Italy, region Aosta') %>%
#   
#   # Add tracks
#   addPolylines(data=aosta.h1.track, 
#                color='blue', group='Hiking routes') %>%
#   addPolylines(data=aosta.h2.track, 
#                color='red', group='Hiking routes') %>%
#   
#   # Add photo markers
#   addMarkers(data=aosta.h1.photos, 
#              popup=aosta.h1.photos$popup_html, 
#              icon = photoIcon,
#              group='Photo markers') %>%
#   addMarkers(data=aosta.h2.photos, 
#              popup=aosta.h2.photos$popup_html,
#              icon = photoIcon,
#              group='Photo markers')
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# Calculate distance in kilometers between two points
# from https://conservationecology.wordpress.com/2013/06/30/distance-between-two-points-in-r/
earth.dist <- function (long1, lat1, long2, lat2)
{
  rad <- pi/180
  a1 <- lat1 * rad
  a2 <- long1 * rad
  b1 <- lat2 * rad
  b2 <- long2 * rad
  dlon <- b2 - a2
  dlat <- b1 - a1
  a <- (sin(dlat/2))^2 + cos(a1) * cos(b1) * (sin(dlon/2))^2
  c <- 2 * atan2(sqrt(a), sqrt(1 - a))
  R <- 6378.145
  d <- R * c
  return(d)
}
