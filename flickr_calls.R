

source("flickr_values.RData")  # holds id and key for flickr. ignored by github

# user_id <- "99418994@N00"
# 
# xx <- GET(url=sprintf(
#   "https://api.flickr.com/services/rest/?method=flickr.photos.getInfo&api_key=%s&photo_id=%s&secret=%s&format=json&nojsoncallback=1"
#   , api_key
#   , photo_id
#   , secret
#   , tok$credentials$oauth_token
# )
# ) %>% content(as = "text")
# xy <- fromJSON(xx)  # check that xy$stat == "ok"
# photo <- xy$photo
# farm_id <- photo$farm
# server_id <- photo$server
# photo_secret <- photo$secret

# test: xx <- flickr_photosets_getlist(api_key, user_id)
flickr_photosets_getlist <- function(the_api_key = api_key, the_user_id = user_id) {
  xx <- GET(url=sprintf(
    "https://api.flickr.com/services/rest/?method=flickr.photosets.getList&api_key=%s&user_id=%s&format=json&nojsoncallback=1"
    , the_api_key
    , the_user_id
  )
  ) %>% content(as = "text") %>%
    fromJSON()  # check that xy$stat == "ok"
  if (xx$stat != "ok") return(xx$stat)
  albums <- xx$photosets[[5]]
  return(albums)
}

# SWCP photoset_id is 72157657606414684
#test:  xx <- flickr_photosets_getphotos()
flickr_photosets_getphotos <- function(the_photoset_id = "72157657606414684", 
                                       time_start = NULL, 
                                       time_end = NULL, 
                                       the_api_key = api_key, 
                                       the_user_id = user_id) {
  # returns data.frame of photos including datetaken, latitude, longitude, 
  # url_m, height_m, width_m and url_s, height_s, height_m
  xx <- GET(url=sprintf(
    "https://api.flickr.com/services/rest/?method=flickr.photosets.getPhotos&api_key=%s&photoset_id=%s&user_id=%s&extras=%s&format=json&nojsoncallback=1"
    , the_api_key
    , the_photoset_id
    , the_user_id
    , "description,date_taken,geo,url_m,url_s"
  )
  ) %>% content(as = "text") %>%
    fromJSON()  # check that xy$stat == "ok"
  if (xx$stat != "ok") {
    print(paste("Flickr error", code, message))
    return(xx$stat)
  }
  # description gets returned as a data.frame. I'm not sure why. so get _content
  xx$description <- xx$description[ ,"_content"]
  xx$photoset$photo$datetaken <- ymd_hms(xx$photoset$photo$datetaken)
  xx$photoset$latitude <- as.numeric(xx$photoset$latitude)
  xx$photoset$longitude <- as.numeric(xx$photoset$longitude)
  xx$photoset$photo$latitude <- ifelse(xx$photoset$photo$latitude != 0, xx$photoset$photo$latitude, NA)
  xx$photoset$photo$longitude <- ifelse(xx$photoset$photo$longitude != 0, xx$photoset$photo$longitude, NA)
  if (is.null(time_start) & is.null(time_end)) return(xx$photoset$photo)
  filter(xx$photoset$photo, is.null(time_start) | (datetaken >= time_start), is.null(time_end) | (datetaken >= time_end))
}

# test:   some_photos <- photos_from_trip(api_key, user_id, "2015 SWCP")
# test:   some_photos <- photos_from_trip(api_key, user_id, "Other UK")
# test:   some_photos <- photos_from_trip(api_key, user_id, "Other Canyons")
# some_photos$title[str_detect(some_photos$title, "(^[A-Z][A-Z][A-Z].+)|(^Photo [0-9].+)|(^$)|(photo)")]

photos_from_trip <- function(api_key, user_id, trip_name, ...) {
  if (is.null(trip_name)) return(NULL)
  photos <- NULL
  if (!is.null(api_key) & !is.null(user_id)) {
    albums <- flickr_photosets_getlist(api_key, user_id)
    if (class(albums) == "data.frame") {
      if (trip_name %in% albums$title$'_content') {
        photos <- flickr_photosets_getphotos(the_photoset_id = albums$id[albums$title == trip_name], 
                                             the_api_key = api_key, 
                                             the_user_id = user_id)
        photos$album_id <- albums$id[albums$title == trip_name]
      } else {
        message(paste("Unable to find album name", trip_name))
        photos <- NULL
      }
    } else {
      message(paste("Unable to retrieve album names", albums))
      photos <- NULL
    }
  }
  # description gets returned as a data.frame. I'm not sure why. so get _content
  if(!is.null(photos)) photos$description <- photos$description[ ,"_content"]
  photos
}

# https://www.flickr.com/photos/99418994@N00/22042568849/in/album-72157657606414684/
# test:  photos_in_album_url(some_photos$album_id[1], some_photos$id[1], user_id)
photos_in_album_url <- function(album_id, photo_id, user_id) {
  # note, changed order of agument June 26, 2016
  sprintf("https://www.flickr.com/photos/%s/%s/in/album-%s/",
                 user_id,
                 photo_id,
                 album_id)
}



