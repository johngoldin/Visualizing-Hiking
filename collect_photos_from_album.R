
#test:  xx <- collect_photos_from_album("Coast to Coast", area = "England", use_api_key = api_key, use_user_id = user_id)
collect_photos_from_album <- function(album_name, area = NA_character_, use_api_key = NULL, use_user_id = NULL,
                                      default = trip_photos_df[trip_photos_df$id == "xxxxx", ]) {
  if (!is.null(use_api_key) & !is.null(use_user_id)) {
    trip_photos <- photos_from_trip(api_key = use_api_key, user_id = use_user_id, album_name)
    if (class(trip_photos) != "data.frame") {
      # returning an empty data frame, but this is a crappy way to handle this
      return(default)
    }
    else {
      trip_photos$include <- TRUE
      trip_photos$include[is.na(trip_photos$longitude) | is.na(trip_photos$latitude)] <- FALSE
      trip_photos$lng <- NA_real_
      trip_photos$lat <- NA_real_
      trip_photos$area <- area
    }
  }
  titles <- !str_detect(trip_photos$title, "(^[A-Z][A-Z][A-Z].+)|(^Photo [0-9].+)|(^$)|(photo)")
  trip_photos$description[titles] <- str_c(trip_photos$title[titles], trip_photos$description[titles], sep = " ")
  
  trip_photos <- select(trip_photos, id, secret, title, description, datetaken,
                        latitude, longitude, url_m, height_m, width_m,
                        url_s, height_s, width_s, album_id, include, lng, lat, area)
}  