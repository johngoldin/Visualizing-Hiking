## Visualizing-Hiking
### R code to map GPS tracks on Leaflet and connect to Flickr photos
Produces a Shiny app that displays a [Leaflet souvenir map of my walks](https://goldin.shinyapps.io/Walks/).


There is a description [here](https://johngoldin.github.io/2016/06/28/technical-note--shiny-souvenir-map-of-walks/). Previously
I had this at a WordPress [blog](https://caniblogtoo.wordpress.com/2016/06/28/technical-note-souvenir-shiny-map/), but moved it to my GitHub blog.

The key document is `app.R`.

The GPS trace data for the Leaflet map is created by `collect_gps_data_for_maps.R`. That creates `trace and photo info for trips.RData` which is loaded by the Shiny app. To add a new trip, I have to edit `collect_gps_data_for_maps.R`.

Early on I tried to create the Leaflet map and save it as `RData`. Then `app.R` would load that dataset. That worked on my local machine, but did not work when I published it to `shinyapps.io`. It turns out that the Leaflet map depends on the file structure of
my local machine and so the map object doesn't transfer succesfully from one computer to another. The first time the app runs on `shineyapps.ip` it saves its map data in `saved_map.RData` to help speed startup of the app.

Photo icons are placed on the map based on the time stamp on the photo. I compare photo time stamps with the time on the GPS trace.
The GPS trace from my Garmin is based on Greenwich time. The clock on my camera timestamp is ususally different. I have
a parameter that adjusts each trace by hours. Sometimes it is tricky to figure out how to do that adjustment. I created
`camera_gps_time_difference.R` to help. It looks at photos that have location information attached and compares that location to the GPS trace. It then reports on the typical difference between the photo time stamp and the time on the GPS trace.

On my local Mac, the GPS traces are not stored in the same folder as my RStudio project and those traces are not uploaded to this GitHub repository.
