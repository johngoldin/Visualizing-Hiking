# to get react log:     
# options(shiny.reactlog=TRUE) 

# error message:
# path[1]="/Library/Frameworks/R.framework/Versions/3.2/Resources/library/leaflet/htmlwidgets/lib/leaflet-providers": No such file or directory

# based on: https://rstudio.github.io/leaflet/shiny.html
library(shiny)
library(leaflet)
#library("rgdal")
library("lubridate")
library("plyr")
library("dplyr")
#library("ggplot2")
library("stringr")
library("purrr")
#library("RColorBrewer")
#library("httr")
#library("jsonlite")

source("add_trips_to_map.R")
source("photoIcon.R")

# code in collect_gps_data_for_maps.Rmd is used to create these datasets.
#save(trips_df, trips_list, trip_photos_df, other_photos_df, colors_list, file = "trace and photo info for trips.RData")
load("trace and photo info for trips.RData")

print("Loading trace and photo info")
print(system.time(load("trace and photo info for trips.RData")))
print("Starting...")
if (file.exists("saved_map.RData")) {
  print("Loading map from file")
  print(system.time(load("saved_map.RData")))
} else {
  print("Adding traces")
  usa_map <- add_trips_to_map(NULL, trips_list, colors_list, pick_area = (trips_df$area == "USA"))
  england_map <- add_trips_to_map(NULL, trips_list, colors_list, pick_area = (trips_df$area == "England"))
  spain_map <- add_trips_to_map(NULL, trips_list, colors_list, pick_area = (trips_df$area == "Spain"))
  usa_map <- add_photos_to_map(all_photos_df, usa_map, select_area = "USA")
  england_map <- add_photos_to_map(all_photos_df, england_map, select_area = "England")
  spain_map <- add_photos_to_map(all_photos_df, spain_map, select_area = "Spain")
  
  save(usa_map, england_map, spain_map, file = "saved_map.RData")
  # file.remove("saved_map.RData")
}

# the_map <- add_trips_to_map(NULL, trips_list, colors_list)
# the_map <- add_photos_to_map(all_photos_df, the_map)

map_choices <- c("Pennine Way north", "Pennine Way south", "Coast to Coast",
                 "Grand Canyon", "Tucson", "Phoenix", "Bryce Canyon", "South West Coast Path",
                 "Wales", "Costswolds", "Andorra", "Pyrenees", "Madrid", "Cabo de Gata")
map_area <- c("England", "England", "England", "USA", "USA", "USA", "USA", "England", "England", "England",
              "Spain", "Spain", "Spain", "Spain")
default_area <- c("USA" = "Grand Canyon 2016", "England" = "Pennine Way north 2014", 
                  "Spain" = "Pyrenees 2014")


ui <- fluidPage(
  img(src = "stuff.jpg", height = 608 / 2.8, width = 2035 / 2.8),
  fluidRow(
     column(1),
     column(2, selectInput("which_area", "Select map area:", unique(map_area), selected = "USA")),
     column(3, selectInput("which_trip", "Focus on trip:", 
                           paste(trips_df$trip[trips_df$area == "USA"], trips_df$year[trips_df$area == "USA"]), 
                           selected = "Grand Canyon 2016")),
     column(1),
     column(1, fluidRow(" ", p()," ", p(), p(), actionButton("focusButton", strong("Re-focus Map"))))
     #column(1, fluidRow(" ", p()," ", p(), p(), actionButton("notesButton", strong("Notes"))))
     
  ),
  fluidRow("     Click on photo icon to see photo and then click on the photo to open a new tab showing the photo in Flickr. For notes, click", 
           a("here", href="https://caniblogtoo.wordpress.com/2016/06/28/notes-for-souvenirs-of-my-walks", target="_blank"), "."),
  leafletOutput("mymap", height = 500, width = 700)
)

server <- function(input, output, session) {
  r_colors <- rgb(t(col2rgb(colors()) / 255))
  names(r_colors) <- colors()

  get_map_data <- reactive({
    print(paste("In get_map_data", input$which_area))
    if (input$which_area == "England") {england_map}
    else if (input$which_area == "USA") {usa_map}
    else if (input$which_area == "Spain") {spain_map}
    else {usa_map}
  })
  get_bounds <- reactive({
    # print(paste("Changing get_bounds", input$which_trip))
    # print(as.list(trips_df[input$which_trip == paste(trips_df$trip, trips_df$year), 
    #                        c("bbox11", "bbox21", "bbox12", "bbox22")]))
    as.list(trips_df[input$which_trip == paste(trips_df$trip, trips_df$year), 
                          c("bbox11", "bbox21", "bbox12", "bbox22")])
  })
  
  observeEvent(input$focusButton, {
    # print(paste("after running fitBounds in observeEvent.",  input$which_trip,
    #             get_bounds()$bbox11, get_bounds()$bbox21,
    #             get_bounds()$bbox12, get_bounds()$bbox22 ))
    leafletProxy("mymap", session) %>%
      #fitBounds(-112.057, 36.211, -111.9775, 36.3864)
      fitBounds(get_bounds()$bbox11, get_bounds()$bbox21,
                get_bounds()$bbox12, get_bounds()$bbox22)
    
  })
  observeEvent(input$notesButton, {
    
  })
  
  observe({
        # Change values for input$which_trip
        updateSelectInput(session, "which_trip", 
                          choices =  paste(trips_df$trip[trips_df$area == input$which_area], 
                                           trips_df$year[trips_df$area == input$which_area]),
                          selected = default_area[input$which_area])
      })
    
  output$mymap <- renderLeaflet({
    # If I put get_bounds() into a print statement here, it will cause
    # renderLeaflet to run whenever input$which_trip changes, which is not what I want.
    #get_map_data()  -- this was when I was doing separate map for three areas instead of loading the whole map
    addLayersControl(get_map_data(), baseGroups = c("Topographical", "Satellite", "Road map"),
                     overlayGroups = c("Hiking routes", "Photo markers"),
                     options = layersControlOptions(collapsed = FALSE)) %>% 
      addScaleBar(position = c("topleft")) 
  })
  
  # from: https://rstudio.github.io/leaflet/shiny.html
  # Incremental changes to the map should be performed in
  # an observer. Each independent set of things that can change
  # should be managed in its own observer.
  observe({
    # print(paste("Running fitBounds in observe",  input$which_trip, get_bounds()$bbox11, get_bounds()$bbox21,
    #             get_bounds()$bbox12, get_bounds()$bbox22))
    leafletProxy("mymap", session) %>%
      fitBounds(get_bounds()$bbox11, get_bounds()$bbox21,
                get_bounds()$bbox12, get_bounds()$bbox22)
  })
  
  
}

shinyApp(ui, server)
