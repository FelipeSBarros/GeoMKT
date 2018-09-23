#https://community.rstudio.com/t/large-number-of-data-points-shiny-leaflet/3853/16
library(shiny)
library(leaflet)
library(sf)
librry(tibble)
#> Linking to GEOS 3.6.1, GDAL 2.2.0, proj.4 4.9.3
library(httr)

# ____________________________________________________________________________
# Download a test gpx track and convert it to a `sf` LINEsTRING
tmp <- tempfile(fileext = ".zip")
get_zip <- httr::GET('http://software.frankingermann.de/images/gpxtracks/kuhkopfsteig-fv.zip', 
                     httr::write_disk(tmp))
in_track <- unzip(tmp) %>% sf::st_read(layer = "track_points", quiet = TRUE)

track_data <- tibble::tibble(track_id  = 1,
                             timestamp = in_track$time,
                             x = sf::st_coordinates(in_track)[,1],
                             y = sf::st_coordinates(in_track)[,2]) %>% 
  sf::st_as_sf(coords = c("x","y")) %>% 
  sf::st_set_crs(4326) %>% 
  # NOTE: Here we have to convert to a metric projection because `st_line_sample`
  # only works with projected data
  sf::st_transform(3857) %>%
  # This needed to transform the "points" to a LINESTRING
  dplyr::group_by(track_id) %>% 
  dplyr::summarise(do_union = FALSE) %>% 
  sf::st_cast("LINESTRING")
track_data
#> Simple feature collection with 1 feature and 1 field
#> geometry type:  LINESTRING
#> dimension:      XY
#> bbox:           xmin: 717227.3 ymin: 6568866 xmax: 719747.2 ymax: 6571670
#> epsg (SRID):    3857
#> proj4string:    +proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +wktext +no_defs
#>   track_id                       geometry
#> 1        1 LINESTRING (717522.70749015...


# ____________________________________________________________________________
# Now set-up a shiny app showing the effect of simplifying the linestring
# using the `density` argument.

ui = shinyUI(fluidPage(
  titlePanel("Test tracks"),
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput(inputId = "shps.select", label="Select layer",
                         choices = c("line" = "line", "points" = "points")),
      sliderInput("density", "Points Density:", 50, min = 1, max = 250)
    ),
    mainPanel(leafletOutput("map", width = "100%", height = 400))
  )
))

server = shinyServer(function(input, output) {
  
  output$map <- renderLeaflet({ leaflet() %>% 
      addTiles() %>% 
      setView(6.45471, 50.71, zoom = 14)})
  
  observe({
    
    # Create map
    map <- leafletProxy("map")
    map %>% clearShapes()
    
    # Get select inputs
    shps.select <- input$shps.select # the function is triggered when the select option changes
    
    if (length(shps.select) > 0) {
      if ('lines' %in% shps.select) {
        density <- input$density # triggers this function when you update density
        
        # "sample" the linestring according to selected density
        linetrack <- track_data %>%
          sf::st_line_sample(density = 1/density, type = "regular") %>%
          sf::st_transform(4326) %>% 
          st_cast("LINESTRING")
        
        leafletProxy("map")  %>% addPolylines(group = "track_id",
                                              data = linetrack)
      }
      
      if ('points' %in% shps.select) {
        
        density <- input$density # triggers this function when you update dates
        
        # "sample" the linestring according to selected "density" and go back to
        #  points representation        
        track <- track_data %>%
          sf::st_line_sample(density = 1/density) %>%
          sf::st_cast("POINT") %>%
          sf::st_transform(4326)
        
        leafletProxy("map")  %>% addCircles(group = "track_id",
                                            data = track)
      }
    }  
  })
})

shinyApp(ui = ui, server = server)