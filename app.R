# https://redoakstrategic.com/geoshaper/
#https://rstudio.github.io/crosstalk/
# https://stackoverflow.com/questions/45953741/select-and-deselect-polylines-in-shiny-leaflet
library(shiny)
library(leaflet)

library(sf)
library(openxlsx)
library(reshape2)
library(ggplot2)
library(dplyr)
library(tidyr)
library(plotly)

# Loading Data ----
Demo <- read.xlsx("./Demo GeoMKT Taurus_FELIPE.xlsx")
Demo$Lat <- as.numeric(sapply(strsplit(as.character(Demo$GPS), " "), "[[", 2))
Demo$Lon <- as.numeric(sapply(strsplit(as.character(Demo$GPS), " "), "[[", 4))
sucursales <- st_as_sf(Demo, coords = c("Lon", "Lat"), crs = 4326)
data <- melt(Demo, id.vars="Cliente", measure.vars=paste0(rep("Mes.", 12), seq(1:12)), value.name="Mes")
sucursalesPol <- sf::st_buffer(x = sucursales, 0.001)
# Maxi ----
## Cambiar nombre de las columnas de los Meses
names(Demo)[10:21]<- c("Ene","Feb","Mar","Abr","May","Jun","Jul","Ago","Sep","Oct","Nov","Dic")

## Tablero de Ventas por Mes según Cliente
tablero <- Demo %>% select(Cliente, Ene, Feb, Mar, Abr, May, Jun, Jul, Ago, Sep, Oct, Nov, Dic)

## Tabla ventas según Mes
tabla_clientes <- Demo %>% gather(Mes, Ventas, 10:21) %>% select(Cliente, Mes, Ventas)

## Cambiar a factor y ordenar los datos
tabla_clientes$Mes <- factor(tabla_clientes$Mes, levels = c("Ene","Feb","Mar","Abr","May","Jun","Jul","Ago","Sep","Oct","Nov","Dic") )

tabla_clientes[is.na(tabla_clientes)] <- 0

# UI ----
ui <- fluidPage(
    sidebarLayout(
      sidebarPanel(
        # Selecionable ----
        selectInput(inputId = "clientes",
                    label = "Select cliente",
                    choices = c("Todos", unique(sucursales$Cliente)),
                    selected = "Todos",
                    multiple = FALSE),
        # Boton borrador ----
        actionButton( inputId = "clearHighlight"
                      , icon = icon( name = "eraser")
                      , label = "Clear the Map"
                      , style = "color: #fff; background-color: #D75453; border-color: #C73232"
        )
      ),
      mainPanel(
        leafletOutput("map", height = 500),
        plotlyOutput(outputId = "plot"),
        plotlyOutput(outputId = "linePlot")
      )
    )
  ) 


# server ----
server <- function(input, output){
  # create foundational map ----
  foundational.map <- shiny::reactive({
    # Filter as selected
    #if(input$clientes == "Todos"){
    #}else{
    #  sucursales <- sucursales[which(sucursales$Cliente == input$clientes),]
    #}
    leaflet() %>%
      addTiles() %>%
      addProviderTiles("OpenStreetMap.Mapnik", group = "OpenStreetMap") %>%
      addProviderTiles("Esri.WorldImagery", group = "ESRI Aerial") %>%
      addCircleMarkers(data=sucursales,
                      layerId = sucursales$Cliente,
                      group = "click.list",
                      radius = 10,
                      opacity=1,
                      color = "black",
                      stroke=TRUE,
                      fillOpacity = 0.75,
                      weight=2,
                      fillColor = "white",
                      clusterOptions = TRUE,
                      options = markerOptions(interactive = FALSE)) %>%
      
      addPolygons( data = sucursalesPol
                   , fillOpacity = 0.5
                   , opacity = 0
                   , color = "black"
                   , fillColor = "black"
                   , weight = 2
                   , layerId = sucursalesPol$Cliente
                   , group = "click.list") %>%
      #, popup = paste0("Spring Name: ", df.SP$SpringName, "<br> Temp_F: ", df.SP$Temp_F, "<br> Area: ", df.SP$AREA)) %>%
      addLayersControl(
        baseGroups = c("OpenStreetMap", "ESRI Aerial"),
        #overlayGroups = c("Hot SPrings"),
        options = layersControlOptions(collapsed = T))
  })
  
  # create My map render ----
  output$map <- leaflet::renderLeaflet(
    {
      foundational.map()
    }
  )
  
  # Highlight ----
  # store the list of clicked polygons in a vector
  click.list <- shiny::reactiveValues( ids = vector() )
  
  # observe where the user clicks on the leaflet map
  # during the Shiny app session
  # Courtesy of two articles:
  # https://stackoverflow.com/questions/45953741/select-and-deselect-polylines-in-shiny-leaflet
  # https://rstudio.github.io/leaflet/shiny.html
  shiny::observeEvent( input$map_shape_click, {
    
    # store the click(s) over time
    click <- input$map_shape_click
    # store the polygon ids which are being clicked
    click.list$ids <- c( click.list$ids, click$id )
    print(click.list$ids)
    # filter the spatial data frame
    # by only including polygons
    # which are stored in the click.list$ids object
    lines.of.interest <- sucursalesPol[ which( sucursalesPol$Cliente %in% click.list$ids ) , ]
    print(lines.of.interest$Cliente)
    # if statement
    if( is.null( click$id ) ){
      # check for required values, if true, then the issue
      # is "silent". See more at: ?req
      req( click$id )
      print("req")
    } else if( !click$id %in% lines.of.interest$Cliente ){
      print("elseif")
      # call the leaflet proxy
      leaflet::leafletProxy( mapId = "map" ) %>%
        # and add the polygon lines
        # using the data stored from the lines.of.interest object
      # addCircleMarkers(data=sucursales, 
      #                  layerId = sucursales$Cliente,
      #                  group = "click.list", 
      #                  radius = 50, 
      #                  opacity=1, 
      #                  color = "black", 
      #                  stroke=TRUE, 
      #                  fillOpacity = 1, weight=2, fillColor = "red", 
      #                  clusterOptions = TRUE,
      #                  options = markerOptions(interactive = FALSE)) %>%
        addPolylines( data = lines.of.interest,
                      layerId = lines.of.interest$Cliente,
                      color = "red",
                      weight=5,
                      opacity = 1)
      
    } # end of if else statement
    
  }) # end of shiny::observeEvent({})
  
  
  # Create the logic for the "Clear the map" action button
  # which will clear the map of all user-created highlights
  # and display a clean version of the leaflet map
  shiny::observeEvent( input$clearHighlight, {
    
    # recreate $myMap
    output$map <- leaflet::renderLeaflet({
      
      # first
      # set the reactive value of click.list$ids to NULL
      click.list$ids <- NULL
      
      # second
      # recall the foundational.map() object
      foundational.map()
      
    }) # end of re-rendering $myMap
    
  }) # end of clearHighlight action button logic
  
  # Grafico 1----
  output$plot <- renderPlotly(
    {
      if(input$clientes == "Todos"){
      }else{
        tabla_clientes <- subset(tabla_clientes, Cliente == input$clientes)
      }
      grafico_barra <- tabla_clientes %>% ggplot(aes(Mes, Ventas, fill = Cliente)) +
        geom_bar(stat = "identity", position = "stack", show.legend = F, colour = "black") +
        scale_y_continuous() +
        scale_fill_brewer(palette = 1) +
        scale_y_continuous(breaks = c(0, 300000,600000))
      
      grafico_barra + theme_minimal()
      ggplotly(grafico_barra) 
    }
  )
  
  # Grafico 2 ----
  output$linePlot <- renderPlotly(
    {
      if(input$clientes == "Todos"){
      }else{
        tabla_clientes <- subset(tabla_clientes, Cliente == input$clientes)
      }
      grafico_linha <- tabla_clientes %>% ggplot(aes(Mes, Ventas, group = Cliente, colour = Cliente)) +
        geom_line(stat = "identity", show.legend = F) #+
        #scale_y_continuous() +
        #scale_fill_brewer(palette = 1) +
        #scale_y_continuous(breaks = c(0, 300000,600000))
      
      grafico_linha + theme_minimal()
      ggplotly(grafico_linha) 
    }
  )
} # end server

# app ----
shinyApp(ui = ui, server = server)
