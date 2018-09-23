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
        selectInput(inputId = "clientes",
                    label = "Select cliente",
                    choices = c("Todos", unique(sucursales$Cliente)),
                    selected = "Todos",
                    multiple = FALSE)
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
  
  output$map <- renderLeaflet(
    {
      if(input$clientes == "Todos"){
      }else{
        sucursales <- sucursales[which(sucursales$Cliente == input$clientes),]
      }
      
      m <- leaflet() %>%
      addTiles() %>%
      addProviderTiles("OpenStreetMap.Mapnik", group = "OpenStreetMap") %>%
      addProviderTiles("Esri.WorldImagery", group = "ESRI Aerial") %>%
      addCircleMarkers(data=sucursales, group="Cliente", radius = 10, opacity=1, color = "black",stroke=TRUE, fillOpacity = 0.75, weight=2, fillColor = "blue", clusterOptions = TRUE) %>%
      #, popup = paste0("Spring Name: ", df.SP$SpringName, "<br> Temp_F: ", df.SP$Temp_F, "<br> Area: ", df.SP$AREA)) %>%
      addLayersControl(
        baseGroups = c("OpenStreetMap", "ESRI Aerial"),
        #overlayGroups = c("Hot SPrings"),
        options = layersControlOptions(collapsed = T))
    
    m
    }
  )
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
}

# app ----
shinyApp(ui = ui, server = server)
