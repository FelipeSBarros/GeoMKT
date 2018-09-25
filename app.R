library(shiny)
library(leaflet)

library(sf)
library(openxlsx)
library(reshape2)
library(ggplot2)
library(dplyr)
library(tidyr)
library(plotly)
library(scales)

# Loading Data ----
Demo <- read.xlsx("./Demo GeoMKT Taurus_FELIPE.xlsx")
Demo$Lat <- as.numeric(sapply(strsplit(as.character(Demo$GPS), " "), "[[", 2))
Demo$Lon <- as.numeric(sapply(strsplit(as.character(Demo$GPS), " "), "[[", 4))
sucursales <- st_as_sf(Demo, coords = c("Lon", "Lat"), crs = 4326)
Demo <- Demo[-which(is.na(Demo$Mes.1)),] # removing values with NA
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
ui <- navbarPage("Geomarketing Taurus",
                 tabPanel("Análisis General",
                          sidebarLayout(
                            
                            sidebarPanel(
                              # Input: Specification of range within an interval ----
                              sliderInput(inputId = "Generalmeses",
                                          label = "Elija el mes:",
                                          min = 1, max = 12,
                                          value = c(1, 12))
                              ),
                            mainPanel(
                              leafletOutput("Generalmap", height = 500),
                              plotlyOutput(outputId = "Genrealplot1"),
                              plotlyOutput(outputId = "Generalpolt2")
                              )
                            )
                          )
                 tabPanel("Análisis por cliente",
                          sidebarLayout(
                            
                            sidebarPanel(
                              # Input: Specification of range within an interval ----
                              sliderInput(inputId = "Generalmeses",
                                          label = "Elija el mes:",
                                          min = 1, max = 12,
                                          value = c(1, 12))
                            ),
                            mainPanel(
                              leafletOutput("Generalmap", height = 500),
                              plotlyOutput(outputId = "Genrealplot1"),
                              plotlyOutput(outputId = "Generalpolt2")
                            )
                          )
                 )
                 )

                 
# server ----
server <- function(input, output){
  
  # Mapa General -----
  output$Generalmap <- renderLeaflet(
    { m <- leaflet() %>%
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
  
  # General Grafico 1----
  output$Generalplot1 <- renderPlotly(
    {mesesTotais <- c("Ene","Feb","Mar","Abr","May","Jun","Jul","Ago","Sep","Oct","Nov","Dic")
      mesesEscolhidos <- mesesTotais[seq(input$Generalmeses[1], input$Generalmeses[2])]
      tabla_clientes <- tabla_clientes[tabla_clientes$Mes %in% mesesEscolhidos,]
      
      grafico_barra <- tabla_clientes %>% ggplot(aes(Mes, Ventas, fill = Cliente)) +
        geom_bar(stat = "identity", position = "stack", show.legend = F, colour = "black") +
        scale_y_continuous(labels = dollar)
      
      grafico_barra + theme_minimal()
      ggplotly(grafico_barra) 
    }
  )
  # General Grafico 2----
  output$Generalplot2 <- renderPlotly(
    {mesesTotais <- c("Ene","Feb","Mar","Abr","May","Jun","Jul","Ago","Sep","Oct","Nov","Dic")
    mesesEscolhidos <- mesesTotais[seq(input$Generalmeses[1], input$Generalmeses[2])]
    tabla_clientes <- tabla_clientes[tabla_clientes$Mes %in% mesesEscolhidos,]
    
    grafico_barra <- tabla_clientes %>% ggplot(aes(Mes, Ventas, fill = Cliente)) +
      geom_bar(stat = "identity", position = "stack", show.legend = F, colour = "black") +
      scale_y_continuous(labels = dollar)
    
    grafico_barra + theme_minimal()
    ggplotly(grafico_barra) 
    }
  )
# Mapa Específico -----
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
    {if(input$clientes == "Todos"){
    }else{
      tabla_clientes <- tabla_clientes[which(tabla_clientes$Cliente == input$clientes),]
    }
      mesesTotais <- c("Ene","Feb","Mar","Abr","May","Jun","Jul","Ago","Sep","Oct","Nov","Dic")
      mesesEscolhidos <- mesesTotais[seq(input$meses[1], input$meses[2])]
      tabla_clientes <- tabla_clientes[tabla_clientes$Mes %in% mesesEscolhidos,]
      
      grafico_barra <- tabla_clientes %>% ggplot(aes(Mes, Ventas, fill = Cliente)) +
        geom_bar(stat = "identity", position = "stack", show.legend = F, colour = "black") +
        scale_y_continuous(labels = dollar)
      
      grafico_barra + theme_minimal()
      ggplotly(grafico_barra) 
    }
  )
  
  # Grafico 2 ----
  output$linePlot <- renderPlotly(
    {if(input$clientes == "Todos"){
    }else{
      tabla_clientes <- tabla_clientes[which(tabla_clientes$Cliente == input$clientes),]
    }
      mesesTotais <- c("Ene","Feb","Mar","Abr","May","Jun","Jul","Ago","Sep","Oct","Nov","Dic")
      mesesEscolhidos <- mesesTotais[seq(input$meses[1], input$meses[2])]
      tabla_clientes <- tabla_clientes[tabla_clientes$Mes %in% mesesEscolhidos,]
      
      grafico_linha <- tabla_clientes %>% ggplot(aes(Mes, Ventas, group = Cliente, colour = Cliente)) +
        geom_line(stat = "identity", show.legend = F) +
        scale_y_continuous(labels = dollar)
      
      grafico_linha + theme_minimal()
      ggplotly(grafico_linha) 
    }
  )
}

# app ----
shinyApp(ui = ui, server = server)
