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
library(plyr)

# Loading Data ----
Demo <- read.xlsx("./Demo GeoMKT Taurus_FELIPE.xlsx")
#Indice <- read.xlsx("./indice.xlsx")
# Organizing
Demo$Lat <- as.numeric(sapply(strsplit(as.character(Demo$GPS), " "), "[[", 2))
Demo$Lon <- as.numeric(sapply(strsplit(as.character(Demo$GPS), " "), "[[", 4))
#sucursales <- st_as_sf(Demo, coords = c("Lon", "Lat"), crs = 4326)
#sucursales$pos <- TRUE
#sucursales$Departamentos <- paste(st_intersection(sucursales,dpto)$departamen)
#st_write(sucursales, './datos/Deptos.gpkg', 'sucursales')

sucursales <- st_read('./datos/Deptos.gpkg', 'sucursales', stringsAsFactors = FALSE)
dpto <- st_read('./datos/Deptos.gpkg', 'Deptos', stringsAsFactors = FALSE)
Demo <- Demo[-which(is.na(Demo$Mes.1)),] # removing values with NA

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

## Variación
Indice <- ddply(tabla_clientes, "Mes", summarise, Sum = sum(Ventas)) %>%
  mutate(Diff = Sum - lag(Sum)) %>%
  mutate(pos = Diff >= 0) %>% 
  mutate_if(is.logical, funs(replace(., is.na(.), "TRUE"))) %>% 
  mutate_if(is.numeric, funs(replace(., is.na(.), 0)))
# Var Cliente
IndiceCliente <- tabla_clientes %>%
  group_by(Cliente, Mes) %>%
  mutate(Diff = Ventas - lag(Ventas, 3)) %>%
  mutate(pos = Diff >= 0) %>% 
  mutate_if(is.logical, funs(replace(., is.na(.), "TRUE"))) %>% 
  mutate_if(is.numeric, funs(replace(., is.na(.), 0)))

# Creando mapa
m <- leaflet() %>%
  addTiles() %>%
  addProviderTiles("OpenStreetMap.Mapnik", group = "OpenStreetMap") %>%
  addProviderTiles("Esri.WorldImagery", group = "ESRI Aerial") %>%
  addLayersControl(
    baseGroups = c("OpenStreetMap", "ESRI Aerial"),
    options = layersControlOptions(collapsed = T))
getColor <- function(datos) {
  sapply(datos$pos, function(pos) {
    if(pos == TRUE) {
      "green"
    } else {
      "red"
    } })
}
# UI ----
ui <- navbarPage("Taurus Geomarketing",

#Panel Cliente ----
                 tabPanel("Análisis por cliente",
                          sidebarLayout(
                            
                            sidebarPanel(
                              # Input: Specification of range within an interval ----
                              selectInput(inputId = "clientes",
                                          label = "Elija el cliente",
                                          choices = c("TODOS", unique(sucursales$Cliente)),
                                          multiple = FALSE),
                              selectInput(inputId = "departamentos",
                                          label = "Elija el Departamento",
                                          choices = c("TODOS", unique(sucursales$Departamentos)),
                                          multiple = FALSE),
                              sliderInput(inputId = "meses",
                                          label = "Mes de análisis:",
                                          min = 1, max = 12,
                                          value = c(1, 12))
                            ),
                            mainPanel(
                              leafletOutput("Clientemap", height = 500),
                              plotlyOutput(outputId = "Clienteplot1"),
                              plotlyOutput(outputId = "ClienteVariacion")
                            )
                          )
                 ),
#Panel general ----
tabPanel("Análisis General",
         sidebarLayout(
           
           sidebarPanel(
             # Input: Specification of range within an interval ----
             sliderInput(inputId = "Generalmeses",
                         label = "Mes de análisis:",
                         min = 1, max = 12,
                         value = c(1, 12))
           ),
           mainPanel(
             leafletOutput("Generalmap", height = 500),
             plotlyOutput(outputId = "Generalplot1"),
             plotlyOutput(outputId = "GeneralVariacion"),
             plotlyOutput(outputId = "Generalplot3")
           )
         )
      )
)


# server ----
server <- function(input, output){
  
  # Mapa General -----
  output$Generalmap <- renderLeaflet(
    {
      icons <- awesomeIcons(
        icon = 'ios-close',
        iconColor = 'black',
        library = 'ion',
        markerColor = getColor(sucursales)
      )
      
      mgeneral <- leaflet() %>%
      addTiles() %>%
      addProviderTiles("OpenStreetMap.Mapnik", group = "OpenStreetMap") %>%
      addProviderTiles("Esri.WorldImagery", group = "ESRI Aerial") %>%
      #addCircleMarkers(data=sucursales, group="Cliente", radius = 10, opacity=1, color = "black",stroke=TRUE, fillOpacity = 0.75, weight=2, fillColor = "blue", clusterOptions = NULL, popup = paste0("<b>Cliente: </b>", sucursales$Cliente)) %>%
        addAwesomeMarkers(data=sucursales, group="Cliente", popup = paste0("<b>Cliente: </b>", sucursales$Cliente, label=~as.character(pos))) %>%
      addLayersControl(
        baseGroups = c("OpenStreetMap", "ESRI Aerial"),
        options = layersControlOptions(collapsed = T)) %>%
      addPolygons(data = dpto, color = "#444444", weight = 1, smoothFactor = 0.5, opacity = 0.5, fillOpacity = 0.5, fillColor = "lightgrey", options = markerOptions(interactive = FALSE))
    
    mgeneral
    }
  )
  
  # General Grafico 1----
  output$Generalplot1 <- renderPlotly(
    {mesesTotais <- c("Ene","Feb","Mar","Abr","May","Jun","Jul","Ago","Sep","Oct","Nov","Dic")
    mesesEscolhidos <- mesesTotais[seq(input$Generalmeses[1], input$Generalmeses[2])]
    tabla_clientes <- tabla_clientes[tabla_clientes$Mes %in% mesesEscolhidos,]
    
    #organizando datos
    GerenalMes <- ddply(tabla_clientes, c("Mes"), summarise, Promedio = mean(Ventas), Sd = sd(Ventas), se = Sd/sqrt(3))
    
    #Analysis del promedio y la variación observada
    grafico1 <- GerenalMes %>% ggplot(aes(x=Mes, y=Promedio, group = 1)) + geom_ribbon(aes(ymin = Promedio-Sd, ymax = Promedio + Sd), alpha = .4, fill = "grey70") + geom_line() +scale_y_continuous(labels = dollar) + ggtitle("Valor promedio de ventas + variación") +
      ylab("")
    
    grafico1 + theme_minimal()
    ggplotly(grafico1) 
    }
  )
  # Grafico 3 VARIACION ----
  # output$GeneralVariacion <- renderPlotly(
  #   {mesesTotais <- c("Ene","Feb","Mar","Abr","May","Jun","Jul","Ago","Sep","Oct","Nov","Dic")
  #   mesesEscolhidos <- mesesTotais[seq(input$Generalmeses[1], input$Generalmeses[2])]
  #   Indice <- Indice[Indice$Mes %in% mesesEscolhidos,]
  #   
  #   variacion <- ggplot(Indice, aes(Mes, Diff, fill = pos)) + 
  #     geom_hline(yintercept = 0, color ="black") +
  #     geom_bar(stat = "identity") +
  #     ylab("") +
  #     ggtitle("Variación mensual de ventas") 
  #   ggplotly(variacion) 
  #   }
  #)
  output$GeneralVariacion <- renderPlotly(
    {if(input$clientes == "TODOS"){
    }else{
      tabla_clientes <- tabla_clientes[which(tabla_clientes$Cliente == input$clientes),]
    }
      mesesTotais <- c("Ene","Feb","Mar","Abr","May","Jun","Jul","Ago","Sep","Oct","Nov","Dic")
      mesesEscolhidos <- mesesTotais[seq(input$Generalmeses[1], input$Generalmeses[2])]
      tabla_clientes <- tabla_clientes[tabla_clientes$Mes %in% mesesEscolhidos,]
      
      grafico_barra <- tabla_clientes %>% ggplot(aes(Mes, Ventas, fill = Cliente)) +
        geom_bar(stat = "identity", position = "stack", show.legend = F, colour = "black") +
        scale_y_continuous(labels = dollar)
      
      grafico_barra + theme_minimal()
      ggplotly(grafico_barra) 
    }
  )
  # General Grafico 3----
  output$Generalplot3 <- renderPlotly(
    {mesesTotais <- c("Ene","Feb","Mar","Abr","May","Jun","Jul","Ago","Sep","Oct","Nov","Dic")
    mesesEscolhidos <- mesesTotais[seq(input$Generalmeses[1], input$Generalmeses[2])]
    tabla_clientes <- tabla_clientes[tabla_clientes$Mes %in% mesesEscolhidos,]
    
    Histograma <- # Distribuición de los valores de venta
      ggplot(tabla_clientes, aes(x=Ventas)) + geom_histogram(bins = 20, fill="white", colour="black")+scale_x_continuous(labels = dollar)+ggtitle("Distribuición valores de venta") +
      ylab("")
    
    Histograma + theme_minimal()
    ggplotly(Histograma) 
    }
  )
  
  # Mapa Cliente -----
  output$Clientemap <- renderLeaflet(
    { 
      sucursalesFilt <- sucursales
      dptoFilt <- dpto
      if(input$clientes == "TODOS"){
        
      }else{
        sucursalesFilt <- sucursalesFilt[which(sucursales$Cliente == input$clientes),]
      }
      if(input$departamentos == "TODOS"){
        
      }else{
        print(input$departamentos)
        sucursalesFilt <- sucursalesFilt[which(sucursalesFilt$Departamentos == input$departamentos),]
        dptoFilt <- dptoFilt[which(dptoFilt$departamen == input$departamentos),]
      }
      mesesTotais <- c("Ene","Feb","Mar","Abr","May","Jun","Jul","Ago","Sep","Oct","Nov","Dic")
      sucursalesFilt <- merge(sucursalesFilt[,c("Cliente")], IndiceCliente[which(
        IndiceCliente$Mes == mesesTotais[input$meses[2]]), c("pos","Cliente")], by = "Cliente")
      
      # Add Positivos
        if( nrow(sucursalesFilt[which(sucursalesFilt$pos==TRUE), 1])>0){
          #m <- m %>% addCircleMarkers(data = sucursalesFilt[which(sucursalesFilt$pos==TRUE), ], group="Cliente", radius = 10, opacity=1, color = "black",stroke=TRUE, fillOpacity = 0.75, weight=2, fillColor = "darkgreen", popup = paste0("<b>Cliente: </b>", sucursalesFilt$Cliente))
          m <- m %>% addAwesomeMarkers(data=sucursalesFilt[which(sucursalesFilt$pos==TRUE), ], icon=awesomeIcons(icon = 'ios-close', iconColor = 'black', library = 'ion', markerColor = "green"), group="Cliente", popup = paste0("<b>Cliente: </b>", sucursales$Cliente))}
      
      # Add Negativos
      if( nrow(sucursalesFilt[which(sucursalesFilt$pos==FALSE), 1]) > 0){
        #m <- m %>% addCircleMarkers(data = sucursalesFilt[which(sucursalesFilt$pos==FALSE), ], group="Cliente", radius = 10, opacity=1, color = "black",stroke=TRUE, fillOpacity = 0.75, weight=2, fillColor = "red", popup = paste0("<b>Cliente: </b>", sucursalesFilt$Cliente)) }
        m <- m %>% addAwesomeMarkers(data=sucursalesFilt[which(sucursalesFilt$pos==FALSE), ], icon=awesomeIcons(icon = 'ios-close', iconColor = 'black', library = 'ion', markerColor = "red"), group="Cliente", popup = paste0("<b>Cliente: </b>", sucursales$Cliente))}
        
      # Add Deptos
        m <- m %>%  addPolygons(data = dptoFilt, color = "#444444", weight = 1, 
                    smoothFactor = 0.5, opacity = 0.5, fillOpacity = 0.5, fillColor = "lightgrey", options = markerOptions(interactive = FALSE))
        m
        }
    )
  
  # Grafico 1----
  output$Clienteplot1 <- renderPlotly(
    {if(input$clientes == "TODOS"){
    }else{
      tabla_clientes <- tabla_clientes[which(tabla_clientes$Cliente == input$clientes),]
    }
      mesesTotais <- c("Ene","Feb","Mar","Abr","May","Jun","Jul","Ago","Sep","Oct","Nov","Dic")
      mesesEscolhidos <- mesesTotais[seq(input$meses[1], input$meses[2])]
      tabla_clientes <- tabla_clientes[tabla_clientes$Mes %in% mesesEscolhidos,]
      
      lineal <- ggplot(tabla_clientes, aes(Mes, Ventas, fill = Cliente, colour = Cliente, group = Cliente)) + geom_point(show.legend = F,  alpha = .25) + geom_line(stat = "identity", show.legend = F, alpha=.25) + 
        stat_smooth(method=lm, level = 0, show.legend = F, alpha = 1) + scale_y_continuous(labels = dollar) 
      lineal + theme_minimal()
      ggplotly(lineal) 
    }
  )
  # Grafico VARIACION ----
  output$ClienteVariacion <- renderPlotly(
    {
      if(input$clientes == "TODOS"){
    }else{
      IndiceCliente <- IndiceCliente[which(IndiceCliente$Cliente == input$clientes),]
    }
    mesesTotais <- c("Ene","Feb","Mar","Abr","May","Jun","Jul","Ago","Sep","Oct","Nov","Dic")
    mesesEscolhidos <- mesesTotais[seq(input$meses[1], input$meses[2])]
    
    GraficoIndiceCliente <- IndiceCliente[IndiceCliente$Mes %in% mesesEscolhidos,]
    
    variacionCliente <- ggplot(GraficoIndiceCliente, aes(Mes, Diff, fill = pos)) + 
      geom_hline(yintercept = 0, color ="black") +
      geom_bar(stat = "identity", position = "dodge", colour="black", show.legend = T) +
      ylab("") + 
      ggtitle("Variación mensual de ventas")
    variacionCliente <- variacionCliente + facet_grid(Cliente ~ .) 
    ggplotly(variacionCliente) 
    }
  )
}

# app ----
shinyApp(ui = ui, server = server)

