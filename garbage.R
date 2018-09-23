#https://community.rstudio.com/t/select-areas-in-leaflet-map-in-a-shiny-app-based-on-selectinput/5065
location = c("100 ail", "16th", "21 de Mayo") 
lastUpdated = c("aa", "bb", "cc") 
firstUpdated = c("2015-09-01 00:00:00","2016-03-06 19:00:00","2016-03-06 19:00:00")
lastUpdated  = c("2018-02-01 09:30:00","2018-02-01 03:00:00","2017-01-13 15:00:00")
latitude=as.numeric(c("47.932907","41.322470","36.809700"))
longitude=as.numeric(c("106.92139000","-95.93799000","-107.65170000"))
pm25=c("TRUE","FALSE","TRUE")
pm10=c("FALSE","FALSE","TRUE")
no2=c("TRUE","FALSE","TRUE")
df = data.frame(location,lastUpdated,firstUpdated,latitude,longitude,pm25,pm10,no2)   

#ui.r
library(shiny)
library(leaflet)
library(shinythemes)
library(dplyr)
library(gissr)
# Define UI for application that draws a histogram
ui <- fluidPage(theme = shinytheme("slate"),
                  
                  # title
                  titlePanel("Select Dates of interest and location"),
                  
                  # first row; This allows a user to enter their start and stop dates
                  fluidRow(
                    column(6, wellPanel(
                      dateRangeInput('dateRange',
                                     label = 'Date range input: yyyy-mm-dd',
                                     start = df$firstUpdated, end = df$lastUpdated
                      ),
                      
                      helpText("Choose an end date.  It is best to choose an end data that is at least 30 days later than your start date")
                    )
                    ),
                    column(
                      width = 6,
                      leafletOutput("map")
                    )
                  ),
                  
                  
                  
                  # second row; this reports user selections of dates, pollutants, and location to be (eventually) passed to OpenAQ to retrieve data.
                  fluidRow(
                    tabsetPanel(
                      type = "tabs",
                      # summary tab
                      tabPanel(
                        "  Select Dates and Location",
                        verbatimTextOutput("dateRangeText"),
                        
                        
                        
                        fluidRow(
                          
                        )
                      ),
                      # Pollutant Selection tab
                      tabPanel(
                        "Select your Pollutant",
                        selectInput("pollutant", label = h4("Choose Pollutant"), 
                                    choices = list("PM10" = "PM10", "PM2.5" = "PM2.5","no2"="no2"), 
                                    selected = 1),
                        
                        
                        
                        helpText("While all pollutants are listed here, not all pollutants are measured at all locations and all times.  
                                 Results may not be available; this will be corrected in further revisions of the app.  Please refer to the measurement availability 
                                 in the 'popup' on the map."),
                        
                        hr(),
                        fluidRow(column(3, uiOutput("pollutant")))
                        
                        ),
                      
                      
                      
                      
                      
                      # scenario tab.  Needs work.  Panel should include a summary of user choices (selected on previous panels) and then 
                      # allow a user to enter their email address.  An action button would be pressed to create the output from OpenAir.
                      tabPanel(
                        "Selection Summary and Process Data",
                        fluidRow(
                          # actionButton("goButton", "OpenAir Local!"),
                          # helpText("When you click the button above, you should see",
                          #          "the output below update to reflect the value you",
                          #          "entered at the top:"),
                          
                        )
                        
                        
                      )
                      )
                  )
)
)


#server.r
# server.R for emission dashboard


server <- function(input, output, session) {
  
  # date output -- reports user-selected state & stop dates in UI
  
  output$dateRangeText  <- renderText({
    paste("Date Range is:", 
          paste(as.character(input$dateRange), collapse = " to ")
    )
  })
  
  
  #Stores the value of the pollutant selection to pass to openAQ request
  output$pollutant  <- renderText({
    paste("Selected pollutant is", as.character(input$pollutant))
  })
  
  
  ###################################
  #output$OALpollutant <- renderUI({OALpollutant})
  
  
  ##################################
  # create the map, using dataframe 'locations' which is polled daily (using ropenaq)
  #MOD TO CONSIDER: addd all available measurements to the popup - true/false for each pollutant, and dates of operation.
  
  output$map <- renderLeaflet({
    df %>%
      select(location, latitude, longitude) %>%
      distinct() %>%
      sp_from_data_frame(type = "points") %>%
      leaflet_plot(popup = "location",
                   colour = "purple")
  })
  
  
  #Process Tab
  
  
  OAL_site <- reactive({
    req(input$map_marker_click)
    location %>%
      filter(latitude == input$map_marker_click$lat,
             longitude == input$map_marker_click$lng)
    
  })
  
})
shinyApp(ui, server=server)