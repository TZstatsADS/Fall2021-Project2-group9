#--------------------------------------------------------------------
###############################Install Related Packages #######################
library(htmltools)
if (!require("leafpop")) {
  install.packages("leafpop")
  library(leafpop)
}
if (!require("htmlTable")) {
  install.packages("htmlTable")
  library(htmlTable)
}


if (!require("sp")) {
  install.packages("sp")
  library(sp)
}
if (!require("mapview")) {
  install.packages("mapview")
  library(mapview)
}
library(leafsync)

if (!require("ggmap")) {
  install.packages("ggmap")
  library(ggmap)
}
if (!require("dplyr")) {
  install.packages("dplyr")
  library(dplyr)
}
if (!require("eeptools")) {
  install.packages("eeptools")
  library(eeptools)
}
if (!require("shinythemes")) {
  install.packages("shinythemes")
  library(shinythemes)
}

if (!require("sf")) {
  install.packages("sf")
  library(sf)
}
if (!require("RCurl")) {
  install.packages("RCurl")
  library(RCurl)
}
if (!require("tmap")) {
  install.packages("tmap")
  library(tmap)
}
if (!require("rgdal")) {
  install.packages("rgdal")
  library(rgdal)
}
if (!require("leaflet")) {
  install.packages("leaflet")
  library(leaflet)
}
if (!require("shiny")) {
  install.packages("shiny")
  library(shiny)
}
if (!require("shinythemes")) {
  install.packages("shinythemes")
  library(shinythemes)
}
if (!require("plotly")) {
  install.packages("plotly")
  library(plotly)
}
if (!require("ggplot2")) {
  install.packages("ggplot2")
  library(ggplot2)
}
if (!require("viridis")) {
  install.packages("viridis")
  library(viridis)
}

#--------------------------------------------------------------------

##############################################################
# Data Sources
#   nyc Health data
#   get out data
# get NYC covid data based on Modified Zip code
# First get ZCTA (zip code) to MODZCTA data:


app_data <- read.csv(url("https://data.cityofnewyork.us/api/views/8gpu-s594/rows.csv?accessType=DOWNLOAD") )

## support year selection option

year.categories <- c("2015","2016","2017","2018","2019","2020","2021" )

year.split <- format(as.POSIXct(app_data$DATE.APPLICATION.SUBMITTED.TO.SED,format="%m/%d/%Y"),"%Y")

app_data$year.split <- year.split
# COMBINE STEPS INTO A FUNCTION
max.lat <- 40.50828
max.lon <- -73.70914
min.lat <- 40.90355
min.lon <- -74.24412

plotMap <- function( application, background.color="black",dot.thickness = 0.5 )
{
  
 
  
  
}

#View(app_data.sub1)




## SERVER 

my.server <- function(input, output) 
{
  
  
  
  output$mapPlot1 <-renderLeaflet({  
    
    app_data.sub1 <- app_data[ app_data$year.split == input$year1,  ]
    
    app_data.sub2 <- app_data[app_data$year.split == input$year2, ]
    
    
    #application.count <- as.data.frame( table( application$PROJECT.NUMBER ) )
    
    #application.weight <- dot.thickness * ( application.count$Freq / 16876 )
    
    if (input$cluster1 == "ENABLE")
    {
    map1 <- leaflet(app_data.sub1) %>%
      addTiles(group = "OSM") %>%
      addProviderTiles(providers$Stamen.TonerLite, group = "Toner Lite") %>%
      setView(-74.00, 40.71, zoom = 11)%>%
      addCircleMarkers(lng = ~LONGITUDE, lat = ~LATITUDE, group = "point",
                       popup = popupTable(app_data.sub1),
                       clusterOptions = markerClusterOptions())%>%
      addLayersControl(baseGroups = c("OSM", "Toner Lite"), 
                       overlayGroups = c("point"),
                       options = layersControlOptions(collapsed = FALSE))
    }
    else
    {map1 <- leaflet(app_data.sub1) %>%
      addTiles(group = "OSM") %>%
      addProviderTiles(providers$Stamen.TonerLite, group = "Toner Lite") %>%
      setView(-74.00, 40.71, zoom = 11)%>%
      addCircleMarkers(lng = ~LONGITUDE, lat = ~LATITUDE, group = "point",
                       popup = popupTable(app_data.sub1),)%>%
      addLayersControl(baseGroups = c("OSM", "Toner Lite"), 
                       overlayGroups = c("point"),
                       options = layersControlOptions(collapsed = FALSE))
      
    }
     
    #latticeView(map1, map2, ncol = 2, sync = list(c(1, 2)), sync.cursor = FALSE, no.initial.sync = FALSE)
    #plotMap( application = app_data.sub1 )
    #title( main=toupper(paste(input$year1,sep=" : ")), line=-3, cex.main=1, col.main="white" )
    #plotMap( application = app_data.sub2 )
    #mapview::latticeView(map1, map2, ncol = 2, sync = list(c(1, 2)), sync.cursor = FALSE, no.initial.sync = FALSE)
    #title( main=toupper(paste(input$year2,sep=" : ")), line=-3, cex.main=1, col.main="white" )
    
  })  # 
  
  output$mapPlot2 <-renderLeaflet({  
    
    
    app_data.sub2 <- app_data[app_data$year.split == input$year2, ]
    
    if(input$cluster2 == "ENABLE")
    {
    map2 <- leaflet(app_data.sub2) %>%
      addTiles(group = "OSM") %>%
      addProviderTiles(providers$Stamen.TonerLite, group = "Toner Lite") %>%
      setView(-74.00, 40.71, zoom = 11)%>%
      addCircleMarkers(lng = ~LONGITUDE, lat = ~LATITUDE, group = "point",
                       popup = popupTable(app_data.sub2),
                       clusterOptions = markerClusterOptions())%>%
      addLayersControl(baseGroups = c("OSM", "Toner Lite"), 
                       overlayGroups = c("point"),
                       options = layersControlOptions(collapsed = FALSE))
    }
    else
    {
      map2 <- leaflet(app_data.sub2) %>%
        addTiles(group = "OSM") %>%
        addProviderTiles(providers$Stamen.TonerLite, group = "Toner Lite") %>%
        setView(-74.00, 40.71, zoom = 11)%>%
        addCircleMarkers(lng = ~LONGITUDE, lat = ~LATITUDE, group = "point",
                         popup = popupTable(app_data.sub2))%>%
        addLayersControl(baseGroups = c("OSM", "Toner Lite"), 
                         overlayGroups = c("point"),
                         options = layersControlOptions(collapsed = FALSE))
    }
    
  })  
  
}




### USER INTERFACE 

my.ui <- fluidPage(
  
  # theme = shinytheme("cyborg"),
  theme = shinytheme("paper"),
  
  # Application title
  titlePanel("Application for State Aid"),
  
  # Sidebar with a slider input for the number of bins
  sidebarLayout(
    
    sidebarPanel(
      
      h2( helpText("First Map") ),
      
      selectInput( inputId="year1", 
                   label="Select Year", 
                   choices= year.categories,
                   selected="2019"
      ),
      
      selectInput( inputId="cluster1", 
                   label="Cluster Option", 
                   choices= c("ENABLE","DISABLE"),
                   selected="yes"
      ),
    
      h2( helpText("Second Map") ),
      
      selectInput( inputId="year2", 
                   label="Select Year", 
                   choices= year.categories,
                   selected="2019"
      ),
      selectInput( inputId="cluster2", 
                   label="Cluster Option", 
                   choices= c("ENABLE","DISABLE"),
                   selected="yes"
      )
    ),
    
    # Show a plot of the generated distribution
    mainPanel( fluidRow( column(12, leafletOutput( "mapPlot1" ) ) ),
               HTML("<hr>"),
               fluidRow( column(12, leafletOutput( "mapPlot2" ) ) )
               )
    
  )
)




### LAUNCH THE APP !

shinyApp( ui = my.ui, server = my.server )

