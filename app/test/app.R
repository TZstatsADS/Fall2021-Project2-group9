#--------------------------------------------------------------------
###############################Install Related Packages #######################
library(htmltools)
if (!require("leafpop")) {
  install.packages("leafpop")
  library(leafpop)
}
library(htmltools)
if (!require("shinydashboard")) {
  install.packages("shinydashboard")
  library(shinydashboard)
}
if (!require("htmlTable")) {
  install.packages("htmlTable")
  library(htmlTable)
}
if (!require("leaflet.minicharts")) {
  install.packages("leaflet.minicharts")
  library(leaflet.minicharts)
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
if (!require("forcats")) {
  install.packages("forcats")
  library(forcats)
}
if (!require("tidyverse")) {
  install.packages("tidyverse")
  library(tidyverse)
}
#--------------------------------------------------------------------

##############################################################
# Data Sources



app_data <- read.csv(url("https://data.cityofnewyork.us/api/views/8gpu-s594/rows.csv?accessType=DOWNLOAD") )

## support year selection option

year.categories <- c("2015","2016","2017","2018","2019","2020","2021" )

year.split <- format(as.POSIXct(app_data$DATE.APPLICATION.SUBMITTED.TO.SED,format="%m/%d/%Y"),"%Y")

app_data$year.split <- year.split

modified_date <-as.Date(as.POSIXct(app_data$DATE.APPLICATION.SUBMITTED.TO.SED,format="%m/%d/%Y"))

app_data$modified_date <- modified_date

months.categories <- c("NONE","01","02","03","04","05","06","07","08","09","10","11","12" )

months.split <- format(as.POSIXct(app_data$DATE.APPLICATION.SUBMITTED.TO.SED,format="%m/%d/%Y"),"%m")

app_data$months.split <- months.split

table(months.split)




#View(app_data.sub1)
temp <- app_data %>%
  filter(BOROUGH != "") %>%
  mutate(BOROUGH = as.factor(BOROUGH)) 

temp$BOROUGH <- fct_collapse(temp$BOROUGH,
                             MANHATTAN = c("MANHATTAN", "1",  "Manhattan"),
                             BRONX = c("BRONX", "Bronx", "2", "BRONX    "),
                             BROOKLYN = c("BROOKLYN", "3", "Brooklyn"),
                             QUEENS = c("QUEENS", "4", "QUEENS   ", "Queens" ),
                             STATEN_ISLAND = c("STATEN IS", "5", "Staten Island"))

temp <- temp %>%
  group_by(modified_date, BOROUGH) %>%
  mutate(count_per_BOROUGH = n()) %>%
  mutate(SED.APPROVED.ESTIMATE_SUM = sum(SED.APPROVED.ESTIMATE))



p <- temp %>%
  ggplot( aes(x = modified_date, y = count_per_BOROUGH, size = SED.APPROVED.ESTIMATE_SUM, color=BOROUGH)) +
  geom_point() +
  ylab("Applications Submitted per Borough") +
  xlab("Date of Application") +
  ggtitle("Applications per Borough") +
  theme_bw()

# COMBINE STEPS INTO A FUNCTION

## SERVER 

my.server <- function(input, output) 
{
  
  
  
  output$mapPlot1 <-renderLeaflet({  
    
    if(input$month1 == "NONE")
    {app_data.sub1 <- app_data[ app_data$year.split == input$year1,  ]}
    else
    {
      app_data.sub1 <- app_data[ app_data$year.split == input$year1 & app_data$months.split == input$month1, ]
    }
    
    
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
                       popup = popupTable(app_data.sub1))%>%
      addLayersControl(baseGroups = c("OSM", "Toner Lite"), 
                       overlayGroups = c("point"),
                       options = layersControlOptions(collapsed = FALSE))
      
    }
     

    
  })  # 
  

  
    output$Plot0 <- renderPlot({
      
      ggplotly(p)
      
    })
  
  
  
  
  
  
  output$mapPlot2 <-renderLeaflet({  
    
    
    if(input$month2 == "NONE")
    {app_data.sub2 <- app_data[ app_data$year.split == input$year2,  ]}
    else
    {
      app_data.sub2 <- app_data[ app_data$year.split == input$year2 & app_data$months.split == input$month2, ]
    }
    
    
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
  # ------------------ Home ----------------------------------------------------------------
  
  
  # theme = shinytheme("cyborg"),
  theme = shinytheme("paper"),
  
  # Application title
  titlePanel("Applications for State Aid"),
  
  # Sidebar with a slider input for the number of bins
  sidebarLayout(
    
    sidebarPanel(
      
      
      h2( helpText("First Map") ),
      
      selectInput( inputId="year1", 
                   label="Select Year", 
                   choices= year.categories,
                   selected="2019"
      ),
      selectInput( inputId="month1", 
                   label="Select Month", 
                   choices= months.categories,
                   selected="NONE"
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
      selectInput( inputId="month2", 
                   label="Select Month", 
                   choices= months.categories,
                   selected="NONE"
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
body <- dashboardBody(
  
  tags$style(HTML("


.box.box-solid.box-primary>.box-header {
  color:#fff;
  background:#D6EAF8
                    }

.box.box-solid.box-primary{
border-bottom-color:#154360;
border-left-color:#154360;
border-right-color:#154360;
border-top-color:#154360;
}

.skin-black .main-sidebar {
background-color: #154360;
}
")),
  
  
  
  tabItems(
    # ------------------ Introduction ----------------------------------------------------------------
    
    tabItem(tabName = "Introduction", fluidPage(
      
      fluidRow(box(width = 15, title = "Introduction", status = "primary",
                   solidHeader = TRUE, h3("An Interactive View of Applications for State Aid"),
                   h4("An Interface for Determining The Status of NYC Educational Facilities"),
                   h6("Created by : Emily Jennings-Dobbs, Chuyang Liu, Jialiang Sun, and Ziyong Zhang"),
                   h6("Intended for use in:", strong("Education, goverment/legislative activity, construction, retail")))),
      fluidRow(box(width = 15, title = "Intended Purpose", status = "primary", solidHeader=TRUE,
                   h5("General Applications:"),
                   h6("This app can display applications for aid for all educational facilities funded by the state in the NYC area. "),
                   h6( "It can be used to visualise the disruption and possible long term consequences of local or global disasters to the maitenance of educational facilities. This can help us to understand where aid is needed most geographically in the process of recovering from unpredictable and/or unexpected events."),
                   h6("Other possible uses include but are not limited to:"),
                   h6("Monitoring the locations of infrastructure in disrepair to maximize the returns of advertising for contstruction labor or equipment"),
                   h6("Establishing which school districts have the most well-kept facilities for either real-estate or personal reasons"),
                   h6("Determining what times of year will require more labor and equipment for maitenance or creation of educational facilities."),
                   h5("In Regards to COVID:"),
                   h6("During COVID a lot of schools were left empty and abandoned for months on end. Without constant maitenance and monitoring, facilities are bound to find themselves in disrepair. With this tool we can monitor where the worst of the damage was found and take a look at how long it's taking for the aid applications to return to a normal rate. "))),
      fluidRow(box(width = 15, title = "How to Use The App", status = "primary",
                   solidHeader = TRUE,
                   h5("The application is divided into 4 separate tabs"),
                   tags$div(tags$ul(
                     tags$li("The", strong("first"), "tab: Introduction"),
                     tags$li("The", strong("second"), "tab: Two maps of NYC that can be used to compare details of all state applications for aid filed in a given time period."),
                     tags$li("The", strong("third"), "tab: An interactive plot of frequency of applications and amount of money approved for each application per borough. Hover over a specific point for more details."),
                     tags$li("The", strong("fourth"), "tab: Acknowledgements and other information")
                   )))),
      fluidRow(box(width = 15, title = "Note on Included Data", status = "primary",
                   h6("Any application data that did not specify the location related to the application had to be excluded from the data. For more information on the data visit NYC OpenData (link in acknowledgements tab)")))
    )), # end of intro 
    # ------------------ Map-----------------------------------
    tabItem(tabName = "Map",my.ui
            
    ),
    
    #------------------Plot----------------------------
    tabItem(tabName = "Plot", ggplotly(p)
    
    ), 
    
    
    
    # ------------------ Acknowledgments --------------------------------
    tabItem(tabName = "Acknowledgments", fluidPage( 
      HTML(
        "<h2> Data Source </h2>
                <h4> <p><li>NYC State Aid Application Data: <a href='https://data.cityofnewyork.us/Education/Application-for-State-Aid/8gpu-s594'>NYC OpenData</a></li></h4>"
      ),
      
      titlePanel("R Packages"),
      
HTML(
        " <p>This application is built using R shiny app.</p>",
        "<p>The following R packages were used in to build this RShiny application:</p>
          <li>leafpop</li>
          <li>shinythemes</li>
          <li>htmlTable</li>
          <li>Tidyverse</li>
          <li>leaflet</li>
          <li>shinydashboard</li>
          <li>sp</li>
          <li>mapview</li>
          <li>leafsync</li>
          <li>ggmap</li>
          <li>dplyr</li>
          <li>eeptools</li>
          <li>sf</li>
          <li>tmap</li>
          <li>rgdal</li>
          <li>shiny</li>
          <li>Rcurl</li>
          <li>plotly</li>
          <li>ggplot2</li>"
      ),
      
      titlePanel("Created By"),
      
      HTML(
        " <p>This app was made in a collaborative effort by:</p>",
        " <p>Jennings-Dobbs, Emily (emj2153@columbia.edu) </p>",
        " <p>Liu, Chuyang (cl4013@columbia.edu)</p>",
        " <p>Sun, Jialiang (js5951@columbia.edu) </p>",
        " <p>Zhang, Ziyong (zz2893@columbia.edu)</p>")
    )) # end of tab
    
  ) # end of tab items
) # end of body

ui <- dashboardPage(
  title="Applications for State Aid",
  skin = "black", 
  dashboardHeader(title=span("Applications for State Aid",style="font-size: 16px")),
  
  dashboardSidebar(sidebarMenu(
    menuItem("Introduction", tabName = "Introduction"),
    menuItem("Map", tabName = "Map"),
    menuItem("Plot", tabName = "Plot"),
    menuItem("Acknowledgments", tabName = "Acknowledgments")
  )),
  
  body 
)

### LAUNCH THE APP !

shinyApp( ui = ui, server = my.server )

