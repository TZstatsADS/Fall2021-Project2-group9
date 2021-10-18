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

modified_date <-as.Date(as.POSIXct(app_data$DATE.APPLICATION.SUBMITTED.TO.SED,format="%m/%d/%Y"))

app_data$modified_date <- modified_date

months.categories <- c("NONE","01","02","03","04","05","06","07","08","09","10","11","12" )

months.split <- format(as.POSIXct(app_data$DATE.APPLICATION.SUBMITTED.TO.SED,format="%m/%d/%Y"),"%m")

app_data$months.split <- months.split

table(months.split)

# COMBINE STEPS INTO A FUNCTION

#View(app_data.sub1)




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
     
    #latticeView(map1, map2, ncol = 2, sync = list(c(1, 2)), sync.cursor = FALSE, no.initial.sync = FALSE)
    #plotMap( application = app_data.sub1 )
    #title( main=toupper(paste(input$year1,sep=" : ")), line=-3, cex.main=1, col.main="white" )
    #plotMap( application = app_data.sub2 )
    #mapview::latticeView(map1, map2, ncol = 2, sync = list(c(1, 2)), sync.cursor = FALSE, no.initial.sync = FALSE)
    #title( main=toupper(paste(input$year2,sep=" : ")), line=-3, cex.main=1, col.main="white" )
    
  })  # 
  
  ## case_count
  
    output$tsPlot0 <- renderPlot({
      
      if(input$measure == "Case Count")
      {
        data = as.data.frame(table(app_data$year.split))
        plot(num ~ as.Date(month), data, xaxt = "n", type = "o", pch = 22, lty = 1, pty = 2,
             ylab = "monthly confirmed cases", xlab = "",
             main = paste("Number of confirmed cases in ",  input$borough))
        axis.Date(1, at = data$month, format= "%m-%Y", las = 1)
      }
      
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
  background:#F0F8FF
                    }

.box.box-solid.box-primary{
border-bottom-color:#666666;
border-left-color:#666666;
border-right-color:#666666;
border-top-color:#666666;
}

.skin-black .main-sidebar {
background-color: #1E90FF;
}
")),
  
  
  
  tabItems(
    # ------------------ Home ----------------------------------------------------------------
    
    tabItem(tabName = "Home", fluidPage(
      
      fluidRow(box(width = 15, title = "Introduction", status = "primary",
                   solidHeader = TRUE, h3("Application for State Aid"),
                   h4("Jennings-Dobbs Emily, Liu Chuyang, Sun, Jialiang, Zhang Ziyong"),
                   h5("to be filled"),
                   h5("to be filled", strong("Retail, Service, Food and Beverage, Entertainment")))),
      fluidRow(box(width = 15, title = "Targeted User", status = "primary", solidHeader=TRUE,
                   h5("to be filled"))),
      fluidRow(box(width = 15, title = "How to Use The App", status = "primary",
                   solidHeader = TRUE,
                   h5("The application is divided into 3 separate tabs"),
                   tags$div(tags$ul(
                     tags$li("The", strong("first"), "tab: Introduction"),
                     tags$li("The", strong("second"), "tab: The detailed ZIP code map shows the extent of Covid 19 outbreak in NYC. It provided key information including: confirmed cases, infection rate, number of business that are closed in the neighborhood"),
                     tags$li("The", strong("third"), "to be filled"),

                   ))
      ))
    )), # end of home 
    # ------------------ Map-----------------------------------
    tabItem(tabName = "Map",my.ui
            
    ),
    
    #------------------New Business----------------------------
    tabItem(tabName = "New_Business", fluidPage(
      
      # App title ----
      titlePanel("New Business"),
      
      # Sidebar layout with input and output definitions ----
      sidebarLayout(
        
        # Sidebar panel for inputs ----
        sidebarPanel(
          
          # Input: Select for the borough ----
          selectInput(inputId = "measure",
                      label = "Measure:",
                      choices = c("Case Count", "Approved Estimate"),
                      selected="Case Count"),
          
          # Input: Select for the business type ----
          selectInput(inputId = "cal_type",
                      label = "Calculation Type:",
                      choices = c("Sum", "Average"),
                      selected="Sum")
          
        ),
        
        # Main panel for displaying outputs ----
        mainPanel(
          
          # Output: tsPlot on borough ----
          plotOutput("tsPlot0")
          
        )
      )
    )
    ), 
    
    
    
    # ------------------ Appendix --------------------------------
    tabItem(tabName = "Appendix", fluidPage( 
      HTML(
        "<h2> Data Sources </h2>
                <h4> <p><li>NYC Covid 19 Data: <a href='https://github.com/nychealth/coronavirus-data'>NYC covid 19 github database</a></li></h4>
                <h4><li>NYC COVID-19 Policy : <a href='https://www1.nyc.gov/site/coronavirus/businesses/businesses-and-nonprofits.page' target='_blank'> NYC Citywide Information Portal</a></li></h4>
                <h4><li>NYC Business data : <a href='https://data.cityofnewyork.us/Business/Legally-Operating-Businesses/w7w3-xahh' target='_blank'>NYC Open Data</a></li></h4>
                <h4><li>NYC Business Application Data : <a href='https://data.cityofnewyork.us/Business/License-Applications/ptev-4hud' target='_blank'>NYC Open Data</a></li></h4>
                <h4><li>NYC Minority Owned Business : <a href='https://data.cityofnewyork.us/Business/M-WBE-LBE-and-EBE-Certified-Business-List/ci93-uc8s' target='_blank'>NYC Health + Hospitals</a></li></h4>
                <h4><li>NYC Geo Data : <a href='https://github.com/ResidentMario/geoplot-data-old' target='_blank'> Geoplot Github</a></li></h4>"
      ),
      
      titlePanel("Disclaimers "),
      
      HTML(
        " <p>We drew our business insights from NYC Open data, specifically business expiration databases. We recognized that there would be a lag between when the business is closed and when the expiration date, status are updated.</p>",
        " <p>Thus our app may understate the number of businesses that were actually closed. Furthermore, due to the lag between the time point where the business were closed, with when the expiration date be updated, there could be some uncertainty to define on which day, month the businesses were fully closed
 </p>"),
      
      titlePanel("Acknowledgement  "),
      
      HTML(
        " <p>This application is built using R shiny app.</p>",
        "<p>The following R packages were used in to build this RShiny application:</p>
                <li>Shinytheme</li>
                <li>Tidyverse</li>
                <li>Dyplr</li><li>Tibble</li><li>Rcurl</li><li>Plotly</li>
                <li>ggplot2</li>"
      ),
      
      titlePanel("Contacts"),
      
      HTML(
        " <p>For more information please feel free to contact</p>",
        " <p>Wendy Doan(ad3801@columbia.edu) </p>",
        " <p>Qizhen Yang(qy2446@columbia.edu)</p>",
        " <p>Yandong Xiong(yx2659@columbia.edu) </p>",
        " <p>TQiao Li(ql2403@columbia.edu)</p>",
        " <p>James Smiley(jbs2253@columbia.edu) </p>")
    )) # end of tab
    
  ) # end of tab items
) # end of body

ui <- dashboardPage(
  title="Application for State Aid",
  skin = "black", 
  dashboardHeader(title=span("Application for State Aid",style="font-size: 16px")),
  
  dashboardSidebar(sidebarMenu(
    menuItem("Home", tabName = "Home"),
    menuItem("Map", tabName = "Map"),
    menuItem("need change", tabName = "New_Business"),
    menuItem("need change", tabName = "Appendix")
  )),
  
  body 
)

### LAUNCH THE APP !

shinyApp( ui = ui, server = my.server )

