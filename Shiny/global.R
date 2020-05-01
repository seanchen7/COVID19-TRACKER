
#Defining Workspace and Loading Source Code 
# rm(list=ls())
# cat("\014")

# setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # Automatically set working directory to folder where the code is located
# input_path <- "./Data//" #Input control

# 
# #Load Packages
required.packages <- c("rstudioapi", "shiny", "shinythemes", "shinydashboard", "formattable", "DT", "openxlsx",
                       "scales", "data.table", "DescTools", "Hmisc", "plotly", "RColorBrewer", "ggplot2", "leaflet",
                       "tigris", "leaftime")
missing.packages <- setdiff(required.packages, rownames(installed.packages()))
if (length(missing.packages) > 0) {install.packages(missing.packages)} #Install missing packages

## Shiny related packages
suppressMessages(library(rstudioapi))
suppressMessages(library(shiny))
suppressMessages(library(shinythemes))
suppressMessages(library(shinydashboard))
suppressMessages(library(formattable))
suppressMessages(library(DT))


suppressMessages(library(data.table))
suppressMessages(library(plotly))
# suppressMessages(library(ggplot2))
suppressMessages(library(leaflet))
suppressMessages(library(leaftime))
suppressMessages(library(tigris))
suppressMessages(library(RColorBrewer))
# suppressMessages(library(htmlwidgets))


## Other tool customizations
skin_color <- "blue"
# my_pallete <- c("#7FC97F", "#FDC086", "#E41A1C")
my_pallete <- brewer.pal(7,"Set3")


#Loa data
load("GEODATA.RData")
load("COVID_Case.RData")
load("MAP_data.RData")



#Limit memory use
case_all <- case_all[date==last_update]

lastest_state <- case_state[date==last_update][order(-cases)]
# latest_county <- case_all[date==last_update][order(-cases)]

# case_min <- 1000
# top_state <- lastest_state[cases>case_min]
state_count <- 20
top_state <- lastest_state[1:state_count]

#####################################################################################################################  
# Mapping function                                                                                                  #
##################################################################################################################### 

# Map settings
map_provider <- "CartoDB.Positron"
radius_control <- 6
range1 <- geo_county$sqrt_persons
popup1 <- paste0("Population in ", geo_county$NAME, " County: ", "<br>", 
                 format(geo_county$total_persons, nsmall=0, big.mark=","),  
                 "<br> Total Confirmed Cases"," (As of ", last_update, "):<br>",  
                 format(geo_county$cases, nsmall=0, big.mark=","))
popup2 <- paste0("Population in ", geo_county$NAME, " County: ", "<br>", 
                 format(geo_county$total_persons, nsmall=0, big.mark=","))
pal1 <- colorBin("Blues", domain = range1, 5, pretty = T)


#Create base map ----
base_map<-leaflet() %>%
  addProviderTiles(map_provider) %>%
  
  #Generate county boundaries
  addPolylines(data = counties_shape, group="County Boundary", color = "orange",
               weight = 0.5, smoothFactor = 0.2) %>%
  #Generate state boundaries
  addPolylines(data = states_shape, group="State Boundary", color = "orange",
               weight = 0.5, smoothFactor = 0.2) %>%
  
  #Add layers control
  addLayersControl(
    overlayGroups = c("State Boundary", "County Boundary"),
    options = layersControlOptions(collapsed = T))  %>%
  hideGroup("County Boundary")


#Generate heat maps
heat_map <- base_map %>%
  
  #Create legends
  addLegend(pal = pal1, values = range1,
            labFormat = labelFormat(transform = function(x) x^2/10^3, big.mark = ","),
            position = "bottomright", 
            title = "Population (000s)") %>% 
  
  addPolygons(data = geo_county, group = "Census",
              fillColor = ~pal1(sqrt_persons), color = "white", # you need to use hex colors
              fillOpacity = 0.7, weight = 1, smoothFactor = 0.2, 
              popup = popup1,
              highlightOptions = highlightOptions(color = "Orange", weight = 2, bringToFront = F)) 


# ----

# Add marker layer ----

# Current map
current_map <- base_map %>% 
  addCircleMarkers(data= count1, ~long, ~lat, group="Total Confirmed Cases", radius= ~sqrt(cases)/radius_control,
                   popup = ~paste0(county, ", ", state, "<br>",
                                   "Total Confirmed Cases: ", format(cases, nsmall=0, big.mark=","),
                                   "<br>", "Total Deaths: ", format(deaths, nsmall=0, big.mark=","),
                                   "<br>", "Last Updated: ", last_update),
                   color = "#E41A1C", stroke = T, fillOpacity = 0.09, weight = 1)

current_map2 <- heat_map %>% 
  addCircleMarkers(data= count1, ~long, ~lat, group="Total Confirmed Cases", radius= ~sqrt(cases)/radius_control,
                   popup = ~paste0(county, ", ", state, "<br>",
                                   "Total Confirmed Cases: ", format(cases, nsmall=0, big.mark=","),
                                   "<br>", "Total Deaths: ", format(deaths, nsmall=0, big.mark=","),
                                   "<br>", "Last Updated: ", last_update),
                   color = "#E41A1C", stroke = T, fillOpacity = 0.09, weight = 1)


# Timelapsed map
time_map <- base_map %>% 
  addTimeline(data = count3,
              sliderOpts = sliderOptions(position = 'bottomleft', steps = (n_intervals)/24, duration = 30000,
                                         formatOutput = htmlwidgets::JS(
                                           "function(date) {return new Date(date).toDateString()}"
                                            )), 
              # enablePlayback = T, enableKeyboardControls = T), 
              timelineOpts = timelineOptions(
                styleOptions = NULL, # make sure default style does not override
                pointToLayer = htmlwidgets::JS(
                  "
                                function(data, latlng) {
                                  return L.circleMarker(
                                    latlng,
                                    {
                                      radius: +data.properties.radius,
                                      color: '#E41A1C',
                                      fillcolor: '#E41A1C',
                                      fillOpacity: 0.09,
                                      stroke: 'TRUE',
                                      weight: 1,
                                    }
                                  );
                                }
                                ")
              )
  )

time_map2 <- time_map %>% 
  #Create legends
  addLegend(pal = pal1, values = range1,
            labFormat = labelFormat(transform = function(x) x^2/10^3, big.mark = ","),
            position = "bottomright", 
            title = "Population (000s)") %>% 
  
  addPolygons(data = geo_county, group = "Census",
              fillColor = ~pal1(sqrt_persons), color = "white", # you need to use hex colors
              fillOpacity = 0.7, weight = 1, smoothFactor = 0.2, 
              popup = popup1,
              highlightOptions = highlightOptions(color = "Orange", weight = 2, bringToFront = F)) 


#----

