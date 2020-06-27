
# Load Packages
required.packages <- c("rstudioapi", "shiny", "shinythemes", "shinydashboard", "formattable", "DT", "openxlsx",
                       "scales", "data.table", "DescTools", "Hmisc", "plotly", "RColorBrewer", "ggplot2", "leaflet",
                       "tigris", "leaftime", "zoo")
missing.packages <- setdiff(required.packages, rownames(installed.packages()))
if (length(missing.packages) > 0) {install.packages(missing.packages)} #Install missing packages

## Shiny related packages
suppressMessages(library(rstudioapi))
suppressMessages(library(shiny))
suppressMessages(library(shinythemes))
suppressMessages(library(shinydashboard))
suppressMessages(library(formattable))
suppressMessages(library(DT))

## Data and plot
suppressMessages(library(data.table))
suppressMessages(library(zoo))
suppressMessages(library(plotly))
suppressMessages(library(leaflet))
suppressMessages(library(leaftime))
suppressMessages(library(tigris))
suppressMessages(library(RColorBrewer))


# Load data
load("GEODATA.RData")
load("COVID_Case.RData")
load("MAP_data.RData")


# Filter data
county_list <- case_all[order(-cases)]
county_list <- unique(county_list[, .(state, county, FIPSCOUNTY)])
county_count <- 20

state_latest <- case_state[date==last_update][order(-cases)]
state_count <- 20
state_top <- state_latest[1:state_count]
state_list <- unique(state_latest[order(state)][, state])
## Filter to include only states with county-level data
state_list <- state_list[state_list %in% unique(county_list[, state])] 

# Other tool customization
skin_color <- "blue"

## Plot customization
my_pallete <- brewer.pal(8,"Set3")
my_pallete2 <- brewer.pal(8,"Paired")

#####################################################################################################################  
# Mapping function                                                                                                  #
##################################################################################################################### 

# Map settings
map_provider <- "CartoDB.Positron"
radius_control <- 8
range1 <- geo_county$sqrt_persons
popup1 <- paste0("Population in ", geo_county$NAME, " County: ", "<br>", 
                 format(geo_county$total_persons, nsmall=0, big.mark=","),  
                 "<br> Total Confirmed Cases"," (As of ", last_update, "):<br>",  
                 format(geo_county$cases, nsmall=0, big.mark=","))
popup2 <- paste0("Population in ", geo_county$NAME, " County: ", "<br>", 
                 format(geo_county$total_persons, nsmall=0, big.mark=","))
pal1 <- colorBin("Blues", domain = range1, 5, pretty = T)
# pal2 = colorBin(brewer.pal(8, "OrRd"), domain = geo_state$rolling, 4, pretty = T)
pal2 = colorNumeric("YlOrRd", domain = geo_state$rolling)

# State map ----

state_map<-leaflet() %>%
  addProviderTiles(map_provider) %>%
  
  #Generate state boundaries
  addPolylines(data = states_shape, group="State Boundary", color = "orange",
               weight = 0.5, smoothFactor = 0.2) %>%

  #Create legends
  addLegend(pal = pal2,
            values = geo_state$rolling,
            labFormat = labelFormat(transform = function(x) x, big.mark = ","),
            position = "bottomright",
            title = "New Cases: <br> 7-Day Average") %>%
  
  addPolygons(data = geo_state, group = "Case",
              fillColor = ~pal2(rolling), color = "white",
              fillOpacity = 0.7, weight = 1, smoothFactor = 0.2, 
              popup = paste0(geo_state$state, "<br>", 
                             "Total Confirmed Cases",  ": ", format(geo_state$cases, nsmall=0, big.mark=","), "<br>", 
                             "Daily New Cases: ", format(geo_state$case_delta, nsmall=0, big.mark=","), "<br>", 
                             "7-Day Moving Average: ", format(round(geo_state$rolling, 0), nsmall=0, big.mark=",")), 
              highlightOptions = highlightOptions(color = "Orange", weight = 2, bringToFront = F)) 

# ----

#Create county base map ----
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
              sliderOpts = sliderOptions(position = 'bottomleft', steps = (n_intervals1)/24, duration = 20000,
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

# Timelapsed map
time_map2 <- base_map %>% 
  addTimeline(data = count4,
              sliderOpts = sliderOptions(position = 'bottomleft', steps = (n_intervals2)/24, duration = 20000,
                                         formatOutput = htmlwidgets::JS(
                                           "function(date) {return new Date(date).toDateString()}"
                                         )), 
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

#----

