
#####################################################################################################################  
# UI function                                                                                                      #
##################################################################################################################### 
dashboardPage(skin = skin_color, 
  dashboardHeader(title = "COVID-19 Tracking in U.S.", titleWidth = 280),
  # Dashoboard Sidebar ----
  dashboardSidebar(width = 280, collapsed = F,
                   sidebarMenu(id = "tabs", 
                               menuItem(strong("Overview"), tabName = "Overview", icon = icon("chart-area")),
                               menuItem(strong("Detailed Charts"), tabName = "Chart", icon = icon("chart-bar")),
                               menuItem(strong("Detailed Maps"), tabName = "Map", icon = icon("map"), startExpanded = T,
                                        menuSubItem("Time Lapsed Map", tabName = "Map3", icon = icon("chart-line")),
                                        # menuSubItem("Time Lapsed Map with Census Overlay", tabName = "Map4", icon = icon("chart-line")),
                                        menuSubItem("Latest Map with Census Overlay", tabName = "Map2", icon = icon("map"))
                                        ),
                               menuItem(strong("Additional Info"), tabName = "Help", icon = icon("question-circle"))
                   )# end of sidebarMenu
  ),#end of dashboardSidebar 
  # ----
  dashboardBody(
    tabItems(
      
      # Overview ----
      tabItem(tabName = "Overview", 
              fluidPage(
                box(title = "Latest U.S. Statistics and Map", width = 12, status = "success", solidHeader = F , collapsible = T,
                  sidebarLayout(
                    sidebarPanel(width=3,
                       fluidRow(
                         p("This website analyzes and visualizes the COVID-19 case data from the NYT. 
                           It may take a few seconds to complete graphics rendering on your browser."),
                         p(em(" For more information and disclaimer, please see 'Additional Info' tab.")),
                         br(),
                         infoBoxOutput("total_case", width = 12),
                         infoBoxOutput("total_death", width = 12),
                         infoBoxOutput("last_update", width = 12)
                       )
                    ), 
                    mainPanel(width=9,
                        box(width = 12,
                          leafletOutput("map1", width = "100%", height = 470)
                        )
                    )
                  ) #end of layout
                ), #end of box
                box(title=em(paste0("Data Plots for Top States by Case Count")), 
                    status = "primary", width = 12, solidHeader = F, collapsible = T,
                    
                         tabBox(width = 12,
                                tabPanel("Latest Total", plotlyOutput("state_case_growth", height = "480px")),
                                tabPanel("New Cases", plotlyOutput("state_case", height = "480px")),
                                tabPanel("Cases over Time", plotlyOutput("time_case")),
                                tabPanel("Cases over Time (Log)", plotlyOutput("time_case2")),
                                tabPanel("Cumulative Deaths", plotlyOutput("state_deaths")),
                                tabPanel("Deaths over Time", plotlyOutput("time_deaths1")),
                                tabPanel("Deaths over Time (Log)", plotlyOutput("time_deaths2"))
                         )

                ) # end of box
              ) #end of page
              ), #end of item
      
      
      # Detailed Charts ----
      tabItem(tabName = "Chart", 
              fluidPage(
                sidebarLayout(
                  sidebarPanel(width=3,
                               fluidRow(
                                 box(title=em("Select Geographies"), status = "primary",  width = 12, solidHeader = F, collapsible = T, 
                                     selectInput("state", "State", state_list, selected = "New York"),
                                     selectizeInput("county", "Counties", county_list[state=="New York", county], 
                                                    selected = county_list[state=="New York", county][1:county_count], multiple = T),
                                     p("Note: The selection of counties will only apply to the first two charts"),
                                 )
                               )
                  ), # end of panel
                  
                  mainPanel(width=9,
                            box(title = paste0("County Level Case Data (As of ", last_update, " )"),
                              status = "success", width = 12, solidHeader = F, collapsible = T,
                              tabBox(width = 12,
                                     tabPanel("Latest Total", plotlyOutput("county_case", height = "480px")),
                                     tabPanel("New Cases", plotlyOutput("county_case_new", height = "480px")),
                                     tabPanel("Top Counties by New Cases", plotlyOutput("county_case_new2", height = "480px")),
                                     tabPanel("Case Growth Rate", plotlyOutput("county_case_new3", height = "480px")),
                                     tabPanel("Case Per Capita", plotlyOutput("county_case_pc", height = "480px"))
                                )
                            )
                  ) # end of panel
                  
                ) #end of sidebar layout    
              ), # end of page
              fluidPage(            
                box(title = "Data for All States and Counties", width = 12, status = "info", solidHeader = F , collapsible = T, collapsed = T,
                    tabBox(width = 12,
                           tabPanel("States", DT::dataTableOutput("contents1")),
                           tabPanel("Counties", DT::dataTableOutput("contents2"))
                    )
                ) # end of box
              ) # end of page
      ),
      # Geo-Analysis ---- 
      
      tabItem(tabName = "Map2",
              div(class="outer",
                  tags$head(
                    # Include our custom CSS
                    includeCSS("./www/styles.css"),
                    includeScript("./www/gomap.js")
                  ),
                  #Map output
                  leafletOutput("map2", height="100%"),
                  
                  tags$div(id="cite",tags$a(href="http://www.linkedin.com/in/seanchen7", 
                                            "Copyright © 2020 by Sean Chen. All rights reserved." , title="Link to Bio"),
                           tags$a(href="https://github.com/nytimes/covid-19-data", 
                                  "COVID-19 data compiled by The New York Times." , title="Github"))
              )
      ), #end of item 
      
      tabItem(tabName = "Map3",
              div(class="outer",
                  tags$head(
                    # Include our custom CSS
                    includeCSS("./www/styles.css"),
                    includeScript("./www/gomap.js")
                  ),
                  #Map output
                  leafletOutput("map3", height="100%"),  

                  tags$div(id="cite",tags$a(href="http://www.linkedin.com/in/seanchen7", 
                                            "Copyright © 2020 by Sean Chen. All rights reserved." , title="Link to Bio"),
                           tags$a(href="https://github.com/nytimes/covid-19-data", 
                                  "COVID-19 data compiled by The New York Times." , title="Github"))
              )
                                    
      ), #end of item 
      # tabItem(tabName = "Map4",
      #         div(class="outer",
      #             tags$head(
      #               # Include our custom CSS
      #               includeCSS("./www/styles.css"),
      #               includeScript("./www/gomap.js")
      #             ),
      #             #Map output
      #             leafletOutput("map4", height="100%"),
      #             
      #             tags$div(id="cite",tags$a(href="http://www.linkedin.com/in/seanchen7",
      #                                       "Copyright © 2020 by Sean Chen. All rights reserved." , title="Link to Bio"),
      #                      tags$a(href="https://github.com/nytimes/covid-19-data",
      #                             "COVID-19 data compiled by The New York Times." , title="Github"))
      #         )
      # ), #end of item 
      # ---- 

      tabItem(tabName = "Help",
              fluidPage(
                h3(strong("Additional Information")),
                h4("Developer Notes:"),
                tags$ul(
                  tags$li("The mapping functionality to render census information is CPU intensive and the server may not be able to support
                            multiple run requests."),
                  tags$li("The detailed maps are about 20 to 40Mb in size and may take some time to render on your browser. You can also 
                            download the map files in html format on ", 
                          a("GitHub", href="https://github.com/seanchen7/COVID-19-Maps"), "."),
                  tags$li("Data exception: All cases in New York City have been assigned with a FIPS code of 36061; Kansas City, MO with 29095"),
                  tags$li(a("Contact Website/Tool Developer", href="mailto:seanchen7@jhu.edu"))
                ),
                hr(),
                
                h4("References:"),
                tags$ul(
                  tags$li(a("NYT COVID-19 data", href="https://github.com/nytimes/covid-19-data")),
                  tags$li(a("NYT US Tracking", href="https://www.nytimes.com/interactive/2020/us/coronavirus-us-cases.html")),
                  tags$li(a("Census data", href="https://www.ffiec.gov/census/censusInfo.aspx")),
                  tags$li(a("Census shape file", href="https://www.census.gov/geographies/mapping-files/time-series/geo/tiger-line-file.html")),
                  tags$li(a("FIPS code", href="https://www.census.gov/geographies/reference-files/2017/demo/popest/2017-fips.html"))
                ),
                br(),
                h4("Disclaimer:"),
                p("The information contained in this website/tool is for general information purposes only and does not represent the views
                of the Tool Developer or the organizations the Tool Developer is affiliated with. 
                The information is provided by the Tool Developer and while we endeavour to keep the information up to date and correct, 
                we make no representations or warranties of any kind,  express or implied, about the completeness, accuracy, reliability, 
                suitability or availability with respect to the website or the  information, products, services, or related graphics contained 
                the website for any purpose. Any reliance you place on such information is therefore strictly at your own risk."),
                p("In no event will we be liable for any loss or damage including without limitation, indirect or consequential loss or damage, or 
                  any loss or damage whatsoever arising from loss of data or profits arising out of, or in connection with, the use of this website.
                  Through this website you are able to link to other websites which are not under the control of website owner. We have no control 
                  over the nature, content and availability of those sites. The inclusion of any links does not necessarily imply a recommendation or 
                  endorse the views expressed within them. Every effort is made to keep the website up and running smoothly. However, 
                  we take no responsibility for, and will not be liable for, the website being temporarily unavailable due to technical 
                  issues beyond our control.")
                
                
              )
      )
                
      
    ) #end of items

  ) # end of dashboard contents
) # end of dashboard UI

