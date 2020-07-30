

#####################################################################################################################  
# Server function                                                                                                   #
##################################################################################################################### 

server <- function(input, output, session) {

  # Data overview ----
  output$contents1  <-  renderDataTable({
    datatable(case_state[,. (date, state, fips, cases, deaths)][order(-date, -cases)], 
              options = list(pageLength = 25))
  })
  
  output$contents2  <-  renderDataTable({
    datatable(case_all[,. (date, state, county, FIPSCOUNTY, cases, deaths, total_persons)][order(-date, -cases)], 
              options = list(pageLength = 25))
  })
  
  output$total_case <- renderInfoBox({
    infoBox(
      title = "Total Cases", value = format(total_case, nsmall=0, big.mark=","),
      icon = icon("procedures"), fill = TRUE, color = "orange"
    )
  })
  
  output$total_death <- renderInfoBox({
    infoBox(
      title = "Total Deaths", value = format(total_deaths, nsmall=0, big.mark=","),
      icon = icon("user-friends"), fill = TRUE, color = "maroon"
    )
  })
  
  output$last_update <- renderInfoBox({
    infoBox(
      title = "Data Updated", value = last_update,
      icon = icon("calendar"), fill = TRUE, color = "olive"
    )
  })
  #----
  
  # State Plots ----
  output$state_case <- renderPlotly({
    
    DT1 = state_top
    # DT1$state <- factor(DT1$state, levels = unique(DT1$state)[order(DT1$cases, decreasing = TRUE)])
    # plot_ly(DT1, x = ~state, y = ~cases, name = ~state, color = ~state, colors = my_pallete, 
    DT1$state <- factor(DT1$state, levels = unique(DT1$state)[order(DT1$case_delta, decreasing = TRUE)])
    plot_ly(DT1, x = ~state, y = ~case_delta, name = ~state, color = ~state, colors = my_pallete, 
            type = "bar") %>%
      layout(title = "Daily New Confirmed Cases",
             xaxis = list(title = ""), yaxis = list(title = ""), 
             showlegend = FALSE)
  })
  
  output$state_case_growth <- renderPlotly({
    
    DT1 <- case_state[date %in% c(last_update, last_update-1)][order(-cases)]
    DT1 <- DT1[state %in% state_top[, state]]
    DT1$state <- factor(DT1$state, levels = unique(DT1$state)[order(DT1$cases, decreasing = TRUE)])
    
    plot_ly(DT1[date == (last_update-1)], x = ~state, y = ~cases, colors = my_pallete, name = "Last Reported",
            type = "bar") %>%
      add_trace(data = DT1[date == (last_update)], y = ~case_delta, name = 'New Cases') %>%
      layout(title = paste0("Total Confirmed Cases: Updated ", last_update), barmode = 'stack',
             xaxis = list(title = ""), yaxis = list(title = ""), legend = list(x = 0.8, y = 0.9))
    
  })
  
  
  output$state_deaths <- renderPlotly({
    
    DT1 = state_top
    DT1$state <- factor(DT1$state, levels = unique(DT1$state)[order(DT1$deaths, decreasing = TRUE)])
    plot_ly(DT1, x = ~state, y = ~deaths, name = ~state, color = ~state, colors = my_pallete, type = "bar") %>%
      layout(title = "Total Deaths",
             xaxis = list(title = ""),
             yaxis = list(title = ""), 
             showlegend = FALSE)
    
  })
  
  time_case_plot <-  plot_ly(
    case_state[state %in% state_top[, state]], 
    x = ~date, y = ~cases, name = ~state, color = ~state, colors = my_pallete) %>%
      add_lines()  %>%
      layout(title = "",
             xaxis = list(title = "Date Slider",
                          range = c(as.Date("2020-03-01"), last_update),
                          rangeslider = list(type = "date")),
             yaxis = list(title = ""))
  
  output$time_case <- renderPlotly({time_case_plot})
  
  output$time_case2 <- renderPlotly({time_case_plot %>% 
      layout(yaxis = list(type = "log"))})
  
  time_deaths_plot <- 
    plot_ly(case_state[state %in% state_top[, state]], 
            x = ~date, y = ~deaths, name = ~state, color = ~state, colors = my_pallete) %>%
    add_lines()  %>%
    layout(title = "",
           xaxis = list(title = "Date Slider",
                        range = c(as.Date("2020-03-01"), last_update),
                        rangeslider = list(type = "date")),
           yaxis = list(title = ""))
  
  output$time_deaths1 <- renderPlotly({time_deaths_plot})
  output$time_deaths2 <- renderPlotly({time_deaths_plot  %>% 
      layout(yaxis = list(type = "log"))})
  
  #----

  # State level growth
 
  output$state_growth <- renderPlotly({
    DT1 = case_state[state==input$state]
    DT1[, rolling:=rollmean(case_delta, k=7, fill = NA, align = "right")]
    plot_ly(DT1,  x = ~date, y = ~case_delta, type = "bar", name = "Daily increase") %>%
      add_lines(data = DT1, x = ~date, y = ~rolling, name = "7-day average")  %>%
      layout(title = "Daily New Confirmed Cases",
             xaxis = list(title = "Date Slider",
                          range = c(as.Date("2020-03-01"), last_update),
                          rangeslider = list(type = "date")),
             yaxis = list(title = ""),
             legend = list(x = 0.01, y = 0.9))
    })

  
  # County Plots ----
  
  output$county_case <- renderPlotly({
    
    DT1 = case_all[state==input$state & county %in% input$county]
    DT1$county <- factor(DT1$county, levels = unique(DT1$county)[order(DT1$cases, decreasing = TRUE)])
    plot_ly(DT1, x = ~county, y = ~cases, name = ~county, color = ~county, colors = my_pallete2, 
            type = "bar") %>%
      layout(title = "Total Confirmed Cases",
             xaxis = list(title = ""), yaxis = list(title = ""), 
             showlegend = FALSE)
  })
  
  output$county_case_new <- renderPlotly({
    
    DT1 = case_all[state==input$state & county %in% input$county]
    
    DT1$county <- factor(DT1$county, levels = unique(DT1$county)[order(DT1$case_delta, decreasing = TRUE)])
    plot_ly(DT1, x = ~county, y = ~case_delta, name = ~county, color = ~county, colors = my_pallete2, 
            type = "bar") %>%
      layout(title = "Daily New Confirmed Cases",
             xaxis = list(title = ""), yaxis = list(title = ""), 
             showlegend = FALSE)
  })
  
  output$county_case_new2 <- renderPlotly({
    
    DT1 = case_all[state==input$state][order(-case_delta)][1:county_count]
    
    DT1$county <- factor(DT1$county, levels = unique(DT1$county)[order(DT1$case_delta, decreasing = TRUE)])
    plot_ly(DT1, x = ~county, y = ~case_delta, name = ~county, color = ~county, colors = my_pallete, 
            type = "bar") %>%
      layout(title = "Daily New Confirmed Cases",
             xaxis = list(title = ""), yaxis = list(title = ""), 
             showlegend = FALSE)
  })
  
  output$county_case_new3 <- renderPlotly({
    
    DT1 = case_all[state==input$state]
    DT1[, case_pct:=case_delta/cases]
    DT1 = DT1[order(-case_pct)][1:county_count]
    
    DT1$county <- factor(DT1$county, levels = unique(DT1$county)[order(DT1$case_pct, decreasing = TRUE)])
    plot_ly(DT1, x = ~county, y = ~case_pct, name = ~county, color = ~county, colors = my_pallete, 
            type = "bar") %>%
      layout(title = "Daily Confirmed Case Growth Rate",
             xaxis = list(title = ""), yaxis = list(title = "", tickformat = ".2%"), 
             showlegend = FALSE)
  })
  
  output$county_case_pc <- renderPlotly({
    
    DT1 = case_all[state==input$state][order(-case_pc)][1:county_count]
    
    DT1$county <- factor(DT1$county, levels = unique(DT1$county)[order(DT1$case_pc, decreasing = TRUE)])
    plot_ly(DT1, x = ~county, y = ~case_pc, name = ~county, color = ~county, colors = my_pallete, 
            type = "bar") %>%
      layout(title = "Total Confirmed Cases / Total Population",
             xaxis = list(title = ""), yaxis = list(title = "", tickformat = ".2%"), 
             showlegend = FALSE)
  })
  
  #----
  
  # Update input boxes ----
  observeEvent(input$state, 
               {county_list2 <- county_list[state==input$state, county]
                updateSelectInput(session, 'county', label = "Counties", choices = county_list2, selected = county_list2[1:county_count])
                }
               )
  
  # Mapping functions ----
  
  zoom_level <- 4
  
  output$map <- renderLeaflet({
    state_map %>%
      setView(lng = -93.85, lat = 37.45, zoom = zoom_level)
    
  })
  
  output$map1 <- renderLeaflet({
    current_map %>%
      setView(lng = -93.85, lat = 37.45, zoom = zoom_level)
    
  })

  output$map2 <- renderLeaflet({
    current_map2 %>%
      setView(lng = -93.85, lat = 37.45, zoom = zoom_level)
  })
  
  output$map3 <- renderLeaflet({
    time_map %>%
      setView(lng = -93.85, lat = 37.45, zoom = zoom_level)
  })
  
  # output$map4 <- renderLeaflet({
  #   time_map2 %>%
  #     setView(lng = -93.85, lat = 37.45, zoom = zoom_level)
  # })
  # 

  #---
}

