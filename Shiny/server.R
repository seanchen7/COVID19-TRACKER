


#####################################################################################################################  
# Server function                                                                                                   #
##################################################################################################################### 

server <- function(input, output, session) {

  #Data overview ----
  output$contents1  <-  renderDataTable({
    datatable(case_state[,. (date, state, fips, cases, deaths)][order(-date, -cases)], 
              options = list(pageLength = 25))
  })
  
  output$contents2  <-  renderDataTable({
    datatable(case_all[,. (date, state, county, FIPSCOUNTY, cases, deaths)][order(-date, -cases)], 
              options = list(pageLength = 25))
  })
  
  # Plots ----
  output$state_case <- renderPlotly({
    
    DT1 = top_state
    DT1$state <- factor(DT1$state, levels = unique(DT1$state)[order(DT1$cases, decreasing = TRUE)])
    plot_ly(DT1, x = ~state, y = ~cases, name = ~state, color = ~state, colors = my_pallete, 
            type = "bar") %>%
      layout(title = "Total Confirmed Cases",
             xaxis = list(title = ""), yaxis = list(title = ""), 
             showlegend = FALSE)
  })
  
  output$state_case_growth <- renderPlotly({
    
    DT1 <- case_state[date %in% c(last_update, last_update-1)][order(-cases)]
    DT1 <- DT1[state %in% top_state[, state]]
    DT1$state <- factor(DT1$state, levels = unique(DT1$state)[order(DT1$cases, decreasing = TRUE)])
    
    plot_ly(DT1[date == (last_update-1)], x = ~state, y = ~cases, colors = my_pallete, name = "Last Reported",
            type = "bar") %>%
      add_trace(data = DT1[date == (last_update)], y = ~case_delta, name = 'New Cases') %>%
      layout(title = paste0("Total Confirmed Cases: Updated ", last_update), barmode = 'stack',
             xaxis = list(title = ""), yaxis = list(title = ""), legend = list(x = 0.8, y = 0.9))
    
  })
  
  
  output$state_deaths <- renderPlotly({
    
    DT1 = top_state
    DT1$state <- factor(DT1$state, levels = unique(DT1$state)[order(DT1$deaths, decreasing = TRUE)])
    plot_ly(DT1, x = ~state, y = ~deaths, name = ~state, color = ~state, colors = my_pallete, type = "bar") %>%
      layout(title = "Total Deaths",
             xaxis = list(title = ""),
             yaxis = list(title = ""), 
             showlegend = FALSE)
    
  })
  
  time_case_plot <-  plot_ly(
    case_state[state %in% top_state[, state]], 
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
    plot_ly(case_state[state %in% top_state[, state]], 
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
  zoom_level <- 4
    
  # Mapping functions ----
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
  
  output$map4 <- renderLeaflet({
    time_map2 %>%
      setView(lng = -93.85, lat = 37.45, zoom = zoom_level)
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
  #---
}

