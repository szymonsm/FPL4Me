#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


library(shiny)
library(ggplot2)
library(plyr)
library(dplyr)
library(plotly)
library(data.table)
library(DT)

merged_gw <- read.csv(url("https://raw.githubusercontent.com/vaastav/Fantasy-Premier-League/be279f791fd0bfbcbeddf2006cab5b60fd69067f/data/2022-23/gws/merged_gw.csv"))


# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  #merged_gw <- read.csv(url("https://raw.githubusercontent.com/vaastav/Fantasy-Premier-League/be279f791fd0bfbcbeddf2006cab5b60fd69067f/data/2022-23/gws/merged_gw.csv"))
  
  # Set the Access-Control-Allow-Origin header
  # onHeaders(response, addHeader("Access-Control-Allow-Origin", "*"))
  
  # merged_cv <- renderDataTable({
  #   csv_data <- read.csv(url("https://raw.githubusercontent.com/vaastav/Fantasy-Premier-League/be279f791fd0bfbcbeddf2006cab5b60fd69067f/data/2022-23/gws/merged_gw.csv"))
  #   
  #   datatable(csv_data)
  # })
  
  
  output$formPlot <- renderPlot({
    
    data <- merged_gw %>% 
      filter(name == input$player)
    
    fig <- ggplot(data, aes(x = GW, y = total_points)) +
      geom_point() + 
      theme_bw() +
      scale_x_continuous(limits = c(1,max(data$GW)+1,1)) +
      scale_y_continuous(limits = c(-2,25))
    
    if(input$formTrend){
      fig <- fig + geom_smooth()
    }
    fig
    
  })
  
  output$pricePlot <- renderPlot({
    
    data <- merged_gw %>% 
      filter(name == input$player)
    
    fig <- ggplot(data, aes(x = GW, y = value/10)) +
      geom_point() + 
      theme_bw() +
      scale_x_continuous(limits = c(1,max(data$GW)+1,1)) #+
      #scale_y_continuous(limits = c(3.5,14))
    
    if(input$priceTrend){
      fig <- fig + geom_smooth()
    }
    fig
    
  })
  
  output$correlation <- renderInfoBox({
    
    by_players <- merged_gw %>% 
      group_by(name) %>% 
      mutate(next_gw = GW + 1) %>% 
      filter(ict_index != 0)
    
    df1 <- by_players
    df2 <- by_players
    merge(df1,df2,by.x=c("name", "GW"),by.y=c("name","next_gw"))
    
    df <- inner_join(by_players,by_players, by.x = c("name", "GW"), by.y = c("name","next_gw")) %>% 
      merge(by_players, by.x = c("name", "GW"), by.y = c("name","next_gw")) %>% 
      select(name, ict_index.x, GW, next_gw, total_points.y)
    
    require(plyr)
    func <- function(df)
    {
      return(data.frame(COR = cor(df$total_points.y, df$ict_index.x)))
    }
    
    cor_players <- na.omit(ddply(df, .(name), func))

    output <- cor_players %>% 
      filter(name == input$player)
    
    infoBox(
            "Correlation",
            value = round(output[1,2],3),
            width = 4,
            fill = TRUE,
            icon = icon("link"))
  })
  
  output$vpc <- renderInfoBox({
    
    points <- merged_gw %>% 
      filter(name == input$player) %>% 
      summarise(pts = sum(total_points))
    cost <- merged_gw %>% 
      filter(name == input$player) %>% 
      filter(GW == max(GW,na.rm = T)) %>% 
      select(value)
    cost <- cost/10
    
    value <- round(points/cost,3)
    infoBox("Points per million",
            value = value,
            width = 4,
            fill = TRUE,
            icon = icon("coins"),
            color = "yellow")
  })
  
  output$ownership <- renderInfoBox({
    
    time <- merged_gw %>% 
      filter(name == input$player) %>% 
      summarise(time = sum(minutes,na.rm = T))
    
    time <- time/max(merged_gw$GW * 90,na.rm = T) * 100
    
    infoBox("Total time on pitch",
            value = paste0(round(time,2),"%"),
            width = 4,
            fill = TRUE,
            icon = icon("clock"),
            color = "green")
  })
  
  output$xPPlot <- renderPlot({
    data <- merged_gw %>% 
      filter(name == input$player)
    
    fig <- ggplot(data, aes(x = GW, y = xP)) +
      geom_point() + 
      theme_bw() +
      scale_x_continuous(limits = c(1,max(data$GW,na.rm = T)+1,1)) +
      scale_y_continuous(limits = c(0,20))
    
    if(input$xPTrend){
      fig <- fig + geom_smooth()
    }
    fig
  })
  
  
  output$topvpc <- DT::renderDataTable({
    data <- merged_gw %>% 
      dplyr::group_by(name) %>% 
      summarise(pts = sum(total_points,na.rm = T), .groups = "keep")
    data2 <- merged_gw %>% 
      filter(GW == max(GW,na.rm = T)) %>% 
      select(name,value)
    data <- inner_join(data, data2, by = "name")
    data$vpc <- data$pts / data$value * 10
    data <- data %>% 
      select(name,vpc) %>% 
      arrange(-vpc) %>% 
      head(10)
    data$vpc <- round(data$vpc,2)
    DT::datatable(data,options = list(pageLength = 5))
  })
  
  output$topttop <- DT::renderDataTable({
    data <- merged_gw %>% 
      group_by(name) %>% 
      filter(position != "GK") %>% 
      summarise(minutes = sum(minutes,na.rm = T)) %>% 
      arrange(-minutes) %>% 
      head(10)
    DT::datatable(data,options = list(pageLength = 5))
  })
  
  output$corrtab <- renderDataTable({
    by_players <- merged_gw %>%
      group_by(name) %>%
      mutate(next_gw = GW + 1) %>%
      filter(ict_index != 0)
    
    df <- inner_join(by_players,by_players, by.x = c("name", "GW"), by.y = c("name","next_gw")) %>%
      merge(by_players, by.x = c("name", "GW"), by.y = c("name","next_gw")) %>%
      select(name, ict_index.x, GW, next_gw, total_points.y)
    
    #require(plyr)
    func <- function(df)
    {
      return(data.frame(COR = cor(df$total_points.y, df$ict_index.x)))
    }
    
    cor_players <- na.omit(ddply(df, .(name), func))
    head(cor_players)
    cor_players <- cor_players %>%
      arrange(-COR)
    head(cor_players)
    
    more_than__half <- merged_gw %>%
      group_by(name) %>%
      filter(ict_index != 0) %>%
      summarise(minutes_played = sum(minutes,na.rm = T)) %>%
      filter(minutes_played >= 90 * max(merged_gw$GW,na.rm = T) / 2)
    
    positive_cor <- left_join(more_than__half,cor_players, by = "name") %>%
      arrange(-COR)
    negative_cor <- positive_cor %>%
      arrange(COR)
    
    p <- rbind(head(positive_cor,10),head(negative_cor,10)) %>%
      arrange(-COR)
    p <- p %>%
      select(name,COR)
    merged_gw_last <- merged_gw %>%
      filter(GW == max(GW,na.rm = T))
    
    out <- left_join(p,merged_gw_last,by = "name") %>%
      select(name,COR,ict_index)
    out$COR <- round(out$COR,2)
    out <- out %>% 
      select(name,COR)
    DT::datatable(out, options = list(pageLength = 5))
  })
  
})
