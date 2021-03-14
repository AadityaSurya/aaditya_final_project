library(deSolve)
library(shiny)
library(dplyr)
library(shinyWidgets)
library(cryptor)
library(ggplot2)
library(plotly)

#Shiny app
ui <- fluidPage(
  titlePanel("Coin Plots"),
  sidebarLayout(sidebarPanel(
    sliderInput(inputId = "btcrange",
                label = "Bitcoin Date Range",
                min = 1,
                max = 2000,
                value = 1000),
    
    sliderInput(inputId = "ethrange",
                label = "Ethereum Date Range",
                min = 1,
                max = 2000,
                value = 1000,),
    
    sliderInput(inputId = "dotrange",
                label = "Polkadot Date Range",
                min = 1,
                max = 2000,
                value = 1000,),
    
  ), mainPanel(
    plotlyOutput(outputId = "timeSeriesBTC"),
    plotlyOutput(outputId = "timeSeriesETH"),
    plotlyOutput(outputId = "timeSeriesDOT")
  )),
  
) 
server = function(input, output, session) {
  
  #BTC Plot
  output$timeSeriesBTC <- renderPlotly({
    btc_price_history_full = get_historical_price("BTC", "INR", limit = input$btcrange)
    btc_price = btc_price_history_full$close
    btc_time = as.Date(btc_price_history_full$time)
    btc_frame = data.frame(btc_time, btc_price)
    btc_plot <- ggplot(btc_frame, aes(x = btc_time, y = btc_price)) +
      geom_line() + 
      xlab("Time") +
      ylab("Bitcoin Price")
      ggplotly(btc_plot) %>% layout(title="Bitcoin")
  })
  
  #ETH Plot
  output$timeSeriesETH <- renderPlotly({
    eth_price_history_full = get_historical_price("ETH", "INR", limit = input$ethrange)
    eth_price = eth_price_history_full$close
    eth_time = as.Date(eth_price_history_full$time)
    eth_frame = data.frame(eth_time, eth_price)
    eth_plot <- ggplot(eth_frame, aes(x = eth_time, y = eth_price)) +
      geom_line() + 
      xlab("Time") +
      ylab("Ethereum Price")
    ggplotly(btc_plot) %>% layout(title="Ethereum")
  })
  
  #DOT Plot
  output$timeSeriesDOT <- renderPlotly({
    dot_price_history_full = get_historical_price("DOT", "INR", limit = input$dotrange)
    dot_price = dot_price_history_full$close
    dot_time = as.Date(dot_price_history_full$time)
    dot_frame = data.frame(dot_time, dot_price)
    dot_plot <- ggplot(dot_frame, aes(x = dot_time, y = dot_price)) +
      geom_line() + 
      xlab("Time") +
      ylab("Polkadot Price")
    ggplotly(dot_plot) %>% layout(title="Polkadot")
  })
}
  #btc_price_history_full = get_historical_price("BTC", "USD", limit = 20)
  #btc_price = btc_price_history_full$close
  #btc_time = as.Date(btc_price_history_full$time)
  #btc_frame = data.frame(btc_time, btc_price)
  #btc_plot <- ggplot(btc_frame, aes(x = btc_time, y = btc_price)) +
   # geom_line() + 
    #xlab("Time") +
    #ylab("Bitcoin Price")
  
  #output$timeSeries = renderPlotly({
   # ggplotly(btc_plot)
  #}
  #)
  

shinyApp(ui = ui, server = server)
    
