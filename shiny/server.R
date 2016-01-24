
#setwd("~/Google Drive/TSI/R-Version/GITed Version/shiny")

# Libraries
library(shiny)
library(Quandl)
Quandl.api_key("Uy7EG7K9GveKstF_LRzy")
library(TTR)
library(dplyr)
library(googleVis)

shinyServer(function(input, output) {
  
  # DATE
  output$dt <-renderText(as.character(Sys.time()))
  
  ################################################################################################################################
  ################
  # MAKE TICKERS
  ticks <- read.csv("TecDAXSDAX.csv", stringsAsFactors = FALSE)
  ticks$Ticker <- gsub(".DE", "", ticks$Ticker)
  ticks$Ticker <- gsub("\\^", "", ticks$Ticker)
  ticks$Ticker <- paste("SSE/",ticks$Ticker, sep = "")
  ################
  
  ################
  # Download Data
  withProgress(message = "Downloading Stock Data from Quandl", value = 0, {
    s <- new.env()
    for (i in 1:nrow(ticks)){
      incProgress(amount = 1/nrow(ticks), detail = print(ticks$Ticker[i]))
      
      try(s[[ticks$Ticker[i]]] <- Quandl(ticks$Ticker[i], start_date = "2015-1-1"))
    }
  })
  
  ################
  # TSI Loop
  withProgress(message = "Calculating Momentum", value = 0, {
    TSI <- data.frame()
    for (i in ls(s)){
      incProgress(amount = 1/length(ls(s)), detail = print(i))
      Sys.sleep(.1)
      
      try({
      tmp <- s[[i]]
      tmp <- tmp %>% arrange(Date)
      tmp$sma125 <- SMA(tmp$`Previous Day Price`, 125)
      tmp$sma25 <- SMA(tmp$`Previous Day Price`, 25)
      tmp$TSI <- tmp$sma25 / tmp$sma125
      tmp <- tmp %>% arrange(desc(Date))
      s[[i]] <- tmp
      TSI <- rbind(TSI, data.frame(tmp$Date[1], i, tmp$TSI[1]))
      })#End Try 
    }#End FOR loop
    
    # Cosmetics
    colnames(TSI) <- c("Date", "Ticker", "TSI")
    # Merge
    TSIlist <- merge(TSI, ticks, by.x = "Ticker", by.y = "Ticker")
    TSIlist <- TSIlist %>% arrange(desc(TSI)) %>% select(Name, TSI, Date)
    
    output$TecSMEAN <- renderText(sprintf("Sell-level: %.3f",mean(TSIlist$TSI)))
    output$TecS <- renderDataTable(TSIlist, options = list(orderClasses = TRUE))
    
    
  
  })#End Progress Bar
  
  
  ################################################################################################################################
 
  # Download Index data
  withProgress(message = "Downloading Data",{
    setProgress(value = .2, message = "Quandl Connect", detail = "DAX data")
    dat <- Quandl("YAHOO/INDEX_GDAXI", type = "raw", start_date = "2015-3-20")
  })
  
  
  
  dat <- dat %>% arrange(Date)
  dat$sma125 <- SMA(dat$`Adjusted Close`, 125)
  dat$sma25 <- SMA(dat$`Adjusted Close`, 25)
  dat$TSI <- dat$sma25/dat$sma125
  dat$limit <- 1
  dat <- dat %>% arrange(desc(Date))
  
  # Last TSI
  output$tsi <- renderText(paste("Current TSI as of ",Sys.Date(),sprintf("%.2f",dat$TSI[1])))


  output$tab <- renderDataTable(dat)
  output$pl <- renderGvis({
    
    gvisAreaChart(head(dat,30), xvar = "Date", yvar = c("limit", "TSI"))#, options = list(height = "400px"))

#     gvisAreaChart(dat, xvar = "Date", yvar = c("Adjusted Close", "limit", "TSI"),
#                   options=list(
#                     series="[{targetAxisIndex: 0},
#                     {targetAxisIndex:1},
#                     {targetAxisIndex:1}]",
#                     height = "400px",
#                     vAxes="[{title:'DAX Close'}, {title:'TSI'}]"
#                   ))
    
  })
  

  
  
  
})# Closing server function