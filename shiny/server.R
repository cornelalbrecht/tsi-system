
#setwd("~/Google Drive/TSI/R-Version/GITed Version/shiny")

# Libraries
library(shiny)
library(Quandl)
library(TTR)
library(dplyr)
library(googleVis)

shinyServer(function(input, output) {
  
  # DATE
  output$dt <-renderText(as.character(Sys.time()))
 
  # Download Index data
  dat <- Quandl("YAHOO/INDEX_GDAXI", type = "raw", start_date = "2015-3-20")
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