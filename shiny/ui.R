

#setwd("~/Google Drive/TSI/R-Version/GITed Version/shiny")

# Libraries
library(shiny)
library(Quandl)
Quandl.api_key("Uy7EG7K9GveKstF_LRzy")
library(TTR)
library(dplyr)
library(googleVis)
library(DT)

# Shiny
shinyUI(fluidPage(
  
  #Header
  headerPanel("Momentum System"),
  
  # Sidebar
  sidebarPanel(
    p("System Status:"),
    textOutput("dt")
    
               ),#End SidbarPanel
  
  
  # Main Panel
  mainPanel(
    
    # Tabset Panels
    tabsetPanel(
      
      #Tabpanel Ampel
      tabPanel("TecDAX/SDAX",
               h3("TecDAX/SDAX Momentum Indicator"),
               p(textOutput("TecSMEAN")),
               hr(),
               #dataTableOutput("TecS")
               htmlOutput("TecS")
               
               ),#Close Tabset
      
      #TabPanel HDAX
      tabPanel("HDAX",
               h3("HDAX List"),
               h4("Summary"),
               textOutput("tsi"),
               hr(),
               h4("Development"),
               htmlOutput("pl"),
               hr(),
               h4("Data"),
               dataTableOutput("tab")
               
               ), #Close Tabset
      
      # Tab Nasdaq
      tabPanel("NASDAQ100",
               h3("NASDAQ100 List")
               
               ) #Close Tabset
      
      
      
      
    )#Close TabsetPanel
    
    
    
    
    
  )#Close Main Panel
  
  
  
  
  
  
  
)) #Close fluid page