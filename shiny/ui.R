

#setwd("~/Google Drive/TSI/R-Version/GITed Version/shiny")

# Libraries
library(shiny)
library(Quandl)

# Shiny
shinyUI(fluidPage(
  
  #Header
  headerPanel("Momentum System"),
  
  # Sidebar
  sidebarPanel(
    textOutput("dt")
    
               ),#End SidbarPanel
  
  
  # Main Panel
  mainPanel(
    
    # Tabset Panels
    tabsetPanel(
      
      #Tabpanel Ampel
      tabPanel("Market Indicator",
               h3("Market Momentum Indicator")
               
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