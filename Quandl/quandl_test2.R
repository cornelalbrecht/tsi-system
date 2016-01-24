
setwd("~/Google Drive/TSI/R-Version/GITed Version/Quandl")

library(Quandl)
library(lubridate)
library(dplyr)
library(quantmod)


Quandl.api_key("Uy7EG7K9GveKstF_LRzy")

stdt <- "2013-01-01"

tickers <- read.csv("german.csv")
tickers$Quandl.Code <- as.character(tickers$Quandl.Code)
tickers$Ticker <- as.character(tickers$Ticker)

# Make new Env
dat <- new.env()

#DEbug
#tickers <- tickers %>% top_n(200)

# Only do Adjusted Closing
tickers$sub <- paste(tickers$Quandl.Code,"", sep = "")


# Download loop
system.time(
for (i in 1:nrow(tickers)) {
  try(dat[[tickers$Ticker[i]]] <- Quandl(tickers$sub[i], start_date = stdt, type = "xts"))
}
)

# Analysis Loop

for(i in ls(dat)){
  
  # same as before: pull data
  tmp <- dat[[i]]
  
  tmp$a <- SMA(tmp[,6], 125)
  tmp$b <- SMA(tmp[,6], 25)
  
  tmp$c <- tmp$b/tmp$a
  
  dat[[i]] <- tmp
  
}

  
