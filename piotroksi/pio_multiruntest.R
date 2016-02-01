
### optnal
setwd("~/Google Drive/TSI/R-Version/GITed Version/piotroksi")
library(dplyr)
library(quantmod)
library(xtable)

source("piotroski.R")

ticks <- read.csv("nasdaq100list.csv")


ticks$Symbol <- as.character(ticks$Symbol)



#2###############
fins <- new.env()
for (i in 1:nrow(ticks)){
  
  try(getFinancials(ticks$Symbol[i], env = fins))
}

pio <- data.frame(matrix(ncol=2))
colnames(pio) <- c("symb", "score")

for(i in ls(fins)){
  print(i)
  pio[i,1] <- i
  try(
  pio[i,2] <- Piotroski(fins[[i]])
  )
  
}

pio <- pio %>% arrange(desc(score)) 

#1###############

#pull and clean market symbols
a <- stockSymbols(exchange = c("NASDAQ","AMEX","NYSE"))
a$MarketCap <- gsub("\\$","", a$MarketCap)
a$MarketCap <- gsub("\\.","", a$MarketCap)
a$MarketCap <- gsub("M","000000", a$MarketCap)
a$MarketCap <- gsub("B","000000000", a$MarketCap)
a$MarketCap <- as.numeric(a$MarketCap)

#a <- a %>%  group_by(Sector) %>%  arrange(desc(MarketCap)) %>% filter(IPOyear < "2013", MarketCap > "50000000")

a <- a %>%  group_by(Sector) %>%  arrange(desc(MarketCap)) %>% filter(MarketCap > "50000000")

ticks <- a


#3#########merge
pio$symb <- gsub(".f", "", pio$symb)

out <- merge(pio, ticks, by.x = "symb", by.y = "Symbol")
out <- out %>% arrange(desc(score)) %>% select(symb:Name)

write(print(xtable(out), type = "html"),"pio.html")

