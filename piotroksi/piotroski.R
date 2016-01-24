


Piotroski <- function(stock){
  
  # Pull Statments
  IS <- stock$IS$Q
  BS <- stock$BS$Q
  CF <- stock$CF$Q
  
  ISa <- stock$IS$A
  BSa <- stock$BS$A
  CFa <- stock$CF$A
  
  Score <- vector()
  
  ## Profitability
  # ROA
  ROA <- (IS["Net Income Before Extra. Items",1] / BS["Total Assets",1]) / 
    (IS["Net Income Before Extra. Items",2] / BS["Total Assets",2])
  
  Score[1] <- ifelse(ROA > 1, 1, 0)
  
  # CFO
  CFO <- (CF["Cash from Operating Activities",1] / BS["Total Assets",1]) /
    (CF["Cash from Operating Activities",2] / BS["Total Assets",2])
  
  Score[2] <- ifelse(CFO > 1, 1, 0)
  
  # Delta CFO
  dROA <- (IS["Net Income Before Extra. Items",1] / BS["Total Assets",1]) - 
    (IS["Net Income Before Extra. Items",5] / BS["Total Assets",5])
  
  Score[3] <- ifelse(dROA > 0, 1, 0)
  
  # Accrual
  acc <- (IS["Net Income Before Extra. Items",1] - CF["Cash from Operating Activities",1]) /
    BSa["Total Assets",1]
  
  Score[4] <- ifelse(acc > 0, 1, 0)
  
  ## Leverage, liquidity, source of funds
  # Long-term debt
  dLever <- (BS["Long Term Debt",1] / BS["Total Assets",1]) / (BS["Long Term Debt",5] / BS["Total Assets",5]) 
  Score[5] <- ifelse(dLever > 1, 1, 0)
  
  # Current Ratio
  curr <- (BS["Total Current Assets",1] / BS["Total Current Liabilities",1]) /
    (BS["Total Current Assets",5] / BS["Total Current Liabilities",5])
  
  Score[6] <- ifelse(curr > 1, 1, 0)
  
  # Equity Issuance
  eqty <- BS["Total Common Shares Outstanding",1] / BS["Total Common Shares Outstanding",5]
  
  Score[7] <- ifelse(eqty <= 1, 1, 0)
  
  ## Financial Performance
  # Margin
  margin <- (IS["Gross Profit",1] / IS["Revenue",1]) - (IS["Gross Profit",5] / IS["Revenue",5])
  
  Score[8] <- ifelse(margin > 0, 1, 0)
  
  # Asset Turnover
  turn <- (IS["Revenue",1] / BSa["Total Assets",1]) - (IS["Revenue",5] / BSa["Total Assets",2])
  
  Score[9] <- ifelse(turn > 0, 1, 0)
  
  ###
  if(sum(is.na(Score)) > 0){message("WARNING: some scores missing")}
  piotr <- sum(Score, na.rm = TRUE)
 
}