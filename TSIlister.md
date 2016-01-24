---
title: "TSI Lists"
author: "CA"
date: "January 15, 2016"
output: 
  html_document: 
    keep_md: yes
    toc: yes
---

## TSI Momentum Indicator


```r
# Setup
setwd("~/Google Drive/TSI/R-Version/GITed Version")

# Packages
library(ggplot2)
library(dplyr)
library(zoo)
library(TTR)
library(lubridate)
library(quantmod)
#library(googleVis)
library(xtable)
#library(downloader)
library(rmarkdown)
```
{r, echo=TRUE, message=FALSE, warning=FALSE, results='hide', cache=FALSE, error=TRUE}

```r
knitr::opts_chunk$set(error=FALSE) 
## MARKDOWN ERROR SUPPRESSSION DOESNT WORK :-()

ticks <- read.csv("./tickerlists/TecDAXSDAX.csv", as.is = TRUE)

#Debug
#ticks <- ticks[1:10,]

# Dates
to <- Sys.Date()
from <- to-300

stockdata <- new.env()

# Pull data
a <- try(getSymbols(ticks$X.TECDAX, env = stockdata, from = from, to = to))
```

```
## Warning in download.file(paste(yahoo.URL, "s=", Symbols.name, "&a=",
## from.m, : download had nonzero exit status
```

```
## pausing 1 second between requests for more than 5 symbols
## pausing 1 second between requests for more than 5 symbols
## pausing 1 second between requests for more than 5 symbols
## pausing 1 second between requests for more than 5 symbols
## pausing 1 second between requests for more than 5 symbols
## pausing 1 second between requests for more than 5 symbols
## pausing 1 second between requests for more than 5 symbols
## pausing 1 second between requests for more than 5 symbols
## pausing 1 second between requests for more than 5 symbols
## pausing 1 second between requests for more than 5 symbols
## pausing 1 second between requests for more than 5 symbols
## pausing 1 second between requests for more than 5 symbols
## pausing 1 second between requests for more than 5 symbols
## pausing 1 second between requests for more than 5 symbols
## pausing 1 second between requests for more than 5 symbols
## pausing 1 second between requests for more than 5 symbols
## pausing 1 second between requests for more than 5 symbols
## pausing 1 second between requests for more than 5 symbols
## pausing 1 second between requests for more than 5 symbols
## pausing 1 second between requests for more than 5 symbols
## pausing 1 second between requests for more than 5 symbols
## pausing 1 second between requests for more than 5 symbols
## pausing 1 second between requests for more than 5 symbols
## pausing 1 second between requests for more than 5 symbols
## pausing 1 second between requests for more than 5 symbols
## pausing 1 second between requests for more than 5 symbols
## pausing 1 second between requests for more than 5 symbols
## pausing 1 second between requests for more than 5 symbols
## pausing 1 second between requests for more than 5 symbols
## pausing 1 second between requests for more than 5 symbols
```

```
## Warning in download.file(paste(yahoo.URL, "s=", Symbols.name, "&a=",
## from.m, : download had nonzero exit status
```

```
## pausing 1 second between requests for more than 5 symbols
## pausing 1 second between requests for more than 5 symbols
## pausing 1 second between requests for more than 5 symbols
## pausing 1 second between requests for more than 5 symbols
## pausing 1 second between requests for more than 5 symbols
## pausing 1 second between requests for more than 5 symbols
## pausing 1 second between requests for more than 5 symbols
## pausing 1 second between requests for more than 5 symbols
## pausing 1 second between requests for more than 5 symbols
## pausing 1 second between requests for more than 5 symbols
## pausing 1 second between requests for more than 5 symbols
## pausing 1 second between requests for more than 5 symbols
## pausing 1 second between requests for more than 5 symbols
## pausing 1 second between requests for more than 5 symbols
## pausing 1 second between requests for more than 5 symbols
## pausing 1 second between requests for more than 5 symbols
## pausing 1 second between requests for more than 5 symbols
## pausing 1 second between requests for more than 5 symbols
## pausing 1 second between requests for more than 5 symbols
## pausing 1 second between requests for more than 5 symbols
## pausing 1 second between requests for more than 5 symbols
```

```
## Warning in download.file(paste(yahoo.URL, "s=", Symbols.name, "&a=",
## from.m, : download had nonzero exit status
```

```
## pausing 1 second between requests for more than 5 symbols
## pausing 1 second between requests for more than 5 symbols
## pausing 1 second between requests for more than 5 symbols
## pausing 1 second between requests for more than 5 symbols
## pausing 1 second between requests for more than 5 symbols
## pausing 1 second between requests for more than 5 symbols
## pausing 1 second between requests for more than 5 symbols
## pausing 1 second between requests for more than 5 symbols
## pausing 1 second between requests for more than 5 symbols
## pausing 1 second between requests for more than 5 symbols
```

```
## Warning in download.file(paste(yahoo.URL, "s=", Symbols.name, "&a=",
## from.m, : download had nonzero exit status
```

```
## pausing 1 second between requests for more than 5 symbols
## pausing 1 second between requests for more than 5 symbols
## pausing 1 second between requests for more than 5 symbols
## pausing 1 second between requests for more than 5 symbols
## pausing 1 second between requests for more than 5 symbols
## pausing 1 second between requests for more than 5 symbols
## pausing 1 second between requests for more than 5 symbols
## pausing 1 second between requests for more than 5 symbols
## pausing 1 second between requests for more than 5 symbols
## pausing 1 second between requests for more than 5 symbols
## pausing 1 second between requests for more than 5 symbols
## pausing 1 second between requests for more than 5 symbols
## pausing 1 second between requests for more than 5 symbols
## pausing 1 second between requests for more than 5 symbols
## pausing 1 second between requests for more than 5 symbols
## pausing 1 second between requests for more than 5 symbols
```

```r
# Loop over Envirment to compute ratio
TSI <- xts()


for (i in ls(stockdata)){
  
  # pull to tmp
  #rm(tmp)
  tmp <- stockdata[[i]]
  
  # Compute TSI Ratio
  tmp$Ratio <- SMA(tmp[,6], n = 30) / SMA(tmp[,6], n = 126)
  
  # Remove NAs
  tmp <- tmp[!is.na(tmp$Ratio)]
  
  # Put back into environment
  #stockdata[[i]] <- tmp
  
  # Pull last 30 TSIs for each
  TSI <- cbind.xts(TSI,last(tmp$Ratio, 30))

}


# Set TSI ColumnNames
colnames(TSI) <- ls(stockdata)

# Since we pull the last 20 values, but not every day is traded everywhere, we
# ended up with far more values, but we're really only interested in the last 20
TSI <- last(TSI, 20)

####### messy from here on, needs fixing ######

# Sort by TSI
b <- data.frame(Names = names(TSI), TSI = matrix(t(last(TSI))))
c <- merge(ticks, b, by.x = "X.TECDAX", by.y = "Names")

c <- arrange(c, desc(TSI))
```
# TSI LIST


```r
print(xtable(c), type = "html")
```

<!-- html table generated in R 3.2.2 by xtable 1.8-0 package -->
<!-- Sat Jan 16 18:58:23 2016 -->
<table border=1>
<tr> <th>  </th> <th> X.TECDAX </th> <th> X_____.TecDAX._____ </th> <th> TSI </th>  </tr>
  <tr> <td align="right"> 1 </td> <td> BAF.DE </td> <td> Balda AG </td> <td align="right"> 1.42 </td> </tr>
  <tr> <td align="right"> 2 </td> <td> S92.DE </td> <td> SMA Solar Technology AG </td> <td align="right"> 1.24 </td> </tr>
  <tr> <td align="right"> 3 </td> <td> NEM.DE </td> <td> Nemetschek AG </td> <td align="right"> 1.18 </td> </tr>
  <tr> <td align="right"> 4 </td> <td> COK.DE </td> <td> Cancom SE </td> <td align="right"> 1.17 </td> </tr>
  <tr> <td align="right"> 5 </td> <td> GLJ.DE </td> <td> GrenkeLeasing AG </td> <td align="right"> 1.13 </td> </tr>
  <tr> <td align="right"> 6 </td> <td> P1Z.DE </td> <td> Patrizia Immobilien AG </td> <td align="right"> 1.13 </td> </tr>
  <tr> <td align="right"> 7 </td> <td> PMO.DE </td> <td> PRIME OFFICE REIT </td> <td align="right"> 1.13 </td> </tr>
  <tr> <td align="right"> 8 </td> <td> NDX1.DE </td> <td> Nordex SE </td> <td align="right"> 1.13 </td> </tr>
  <tr> <td align="right"> 9 </td> <td> COP.DE </td> <td> CompuGroup Medical AG </td> <td align="right"> 1.13 </td> </tr>
  <tr> <td align="right"> 10 </td> <td> SKB.DE </td> <td> Koenig &amp; Bauer AG </td> <td align="right"> 1.12 </td> </tr>
  <tr> <td align="right"> 11 </td> <td> SBS.DE </td> <td> STRATEC Biomedical AG </td> <td align="right"> 1.10 </td> </tr>
  <tr> <td align="right"> 12 </td> <td> SRT3.DE </td> <td> Sartorius Aktiengesellschaft </td> <td align="right"> 1.10 </td> </tr>
  <tr> <td align="right"> 13 </td> <td> JUN3.DE </td> <td> Jungheinrich AG </td> <td align="right"> 1.09 </td> </tr>
  <tr> <td align="right"> 14 </td> <td> GMM.DE </td> <td> Grammer AG </td> <td align="right"> 1.09 </td> </tr>
  <tr> <td align="right"> 15 </td> <td> ZO1.DE </td> <td> zooplus AG </td> <td align="right"> 1.08 </td> </tr>
  <tr> <td align="right"> 16 </td> <td> DBAN.DE </td> <td> Deutsche Beteiligungs AG Ord </td> <td align="right"> 1.08 </td> </tr>
  <tr> <td align="right"> 17 </td> <td> WDI.DE </td> <td> Wirecard AG </td> <td align="right"> 1.08 </td> </tr>
  <tr> <td align="right"> 18 </td> <td> KGX.DE </td> <td> KION GROUP </td> <td align="right"> 1.08 </td> </tr>
  <tr> <td align="right"> 19 </td> <td> JEN.DE </td> <td> Jenoptik AG </td> <td align="right"> 1.07 </td> </tr>
  <tr> <td align="right"> 20 </td> <td> DIC.DE </td> <td> DIC ASSET N </td> <td align="right"> 1.06 </td> </tr>
  <tr> <td align="right"> 21 </td> <td> ADV.DE </td> <td> ADVA Optical Networking SE </td> <td align="right"> 1.06 </td> </tr>
  <tr> <td align="right"> 22 </td> <td> UTDI.DE </td> <td> United Internet AG </td> <td align="right"> 1.06 </td> </tr>
  <tr> <td align="right"> 23 </td> <td> AFX.DE </td> <td> Carl Zeiss Meditec AG </td> <td align="right"> 1.06 </td> </tr>
  <tr> <td align="right"> 24 </td> <td> VT9.DE </td> <td> VTG Aktiengesellschaft </td> <td align="right"> 1.06 </td> </tr>
  <tr> <td align="right"> 25 </td> <td> BC8.DE </td> <td> Bechtle AG </td> <td align="right"> 1.05 </td> </tr>
  <tr> <td align="right"> 26 </td> <td> PSAN.DE </td> <td> PSI Aktiengesellschaft f </td> <td align="right"> 1.05 </td> </tr>
  <tr> <td align="right"> 27 </td> <td> 2HR.DE </td> <td> H&amp;R  </td> <td align="right"> 1.05 </td> </tr>
  <tr> <td align="right"> 28 </td> <td> SIX2.DE </td> <td> Sixt Societas Europaea </td> <td align="right"> 1.05 </td> </tr>
  <tr> <td align="right"> 29 </td> <td> PUM.DE </td> <td> PUMA SE </td> <td align="right"> 1.03 </td> </tr>
  <tr> <td align="right"> 30 </td> <td> CWC.DE </td> <td> CEWE Stiftung &amp; Co. KGaA </td> <td align="right"> 1.03 </td> </tr>
  <tr> <td align="right"> 31 </td> <td> SAX.DE </td> <td> Stroer </td> <td align="right"> 1.03 </td> </tr>
  <tr> <td align="right"> 32 </td> <td> GSC1.DE </td> <td> GESCO N </td> <td align="right"> 1.03 </td> </tr>
  <tr> <td align="right"> 33 </td> <td> TTK.DE </td> <td> Takkt AG </td> <td align="right"> 1.03 </td> </tr>
  <tr> <td align="right"> 34 </td> <td> COM.DE </td> <td> comdirect bank AG </td> <td align="right"> 1.02 </td> </tr>
  <tr> <td align="right"> 35 </td> <td> EVT.DE </td> <td> Evotec AG </td> <td align="right"> 1.02 </td> </tr>
  <tr> <td align="right"> 36 </td> <td> HAB.DE </td> <td> Hamborner AG </td> <td align="right"> 1.02 </td> </tr>
  <tr> <td align="right"> 37 </td> <td> INH.DE </td> <td> INDUS Holding AG </td> <td align="right"> 1.02 </td> </tr>
  <tr> <td align="right"> 38 </td> <td> FNTN.DE </td> <td> freenet AG </td> <td align="right"> 1.02 </td> </tr>
  <tr> <td align="right"> 39 </td> <td> BDT.DE </td> <td> Bertrandt AG </td> <td align="right"> 1.02 </td> </tr>
  <tr> <td align="right"> 40 </td> <td> EVD.DE </td> <td> CTS Eventim AG </td> <td align="right"> 1.02 </td> </tr>
  <tr> <td align="right"> 41 </td> <td> SLT.DE </td> <td> Schaltbau Holding AG </td> <td align="right"> 1.01 </td> </tr>
  <tr> <td align="right"> 42 </td> <td> SOW.DE </td> <td> Software AG </td> <td align="right"> 1.01 </td> </tr>
  <tr> <td align="right"> 43 </td> <td> HAW.DE </td> <td> Hawesko Holding AG </td> <td align="right"> 1.00 </td> </tr>
  <tr> <td align="right"> 44 </td> <td> HBH3.DE </td> <td> Hornbach Holding AG </td> <td align="right"> 1.00 </td> </tr>
  <tr> <td align="right"> 45 </td> <td> QIA.DE </td> <td> Qiagen NV </td> <td align="right"> 1.00 </td> </tr>
  <tr> <td align="right"> 46 </td> <td> AOX.DE </td> <td> Alstria Office Reit-AG </td> <td align="right"> 0.99 </td> </tr>
  <tr> <td align="right"> 47 </td> <td> B5A.DE </td> <td> BAUER Aktiengesellschaft </td> <td align="right"> 0.99 </td> </tr>
  <tr> <td align="right"> 48 </td> <td> O1BC.DE </td> <td> XING AG </td> <td align="right"> 0.97 </td> </tr>
  <tr> <td align="right"> 49 </td> <td> KWS.DE </td> <td> KWS SAAT AG </td> <td align="right"> 0.96 </td> </tr>
  <tr> <td align="right"> 50 </td> <td> WAC.DE </td> <td> Wacker Neuson SE </td> <td align="right"> 0.95 </td> </tr>
  <tr> <td align="right"> 51 </td> <td> VIB3.DE </td> <td> Villeroy &amp; Boch AG </td> <td align="right"> 0.95 </td> </tr>
  <tr> <td align="right"> 52 </td> <td> PFV.DE </td> <td> Pfeiffer Vacuum Technology AG </td> <td align="right"> 0.95 </td> </tr>
  <tr> <td align="right"> 53 </td> <td> SW1.DE </td> <td> SHW </td> <td align="right"> 0.95 </td> </tr>
  <tr> <td align="right"> 54 </td> <td> KBC.DE </td> <td> Kontron AG </td> <td align="right"> 0.95 </td> </tr>
  <tr> <td align="right"> 55 </td> <td> HHFA.DE </td> <td> Hamburger Hafen und Logistik AG </td> <td align="right"> 0.95 </td> </tr>
  <tr> <td align="right"> 56 </td> <td> SFQ.DE </td> <td> Saf-Holland SA </td> <td align="right"> 0.95 </td> </tr>
  <tr> <td align="right"> 57 </td> <td> DEX.DE </td> <td> Delticom AG </td> <td align="right"> 0.94 </td> </tr>
  <tr> <td align="right"> 58 </td> <td> HDD.DE </td> <td> Heidelberger Druckmaschinen Aktiengesellschaft </td> <td align="right"> 0.94 </td> </tr>
  <tr> <td align="right"> 59 </td> <td> CEV.DE </td> <td> CENTROTEC Sustainable AG </td> <td align="right"> 0.94 </td> </tr>
  <tr> <td align="right"> 60 </td> <td> VOS.DE </td> <td> Vossloh AG </td> <td align="right"> 0.93 </td> </tr>
  <tr> <td align="right"> 61 </td> <td> DRI.DE </td> <td> Drillisch Aktiengesellschaft </td> <td align="right"> 0.93 </td> </tr>
  <tr> <td align="right"> 62 </td> <td> MLP.DE </td> <td> MLP AG </td> <td align="right"> 0.93 </td> </tr>
  <tr> <td align="right"> 63 </td> <td> BYW6.DE </td> <td> BayWa AG </td> <td align="right"> 0.93 </td> </tr>
  <tr> <td align="right"> 64 </td> <td> AAD.DE </td> <td> Amadeus FiRe AG </td> <td align="right"> 0.93 </td> </tr>
  <tr> <td align="right"> 65 </td> <td> AB1.DE </td> <td> Air Berlin PLC &amp; Co. Luftverkehrs KG </td> <td align="right"> 0.92 </td> </tr>
  <tr> <td align="right"> 66 </td> <td> ANN.DE </td> <td> DT ANNINGTON IM N </td> <td align="right"> 0.92 </td> </tr>
  <tr> <td align="right"> 67 </td> <td> GFK.DE </td> <td> GfK SE </td> <td align="right"> 0.92 </td> </tr>
  <tr> <td align="right"> 68 </td> <td> QSC.DE </td> <td> QSC AG </td> <td align="right"> 0.92 </td> </tr>
  <tr> <td align="right"> 69 </td> <td> MOR.DE </td> <td> Morphosys AG </td> <td align="right"> 0.92 </td> </tr>
  <tr> <td align="right"> 70 </td> <td> TIM.DE </td> <td> Tipp24 SE </td> <td align="right"> 0.91 </td> </tr>
  <tr> <td align="right"> 71 </td> <td> O2D.DE </td> <td> TELEFONICA DT H N </td> <td align="right"> 0.91 </td> </tr>
  <tr> <td align="right"> 72 </td> <td> LPK.DE </td> <td> LPKF Laser &amp; Electronics AG </td> <td align="right"> 0.91 </td> </tr>
  <tr> <td align="right"> 73 </td> <td> DRW3.DE </td> <td> Dragerwerk AG &amp; Co. KGaA </td> <td align="right"> 0.89 </td> </tr>
  <tr> <td align="right"> 74 </td> <td> DEZ.DE </td> <td> Deutz AG </td> <td align="right"> 0.87 </td> </tr>
  <tr> <td align="right"> 75 </td> <td> O2C.DE </td> <td> C.A.T. Oil AG </td> <td align="right"> 0.87 </td> </tr>
  <tr> <td align="right"> 76 </td> <td> BIO3.DE </td> <td> Biotest AG </td> <td align="right"> 0.82 </td> </tr>
  <tr> <td align="right"> 77 </td> <td> DLG.DE </td> <td> Dialog Semiconductor Plc </td> <td align="right"> 0.81 </td> </tr>
  <tr> <td align="right"> 78 </td> <td> AIXA.DE </td> <td> Aixtron SE </td> <td align="right"> 0.76 </td> </tr>
  <tr> <td align="right"> 79 </td> <td> BBZA.DE </td> <td> BB BIOTECH N </td> <td align="right"> 0.76 </td> </tr>
  <tr> <td align="right"> 80 </td> <td> TTI.DE </td> <td> Tom Tailor Holding AG </td> <td align="right"> 0.74 </td> </tr>
   </table>

EOF
