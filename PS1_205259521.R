library(data.table)
library(lubridate)
library(dplyr)
library(fBasics)

setwd("C:/Users/seayo/Desktop/QAM/PS1")
###### QUESTION 1


CRSP_Stocks = as.data.table(read.table("CRSP_Stocks.csv", sep=",",header = TRUE))


PS1_Q1 = function(crsp){
  # Convert format
  crsp[, RET := as.numeric(levels(RET))[RET]]
  crsp[, DLRET := as.numeric(levels(DLRET))[DLRET]]
  crsp[, date := as.Date(as.character(date),format="%Y%m%d")] 
  
  # Raw data price has negative value
  crsp[, PRC := abs(PRC)] 
  crsp[, CAP := PRC * SHROUT]
  
  crsp[, lag_CAP := shift(CAP), by = PERMNO]
  
  #Select SHRCD = (10,11), EXCHCD = (1,2,3)
  crsp = crsp[SHRCD %in% c(10,11)]
  crsp = crsp[EXCHCD %in% c(1,2,3)]
  crsp = na.omit(crsp, cols="lag_CAP")
  
  # Deal with RET and DLRET
  crsp <- crsp[!is.na(RET) | !is.na(DLRET), ]
  crsp <- crsp[is.na(RET), RETURN := DLRET]
  crsp <- crsp[is.na(DLRET), RETURN := RET]
  crsp <- crsp[!is.na(RET) & !is.na(DLRET), RETURN := (1+RET) * (1+DLRET) - 1]
  crsp <- crsp[, -c("SHRCD", "EXCHCD", "DLRET", "PRC", "SHROUT", "RET", "CAP")]
  
  
  setkey(crsp, date)
  crsp[, VW := lag_CAP / sum(lag_CAP),  by = date]
  
  # EWRET
  EWRET <- crsp[, .(Stock_Ew_Ret  = round(mean(RETURN, na.rm = T),4)),  by = date]
  
  # VWRET
  VWRET <- crsp[, .(Stock_Vw_Ret  = round(sum(VW*RETURN, na.rm=T),4)), by = date]
  
  # Lagged Marekt Value
  lag_CAP <- crsp[, .(Stock_lag_MV = round(sum(lag_CAP)/1000000, 4)), by = date]
  
  # Add Year and Month
  lag_CAP[, Year:= year(date)]
  lag_CAP[, Month:= month(date)]
  
  output = full_join(lag_CAP, EWRET, by = "date")
  output = full_join(output, VWRET , by = "date")
  output = output[, c(3,4,2,5,6)]
  
  return(output)
}

Monthly_CRSP_Stocks = PS1_Q1(CRSP_Stocks)


###### QUESTION 2


FF_mkt = na.omit(as.data.table(read.table("F-F_Research_Data_Factors.csv", skip = 3, sep=",",header = TRUE)))
colnames(FF_mkt) = c("date", "Market_minus_Rf", "SMB","HML","Rf")
FF_mkt[, date:= as.numeric(levels(date))[date]]   
FF_mkt[, Year:= date%/%100]   
FF_mkt[, Month:= date - Year*100]
FF_mkt = FF_mkt[,c(6,7,2,3,4,5)]


PS1_Q2 = function(crsp,ff){
  crsp = crsp[which(crsp$Year==1926 & crsp$Month ==7):length(crsp$Year),]
  est = crsp$Stock_Vw_Ret-ff$Rf/100
  real = ff$Market_minus_Rf/100
  output = matrix(0,nrow = 5,ncol = 2)
  row.names(output) = c("Annualized Mean", "Annualized Standard Deviation",
                        "Annualized Sharpe Ratio", "Skewness", "Excess Kurtosis")
  colnames(output) = c("Estimated FF Market Excess Return", "Actual FF Market Excess Return")
  output[1,1] = round(mean(est)*12,4)
  output[1,2] = round(mean(real)*12,4)
  output[2,1] = round(sd(est)*sqrt(12),4)
  output[2,2] = round(sd(real)*sqrt(12),4)
  output[3,1] = round(output[1,1]/output[2,1],4)
  output[3,2] = round(output[1,2]/output[2,2],4)
  output[4,1] = round(skewness(est),4)
  output[4,2] = round(skewness(real),4)
  output[5,1] = round(kurtosis(est,method = "excess"),4)
  output[5,2] = round(kurtosis(real,method = "excess"),4)
  return(output)
}

Comparision = PS1_Q2(Monthly_CRSP_Stocks,FF_mkt)


###### QUESTION 3

PS1_Q3 = function(crsp,ff){
  crsp = crsp[which(crsp$Year==1926 & crsp$Month ==7):length(crsp$Year),]
  est = crsp$Stock_Vw_Ret-ff$Rf/100
  real = ff$Market_minus_Rf/100
  output = vector(mode="numeric",length=2)
  output[1] = round(cor(est,real),8)
  output[2] = round(max(abs(est-real)),8)
  return(output)
}

Difference = PS1_Q3(Monthly_CRSP_Stocks,FF_mkt)
