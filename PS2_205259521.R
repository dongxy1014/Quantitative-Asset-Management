library(data.table)
library(lubridate)
library(dplyr)
library(fBasics)

setwd("C:/Users/seayo/Desktop/QAM/PS2")


###### QUESTION 1
CRSP_Bonds = as.data.table(read.table("CRSP_Bonds.csv", sep=",",header = TRUE))

PS2_Q1 = function(crsp){
  # Convert format
  crsp[, KYCRSPID := as.character(levels(KYCRSPID))[KYCRSPID]]
  crsp[, MCALDT := as.character(levels(MCALDT))[MCALDT]] 
  crsp[, MCALDT := as.Date(MCALDT,"%m/%d/%Y")]
  
  
  crsp[, lag_CAP := shift(TMTOTOUT), by = KYCRSPID]
  crsp = crsp[complete.cases(lag_CAP),]
  # Deal with RET with -99
  crsp = crsp[-which(TMRETNUA==-99),]
  
  crsp[, VW := lag_CAP /sum(lag_CAP),  by = MCALDT]
  setkey(crsp, MCALDT)
  # EWRET
  EWRET <- crsp[, .(Bond_Ew_Ret  = round(mean(TMRETNUA),4)),  by = MCALDT]
  
  # VWRET
  VWRET <- crsp[, .(Bond_Vw_Ret  = round(sum(VW*TMRETNUA),4)), by = MCALDT]
  
  # Lagged Marekt Cap
  lag_CAP <- crsp[, .(Bond_lag_MV = round(sum(lag_CAP), 4)), by = MCALDT]
  
  # Add Year and Month
  lag_CAP[, Year:= year(MCALDT)]
  lag_CAP[, Month:= month(MCALDT)]
  
  output = full_join(lag_CAP, EWRET, by = "MCALDT")
  output = full_join(output, VWRET , by = "MCALDT")
  
  # If lagged market cap is not available, so is VWRET 
  output = output[, c(3,4,2,5,6)]
  output$Bond_lag_MV[which(output$Bond_lag_MV==0)] = NA
  output = na.omit(output)
  
  return(output)
}
  
Monthly_CRSP_Bonds = PS2_Q1(crsp = CRSP_Bonds[,-1])


###### QUESTION 2
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
  lag_CAP <- crsp[, .(Stock_lag_MV = round(sum(lag_CAP)/1000, 4)), by = date]
  
  # Add Year and Month
  lag_CAP[, Year:= year(date)]
  lag_CAP[, Month:= month(date)]
  
  output = full_join(lag_CAP, EWRET, by = "date")
  output = full_join(output, VWRET , by = "date")
  output = output[, c(3,4,2,5,6)]
  
  return(output)
}

Monthly_CRSP_Stocks = PS1_Q1(CRSP_Stocks)

# Risk free rate from CRSP
Monthly_CRSP_Riskless = as.data.table(read.table("CRSP_riskfree.csv", sep=",",header = TRUE))
Monthly_CRSP_Riskless[, caldt := as.Date(as.character(caldt),format="%Y%m%d")] 

###
PS2_Q2 = function(stock, bond, riskless){
  Year = stock$Year
  Month = stock$Month
  Stock_lag_MV = stock$Stock_lag_MV
  Stock_Excess_Vw_Ret = stock$Stock_Vw_Ret - riskless$t30ret
  Bond_lag_MV = bond$Bond_lag_MV
  Bond_Excess_Vw_Ret = bond$Bond_Vw_Ret - riskless$t30ret
  output = as.data.table(data.frame(Year, Month, Stock_lag_MV, Stock_Excess_Vw_Ret, Bond_lag_MV, Bond_Excess_Vw_Ret))
  return(output)
}

Monthly_CRSP_Universe = PS2_Q2(Monthly_CRSP_Stocks, Monthly_CRSP_Bonds, Monthly_CRSP_Riskless)


###### QUESTION 3

PS2_Q3 = function(universe){
  universe[,index := seq(1,1116,1)]
  universe[, Excess_60_40_Ret := 0.6*Stock_Excess_Vw_Ret+0.4*Bond_Excess_Vw_Ret, by=index] 
  universe[, Excess_Vw_Ret := Stock_lag_MV/(Stock_lag_MV + Bond_lag_MV)*Stock_Excess_Vw_Ret
           +Bond_lag_MV/(Stock_lag_MV + Bond_lag_MV)*Bond_Excess_Vw_Ret, by=index]
  
  Stock_inverse_sigma_hat = vector(mode="numeric", length=1116)
  Bond_inverse_sigma_hat = vector(mode="numeric", length=1116)
  Unlevered_k = vector(mode="numeric", length=1116)
  
  # Rolling-window sigma
  for (i in 1:1116){
    if(i<=36){
      Stock_inverse_sigma_hat[i] = NA
      Bond_inverse_sigma_hat[i] = NA
      Unlevered_k[i] =NA
    }
    else{
      Stock_inverse_sigma_hat[i] = 1/sd(universe$Stock_Excess_Vw_Ret[(i-36):(i-1)], na.rm = T)
      Bond_inverse_sigma_hat[i] = 1/sd(universe$Bond_Excess_Vw_Ret[(i-36):(i-1)], na.rm = T)
      Unlevered_k[i] = 1/sum(Stock_inverse_sigma_hat[i]+Bond_inverse_sigma_hat[i])
    }
  }
  
  universe[, Stock_inverse_sigma_hat:= Stock_inverse_sigma_hat]
  universe[, Bond_inverse_sigma_hat:= Bond_inverse_sigma_hat]
  universe[, Unlevered_k:= Unlevered_k]
  universe[, Excess_Unlevered_RP_Ret:= Stock_inverse_sigma_hat*Unlevered_k*Stock_Excess_Vw_Ret +
             Bond_inverse_sigma_hat*Unlevered_k*Bond_Excess_Vw_Ret, by = index]
  universe[, Levered_k := sd(Excess_Vw_Ret[-36])/sd(Stock_inverse_sigma_hat*Stock_Excess_Vw_Ret +
                                                      Bond_inverse_sigma_hat*Bond_Excess_Vw_Ret ,
                                                    na.rm=T)]
  universe[, Excess_Levered_RP_Ret:= Stock_inverse_sigma_hat*Levered_k*Stock_Excess_Vw_Ret +
             Bond_inverse_sigma_hat*Levered_k*Bond_Excess_Vw_Ret, by = index]
  universe = universe[,c(1,2,4,6,8,9,10,11,12,13,14,15)]
}
Port_Rets = PS2_Q3(universe = Monthly_CRSP_Universe)


PS2_Q4 = function(port){
  # Select from 1930.01 to 2010.06
  port = port[49:1014,]
  output = matrix(0,6,6)
  colnames(output) = c("Annualized Mean", "t-stat of Annualized Mean", "Annualized Standard Deviation",
                       "Annualized Sharpe Ratio", "Skewness", "Excess Kurtosis")
  row.names(output) = c("CRSP stocks", "CRSP bonds", "Value-weighted portfolio", "60/40 portfolio", "unlevered RP", "levered RP")
  output[1,1] = mean(port$Stock_Excess_Vw_Ret)*12
  output[2,1] = mean(port$Bond_Excess_Vw_Ret)*12
  output[3,1] = mean(port$Excess_Vw_Ret)*12
  output[4,1] = mean(port$Excess_60_40_Ret)*12
  output[5,1] = mean(port$Excess_Unlevered_RP_Ret)*12
  output[6,1] = mean(port$Excess_Levered_RP_Ret)*12
  length = dim(port)[1]
  output[1,2] = mean(port$Stock_Excess_Vw_Ret)*12 / sd(port$Stock_Excess_Vw_Ret*12)*sqrt(length-1)
  output[2,2] = mean(port$Bond_Excess_Vw_Ret)*12 /sd(port$Bond_Excess_Vw_Ret*12)*sqrt(length-1)
  output[3,2] = mean(port$Excess_Vw_Ret)*12 / sd(port$Excess_Vw_Ret*12)*sqrt(length-1)
  output[4,2] = mean(port$Excess_60_40_Ret)*12 / sd(port$Excess_60_40_Ret*12)*sqrt(length-1)
  output[5,2] = mean(port$Excess_Unlevered_RP_Ret)*12 /sd(port$Excess_Unlevered_RP_Ret*12)*sqrt(length-1)
  output[6,2] = mean(port$Excess_Levered_RP_Ret)*12 / sd(port$Excess_Levered_RP_Ret*12)*sqrt(length-1)
  
  output[1,3] = sd(port$Stock_Excess_Vw_Ret)*sqrt(12)
  output[2,3] = sd(port$Bond_Excess_Vw_Ret)*sqrt(12)
  output[3,3] = sd(port$Excess_Vw_Ret)*sqrt(12)
  output[4,3] = sd(port$Excess_60_40_Ret)*sqrt(12)
  output[5,3] = sd(port$Excess_Unlevered_RP_Ret)*sqrt(12)
  output[6,3] = sd(port$Excess_Levered_RP_Ret)*sqrt(12)
  
  output[1,4] = output[1,1] / output[1,3] 
  output[2,4] = output[2,1] / output[2,3] 
  output[3,4] = output[3,1] / output[3,3] 
  output[4,4] = output[4,1] / output[4,3] 
  output[5,4] = output[5,1] / output[5,3] 
  output[6,4] = output[6,1] / output[6,3] 
  
  output[1,5] = skewness(port$Stock_Excess_Vw_Ret*12)
  output[2,5] = skewness(port$Bond_Excess_Vw_Ret*12)
  output[3,5] = skewness(port$Excess_Vw_Ret*12)
  output[4,5] = skewness(port$Excess_60_40_Ret*12)
  output[5,5] = skewness(port$Excess_Unlevered_RP_Ret*12)
  output[6,5] = skewness(port$Excess_Levered_RP_Ret*12)
  
  output[1,6] = kurtosis(port$Stock_Excess_Vw_Ret*12)
  output[2,6] = kurtosis(port$Bond_Excess_Vw_Ret*12)
  output[3,6] = kurtosis(port$Excess_Vw_Ret*12)
  output[4,6] = kurtosis(port$Excess_60_40_Ret*12)
  output[5,6] = kurtosis(port$Excess_Unlevered_RP_Ret*12)
  output[6,6] = kurtosis(port$Excess_Levered_RP_Ret*12)
  
  return(output)
}

output = PS2_Q4(Port_Rets)
print(output)

write.table(Port_Rets,file="test.csv",sep=",")

