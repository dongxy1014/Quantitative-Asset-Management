library(data.table)
library(lubridate)
library(dplyr)
library(fBasics)
library(foreign)

###### QUESTION 1

CRSP_Stocks = as.data.table(read.table("C:/Users/seayo/Desktop/QAM/PS2/CRSP_Stocks.csv", sep=",",header = TRUE))

PS3_Q1 = function(crsp){
  
  #Select SHRCD = (10,11), EXCHCD = (1,2,3)
  crsp = crsp[SHRCD %in% c(10,11)]
  crsp = crsp[EXCHCD %in% c(1,2,3)]
  
  # Convert format
  crsp[, RET := as.numeric(levels(RET))[RET]]
  crsp[, DLRET := as.numeric(levels(DLRET))[DLRET]]
  crsp[, date := as.Date(as.character(date),format="%Y%m%d")] 
  all_month = sort(unique(crsp$date))
  
  # Create Year and Month
  crsp[, Year := year(date)] 
  crsp[, Month := month(date)] 
  
  # Raw data price has negative value
  crsp[, PRC := abs(PRC)] 
  crsp[, CAP := PRC * SHROUT/1000]
  crsp[, lag_Mkt_Cap := shift(CAP), by = PERMNO]
  
  # Deal with RET and DLRET
  # As CRSP calculate the return as P(t+1)/P(t) - 1, but the paper use cummulative log return,
  # we convert the return into log return
  setkey(crsp, date)
  
  crsp <- crsp[!is.na(RET) | !is.na(DLRET), ]
  crsp <- crsp[is.na(RET), Ret := DLRET]
  crsp <- crsp[is.na(DLRET), Ret := RET]
  crsp <- crsp[!is.na(RET) & !is.na(DLRET), Ret := (1+RET) * (1+DLRET)-1]
  
  crsp = crsp[,logret := log(Ret+1)]
  crsp[, Ranking_Ret := shift(logret, n=12)+shift(logret, n=11)+shift(logret, n=10)+
         shift(logret, n=9)+shift(logret, n=8)+shift(logret, n=7)+shift(logret, n=6)+
         shift(logret, n=5)+shift(logret, n=4)+shift(logret, n=3)+shift(logret, n=2),
         by = PERMNO]
  
  # for current month return, we convert the log return back to arithmetic return
  crsp = na.omit(crsp, cols=c("lag_Mkt_Cap", "Ranking_Ret", "Ret"))
  crsp = crsp[, c("date", "Year", "Month", "PERMNO",  "EXCHCD", "lag_Mkt_Cap", "Ret", "Ranking_Ret")]
  return(crsp)
}

# For the output, I kept columm date for the convinience of the following code
CRSP_Stocks_Momentum = PS3_Q1(crsp = CRSP_Stocks)


###### QUESTION 2

PS3_Q2 = function(mom){
  months = sort(unique(mom$date))
  for (m in 1:1104){
    mom[date==months[m],DM_decile :=cut(mom[date==months[m],]$Ranking_Ret,
                                        breaks=quantile(mom[date==months[m],]$Ranking_Ret,probs=c(0:10)/10,na.rm=TRUE), 
                                        include.lowest=TRUE, labels=FALSE)] 
    # Using Kenneth R. French method, we only use NYSE to do the sorting. if we still use quantile() function
    # it gives 11 values from 0%, 10%, ..., to 100%. Using cut() function with the quantiles as break will 
    # lead to a problem, that is for a stock not listed in NYSE, its Ranking_Ret may not be in the range (0%, 10%)
    # and (90%, 100%), it may be lower than 0% or higher than 100%. To make it sure such values can be sorted into
    # the correct range, we do the following:
    mom[date==months[m],KRF_decile :=cut(mom[date==months[m],]$Ranking_Ret,
                                        breaks=c(-Inf, quantile(mom[date==months[m] & EXCHCD==1,]$Ranking_Ret,
                                                                probs=c(0:10)/10,na.rm=TRUE)[2:10],Inf), 
                                        include.lowest=TRUE, labels=FALSE)] 
  }
  mom = mom[,-c("EXCHCD", "Ranking_Ret")]
  return(mom)
}

CRSP_Stocks_Momentum_decile = PS3_Q2(mom = CRSP_Stocks_Momentum)


###### QUESTION 3

FF_mkt = na.omit(as.data.table(read.table("C:/Users/seayo/Desktop/QAM/PS1/F-F_Research_Data_Factors.csv", skip = 3, sep=",",header = TRUE)))
colnames(FF_mkt) = c("date", "Market_minus_Rf", "SMB","HML","Rf")
FF_mkt[, date:= as.numeric(levels(date))[date]]   
FF_mkt[, Year:= date%/%100]   
FF_mkt[, Month:= date - Year*100]
FF_mkt = FF_mkt[,c(6,7,2,3,4,5)]
FF_mkt = FF_mkt[which(FF_mkt$Year==1927 & FF_mkt$Month ==1):length(FF_mkt$Year),]


PS3_Q3 = function(decile,ff){
  
  Rf = vector(mode = "numeric", length=11040)
  for (i in 1:1104){
    Rf[10*i-9] = ff$Rf[i]/100
    Rf[10*i-8] = ff$Rf[i]/100
    Rf[10*i-7] = ff$Rf[i]/100
    Rf[10*i-6] = ff$Rf[i]/100
    Rf[10*i-5] = ff$Rf[i]/100
    Rf[10*i-4] = ff$Rf[i]/100
    Rf[10*i-3] = ff$Rf[i]/100
    Rf[10*i-2] = ff$Rf[i]/100
    Rf[10*i-1] = ff$Rf[i]/100
    Rf[10*i] = ff$Rf[i]/100
  }
  decile[, DM_VW := lag_Mkt_Cap / sum(lag_Mkt_Cap),  by = list(date, DM_decile)]
  decile[, KRF_VW := lag_Mkt_Cap / sum(lag_Mkt_Cap),  by = list(date, KRF_decile)]
  DM = decile[, .(DM_Ret  = sum(DM_VW*Ret, na.rm=T)),  by = list(date, DM_decile)]
  DM = DM[order(date,DM_decile)]
  KRF = decile[, .(KRF_Ret  = sum(KRF_VW*Ret, na.rm=T)),  by = list(date, KRF_decile)]
  KRF = KRF[order(date,KRF_decile)]
  colnames(KRF)[2] = "decile"
  colnames(DM)[2] = "decile"
  output = as.data.table(full_join(DM, KRF , by = c("date","decile")))
  output[, Year := year(date)] 
  output[, Month := month(date)]
  output = output[,c("date","Year","Month","decile","DM_Ret", "KRF_Ret")]
  output = cbind(output, Rf)
}

CRSP_Stocks_Momentum_returns = PS3_Q3(decile = CRSP_Stocks_Momentum_decile, ff = FF_mkt)


###### QUESTION 4

# Select the data from 1927.01 to 2013.03
start = which(CRSP_Stocks_Momentum_returns$Year==1927 & CRSP_Stocks_Momentum_returns$Month==1 & CRSP_Stocks_Momentum_returns$decile==1)
end = which(CRSP_Stocks_Momentum_returns$Year==2013 & CRSP_Stocks_Momentum_returns$Month==3 & CRSP_Stocks_Momentum_returns$decile==10)
CRSP_Stocks_Momentum_returns_2013 = CRSP_Stocks_Momentum_returns[start:end,]

PS3_Q4 = function(dec_mm){
  
  output = matrix(0,nrow = 4, ncol = 11)
  row.names(output) = c("mean excess return", "SD", "SR","Skewness")
  colnames(output) = c("Decile 1", "Decile 2", "Decile 3", "Decile 4", "Decile 5", "Decile 6", "Decile 7",
"Decile 8", "Decile 9", "Decile 10", "WML")
  for (d in 1:10){
    output[1,d] = round(mean(dec_mm[decile==d, ]$DM_Ret - dec_mm[decile==d, ]$Rf)*12,4)
    output[2,d] = round(sqrt(12)*sd(dec_mm[decile==d, ]$DM_Ret - dec_mm[decile==d, ]$Rf),4)
    output[3,d] = round(output[1,d]/output[2,d],4)
    output[4,d] = round(skewness(log(dec_mm[decile==d, ]$DM_Ret+1)),4)
  }
  output[1,11] = round(mean(dec_mm[decile==10, ]$DM_Ret - dec_mm[decile==1, ]$DM_Ret)*12,4)
  output[2,11] = round(sqrt(12)*sd(dec_mm[decile==10, ]$DM_Ret - dec_mm[decile==1, ]$DM_Ret),4)
  output[3,11] = round(output[1,d]/output[2,d],4)
  output[4,11] = round(skewness(log(dec_mm[decile==10, ]$DM_Ret - dec_mm[decile==1, ]$DM_Ret +1 + dec_mm[decile==10, ]$Rf)),4)
  return(output)
}

Q4_result = PS3_Q4(CRSP_Stocks_Momentum_returns_2013)


###### QUESTION 5

KRF_returns = na.omit(as.data.table(read.table("C:/Users/seayo/Desktop/QAM/PS3/10_Portfolios_Prior_12_2.csv", sep=",",header = TRUE)))
colnames(KRF_returns)[1] = "date"
KRF_returns[, Year:= date%/%100]   
KRF_returns[, Month:= date - Year*100]
KRF_returns[,2:11] =KRF_returns[,2:11]/100 
KRF_returns = KRF_returns[,c(12,13,2,3,4,5,6,7,8,9,10,11)]
KRF_returns = KRF_returns[1:1104,]

DM_returns = read.table("C:/Users/seayo/Desktop/QAM/PS3/m_m_pt_tot.txt",header=FALSE, sep = "", dec=".")[,c(1,2,3)]
library(reshape2)
DM_returns = as.data.table(dcast(DM_returns, V1 ~V2, value.var='V3' ))
DM_returns$V1 = as.Date(as.character(DM_returns$V1),format="%Y%m%d")
DM_returns[, Year:= year(V1)]   
DM_returns[, Month:= month(V1)]
DM_returns = DM_returns[,c(12,13,2,3,4,5,6,7,8,9,10,11)]


PS3_Q5 = function(dec_mm, dm, krf){
  # DM from 1927.01 - 2016.12
  # KRF from 1927.01 - 2018.12

  start = which(dec_mm$Year==1927 & dec_mm$Month==1 & dec_mm$decile==1)
  end = which(dec_mm$Year==2016 & dec_mm$Month==12 & dec_mm$decile==10)
  dec_mm_2016 = dec_mm[start:end,]

  output = matrix(0,nrow = 2, ncol = 11)
  row.names(output) = c("DM correlation", "KRF correlation")
  colnames(output) = c("Decile 1", "Decile 2", "Decile 3", "Decile 4", "Decile 5", "Decile 6", "Decile 7",
                       "Decile 8", "Decile 9", "Decile 10", "WML")
  dm = as.matrix(dm[,3:12])
  krf = as.matrix(krf[,3:12])
  
  for (d in 1:10){
    output[1,d] = cor(dec_mm_2016[decile==d, ]$DM_Ret, dm[,d]) 
    output[2,d] = cor(dec_mm[decile==d, ]$KRF_Ret, krf[,d]) 
  }
  output[1,11] = cor((dec_mm_2016[decile==10, ]$DM_Ret - dec_mm_2016[decile==1, ]$DM_Ret), (dm[,10] - dm[,1])) 
  output[2,11] = cor((dec_mm[decile==10, ]$KRF_Ret - dec_mm[decile==1, ]$KRF_Ret), (krf[,10] - krf[,1])) 
  output = round(output,4)
  return(output)
}

Q5_result = PS3_Q5(dec_mm=CRSP_Stocks_Momentum_returns, dm=DM_returns, krf=KRF_returns)

###### QUESTION 6

# Let's view the momentum portfilio monthly returns from 2000 to now
start = which(CRSP_Stocks_Momentum_returns$Year==2000 & CRSP_Stocks_Momentum_returns$Month==1 & CRSP_Stocks_Momentum_returns$decile==1)
end = which(CRSP_Stocks_Momentum_returns$Year==2018 & CRSP_Stocks_Momentum_returns$Month==12 & CRSP_Stocks_Momentum_returns$decile==10)
dec_mm_recent = CRSP_Stocks_Momentum_returns[start:end,]
time = sort(unique(dec_mm_recent$date))
  
recent = PS3_Q4(dec_mm_recent)

library(ggplot2)
dec_mm_recent$decile = factor(dec_mm_recent$decile)

dm = ggplot(dec_mm_recent, aes(x= date, y=DM_Ret, fill = decile))+geom_area()
dm

krf = ggplot(dec_mm_recent, aes(x= date, y=KRF_Ret, fill = decile))+geom_area()
krf

DM_WML = dec_mm_recent[decile=='10',]$DM_Ret - dec_mm_recent[decile=='1',]$DM_Ret
KRF_WML = dec_mm_recent[decile=='10',]$KRF_Ret - dec_mm_recent[decile=='1',]$KRF_Ret
DM_WML = log(DM_WML+1)
KRF_WML = log(KRF_WML+1)
WML = data.frame(time, cumsum(DM_WML), cumsum(KRF_WML))
dm_wml = ggplot()+geom_line(aes(x= WML$time, y=WML$cumsum.DM_WML.), colour='blue') 
dm_wml
krf_wml = ggplot()+geom_line(aes(x= WML$time, y=WML$cumsum.KRF_WML.), colour='blue') 
krf_wml