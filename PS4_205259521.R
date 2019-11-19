library(tidyverse)
library(dplyr)
require(data.table)
library(lubridate)
library(fBasics)
library(zoo)

#------------------------------------------ Question 1 ---------------------------------------#
setwd("C:/Users/seayo/Desktop/QAM/PS4")

data.prba = as.data.table(read.csv("prba.csv")) 
data.comp = as.data.table(read.csv("compustat.csv")) 
data.crsp = as.data.table(read.csv("crsp.csv")) 
data.link = as.data.table(read.csv("linktable.csv"))

########################## Clean Compustat ##########################
data.comp = data.comp[curcd=="USD"]
data.comp = data.comp[indfmt=="INDL"]
data.comp = merge(data.comp,data.prba,by=c("gvkey","datadate"),all.x=T)
data.comp[, datadate:= as.Date(as.character(datadate),"%Y%m%d")]
data.comp[, Month:= month(datadate)]
data.comp[,Year:= year(datadate)]
data.comp = data.comp[,-c(4:8)]

data.comp[,BE:= coalesce(seq, ceq + pstk, at - lt -mib, at - lt) + coalesce(txditc, txdb + itcb, 0) - 
            coalesce(pstkrv, pstkl, pstk, 0) - coalesce(prba, 0) ]
data.comp = data.comp[,.(gvkey, datadate, fyear, Month, Year, BE)]

########################## Clean CRSP ##########################
data.crsp = data.crsp[SHRCD %in% c(10,11)]
data.crsp = data.crsp[EXCHCD %in% c(1,2,3)]

data.crsp[, RET := as.numeric(levels(RET))[RET]]
data.crsp[, DLRET := as.numeric(levels(DLRET))[DLRET]]
data.crsp[, date := as.Date(as.character(date),format="%Y%m%d")] 
data.crsp[, Year := year(date)] 
data.crsp[, Month := month(date)] 

data.crsp[, PRC := abs(PRC)] 
data.crsp[, CAP := PRC * SHROUT/1000]
data.crsp[, lag_Mkt_Cap := shift(CAP), by = PERMNO]

data.crsp <- data.crsp[!is.na(RET) | !is.na(DLRET), ]
data.crsp <- data.crsp[is.na(RET), Ret := DLRET]
data.crsp <- data.crsp[is.na(DLRET), Ret := RET]
data.crsp <- data.crsp[!is.na(RET) & !is.na(DLRET), Ret := (1+RET) * (1+DLRET)-1]

########################## Merge Linktable and Compustat data ##########################

#### code from TA session
merged <- merge(data.crsp, data.link, by.x='PERMCO', by.y = 'LPERMCO', allow.cartesian = T)
setkey(merged)

merged[,LINKDT := as.Date(as.character(LINKDT),"%Y%m%d")]
merged[LINKENDDT == 'E', LINKENDDT := NA]
merged[,LINKENDDT := as.Date(as.character(LINKENDDT),"%Y%m%d")]
merged = merged[(is.na(LINKDT) | date >= LINKDT) & (is.na(LINKENDDT) | date <= LINKENDDT)]
setorder(merged, gvkey, date)

# Multiple GVKEYs per PERMCO

### First, if LC not LC linktype, only keep LC
# identify Same PERMCO but different PERMNO
merged[, prob := .N > 1, by = .(PERMCO, date)]
merged[, Good_match := sum(LINKTYPE == 'LC'), by =.(PERMCO, date)]
merged = merged[!(prob == T & Good_match == T & LINKTYPE != 'LC')]

### Second, if P and not P linkprim, only keep p
merged[, prob := .N > 1, by= .(PERMCO, date)]
merged[, Good_match := sum(LINKPRIM == 'P'), by =.(PERMCO, date)]
merged <- merged[!(prob == T & Good_match == T & LINKPRIM != 'P')]

### Third, if 1 and not liid, only keep 1 
merged[, prob := .N > 1, by = .(PERMCO, date)]
merged[, Good_match := sum(LIID == 1), by =.(PERMCO,date)]
merged = merged[!(prob == T & Good_match == T & LIID != 1)]

### Fourth, use the link that's current
merged[, prob := .N > 1, by = .(PERMCO, date)]
merged[, Good_match := sum(is.na(LINKENDDT)), by = .(PERMCO, date)]
merged = merged[!(prob==T & Good_match == T & !is.na(LINKENDDT))]

### Fifth, use the link that's been around the longest
merged[, prob := .N > 1, by = .(PERMCO, date)]
merged[, Good_match := NULL]
merged[is.na(LINKENDDT), LINKENDDT := as.Date('2019-12-31', '%Y-%m-%d')]
merged[, Date_diff := as.integer(LINKENDDT - LINKDT)]
setorder(merged, PERMCO, date, Date_diff)
merged[prob == T, Good_match := Date_diff == Date_diff[.N], by = .(PERMCO, date)]
merged = merged[!(prob==T & Good_match != T)]

### Sixth, use the gvkey that has been around the longest
merged[, prob := .N > 1, by = .(PERMCO, date)]
merged[, Good_match :=NULL]
setorder(merged, gvkey, LINKDT)
merged[prob == T, start_Date := LINKDT[1], by = .(gvkey)]
setorder(merged, gvkey, LINKENDDT)
merged[prob == T, end_Date := LINKENDDT[.N], by = .(gvkey)]
merged[, Date_diff := as.integer(end_Date - start_Date)]
setorder(merged, PERMCO, date, Date_diff)
merged[prob == T, Good_match := Date_diff == Date_diff[.N], by = .(PERMCO, date)]
merged = merged[!(prob == T & Good_match != T)]

### Seventh, use the smaller gvkey
setorder(merged, PERMCO, date, gvkey)
merged = unique(merged, by = c('PERMCO', 'date'))

### Clean up extra variables and final check of match
merged = merged[, .(gvkey, date, EXCHCD, CAP, PERMCO, PERMNO, Ret , Year, Month)]

################################ Merge CRSP and Compustat together ############################################### 

data = as.data.table(left_join(merged,data.comp,by =c("gvkey"="gvkey","Year"="fyear")))
data = data[, .(gvkey, date, PERMNO, PERMCO, EXCHCD, Ret, CAP, BE)] 
data = data[complete.cases(data)]

data[, Year:= year(date)]
data[, Month:= month(date)]
setorder(data, Year, Month)

################################ Size Portfolio ############################################### 


# Sum up the market cap for companies with multiple securities
data = data[, MktCap := sum(CAP,na.rm = T), .(PERMCO, date)]
data.june <- data[Month == 6, .(PERMCO, Year, MktCap, EXCHCD)]
years = sort(unique(data.june$Year))
for (y in 1:length(years)){
  data.june[Year==years[y],size_decile :=cut(data.june[Year==years[y],]$MktCap,
                                             breaks=c(-Inf, quantile(data.june[Year==years[y] & EXCHCD==1,]$MktCap,
                                                                     probs=c(0:10)/10,na.rm=TRUE)[2:10],Inf), 
                                             include.lowest=TRUE, labels=FALSE)] 

}
data.june = data.june[,.(PERMCO, Year, size_decile)]
setorder(data, Year, Month)
setorder(data.june, Year)

data.size = merge(data, data.june, by = c("PERMCO","Year"), all.x = T)
data.size[,c("Size","Lagged_MktCap") := .(shift(size_decile, 6), shift(MktCap,1)), by=PERMNO]

data.size = data.size[Lagged_MktCap != 0]
data.size = data.size[!is.na(Size) & !is.na(Lagged_MktCap)]

size_port = data.size[,.(Size_Ret = weighted.mean(Ret, Lagged_MktCap, na.rm = TRUE)), .(Year, Month, Size)]
setkey(size_port, Year, Month, Size)

################################ BTM Portfolio ############################################### 
data.btm = data[Month == 12,]
data.btm[,BtM := BE/MktCap]
data.btm = data.btm[!is.na(BtM) & BtM>0]
data.btm[,BtM_decile := findInterval(BtM, quantile(.SD[EXCHCD==1,BtM], seq(0.1,0.9,0.1)), left.open = T) + 1,by = .(Year, Month)]
data.btm[,lagged_BtM_decile := shift(BtM_decile), by=PERMNO]

setorder(data, Year, Month)
setorder(data.btm, Year)

# MERGE the BtM decile information back to crsp cleaned up data.
data.btm = data.btm[, .(PERMNO,PERMCO, Year, BtM_decile, lagged_BtM_decile)]
data.btm.decile = merge(data, data.btm, by = c("PERMNO","PERMCO","Year"), all.x = T)
data.btm.decile[,c("BtM_Rank","lagged_MktCap") := .(shift(lagged_BtM_decile, 6), shift(MktCap, 1)), .(PERMNO)]

data.btm.decile = data.btm.decile[!is.na(BtM_Rank) & !is.na(lagged_MktCap) & lagged_MktCap!=0.0]
BtM_port = data.btm.decile[,.(BtM_Ret = weighted.mean(Ret, lagged_MktCap, na.rm = TRUE)), .(Year, Month, BtM_Rank)]
setkey(BtM_port, Year, Month, BtM_Rank)

#################################### Find the HML SMB  ####################################
data.size[,SMB:=ifelse(Size<=6,"S","B")]
data.btm.decile[,HML:=ifelse(BtM_Rank<=3,"L",ifelse(BtM_Rank<=7,"M","H"))]
BtM = data.btm.decile[,.(PERMNO, PERMCO, Year, Month, Ret, HML,lagged_MktCap)]
size = data.size[,.(PERMNO, PERMCO, Year, Month, Ret, SMB,Lagged_MktCap)]

setkey(BtM,PERMCO,PERMNO,Year,Month)
setkey(size,PERMCO,PERMNO,Year,Month)

SizeBtM =  merge(BtM,size)

SMB_HML =  SizeBtM[,.(Ret = weighted.mean(Ret.x, lagged_MktCap, na.rm = T)),.(Year,Month, HML,SMB)]
setkey(SMB_HML, Year,Month)
setorder(SMB_HML, Year)

SMB = SMB_HML[,.(SMB_Ret = (.SD[SMB=="S" & HML=="L",Ret] + .SD[SMB=="S" & HML=="M",Ret]+ .SD[SMB=="S" & HML=="H",Ret] - .SD[SMB=="B" & HML=="L",Ret] - .SD[SMB=="B" & HML=="M",Ret] - .SD[SMB=="B" & HML=="H",Ret])/3), by =.(Year, Month)]
HML = SMB_HML[, .(HML_Ret =(.SD[SMB=="S" & HML=="H",Ret] + .SD[SMB=="B" & HML=="H",Ret] -.SD[SMB=="S" & HML=="L",Ret] - .SD[SMB=="B" & HML=="L",Ret])/2), by =.(Year, Month)]

Q1_output = full_join(size_port, BtM_port, by=c("Year", "Month"))
Q1_output<- merge(Q1_output,HML)
Q1_output<- merge(Q1_output,SMB)
colnames(Q1_output)[3] = "Size_Rank"
write.table(Q1_output, file = "Q1_output.csv", row.names=FALSE, sep=",")

#------------------------------------------ Question 2 ---------------------------------------#

FF_Factors<- as.data.table(read.csv("F-F_Research_Data_Factors_.csv"))
size_FF_portfolio<- as.data.table(read.csv("Portfolios_Formed_on_ME.csv"))

FF_Factors[, Year:= X%/%100]   
FF_Factors[, Month:= X - Year*100]
FF_Factors= FF_Factors[ X>=197307& X<=201812,]
FF_Factors= FF_Factors[,.(Year, Month, SMB,HML,RF)]
setorder(FF_Factors,Year,Month)


size_FF_portfolio = size_FF_portfolio[X>=197307& X<=201812,]
size_FF_portfolio = size_FF_portfolio[, c(1,11:20)]
colnames(size_FF_portfolio) = c("date", 1:10)
size_FF_portfolio = melt(size_FF_portfolio, id.vars = "date")
size_FF_portfolio[, Year:= date%/%100]   
size_FF_portfolio[, Month:= date - Year*100]

size_FF_portfolio = size_FF_portfolio[, .(Year, Month, Ret= value/100, Size_rank = variable)]
FF_Factors = FF_Factors[, .(Year, Month, SMB = SMB/100, HML= HML/100, RF= RF/100)]

FF_merged.size = merge(size_FF_portfolio, FF_Factors[, c(1,2,5)], by =c("Year","Month"))

size_mat = matrix(nrow=5, ncol=11,dimnames = list(c("Excess Return","Standard Deviation",
                                                   "Sharpe Ratio","Skewness","Correlation"), c(1:10,"LongShort")))

for(i in 1:10){
  size_mat[1,i] = mean(size_port[Size==i,Size_Ret] - FF_merged.size[Size_rank==i,RF])*12
  size_mat[2,i] = sd(size_port[Size==i,Size_Ret])*sqrt(12)
  size_mat[3,i] = size_mat[1,i]/size_mat[2,i]
  size_mat[4,i] = skewness(size_port[Size==i,Size_Ret])
  size_mat[5,i] = cor(size_port[Size==i,Size_Ret], FF_merged.size[Size_rank==i,Ret])
}

WML_series = size_port[Size==1,Size_Ret] - FF_merged.size[Size_rank==10,Ret]
size_mat[1,11] <- mean(WML_series)*12
size_mat[2,11] <- sd(WML_series)*sqrt(12)
size_mat[3,11] <- size_mat[1,11]/size_mat[2,11]
size_mat[4,11] <- skewness(WML_series)
size_mat[5,11] <- cor(WML_series, (FF_merged.size[Size_rank==1,Ret] - FF_merged.size[Size_rank==10,Ret]))

write.table(size_mat, file = "Size_Result.csv", row.names=TRUE, sep=",")


#------------------------------------------ Question 3 ---------------------------------------#
BEME_FF_portfolio = as.data.table(read.csv("Portfolios_Formed_on_BE-ME.csv"))

BEME_FF_portfolio = BEME_FF_portfolio[X>=197307& X<=201812,]
BEME_FF_portfolio = BEME_FF_portfolio[, c(1,11:20)]
colnames(BEME_FF_portfolio) = c("date", 1:10)
BEME_FF_portfolio = melt(BEME_FF_portfolio, id.vars = "date")
BEME_FF_portfolio[, Year:= date%/%100]   
BEME_FF_portfolio[, Month:= date - Year*100]
BEME_FF_portfolio = BEME_FF_portfolio[, .(Year, Month, Ret= value/100, BtM_rank = variable)]
FF_merged.BtM = merge(BEME_FF_portfolio, FF_Factors[, c(1,2,5)], by =c("Year","Month"))

BtM_mat = matrix(nrow=5, ncol=11,dimnames = list(c("Excess Return","Standard Deviation",
                                                    "Sharpe Ratio","Skewness","Correlation"), c(1:10,"LongShort")))

for(i in 1:10){
  BtM_mat[1,i] = mean(BtM_port[BtM_Rank==i,BtM_Ret] - FF_merged.BtM[BtM_rank==i,RF])*12
  BtM_mat[2,i] = sd(BtM_port[BtM_Rank==i,BtM_Ret])*sqrt(12)
  BtM_mat[3,i] = BtM_mat[1,i]/BtM_mat[2,i]
  BtM_mat[4,i] = skewness(BtM_port[BtM_Rank==i,BtM_Ret])
  BtM_mat[5,i] = cor(BtM_port[BtM_Rank==i,BtM_Ret], FF_merged.BtM[BtM_rank==i,Ret])
}


WML_series = BtM_port[BtM_Rank==10,BtM_Ret] - FF_merged.BtM[BtM_rank==1,Ret]
BtM_mat[1,11] <- mean(WML_series)*12
BtM_mat[2,11] <- sd(WML_series)*sqrt(12)
BtM_mat[3,11] <- BtM_mat[1,11]/BtM_mat[2,11]
BtM_mat[4,11] <- skewness(WML_series)
BtM_mat[5,11] <- cor(WML_series, (FF_merged.BtM[BtM_rank==10,Ret] - FF_merged.BtM[BtM_rank==1,Ret]))

write.table(BtM_mat, file = "BTM_Result.csv", row.names=TRUE, sep=",")

#------------------------------------------ Question 4 ---------------------------------------#

start = which(size_port$Year==2008 & size_port$Month==1 & size_port$Size==1)
end = which(size_port$Year==2018 & size_port$Month==12 & size_port$Size==10)

size_recent = size_port[start:end,]
size_recent[,date:=as.yearmon(as.character(Year*100+Month),"%Y%m")]
library(ggplot2)
size_recent$Size = factor(size_recent$Size )

size.decile.plot = ggplot(size_recent, aes(x=date, y=Size_Ret, fill = Size))+geom_area()
size.decile.plot

time=sort(unique(size_recent$date))
size_WML = size_recent[Size=='1',]$Size_Ret - size_recent[Size=='10',]$Size_Ret
size_WML = log(size_WML+1)
size_WML_port = data.frame(time, cumsum(size_WML))
size_wml.plot = ggplot()+geom_line(aes(x= size_WML_port$time, y=size_WML_port$cumsum.size_WML.), colour='blue') 
size_wml.plot


BtM_recent = BtM_port[start:end,]
BtM_recent[,date:=as.yearmon(as.character(Year*100+Month),"%Y%m")]
BtM_recent$BtM_Rank = factor(BtM_recent$BtM_Rank)

btm.decile.plot = ggplot(BtM_recent, aes(x=date, y=BtM_Ret, fill = BtM_Rank))+geom_area()
btm.decile.plot

btm_WML = BtM_recent[BtM_Rank=='10',]$BtM_Ret - BtM_recent[BtM_Rank=='1',]$BtM_Ret
btm_WML = log(btm_WML+1)
btm_WML_port = data.frame(time, cumsum(btm_WML))
btm_wml.plot = ggplot()+geom_line(aes(x= btm_WML_port$time, y=btm_WML_port$cumsum.btm_WML.), colour='blue') 
btm_wml.plot

#------------------------------------------ Question 5 ---------------------------------------#
setkey(SMB,Year,Month)
setkey(FF_Factors,Year,Month)
SMB_mat <- matrix(nrow=5, ncol=1, dimnames = list(c("Excess Return","Standard Deviation",
                                                    "Sharpe Ratio","Skewness","Correlation"),c("SMB")))

SMB_mat[1,1] <- mean(SMB$SMB_Ret - FF_Factors$RF)*12
SMB_mat[2,1] <- sd(SMB$SMB_Ret)*sqrt(12)
SMB_mat[3,1] <- SMB_mat[1,1]/SMB_mat[2,1]
SMB_mat[4,1] <- skewness(SMB$SMB_Ret)
SMB_mat[5,1] <- cor(SMB$SMB_Ret, FF_Factors$SMB)

write.table(SMB_mat, file = "SMB_Result.csv", row.names=TRUE, sep=",")

setkey(HML,Year,Month)
HML_mat <- matrix(nrow=5, ncol=1, dimnames = list(c("Excess Return","Standard Deviation",
                                                    "Sharpe Ratio","Skewness","Correlation"),c("HML")))

HML_mat[1,1] <- mean(HML$HML_Ret - FF_Factors$RF)*12
HML_mat[2,1] <- sd(HML$HML_Ret)*sqrt(12)
HML_mat[3,1] <- HML_mat[1,1]/HML_mat[2,1]
HML_mat[4,1] <- skewness(HML$HML_Ret)
HML_mat[5,1] <- cor(HML$HML_Ret, FF_Factors$HML)

write.table(HML_mat, file = "HML_Result.csv", row.names=TRUE, sep=",")

#------------------------------------------ Question 6 ---------------------------------------#
size_WML_full = size_port[Size=='1',]$Size_Ret - size_port[Size=='10',]$Size_Ret
SMB_full = SMB$SMB_Ret
size_port[,date:=as.yearmon(as.character(Year*100+Month),"%Y%m")]
time=sort(unique(size_port$date))
size_WML_full = log(size_WML_full+1)
SMB_full = log(SMB_full+1)
compare1 = data.frame(time, cumsum(size_WML_full),cumsum(SMB_full))
colnames(compare1) = c("time", "size_long_short","SMB")

p1 = ggplot(compare1, aes(x=time)) + 
  geom_line(aes(y=size_long_short, color="cyan")) +
  geom_line(aes(y=SMB, color="skylightblue"))+ 
  scale_fill_discrete(name="colour",labels=c("Size", "SMB"))+
  ggtitle("Size Long Short Portfolio vs SMB")
p1

btm_WML_full = BtM_port[BtM_Rank=='10',]$BtM_Ret - BtM_port[BtM_Rank=='1',]$BtM_Ret
HML_full = HML$HML_Ret
BtM_port[,date:=as.yearmon(as.character(Year*100+Month),"%Y%m")]
btm_WML_full = log(btm_WML_full+1)
HML_full = log(HML_full+1)
compare2 = data.frame(time, cumsum(btm_WML_full),cumsum(HML_full))
colnames(compare2) = c("time", "BtM_long_short","HML")

p2 = ggplot(compare2, aes(x=time)) + 
  geom_line(aes(y=BtM_long_short, color="cyan")) +
  geom_line(aes(y=HML, color="skylightblue"))+ 
  ggtitle("BtM Long Short Portfolio vs HML")
p2