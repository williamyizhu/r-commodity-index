# TODO: Add comment

##############################################################################################################
rm(list=ls(all=TRUE)) 
options(width = 438L)

library(WindR)
w.start()
#w.menu()
#w.isconnected()

#---------------- parameters ----------------
#can only roll within next 12 months, i.e., RollMonth <= FrontMonth < BackMonth
Exchange = "SHF"
Underlying = "CU"
TradingCalendar="SHFE"
RollMonth = c(4,8,12)
FrontMonth = c(5,9,13)
BackMonth = c(9,13,17)

Exchange = "SHF"
Underlying = "CU"
TradingCalendar="SHFE"
RollMonth = seq(1:12)
FrontMonth = RollMonth + 1
BackMonth = RollMonth + 2

#historical price range
DataStart = "2011-01-01"
DataEnd = "2015-08-30"
#roll days, i.e., trading day of the month
TradingDayOfMonthRollStart = 6
TradingDayOfMonthRollEnd = 10
#index and ETN initial value
IndexER0 = 1000
IndexTR0 = 1000
ETNIV0 = 100
#interest rate
InterestRate = 0.0035
InterestTerm = 1
#ETN products
LevInd = c("LEV1X", "LEV2X", "INV1X")
Leverage = c(1, 2, -1)
YearlyFee = c(0.0055, 0.0065, 0.0075)

#----------------  ----------------
#trading day of an exchange
TradingDate = w.tdays(DataStart, DataEnd, paste("TradingCalendar=",TradingCalendar,sep="")) 
if (Underlying == "CU") {
	IndexBenchmark = c("CUFI.WI", "NH0012.NHF")
} else if (Underlying == "AL") {	
	IndexBenchmark = c("ALFI.WI", "NH0013.NHF")
} else {	
}

#---------------- trading month and number of tradings in a month ----------------- 
FutureData = data.frame(cbind(as.character(TradingDate$Data[,1]), matrix(as.numeric(unlist(strsplit(as.character(TradingDate$Data[,1]),'-'))), ncol=3, byrow=TRUE)))
colnames(FutureData) = c("TradingDate", "Year", "Month", "Day")
FutureData$Year2 = as.numeric(substr(FutureData$Year,3,4))
FutureData$IsRollMonth = FutureData$Month %in% RollMonth

#> head(TradingMonth)
#Year Month IsRollMonth
#1   2014     5       FALSE
#21  2014     6       FALSE
#41  2014     7       FALSE
#64  2014     8        TRUE
#85  2014     9       FALSE
#106 2014    10       FALSE
TradingMonth = unique(FutureData[,c("Year","Month")])
TradingMonth$IsRollMonth = TradingMonth$Month %in% RollMonth

#trading day sequence in a year-month
FutureData$TradingDayOfMonth = 0 
for(i in 1:dim(TradingMonth)[1]) {
	ind = (FutureData$Year == TradingMonth[i,]$Year) & (FutureData$Month == TradingMonth[i,]$Month)
	if (sum(ind) > 0) {
		FutureData[ind,]$TradingDayOfMonth = seq(1,sum(ind))
	}
}

#----------------- roll percentage weight change schedule ----------------- 
#> head(RollWeightSchedule)
#Month TradingDayOfMonth PW1
#1     4                 6 0.8
#2     8                 6 0.8
#3    12                 6 0.8
#4     4                 7 0.6
#5     8                 7 0.6
#6    12                 7 0.6
nRollDays = TradingDayOfMonthRollEnd - TradingDayOfMonthRollStart + 1
RollWeight = data.frame(cbind(seq(TradingDayOfMonthRollStart,TradingDayOfMonthRollEnd), 1 - seq(1:nRollDays) / nRollDays))
RollWeightSchedule = merge(RollMonth, RollWeight)
colnames(RollWeightSchedule) = c("Month", "TradingDayOfMonth", "PW1")

#percentage weight schedule
FutureData$PW1 = 1
for (i in 1:dim(RollWeightSchedule)[1]){
	ind = (FutureData$Month == RollWeightSchedule[i,]$Month) & (FutureData$TradingDayOfMonth == RollWeightSchedule[i,]$TradingDayOfMonth)
	if (sum(ind) > 0) {
		FutureData[ind,]$PW1 = RollWeightSchedule[i,]$PW1
	}	
}

#----------------- roll contract schedule ----------------- 
#> RollContractSchedule
#Month FrontMonth BackMonth IsRollMonth
#1      1          5         9       FALSE
#2      2          5         9       FALSE
#3      3          5         9       FALSE
#4      4          5         9        TRUE
#5      5          9        13       FALSE
#6      6          9        13       FALSE
#7      7          9        13       FALSE
#8      8          9        13        TRUE
#9      9         13        17       FALSE
#10    10         13        17       FALSE
#11    11         13        17       FALSE
#12    12         13        17        TRUE
RollContractSchedule = data.frame(seq(1,12), rep(0,12), rep(0,12))
colnames(RollContractSchedule) = c("Month","FrontMonth","BackMonth")
RollContractSchedule$IsRollMonth = RollContractSchedule$Month %in% RollMonth
#"FrontMonth" and "BackMonth" are defined in parameters
t1 = c(0, RollMonth)
for (i in 1:length(RollMonth)) {	
	ind = (t1[i] < RollContractSchedule$Month) & (RollContractSchedule$Month <= t1[i+1])
	if (sum(ind) > 0) {
		RollContractSchedule[ind,]$FrontMonth = FrontMonth[i]
		RollContractSchedule[ind,]$BackMonth  = BackMonth[i]		
	}
}

#----------------- decide the front month and back month contracts -----------------
FutureData$FrontMonth = ""
FutureData$BackMonth  = ""
for (i in 1:dim(RollContractSchedule)[1]) {
#	front month
	ind = (FutureData$Month == RollContractSchedule[i,]$Month)
	if(sum(ind) > 0) {
		t2 = floor(RollContractSchedule[i,]$FrontMonth / 13)
		FutureData[ind,]$FrontMonth =  paste(Underlying, FutureData[ind,]$Year2+t2,  formatC(RollContractSchedule[i,]$FrontMonth-12*t2, width=2, flag='0'), ".", Exchange, sep="")	
	}
#	back month
	ind2 = ind & (FutureData$PW1 != 1)
	if(sum(ind2) > 0) {		
		t2 = floor(RollContractSchedule[i,]$BackMonth / 13)
		FutureData[ind2,]$BackMonth =  paste(Underlying, FutureData[ind2,]$Year2+t2, formatC(RollContractSchedule[i,]$BackMonth-12*t2,  width=2, flag='0'), ".", Exchange, sep="")		
	}	
#	days after TradingDayOfMonthRollEnd of roll month
	ind3 = ind & FutureData$IsRollMonth & (FutureData$TradingDayOfMonth > TradingDayOfMonthRollEnd)
	if(sum(ind3) > 0) {
		t2 = floor(RollContractSchedule[i,]$BackMonth / 13)
		FutureData[ind3,]$FrontMonth = paste(Underlying, FutureData[ind3,]$Year2+t2, formatC(RollContractSchedule[i,]$BackMonth-12*t2,  width=2, flag='0'), ".", Exchange, sep="")	
	}
}

#----------------- get the price for both front month and back month -----------------
FutureData$FrontMonthPrice = 0
FutureData$BackMonthPrice  = 0
FutureData$FrontMonthReturnHigh = 0
FutureData$BackMonthReturnHigh  = 0
FutureData$FrontMonthReturnLow = 0
FutureData$BackMonthReturnLow  = 0
fmc = unique(FutureData$FrontMonth)
bmc = unique(FutureData$BackMonth)
#front month price
for (i in fmc) {
	if (i != "") {
		ind = (FutureData$FrontMonth==i)
		if (sum(ind > 0)) {
			start = as.character(head(FutureData[ind,]$TradingDate,n=1))
			end   = as.character(tail(FutureData[ind,]$TradingDate,n=1))		
			w_wsd_data = w.wsd(i, "open,high,low,close,pre_settle,settle,volume,oi", start, end, paste("TradingCalendar=",TradingCalendar,sep=""))			
			FutureData[ind,]$FrontMonthPrice = w_wsd_data$Data$SETTLE
			print(paste("Front Month",i,start,end))
		}
	}
}
#back month price
for (i in bmc) {
	if (i != "") {
		ind = (FutureData$BackMonth==i)
		if (sum(ind > 0)) {
			start = as.character(head(FutureData[ind,]$TradingDate,n=1))
			end   = as.character(tail(FutureData[ind,]$TradingDate,n=1))			
			w_wsd_data = w.wsd(i, "open,high,low,close,pre_settle,settle,volume,oi", start, end, paste("TradingCalendar=",TradingCalendar,sep=""))			
			FutureData[ind,]$BackMonthPrice = w_wsd_data$Data$SETTLE
			print(paste("Back Month",i,start,end))
		}
	}
}

#----------------- get index benchmark for the underlying -----------------
#DataStart and DataEnd maybe different from start and end
start = as.character(head(FutureData$TradingDate,n=1))
end = as.character(tail(FutureData$TradingDate,n=1))
for (i in IndexBenchmark) {
	w_wsd_data = w.wsd(i, "open,high,low,close,settle,volume,oi", start, end)			
	FutureData[,i] = w_wsd_data$Data$CLOSE
}

#----------------- index calculation ----------------- 
nData = dim(FutureData)[1]

#--- contract daily return ---
#percentage weight of previous trading day, see spreadsheet
pwt = FutureData[1:nData-1,]$PW1
pwt[FutureData[1:nData-1,]$PW1==0] = 1
#numerator, current trading day price, previous trading day weight
FutureData$DifNum = c(1, pwt * FutureData[2:nData,]$FrontMonthPrice + (1 - pwt) * FutureData[2:nData,]$BackMonthPrice)
#denominator, previous trading day weighted price
FutureData$DifDen = c(1, FutureData[1:nData-1,]$PW1 * FutureData[1:nData-1,]$FrontMonthPrice + (1 - FutureData[1:nData-1,]$PW1) * FutureData[1:nData-1,]$BackMonthPrice)
FutureData$CDR = FutureData$DifNum / FutureData$DifDen - 1

#--- Treasury Bill Return, use previous trading day interest rate ---
FutureData$InterestRate = InterestRate
FutureData$InterestDays = c(0, as.numeric(as.Date(FutureData[2:nData,]$TradingDate)-as.Date(FutureData[1:nData-1,]$TradingDate)))
FutureData$TBR = c(0, (1 / (1 - FutureData[1:nData-1,]$InterestRate*InterestTerm/360)) ^ (FutureData[2:nData,]$InterestDays/InterestTerm) - 1)

#index excess return
FutureData$IndexER = IndexER0 * cumprod(1 + FutureData$CDR)

#index total return
FutureData$IndexTR = IndexTR0 * cumprod(1 + FutureData$CDR + FutureData$TBR)

#----------------- ETN ----------------- 
CONST1 = paste("CONST1", LevInd, sep="_")
CONST2 = paste("CONST2", LevInd, sep="_")
IV = paste("IV", LevInd, sep="_")
FutureData[,c(CONST1,CONST2,IV)] = rep(0,3*length(LevInd))
FutureData[1,IV] = rep(ETNIV0, length(LevInd))
#--- pricing model, depends on IV(T-1) and InterestRate(T-1) ----
for (i in 2:nData) {	
	FutureData[i,CONST1] = FutureData[i-1,IV] * (1-YearlyFee/365)^FutureData[i,]$InterestDays * Leverage / FutureData[i,]$DifDen	
	ir = (1 / (1 - FutureData[i-1,]$InterestRate*InterestTerm/360)) ^ (FutureData[i,]$InterestDays/InterestTerm)	
	FutureData[i,CONST2] = FutureData[i-1,IV] * (1-YearlyFee/365)^FutureData[i,]$InterestDays * (ir - Leverage)	
	FutureData[i,IV] = FutureData[i,CONST1] * FutureData[i,]$DifNum + FutureData[i,CONST2]
}

#----------------- plot ----------------- 
#cumulative return as reference to FutureData[1,i]
IndexCol = c("IndexER", "IndexTR", IndexBenchmark, IV)
mm = data.frame(as.Date(FutureData$TradingDate))
for (i in IndexCol) {
	mm[,i] = FutureData[,i] / FutureData[1,i]
}
colnames(mm) = c("TradingDate", IndexCol)

#matrix plot
matplot(mm[,IndexCol], type="l", xaxt="n", ylab="Cumulative Return", col=seq(1:length(IndexCol)), main=paste("BOCI Commodity Index", Underlying))
tt = paste(IndexCol, c(rep("",length(IndexCol)-length(YearlyFee)), paste(" Annual Fee = ",YearlyFee*100,"%",sep="")), sep="")
legend("bottomleft",legend=tt, lty=1, col=seq(1:length(IndexCol)), cex=0.7)
ind = c(1, seq(1:dim(TradingMonth)[1]) * floor(dim(mm)[1]/dim(TradingMonth)[1]))
axis(1, at=ind, labels=mm[ind,1])
grid()
#

print(head(FutureData))
print(tail(FutureData))

