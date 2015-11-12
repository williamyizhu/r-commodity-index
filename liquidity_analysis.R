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

MonthBeforeExpStart  = 3
MonthBeforeExpLength = 1

DataYear = seq(15, 15)
DataMonth = seq(1, 12)
#DataMonth = c(1, 5, 9)

FutureContract = merge(DataMonth, DataYear)
colnames(FutureContract) = c("Month", "Year")

#number of month before expiration start
MonthStart = FutureContract$Month - MonthBeforeExpStart
YearStart  = FutureContract$Year
ind = (MonthStart < 1)
MonthStart[ind] = MonthStart[ind] + 12
YearStart[ind]  = YearStart[ind] - 1

#contract symbol in wind terminal
FutureContract$Contract = paste(Underlying, formatC(FutureContract$Year,width=2,flag='0'), formatC(FutureContract$Month,width=2,flag='0'), ".", Exchange, sep="")

#analysis date period
FutureContract$DateStart = as.Date(paste("20", formatC(YearStart,width=2,flag='0'), "-", MonthStart, "-1", sep=""))
FutureContract$DateEnd = as.Date("2000-01-01")
for (i in 1:dim(FutureContract)[1]) {
	FutureContract[i,]$DateEnd = as.Date(seq(FutureContract[i,]$DateStart, length=2, by=paste(MonthBeforeExpLength,"month",sep=" "))[2] - 1)
}

VolumeVector = as.numeric()
HighVector = as.numeric()
LowVector = as.numeric()

for (i in 1:dim(FutureContract)[1]) {
	print(paste(FutureContract[i,]$Contract, FutureContract[i,]$DateStart, FutureContract[i,]$DateEnd))
	w_wsd_data = w.wsd(FutureContract[i,]$Contract, "open,high,low,close,pre_settle,settle,volume,oi", FutureContract[i,]$DateStart, FutureContract[i,]$DateEnd, paste("TradingCalendar=",TradingCalendar,sep=""))			
	is_na_vec = apply(w_wsd_data$Data, 1, function(x){any(is.na(x)==TRUE)})
	
	VolumeVector = c(VolumeVector, w_wsd_data$Data[!is_na_vec,]$VOLUME)
	HighVector = c(HighVector, w_wsd_data$Data[!is_na_vec,]$HIGH / w_wsd_data$Data[!is_na_vec,]$PRE_SETTLE - 1)
	LowVector  = c(LowVector,  w_wsd_data$Data[!is_na_vec,]$LOW  / w_wsd_data$Data[!is_na_vec,]$PRE_SETTLE - 1)
}

#HighLowDensity = density(c(HighVector, LowVector))
#plot(HighLowDensity$x, HighLowDensity$y, col="blue", type="l", xlab="Daily Return", ylab="Density")

VolumeDensity = density(VolumeVector)
plot(VolumeDensity$x, VolumeDensity$y, col="blue", type="l", xlab="Daily Return", ylab="Density")

print(range(VolumeVector))
#
##	---------------------------------------------------- plot 3 ----------------------------------------------------	
##	density plot for "pricing_volatility", "pricing_volatility_adj", "realized_volatility", only "is_position_expired=TRUE" runs
#pricing_volatility_density = density(run_result_expired$pricing_volatility)
#pricing_volatility_adj_density = density(run_result_expired$pricing_volatility_adj)
#realized_volatility_density = density(run_result_expired$realized_volatility)	
#
#xlim = c(min(pricing_volatility_density$x, pricing_volatility_adj_density$x, realized_volatility_density$x), max(pricing_volatility_density$x, pricing_volatility_adj_density$x, realized_volatility_density$x))
#ylim = c(0, max(pricing_volatility_density$y, pricing_volatility_adj_density$y, realized_volatility_density$y))	
#plot(pricing_volatility_density$x, pricing_volatility_density$y, xlim=xlim, ylim=ylim, col="blue", type="l", xlab="Volatility", ylab="Density")
#lines(pricing_volatility_adj_density$x, pricing_volatility_adj_density$y, xlim=xlim, ylim=ylim, col="red")
#lines(realized_volatility_density$x, realized_volatility_density$y, xlim=xlim, ylim=ylim, col="black")
#title(paste(underlying, delivery_month, " | X=[", paste(strike_percentage * 100, "%", sep="", collapse=","), "] | Days=", days, " | Runs=", runs, " | n=", dim(run_result_expired)[1], " | RFI=", risk_free_interest, " | mode=", hedging_volatility_mode, sep=""))
#legend("topleft", legend=c("Pricing Volatility","Pricing Volatility Adjusted","Realized Volatility"), text.col=c("blue","red","black"), pch=c(15,15,15), col=c("blue","red","black"))
#legend("topright", legend=unique(run_result_expired$period_level), pch=seq(1:length(period_level_vec)))	


#w_wsd_data = w.wsd(i, "open,high,low,close,pre_settle,settle,volume,oi", start, end, paste("TradingCalendar=",TradingCalendar,sep=""))			


