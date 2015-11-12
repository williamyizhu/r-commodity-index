# TODO: Add comment
# 
# Author: William Yizhu
###############################################################################

rm(list=ls(all=TRUE)) 
options(width = 438L)

MonthSymbol = c("F","G","H","J","K","M","N","Q","U","V","X","Z") 

for (i in dir(paste(getwd(), "/CFE_VX/", sep=""))) {	
#	contract expiry info	
	Expiry = unlist(strsplit(i, "_"))[2]
	ExpMonth = which(MonthSymbol == substr(Expiry,1,1))
	ExpYear = as.numeric(substr(Expiry,2,3))
	print(paste(i, " Year=", ExpYear, " Month=", ExpMonth, sep=""))
#	read data from csv files	
	fpath = paste(getwd(), "/CFE_VX/", i, sep="")
	if (file.exists(fpath)) {
		dTemp = read.csv(fpath, header=TRUE, sep=",")		
		tt = as.character(as.Date(dTemp[,1], "%m/%d/%Y"))				
		data = data.frame(cbind(tt, matrix(as.numeric(unlist(strsplit(tt,'-'))),ncol=3,byrow=TRUE), dTemp$Settle))
		colnames(data) = c("TradeDate", "Year", "Month", "Day", "Settlement")
#		print(tail(data))
	} else {
		print(paste("Error", i))
	}
}

#--------------------- VX index from Bloomberg ---------------------
fpath = paste(getwd(), "/bloomberg_index_data/SPVX.csv", sep="")
if (file.exists(fpath)) {
	dTemp = read.csv(fpath, header=TRUE, sep=",")	
	tt = as.character(as.Date(dTemp[,1]))	
	IndexVX = cbind(tt, dTemp[,c(2,3)], matrix(as.numeric(unlist(strsplit(tt,'-'))),ncol=3,byrow=TRUE))
	colnames(IndexVX) = c(colnames(dTemp),  "Year", "Month", "Day")							
} else {
	print(paste("Error", fpath))
}


#---- vx expiration date from www.cboe.com and occ ----
VxExirationDate = 
c(
"2008-01-16", "2008-02-19", "2008-03-19", "2008-04-16", "2008-05-21", "2008-06-18", "2008-07-16", "2008-08-20", "2008-09-17", "2008-10-22", "2008-11-19", "2008-12-17", 
"2009-01-21", "2009-02-18", "2009-03-18", "2009-04-15", "2009-05-20", "2009-06-17", "2009-07-22", "2009-08-19", "2009-09-16", "2009-10-21", "2009-11-18", "2009-12-16", 
"2010-01-20", "2010-02-17", "2010-03-17", "2010-04-21", "2010-05-19", "2010-06-16", "2010-07-21", "2010-08-18", "2010-09-15", "2010-10-20", "2010-11-17", "2010-12-22", 
"2011-01-19", "2011-02-16", "2011-03-16", "2011-04-20", "2011-05-18", "2011-06-15", "2011-07-20", "2011-08-17", "2011-09-21", "2011-10-19", "2011-11-16", "2011-12-21", 
"2012-01-18", "2012-02-15", "2012-03-21", "2012-04-18", "2012-05-16", "2012-06-20", "2012-07-18", "2012-08-22", "2012-09-19", "2012-10-17", "2012-11-21", "2012-12-19", 
"2013-01-16", "2013-02-13", "2013-03-20", "2013-04-17", "2013-05-22", "2013-06-19", "2013-07-17", "2013-08-21", "2013-09-18", "2013-10-16", "2013-11-20", "2013-12-18", 
"2014-01-22", "2014-02-19", "2014-03-18", "2014-04-16", "2014-05-21", "2014-06-18", "2014-07-16", "2014-08-20", "2014-09-17", "2014-10-22", "2014-11-19", "2014-12-17", 
"2015-01-21", "2015-02-18", "2015-03-18", "2015-04-15", "2015-05-20", "2015-06-17", "2015-07-22", "2015-08-19", "2015-09-16", "2015-10-21", "2015-11-18", "2015-12-16")

IndexVX$VxExirationDate = IndexVX$Date %in% VxExirationDate

#roll dates and schedule
ss = length(VxExirationDate)
RollSchedule = cbind(VxExirationDate[1:ss-1], as.character(as.Date(VxExirationDate[2:ss])-1))


IndexVX$DayOfRoll = 0
for (i in 1:dim(RollSchedule)[1]) {

	ind = ((as.Date(RollSchedule[i,1])<=as.Date(IndexVX$Date)) & (as.Date(IndexVX$Date)<=as.Date(RollSchedule[i,2]))) 

	if (sum(ind) > 0) {
		IndexVX[ind,]$DayOfRoll = seq(1, sum(ind))
	}
	
}

#expiration date which is not the third wednesday of the month
#c(
#"2008-02-19", "2008-10-22",
#"2009-07-22",
#"2010-12-22",
#"2012-08-22",
#"2013-02-13",
#"2013-05-22",
#"2014-01-22", "2014-03-18", "2014-10-22",
#"2015-07-22")

##---- function for finding the first day of month ----
#FirstDayOfMonth = function(dates, day="Mon", abbreviate=TRUE) {
## 	first 7 days of month
#	s = lapply(as.Date(format(dates,"%Y-%m-01")), seq, by="day", length.out=7)
## 	first day of month
#	d = lapply(s, function(d) d[weekdays(d,abbreviate)==day])
## 	unlist converts to atomic, so use do.call(c,...) instead
#	do.call(c, d)
#}
#
#IndexVX$DayOfWeek = weekdays(as.Date(IndexVX[,1]))
#
#ym = unique(IndexVX[,c("Year","Month")])
##first day of the month
#fdm = seq(as.Date(format(as.Date(IndexVX[1,]$Date),"%Y-%m-01")), by="month", length.out=96)
##third wednesday of the month
#ThirdWed = FirstDayOfMonth(fdm, day="Wed") + 14
##indicator for the third wed
#IndexVX$IsThirdWed = as.Date(IndexVX$Date) %in% ThirdWed

##---- find the third wednesday from the bloomberg data ----
#mg = vector()
#for (i in 1:dim(ym)[1]) {
#	ind = (IndexVX$Year == ym[i,]$Year) & (IndexVX$Month == ym[i,]$Month)	
#	ind2 = (IndexVX[ind,]$DayOfWeek == "Wednesday")
#	WedVec = as.character(IndexVX[ind,][ind2,]$Date)
##	third wednesday
#	if (length(WedVec) >= 3) {			
##		print(WedVec)
#		mg = c(mg, WedVec[3])
#	}
#}
#
#IndexVX$IsThirdWed2 = as.Date(IndexVX$Date) %in% as.Date(mg)
#
##verify with previous method
#tt = (IndexVX$IsThirdWed != IndexVX$IsThirdWed2)
#IndexVX[tt,]




