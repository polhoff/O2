
AddMonth <- function (indata) 
	{
	mnths <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

	indata$c_month <- with(indata, format(date1, '%m'))
	indata$n_month <- with(indata, as.numeric(as.character(format(date1, '%m'))))

	indata$mnths <- mnths[indata$n_month]
	indata$year <- with(indata, as.numeric(as.character(format(date1, '%Y'))))
	
	indata$mnthyear <- with(indata, paste(mnths, year))
	
	return(indata)
	}

#xbollocks <- AddMonth(Ks_indata); xbollocks

















MakeHourlyData <- function(indata)
	{
	AggregateToHourly(indata)
	}







MakeDailyData <- function(indata)
	{
	library (plyr)
	library (zoo)
	
	indata <- as.data.frame(indata)
	#indata <- indata[indata$daynight == 'day',]
	
	outdata <-  ddply (indata[c ( 'Temp', 'DO','date1')], .(date1), summarize, max.Temp = max(get ('Temp')), mean.Temp = mean ( get('Temp')), min.Temp = min (get('Temp')), median.Temp = median (get('Temp')),
	max.DO = max(get ('DO')), mean.DO = mean ( get('DO')), min.DO = min (get('DO')), median.DO = median (get('DO')))

	
	outdata$diffTemp <- outdata$mean.Temp - outdata$median.Temp
	outdata$diffDO <- outdata$mean.DO - outdata$median.DO
	
		
	return (outdata)
	}

#xss <- MakeDailyData(Ebble_CE1_2014_08_21_md)













































MakeDailyDataGen <- function (indata, c_vars, ClassVar = "date1") 
	{
    library(plyr)
    library(zoo)
    indata <- as.data.frame(indata)
    for (i in 1:length(c_vars))
		{
        data_sub <- indata[c(c_vars[i], ClassVar)]
        names(data_sub)[1] <- "Anal"
        outdata <- ddply(data_sub, .(get(ClassVar)), summarize, max = max(Anal, 
            na.rm = TRUE), mean = mean(Anal, na.rm = TRUE), min = min(Anal, 
            na.rm = TRUE), median = median(Anal, na.rm = TRUE))
		}

	names(outdata)[1] <- ClassVar
    return(outdata)
}















MakeDailyDataGenWhichMinMax <- function (indata, c_vars, ClassVar = "date1") 
	{
    library(plyr)
    library(zoo)
    indata <- as.data.frame(indata)
    for (i in 1:length(c_vars))
		{
        data_sub <- indata[c(c_vars[i], ClassVar)]
        names(data_sub)[1] <- "Anal"
        outdata <- ddply(data_sub, .(get(ClassVar)), summarize, which.max = which.max(Anal
			), mean = mean(Anal, na.rm = TRUE), which.min = which.min(Anal)
            , median = median(Anal))
		}

	names(outdata)[1] <- ClassVar
    return(outdata)
}









MakeDailyDataGenWithSum <- function (indata, c_vars, ClassVar = "date1") 
	{
    library(plyr)
    library(zoo)
    indata <- as.data.frame(indata)
    for (i in 1:length(c_vars))
		{
        data_sub <- indata[c(c_vars[i], ClassVar)]
        names(data_sub)[1] <- "Anal"
        outdata <- ddply(data_sub, .(get(ClassVar)), summarize, max = max(Anal, 
            na.rm = TRUE), mean = mean(Anal, na.rm = TRUE), min = min(Anal, 
            na.rm = TRUE), median = median(Anal, na.rm = TRUE), sum = sum(Anal, na.rm = TRUE))
		}

	names(outdata)[1] <- ClassVar
    return(outdata)
}

#MakeDailyDataGen(indata_sun, 'Cnow', ClassVar = 'TimeAfterSun')






