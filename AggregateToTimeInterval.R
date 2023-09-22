
AggregateToHourly <- function(indata, c_var1 = 'Phot', c_var2 = 'Gas', c_var3 = 'Resp')
	{
	#indata <- Sem_AS2_2013_05_01_ModelDayNight
	library (parker)
	data (parker)
	data (meta_data_rovelli)
	
	
	#these give the same
	#format ( as.POSIXct ('2013-01-01 13:00:00'), '%Y-%m-%d %H')
	#format ( as.POSIXct ('2013-01-01 13:59:00'), '%Y-%m-%d %H')
	#therefore add on 30 mins to time stamp, so that 13:59 becomes 14:29 and therefore 2 p.m (rounded)
	#add on half an hour so that on the hour becomes the centre of the interval, rather than beginning or end
	indata$date <- indata$date + 30*60
	#print (indata$date)
	
	indata$date_hour <- format ( indata$date, '%Y-%m-%d %H')
		
	indata_agg <- aggregate ( indata[c (c_var1, c_var2, c_var3)], by = list (indata$date_hour), FUN = sum )
	indata_agg$date <- as.POSIXct ( paste ( indata_agg$Group.1, ':00:00', sep = ''))

	return (indata_agg)
	}







































AggregateToDaily <- function(indata, c_names = 'DO', c_fun)
	{
	#indata <- Sem_AS2_2013_05_01_ModelDayNight
	library (parker)
	data (parker)
	#data (meta_data_rovelli)
		
	#indata$date_day <- format ( indata$date, '%Y-%m-%d %H')
	y_ndx1 <- names(indata) %in% c_names
	y_ndx2 <- names(indata) %in% c('date1', c_names)
	data_sub <- indata[,y_ndx1]
		
	indata_agg <- aggregate ( indata[,y_ndx1], by = list (indata$date1), FUN = c_fun, na.rm = TRUE )
	#indata_agg <- aggregate ( data_sub, by = list (data_sub$date1), FUN = sum )
	#indata_agg <- aggregate ( data_sub, by = list (as.numeric(data_sub$date1)), FUN = sum )
	
	names(indata_agg) <- c('date1', c_names)
	return (indata_agg)
	}

#xxxx <- AggregateToDaily(indata, c_names = 'DO', c_fun = median)

