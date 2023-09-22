
MakeERKsTimeSeriesDataSet <- function ( indata, inparams, which_return = 1, n_quality = 1, l_plot = TRUE )
	{

	library (rovelli)
	stopifnot ( which_return %in%  c(1,2,3))

	#c_indata1 <- 'Ebble_CE1_2013_08_08'
	#c_inparams <- 'Ebble_CE1_2013_08_08_ER_Ks'
	#c_set <- 'for'

	data_new <- merge (indata, inparams, by = 'date1')
	data_new$date1 <- try (data_new$date1.x)

	indata$ER <- NA
	indata$Ks <- NA

	for ( i in 1:dim(inparams)[1] )
		{
		i_sub <- inparams[i,]
		indata$ER[indata$daynight1 == i_sub$daynight1] <- i_sub$ER
		indata$Ks[indata$daynight1 == i_sub$daynight1] <- i_sub$Ks
		}

	indata_sub <- indata[!is.na (indata$ER),]
	banana <- approxfun ( indata_sub$date, indata_sub$ER )
	banana_out <- banana (  indata$date )
	indata$ER <- banana_out


	indata_sub <- indata[!is.na (indata$Ks),]
	banana <- approxfun ( indata_sub$date, indata_sub$Ks )
	banana_out <- banana (  indata$date )
	indata$Ks <- banana_out

	first_notNA <- head (which (!is.na (banana_out)),1)
	last_notNA <- tail (which (!is.na (banana_out)),1)

	early_NA_ndx <- which (indata$date < indata$date[first_notNA])
	late_NA_ndx <- which (indata$date > indata$date[last_notNA])

	indata$ER[early_NA_ndx]  <- indata$ER[first_notNA]
	indata$Ks[early_NA_ndx]  <- indata$Ks[first_notNA]

	indata$ER[late_NA_ndx]  <- indata$ER[last_notNA]
	indata$Ks[late_NA_ndx]  <- indata$Ks[last_notNA]

	if(l_plot)
		{
		plot ( indata$date)
		plot ( indata$date, indata$DO )

		plot ( indata$date, indata$ER )
		plot ( indata$date, indata$Ks )
		}

	if ( !is.null(indata$qualityFilter01))
		{
		indata <- indata[indata$qualityFilter01 >= n_quality,]
		}



	x <- indata [ c ('date','ER','Ks')]
	
	if (which_return == 2)
		{
		x$date1 <- as.Date ( x$date )
		}
	
	if (which_return == 1)
		{
		x <- indata
		}
	
	print (names (x))
	return (x)
	}


#x1 <- MakeERKsTimeSeriesDataSet ()














































































MakeERKsTimeSeriesDaytime <- function ( indata, inparams, which_return = 1, n_quality = 1, l_plot = TRUE )
	{

	library (rovelli)
	stopifnot ( which_return %in%  c(1,2,3))

	#c_indata1 <- 'Ebble_CE1_2013_08_08'
	#c_inparams <- 'Ebble_CE1_2013_08_08_ER_Ks'
	#c_set <- 'for'
	
	if(names(inparams) == c('date1','mean.ER','mean.Ks'))
		{
		names(inparams) <- c('date1','ER','Ks')
		}
	
	indata <- indata[indata$daynight == 'day',]
	
	data_new <- merge (indata, inparams, by = 'date1')
	data_new$date1 <- try (data_new$date1.x)
	
	return(data_new)
	}
	



































































MakeERKsTimeSeriesNoInterp <- function ( indata, inparams, which_return = 1, n_quality = 1, l_plot = TRUE )
	{

	library (rovelli)
	stopifnot ( which_return %in%  c(1,2,3))

	#c_indata1 <- 'Ebble_CE1_2013_08_08'
	#c_inparams <- 'Ebble_CE1_2013_08_08_ER_Ks'
	#c_set <- 'for'

	data_new <- merge (indata, inparams, by = 'date1')
	data_new$date1 <- try (data_new$date1.x)

	indata$ER <- NA
	indata$Ks <- NA

	for ( i in 1:dim(inparams)[1] )
		{
		i_sub <- inparams[i,]
		indata$ER[indata$daynight1 == i_sub$daynight1] <- i_sub$ER
		indata$Ks[indata$daynight1 == i_sub$daynight1] <- i_sub$Ks
		}


	#fill in missing values for daytime using date, not daynight indicator
	for ( i in 1:dim(inparams)[1] )
		{
		i_sub <- inparams[i,]
		indata$ER[indata$date1 == i_sub$date1 & indata$daynight == 'day'] <- i_sub$ER
		indata$Ks[indata$date1 == i_sub$date1 & indata$daynight == 'day'] <- i_sub$Ks
		}

	first_notNA <- head (which (!is.na (indata$ER)),1)
	last_notNA <- tail (which (!is.na (indata$ER)),1)

	early_NA_ndx <- which (indata$date < indata$date[first_notNA])
	late_NA_ndx <- which (indata$date > indata$date[last_notNA])

	indata$ER[early_NA_ndx]  <- indata$ER[first_notNA]
	indata$Ks[early_NA_ndx]  <- indata$Ks[first_notNA]

	indata$ER[late_NA_ndx]  <- indata$ER[last_notNA]
	indata$Ks[late_NA_ndx]  <- indata$Ks[last_notNA]

	if(l_plot)
		{
		plot ( indata$date)
		plot ( indata$date, indata$DO )

		plot ( indata$date, indata$ER )
		plot ( indata$date, indata$Ks )
		}

	if ( !is.null(indata$qualityFilter01))
		{
		indata <- indata[indata$qualityFilter01 >= n_quality,]
		}



	x <- indata [ c ('date','ER','Ks')]
	
	if (which_return == 2)
		{
		x$date1 <- as.Date ( x$date )
		}
	
	if (which_return == 1)
		{
		x <- indata
		}
	
	print (names (x))
	return (x)
	}


#x1 <- MakeERKsTimeSeriesDataSet ()































































































MakeERKsTimeSeriesNoInterp_arc <- function ( indata, inparams, which_return = 1, n_quality = 1, l_plot = FALSE, l_dayafter = TRUE )
	{

	library (rovelli)
	stopifnot ( which_return %in%  c(1,2,3))

	#c_indata1 <- 'Ebble_CE1_2013_08_08'
	#c_inparams <- 'Ebble_CE1_2013_08_08_ER_Ks'
	#c_set <- 'for'

	stopifnot ( 'daynight1'  %in% names(indata))
	stopifnot ( 'daynight1'  %in% names(Ks_indata))


	data_new <- merge (indata, inparams, by = 'daynight1', all.x = TRUE)
	data_new$date1 <- try (data_new$date1.x)

	
	#this assumes that daytime ER is derived from the night before, not the night after
	data_new01 <- merge (indata, inparams, by = 'date1', all.x = TRUE)
	data_new01$date1 <- try (data_new01$date1.x)
	
	
	
	#this associates ER from one night with the day before
	if(!l_dayafter)
		{
		inparams$date1 <- inparams$date1 - 1
		
		data_new01 <- merge (indata, inparams, by = 'date1', all.x = TRUE)
		data_new01$date1 <- try (data_new01$date1.x)
		
		#reassign the date
		inparams$date1 <- inparams$date1 + 1
		}
	
	
	x_ndx <- is.na(data_new$ER)
	
	data_new$ER[x_ndx] <- data_new01$ER[x_ndx]
	data_new$Ks[x_ndx] <- data_new01$Ks[x_ndx]
	
	
	#data_new$daynight1 <- try (data_new$daynight1.x)
	
	indata <- data_new

	if(l_plot)
		{
		plot ( indata$date)
		plot ( indata$date, indata$DO )

		plot ( indata$date, indata$ER )
		plot ( indata$date, indata$Ks )
		}

	if ( !is.null(indata$qualityFilter01))
		{
		indata <- indata[indata$qualityFilter01 >= n_quality,]
		}

	x <- indata [ c ('date','ER','Ks', 'correlation','daynight1')]
	
	if (which_return == 2)
		{
		x$date1 <- as.Date ( x$date )
		}
	
	if (which_return == 1)
		{
		x <- indata
		}
	
	print (names (x))
	return (x)
	}


#x1 <- MakeERKsTimeSeriesNoInterp (indata,Ks_indata)


















































































MakeERKsTimeSeries <- function ( c_indata1, c_inparams, c_set = 'for', c_library = 'rovelli', which_return = 1, n_quality = 2 )
	{

	library (rovelli)
	stopifnot ( c_set %in%  c('for', 'mid', 'lag'))
	stopifnot ( which_return %in%  c(1,2,3))

	#c_indata1 <- 'Ebble_CE1_2013_08_08'
	#c_inparams <- 'Ebble_CE1_2013_08_08_ER_Ks'
	#c_set <- 'for'

	
	do.call ( library, list (c_library))

	do.call ( data, list (c_indata1))
	do.call ( data, list (c_inparams))

	indata <- get ( c_indata1 )
	inparams <- get ( paste (c_inparams, '_',  c_set, sep = ''))

	data_new <- merge (indata, inparams, by = 'date1')
	data_new$date1 <- try (data_new$date1.x)

	indata$ER <- NA
	indata$Ks <- NA

	for ( i in 1:dim(inparams)[1] )
		{
		i_sub <- inparams[i,]
		indata$ER[indata$daynight1 == i_sub$daynight1] <- i_sub$ER
		indata$Ks[indata$daynight1 == i_sub$daynight1] <- i_sub$Ks
		}

	indata_sub <- indata[!is.na (indata$ER),]
	banana <- approxfun ( indata_sub$date, indata_sub$ER )
	banana_out <- banana (  indata$date )
	indata$ER <- banana_out


	indata_sub <- indata[!is.na (indata$Ks),]
	banana <- approxfun ( indata_sub$date, indata_sub$Ks )
	banana_out <- banana (  indata$date )
	indata$Ks <- banana_out

	first_notNA <- head (which (!is.na (banana_out)),1)
	last_notNA <- tail (which (!is.na (banana_out)),1)

	early_NA_ndx <- which (indata$date < indata$date[first_notNA])
	late_NA_ndx <- which (indata$date > indata$date[last_notNA])

	indata$ER[early_NA_ndx]  <- indata$ER[first_notNA]
	indata$Ks[early_NA_ndx]  <- indata$Ks[first_notNA]

	indata$ER[late_NA_ndx]  <- indata$ER[last_notNA]
	indata$Ks[late_NA_ndx]  <- indata$Ks[last_notNA]

	plot ( indata$date)
	plot ( indata$date, indata$DO )

	plot ( indata$date, indata$ER )
	plot ( indata$date, indata$Ks )


	if ( !is.null(indata$qualityFilter))
	{
	indata <- indata[indata$qualityFilter > n_quality,]
	}



	x <- indata [ c ('date','ER','Ks')]
	
	if (which_return == 2)
		{
		x$date1 <- as.Date ( x$date )
		}
	
	if (which_return == 1)
		{
		x <- indata
		}
	
	print (names (x))
	return (x)
	}


#x1 <- MakeERKsTimeSeries ('Ebble_CE1_2013_08_08', 'Ebble_CE1_2013_08_08_ER_Ks_for')
#x1 <- MakeERKsTimeSeries ('Ebble_CE1_2013_04_25', 'Ebble_CE1_2013_04_25_ER_Ks')













































MakeERKsByDay <- function ( Ks_indata, n_tolerance = 0.2)
	{
	#n_tolerance <- 0.6
	#Ks_Ks_indata <- FitRegressionParametersDataSetStd02(Ks_indata)
	
	#Ks_indata01 <- Ks_indata
	#Ks_indata <- Ks_Ks_indata
	#Ks_indata <- KsER_0001
	
	#start_date <- head(Ks_indata_dates$date1,1)
	#end_date <- tail(Ks_indata_dates$date1,1:2)[1]
	
	start_date <- head(Ks_indata$date1,1)
	end_date <- tail(Ks_indata$date1,2)[1]
	
	d_dates <- data.frame('date1' = seq(start_date, end_date, by = 1))
	
	x_ndx01 <- !(abs(Ks_indata$correlation) > n_tolerance)
	x_ndx02 <- Ks_indata$Ks < 0 | is.na(Ks_indata$Ks)
	
	x_ndx <- x_ndx01 | x_ndx02
	
	
	Ks_indata[x_ndx, c('ER','Ks')] <- NA
	
	
	data_sub <- merge(d_dates, Ks_indata, all.x = TRUE)
	
	mean.ER <- rowMeans(embed(data_sub$ER,2),na.rm = TRUE)
	mean.Ks <- rowMeans(embed(data_sub$Ks,2),na.rm = TRUE)
		
	new_dates <- d_dates[-1,]
	
	outdata <- data.frame('date1' = new_dates, mean.ER, mean.Ks)
	return(outdata)
	}


#Ks_indata <- KsER_0001
#x222 <- MakeERKsByDay(KsER_0001)
#x222 <- MakeERKsByDay(KsER_0001, n_tolerance = 0.9)
























































































MakeERKsByDayByLabels <- function ( Ks_indata, n_tolerance = 0.6 )
	{
	#n_tolerance <- 0.6
	#Ks_Ks_indata <- FitRegressionParametersDataSetStd02(Ks_indata)
	
	#Ks_indata01 <- Ks_indata
	#Ks_indata <- Ks_Ks_indata
	#Ks_indata <- KsER_0001
	
	#start_date <- head(Ks_indata_dates$date1,1)
	#end_date <- tail(Ks_indata_dates$date1,1:2)[1]
	
	library(parker)
	
	all_labels <- Labels1()
	
	first_night <- head(Ks_indata$daynight1,1)
	last_night <- tail(Ks_indata$daynight1,1)
	
	all_nights <- paste('night', all_labels, sep = '')
	
	first_night_ndx <- (1:length(all_labels))[all_nights %in% first_night]
	last_night_ndx <- (1:length(all_labels))[all_nights %in% last_night]

	first_day_ndx <- first_night_ndx + 1
	last_day_ndx <- last_night_ndx + 1

	all_nights_indata <- paste('night', all_labels[first_night_ndx:last_night_ndx], sep = '')
	
	
	
	
	start_date <- head(Ks_indata$date1,1)
	end_date <- tail(Ks_indata$date1,1:2)[1]
	
	d_dates <- data.frame('date1' = seq(start_date, end_date, by = 1))
	
	x_ndx <- !(abs(Ks_indata$correlation) > n_tolerance)
	
	Ks_indata[x_ndx, c('ER','Ks')] <- NA
	
	data_sub <- merge(d_dates, Ks_indata, all.x = TRUE)
	
	mean.ER <- rowMeans(embed(data_sub$ER,2),na.rm = TRUE)
	mean.Ks <- rowMeans(embed(data_sub$Ks,2),na.rm = TRUE)
		
	new_dates <- d_dates[-1,]
	
	outdata <- data.frame('date1' = new_dates, mean.ER, mean.Ks)
	return(outdata)
	}


#Ks_indata <- KsER_0001
#x222 <- MakeERKsByDay(KsER_0001)
#x222 <- MakeERKsByDay(KsER_0001, n_tolerance = 0.9)


















































































MakeERKsDates <- function ( indata )
	{
	out_dates <- unique(indata[c('date1','daynight1')])
	return(out_dates)
	}

#indata_dates <- MakeERKsDates(indata)






































MakeERKsByDayNight <- function ( indata, n_tolerance = 0.6 )
	{
	#n_tolerance <- 0.6
	#Ks_indata <- FitRegressionParametersDataSetStd02(indata)
	
	#indata01 <- indata
	#indata <- Ks_indata
	
	data(parker)
	all_labels <- Labels1()
	n_len_labels <- length(all_labels)
	
	
	start_night <- head(indata$daynight1,1)
	end_night <- tail(indata$daynight1,1)


	all_nights <- paste('night', Labels1(),sep = '')
	ndx_first_night <- (1:n_len_labels)[all_nights %in% start_night]
	ndx_first_day <- ndx_first_night + 1


	ndx_last_night <- (1:n_len_labels)[all_nights %in% end_night]
	ndx_last_day <- ndx_last_night + 1
	
	nights_range <- paste('night', all_labels[ndx_first_night:ndx_last_night], sep = '')
	days_range <- paste('night', all_labels[ndx_first_day:ndx_last_day], sep = '')

	nights_range <- data.frame('daynight1' = nights_range)
	x_ndx <- !(abs(indata$correlation) > n_tolerance)
	
	indata[x_ndx,c('ER','Ks')] <- NA
	
	#data_sub <- merge(nights_range,KsER_0001, all.x = TRUE)
	data_sub <- merge(nights_range,indata, all.x = TRUE)
	
	mean.ER <- rowMeans(embed(data_sub$ER,2),na.rm = TRUE)
	mean.Ks <- rowMeans(embed(data_sub$Ks,2),na.rm = TRUE)
		
	new_dates <- d_dates[-1,]
	
	outdata <- data.frame('date1' = new_dates, mean.ER, mean.Ks)
	return(outdata)
	}


#Ks_indata <- KsER_0001
#x <- MakeERKsByDay(KsER_0001)
#x <- MakeERKsByDay(KsER_0001, n_tolerance = 0.9)

