

CalcDOEnvelopeByDaynight <- function(dataset, n_spar1 = 0.1)
	{
	
	library(O2)
	library(parker)
	
	#dataset <- indata; n_spar1 = 0.1
	dataset <- dataset[c('Unix.Timestamp','ID','date','DO','daynight1', 'daynight', 'DO.predict')]

	
	dataset$time <- as.numeric (dataset$date)	
	
	
	dataset_orig <- dataset
	
	
	
	#get nighttime minima
	pick_ndx <- left(dataset$daynight1,5) == 'night'
	dataset <- dataset[pick_ndx,]


	indata_daily_predict <- MakeDailyDataGen(dataset, 'DO.predict', ClassVar = 'daynight1')


	indata_daily_predict$Time.DOmin.predicted <- as.POSIXct(NA)



	for(i in 1:dim(indata_daily_predict)[1])
		{
		test_data <- indata_daily_predict[i,]
		data_sub <- dataset[dataset$daynight1 == test_data$daynight1,]
		
		ndx_Time.DOmin.predicted <- data_sub$DO.predict == test_data$min
		ndx_Time.DOmin.predicted[is.na(ndx_Time.DOmin.predicted)] <- FALSE
		
		Time.DOmin.predicted <- data_sub$date[ndx_Time.DOmin.predicted]
		indata_daily_predict[i, 'Time.DOmin.predicted'] <- Time.DOmin.predicted

		}


	anal_data <- indata_daily_predict
	anal_data$min[anal_data$min == Inf] <- NA
	anal_data$min[anal_data$min == -Inf] <- NA


	anal_data <- anal_data[!is.na(anal_data$min),]
	anal_data$time <- as.numeric (anal_data$Time.DOmin.predicted)


	f_fit1 <- with(anal_data, splinefun(time, min))
	f_fit1.out <- f_fit1(dataset_orig$Unix.Timestamp)
	
		
	f_fit1.out <- as.data.frame(f_fit1.out)
	names(f_fit1.out) <- 'DO.min'


	f_fit1.out$Unix.Timestamp <- dataset_orig$Unix.Timestamp
	f_fit1.out$date <- dataset_orig$date

	return(f_fit1.out)
	}



#x41 <- CalcDOMinimaEnvelope (indata, n_spar1 = 0.1)
#x42 <- CalcDOMinimaEnvelope (indata, n_spar1 = 0.4)
#with(x41, plot(DO.min));with(x42, points(DO.min))



































CalcDOEnvelopeByDate <- function(dataset, n_spar1 = 0.1, l_max = FALSE)
	{
	
	library(O2)
	library(parker)
	
	#dataset <- indata; n_spar1 = 0.1
	dataset <- dataset[c('Unix.Timestamp','ID','date','date1','DO','daynight1', 'daynight')]

	
	dataset$time <- as.numeric (dataset$date)	
	
	
	dataset_orig <- dataset
	
	
	
	df_fitted <- MakeDOPredict (dataset_orig, n_spar = n_spar1)
	df_fitted <- as.data.frame(df_fitted)
	
	dataset <- merge(dataset_orig, df_fitted, all.x = TRUE, all.y = TRUE)
	
	
	indata_daily_predict <- MakeDailyDataGen(dataset, 'DO.predict', ClassVar = 'date1')


	indata_daily_predict$Time.DOmin.predicted <- as.POSIXct(NA)
	indata_daily_predict$Time.DOmax.predicted <- as.POSIXct(NA)




	for(i in 1:dim(indata_daily_predict)[1])
		{
		test_data <- indata_daily_predict[i,]
		data_sub <- dataset[dataset$date1 == test_data$date1,]
		
		ndx_Time.DOmin.predicted <- data_sub$DO.predict == test_data$min
		ndx_Time.DOmin.predicted[is.na(ndx_Time.DOmin.predicted)] <- FALSE
		
		
		ndx_Time.DOmax.predicted <- data_sub$DO.predict == test_data$max
		ndx_Time.DOmax.predicted[is.na(ndx_Time.DOmax.predicted)] <- FALSE
		
		
		Time.DOmin.predicted <- data_sub$date[ndx_Time.DOmin.predicted]
		indata_daily_predict[i, 'Time.DOmin.predicted'] <- Time.DOmin.predicted

		Time.DOmax.predicted <- data_sub$date[ndx_Time.DOmax.predicted]
		indata_daily_predict[i, 'Time.DOmax.predicted'] <- Time.DOmax.predicted

		}


	anal_data <- indata_daily_predict
	anal_data$min[anal_data$min == Inf] <- NA
	anal_data$min[anal_data$min == -Inf] <- NA


	anal_data <- anal_data[!is.na(anal_data$min),]
	anal_data$time <- as.numeric (anal_data$Time.DOmin.predicted)


	f_fit1 <- with(anal_data, splinefun(time, min))
	f_fit1.out <- f_fit1(dataset_orig$Unix.Timestamp)
	
		
	f_fit1.out <- as.data.frame(f_fit1.out)
	names(f_fit1.out) <- 'DO.min'

	
	f_fit1.out$Unix.Timestamp <- dataset_orig$Unix.Timestamp
	f_fit1.out$date <- dataset_orig$date
	f_fit1.out$ID <- dataset_orig$ID



	if(l_max)

		{

		anal_data <- indata_daily_predict
		anal_data$max[anal_data$max == Inf] <- NA
		anal_data$max[anal_data$max == -Inf] <- NA


		anal_data <- anal_data[!is.na(anal_data$max),]
		anal_data$time <- as.numeric (anal_data$Time.DOmax.predicted)


		f_fit1 <- with(anal_data, splinefun(time, max))
		f_fit1.out <- f_fit1(dataset_orig$Unix.Timestamp)
		
			
		f_fit1.out <- as.data.frame(f_fit1.out)
		names(f_fit1.out) <- 'DO.max'

		
		f_fit1.out$Unix.Timestamp <- dataset_orig$Unix.Timestamp
		f_fit1.out$date <- dataset_orig$date
		f_fit1.out$ID <- dataset_orig$ID

		}




	return(f_fit1.out)
	}



#x41 <- CalcDOMinimaEnvelope (indata, n_spar1 = 0.1)
#x42 <- CalcDOMinimaEnvelope (indata, n_spar1 = 0.4)
#with(x41, plot(DO.min));with(x42, points(DO.min))
