

ApplyDOFilter <- function(dataset = data_sub1, var1 = 'DO', var2 = 'DO.predict', n_tolup = 1.2, n_toldown = 0.99)

	{
	xvar <- dataset[var1]
	yvar <- dataset[var2]
	
	ComparePredict <- xvar/yvar
	
	
	ndx_DOtest <- (ComparePredict < n_toldown | ComparePredict > n_tolup) 
	
	return(ndx_DOtest)
	
	}











FindNearestDO <- function(dataset, DO.find, d_date = NA, c_night = NA)
	{
	
	#one must be not NA
	l_test <- xor(!is.na(d_date), !is.na(c_night))
	stopifnot(l_test)

	
	if(!is.na(c_night))
		{
		data_sub01 <- try(dataset[dataset$daynight1 == c_night,])
		}


	if(!is.na(d_date))
		{
		data_sub01 <- try(dataset[dataset$date1 == d_date,])
		}


	

	ndx_near <- which.min(abs(data_sub01$DO - DO.find))
	#ndx_near01 <- min(abs(data_sub$DO - DO.compare)) == abs(data_sub$DO - DO.compare)
	
	pick_record01 <- data_sub01[ndx_near, c('date', 'date1', 'DO', 'daynight1')]
	
	
	#order_near_a <- order((data_sub01$DO - DO.find)^2)
	#ndx_near_a <- order_near_a[order_near_a == max(order_near_a)]
	#pick_record02 <- data_sub01[ndx_near_a, c('date', 'date1', 'DO', 'daynight1')]
	
	
	pick_record01$DO.find <- DO.find
	pick_record01$TestDate <- d_date
	pick_record01$TestNight <- c_night
	
	
	return(pick_record01)
		
	}



#x222 <- FindNearestDO (indata, DO.find = 9.4, c_night = 'night0007')













MakeDOPredict <- function(dataset, n_spar = 0.25)
	{

	anal_data <- dataset[!is.na(dataset$DO),]
	anal_data$time <- as.numeric (anal_data$date)

	f_fit <- with (anal_data, smooth.spline(date, DO, spar = n_spar))
	f_fit.predict <- predict(f_fit, anal_data$time)


	names(f_fit.predict) <- c('time','DO.predict')
	f_fit.predict$date <- anal_data$date

	with(f_fit.predict,	stopifnot(time == as.numeric(date)))

	return(f_fit.predict)
	}





