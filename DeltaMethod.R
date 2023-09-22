
ApproxDeltaMethod <- function (photoperiod, phi)
	{
	eta <- (photoperiod/14)^0.75
	K_s <- 7.5 * ( (5.3*eta - phi)/(eta*phi))^0.85
	return(K_s)
	}

#x1 <- ApproxDeltaMethod (12, 3)






DeltaMethodCurve <- function (kA, photoperiod, phi)
	{
	#Calculating Stream Reaeration Coefficients from Oxygen Profiles Graham B. McBride
	dayperiod = 24
	
	n_theta <- atan (pi / ( kA * photoperiod))
	
	n_gamma <- sin(n_theta)*(1+exp(-kA*(dayperiod-photoperiod))) / (1-exp(-kA*dayperiod))
	
	result <- pi*cos(pi*(1/2+phi/photoperiod)-n_theta)-(photoperiod*kA*n_gamma*exp(-kA*(phi+photoperiod/2)))
	
	return (result)
	}

















DeltaMethodTimeCurve <- function (kA, photoperiod, t)
	{
	#Calculating Stream Reaeration Coefficients from Oxygen Profiles Graham B. McBride
	dayperiod = 24
	
	n_theta <- atan (pi / ( kA * photoperiod))
	
	n_gamma <- sin(n_theta)*(1+exp(-kA*(dayperiod-photoperiod))) / (1-exp(-kA*dayperiod))
	
	result <- pi*cos(pi*t/photoperiod - n_theta)-(photoperiod*kA*n_gamma * exp(-kA*t))
	
	return (result)
	}





DeltaMethodQuotient <- function (kA, photoperiod, phi)
	{
	#Calculating Stream Reaeration Coefficients from Oxygen Profiles Graham B. McBride
	dayperiod = 24
	
	n_theta <- atan (pi / ( kA * photoperiod))
	
	n_gamma <- sin(n_theta)*(1+exp(-kA*(dayperiod-photoperiod))) / (1-exp(-kA*dayperiod))
	
	
	n_delta <- - ( sin ( (pi * tmax)/photoperiod - n_theta) + n_gamma * exp(-kA*tmax))
				+ ( sin ( (pi * tmin)/photoperiod - n_theta) + n_gamma * exp(-kA*tmin))
	
	
	result <- pi*cos(pi*(1/2+phi/photoperiod)-n_theta)-(photoperiod*kA*n_gamma*exp(-kA*(phi+photoperiod/2)))
	
	return (result)
	}












































FitSineCurveWrapper <- function(c_indata, c_library, l_testset = FALSE)
	{
	addedMvAvg <- MovingAverageAddon (c_indata, c_library, l_testset)
	DeficitsAtMinima <- FitSineCurveBatch (addedMvAvg)
	DeficitsAtMinima$phi <- DeficitsAtMinima$TimeMinDeficit - DeficitsAtMinima$SolarNoon
	units (DeficitsAtMinima$phi) <- 'hours'
	
	DeficitsAtMinima$K_s <- as.numeric(NA)


	data (DeltaMethodData)

	
	for (i in 1:dim(DeficitsAtMinima)[1])
		{
		photo <- as.numeric(DeficitsAtMinima$photoperiod[i])
		phi <- DeficitsAtMinima$phi[i]
		
		x_5 <- RoundTo5minutes (photo)
		DeltaMethodData_sub <- DeltaMethodData[DeltaMethodData$photoperiod == x_5,]
		DeltaMethodData_sub <- DeltaMethodData[round(DeltaMethodData$photoperiod,3) == round(x_5,3),]


		input_Ks <- with (DeltaMethodData_sub, approxfun (f_values, k_values))
		#ndx <- which.min ((as.numeric(phi) - DeltaMethodData_sub$f_values)^2)
		DeficitsAtMinima$K_s[i] <- input_Ks(as.numeric(phi))
		}
	
	return (DeficitsAtMinima)
	}




#library(O2); library(parker)
#ListData('mdot.data')
#DeficitsAtMinima <- FitSineCurveWrapper ('Ebble_CE1_2014_08_21_md', 'mdot.data', l_testset = FALSE)
#with (DeficitsAtMinima, plot (date1, K_s))
#with (DeficitsAtMinima, plot (date1, K_s/60, ylim = c (0,0.02)))
#Ks_indata <- GetIndata('Ebble_CE1_2014_08_21_md_ER_Ks_mid','mdot.data')
#X11()
#with (Ks_indata, plot (date1, Ks, main = 'Reaeration river Ebble 2014', ylab = 'per minute', ylim = c (0,0.02)))


#Ks_delta <- DeficitsAtMinima$K_s
#approx_Ks <- with (DeficitsAtMinima, ApproxDeltaMethod (as.numeric(photoperiod), as.numeric(phi)))/60/24
#plot(Ks_delta, approx_Ks); abline(0,1)
#so this looks like per hour

#Ks_indata01 <- GetIndata('Ebble_CE1_2013_08_08_ER_Ks_mid','rovelli.data')
#X11()
#with (Ks_indata01, plot (date1, Ks))

























































































FitSineCurve <- function (x_input, y_output, n_period = NA, l_plot = FALSE)
	{
	if(is.na(n_period))
		{
		#actual period length in seconds
		n_period <- tail(x_input,1) - head(x_input,1)
		#make half sine so sine period is double length of actual period
		sine_period <- 2 * n_period
		}
	
	xc<-cos(2*pi* x_input/sine_period)
	xs<-sin(2*pi* x_input/sine_period)
	fit.lm <- lm(y_output ~ xc+xs)
			
	# access the fitted series (for plotting)
	fit <- fitted(fit.lm)  

	# find predictions for original time series
	pred_var <- predict(fit.lm)
	ndx <- as.numeric(names(pred_var))
	#pred_var <- predict(fit.lm, x_input)    

	Rmse <- sum ((pred_var - y_output[ndx]) ^ 2) / length(pred_var)
	
	
	MinValue <- min(pred_var)
	MinIndex <- as.numeric(names(which.min(pred_var)))


	pred_var_input <- as.numeric(names(pred_var))
	if (l_plot)
		{
		plot(pred_var_input, pred_var)
		#plot(1:length(x_input), pred_var)
		points(1:length(x_input), y_output, col = 'red')
		}
	
	return( list ('Rmse' = Rmse, 'MinIndex' = MinIndex, 'MinValue' = MinValue, 'predicted' = pred_var))
	}

#FitSineCurve (n_time, DO_deficit_mv)

































































































FitSineCurveBatch <- function (indata)
	{
	d_dates <- unique (indata$date1)
	#d_dates <- rev(rev(d_dates)[-1])
	SunriseSunset_df <- unique(indata[c ('date1', 'Sunrise','Sunset')])
	#SunriseSunset_df$SolarNoon <- mean (SunriseSunset_df$Sunset, SunriseSunset_df$Sunrise)
	SunriseSunset_df$photoperiod <- SunriseSunset_df$Sunset - SunriseSunset_df$Sunrise
	SunriseSunset_df$SolarNoon <- (SunriseSunset_df$Sunset - SunriseSunset_df$Sunrise) / 2 + SunriseSunset_df$Sunrise
	
	SunriseSunset_df$phi = as.numeric(NA)
	SunriseSunset_df$TimeMinDeficit  = as.POSIXct(NA)
	SunriseSunset_df$TimeMinDeficitRawData  = as.POSIXct(NA)
	SunriseSunset_df$TimeMinDeficitMvAvg = as.POSIXct(NA)
	
	for (i in 1:length(d_dates))
		{
		data_sub <- indata[indata$date1 == d_dates[i],]
		data_sub <- data_sub[data_sub$daynight == 'day',]
		
		
		n_time <- as.numeric(data_sub$date)
		DO_deficit_mv <- data_sub$DO_deficit_mv
		
		fitted_sine <- FitSineCurve (n_time, DO_deficit_mv)
		SunriseSunset_df$TimeMinDeficit[i] = data_sub$date[fitted_sine$MinIndex]
		SunriseSunset_df$TimeMinDeficitMvAvg[i] = data_sub$date[which.min(data_sub$DO_deficit_mv)]
		
		DO_deficit <- data_sub$DO_deficit
		fitted_sine <- FitSineCurve (n_time, DO_deficit)
		SunriseSunset_df$TimeMinDeficitRawData[i] = data_sub$date[fitted_sine$MinIndex]
		}
	
	return(SunriseSunset_df)
	}




























































































































MakeDeltaMethodDataSet <- function ()
	{
	n_tol <- 0.0001
	l_test  =  TRUE
	x1 <-1

	start_time <- Sys.time()

	phi_df_all <- data.frame (photoperiod = as.numeric(), f_values = as.numeric(), k_values  = as.numeric())


	n_periods <- seq (7.5, 17, by = 5/60)
	#n_periods <- c (7.5, 7.6)

	for ( n_ctr in 1:length(n_periods))
		{
		period = n_periods[n_ctr]

		k_values <- seq (0.001, 3, by = 0.001)
		k_values <- seq (-5, 4, by = 0.2)
		k_values <- exp(k_values)

		#log (k_values) <- seq (0.1, 3, by = 0.1)
		f_values <- k_values
		f_values[] <- NA

		n_factor <- 1

		for ( i in 1:length(k_values))
			{
			k_test <- k_values[i]
			while (l_test)
				{
				#x2 <- x1 + x1/10
				#x3 <- x1 - x1/10

				x2 <- x1 + n_factor
				x3 <- x1 - n_factor

				#x2 <- x1 + 0.00001
				#x3 <- x1 - 0.00001
				
				y2 <- DeltaMethodCurve (k_test, period, x2)
				y3 <- DeltaMethodCurve (k_test, period, x3)

				z2 <- (0-y2)^2
				z3 <- (0-y3)^2
				
				x1 <- x2
				if (z3 < z2) x1 <- x3
				
				n_factor <- n_factor * 0.99
				
				l_test <- (z3 > n_tol)
				print(x1)
				}
			
			print (x1)
			f_values[i] <- x1
			l_test  =  TRUE
			n_factor <- 1
			x1 <-1
			}

		print (x1)
		plot(f_values, log(k_values))
		phi_df <- data.frame (f_values, k_values)
		phi_df$photoperiod <- period
		
		#phi_df_all <- append (phi_df_all, phi_df)
		phi_df_all <- rbind (phi_df_all, phi_df)
		assign ( paste ('phi_df', period, sep = ''), phi_df)
		}

	DeltaMethodData <- phi_df_all

	setwd(dirdmp)
	save( DeltaMethodData, file = 'DeltaMethodData.rda')
	}
	
#MakeDeltaMethodDataSet ()





































































MakeTimeDataSet <- function (l_getmax)
	{
	day_times <- seq (0,24, by = 1/60)
	start_time <- Sys.time()

	phi_df_all <- data.frame (photoperiod = as.numeric(), time_values = as.numeric(), k_values  = as.numeric())

	n_periods <- seq (7.5, 17, by = 5/60)
	#n_periods <- c (7.5, 7.6)


	for ( n_ctr in 1:length(n_periods))
	for ( n_ctr in 1:2)
		{
		#period = n_periods[1:2]
		period = n_periods[n_ctr]
			
		
		if (l_getmax)	day_times <- seq (period, 24, by = 1/60)
		
		#because time of minimum deficit must be between solar noon and sunset
		if (!l_getmax)	day_times <- seq (period/2, period, by = 1/60)

		k_values <- seq (-5, 4, by = 0.2)
		k_values <- exp(k_values)

		time_values <- k_values
		time_values[] <- NA
		#time_values <- rep(time_values,2)
		#time_values <- matrix(time_values,ncol = 2)
		

		for ( i in 1:length(k_values))
			{
			
			#k_test <- k_values[1]
			k_test <- k_values[i]
			
			#identify changeover point from positive to negative and vice-versa
			result <- DeltaMethodTimeCurve (k_test, period, day_times)
			plot(result)
			print(i)
			Wait(.5)
			result <- embed(result,3)
			result1 <- sign(result)
			result2 <- rowSums(result1)^2
			#for rowSums(result1) to be equal to -1 there must be two negative and one positive result
			result2_ndx <- result2 < 9 & rowSums(result1) == -1
			
			result1_direction <- result1[result2_ndx,]
			#time_values[i,] <- day_times[result2_ndx]
			time_values[i] <- day_times[result2_ndx]
			print(i)
			print(warnings()[1])
			
			#pick first and third
			#time_values[i,] <- rev(day_times[result2_ndx])[c(1,3)]
			
			}

		plot(time_values[,1], log(k_values))
		phi_df <- data.frame (time_values, k_values)
		phi_df$photoperiod <- period
		
		#phi_df_all <- append (phi_df_all, phi_df)
		phi_df_all <- rbind (phi_df_all, phi_df)
		assign ( paste ('phi_df', period, sep = ''), phi_df)
		}

	DeltaMethodData <- phi_df_all
	save( DeltaMethodData, file =  paste (dirdmp, 'DeltaMethodTminTmaxData.rda', sep = ''))
	return(DeltaMethodData)
	}
	
#xxxx <- MakeTimeDataSet ()

































































MovingAverageAddon <- function (c_dataset, c_library, c_name, n_quality = 2, n_period = 120, l_testset = FALSE)
	{
	#indata <- Ebble_CE1_2013_04_25
	#c_library <- mdot
	#library (mdot); data (Minidot_02); indata <- Minidot_02; n_quality = 14; c_indata <- 'Minidot_02'
	

	#dirscr <- paste ( dirtop, '/DO/rovscript/', sep = '')
	
	#library(mdot)
	library (parker)
	data (parker)


	library (plyr)
	library (TTR)
	library (zoo)

	
	do.call ( library, list (c_library))
	
	DataSets <- parker::ListData (c_library)
	rownumber <- which (DataSets[,1] == c_dataset)
	
	#return (rownumber)
	indata <- parker::LoadData (c_library, rownumber)

	
	if ( !is.null(indata$qualityFilter))
		{
		indata <- indata[indata$qualityFilter > n_quality,]
		}
	
	if (l_testset)	indata <- head(indata,10000)
	
	#indata$DO_deficit_mv <- runMean(indata$DO_deficit, n = n_period, cumulative = FALSE, na.rm = TRUE)
	indata$DO_deficit_mv <- runMean(indata$DO_deficit, n = n_period, cumulative = FALSE)
	#plot(indata$DO_deficit_mv)

	indata$DO_deficit_mv <- filter (indata$DO_deficit, rep(1/n_period, n_period), sides = 2)

	lag_def <- with (indata, embed (DO_deficit_mv, 2))
	indata$DO_deficit_diff <- c (NA, lag_def[,2] - lag_def[,1])
	lag_def <- with (indata, embed (DO_deficit_diff, 2))
	indata$DO_deficit_diff_diff <- c (NA, lag_def[,2] - lag_def[,1])
	#plot(indata$DO_deficit_mv)
	
	return(indata)
	}


#DeficitsAtMinima <- FitSineCurveBatch (addedMvAvg)


























































RoundTo5minutes <- function(x)
	{
	#calculate minutes from decimal part of hours
	x_int <- floor(x)
	
	x <- (x - floor(x)) * 60
	#round to integer
	x <- round(x,0)
	x_5 <- round (x/5, 0) * 5
	result <- x_int + x_5/60
	return(result)
	mod_5 <- x %% 5
	}


#RoundTo5minutes (7.66)

