ModelDODayNightSampled03 <- function ( indata, time_step = 10 )
	{
	library (deSolve)
	library (O2)


	stopifnot ( TRUE %in% (c ( 'Bo0','PARraw','ModelledLight' )  %in% names (indata)))
	stopifnot ( c ( 'date', 'date1', 'ER','Ks','DO_diff','DO_deficit', 'Temp', 'CSat') %in% names (indata))


	data (O2_sol)
	setwd ( dirdmp )

	#indata <- indata[indata$daynight1 == c_night,]
	#plot ( indata$date, indata$DO )
	
	
	n_len <- dim(indata)[1]
	#I dont know why this works
	#see here
	#http://stackoverflow.com/questions/5237557/extracting-every-nth-element-of-a-vector
	
	indata_sampled_ndx <- c(1,(1:n_len)[1:(1+time_step-1)==(1+time_step-1)]+1)
	data_sub <- data_sub[indata_sampled_ndx,]




	times_1 <- data_sub$date
	plot (times_1)
	times_1_num <- as.numeric (times_1)
	#set first time as 0
	times_1_num <- times_1_num - times_1_num[1] + 0
	#divide by median
	#times_1_num <- times_1_num / (summary(diff(times_1_num))[3])
	#shift all by 1
	times_1_num <- times_1_num + 1


	max_DO_obs <- max (data_sub$DO)
	min_DO_obs <- min (data_sub$DO)


	
	DO_initial = data_sub$DO[1]

	if ( c ( 'ModelledLight' )  %in% names (data_sub))
		{
		Bo0 = as.numeric ( data_sub$ModelledLight )
		Bo0[is.na(Bo0)] = 0
		#remove negative values of light
		Bo0 = pmax (0, Bo0)
		}



	ReAer.env <- new.env()
	
	
	ReAer.env$Phot <- Jassby1
	ReAer.env$Resp <- Resp2
	ReAer.env$Gas <- Gas2
	ReAer1 <- ReAer


	ode.ReAer <- ode

	
	ReAer.env$input_O2 <- approxfun ( times_1_num, data_sub$CSat, rule = 1)
	ReAer.env$input_Ks <- approxfun ( times_1_num, data_sub$Ks, rule = 1)
	ReAer.env$input_ER <- approxfun ( times_1_num, data_sub$ER, rule = 1)
	ReAer.env$input_T <- approxfun ( times_1_num, data_sub$Temp, rule = 1)
	ReAer.env$input_light <- approxfun ( times_1_num, Bo0, rule = 1)
	ReAer.env$input_Pmax <- approxfun ( times_1_num, data_sub$Pmax, rule = 1)
	ReAer.env$input_Ik <- approxfun ( times_1_num, data_sub$I_k, rule = 1)


	ReAer.env$photo_params <- quote ( list ( 'light' = light, 'Pmax' = Pmax, 'I_k' = I_k))
	ReAer.env$resp_params <- quote ( list ( 'ER' = ER) )
	ReAer.env$gas_params <- quote ( list ( 'Ks' = Ks, 'C_sat' = C_sat,'Cnow' = Cnow ) )
	ReAer.env$data_sub <- data_sub

	environment(ReAer1) <- ReAer.env
	environment(ode.ReAer) <- ReAer.env
	
	print (ls(ReAer.env))
	
	
	y_ini <- c (Cnow = DO_initial)


	out <- ode.ReAer(y = y_ini, times = times_1_num, func = ReAer1, parms = NULL)
	out1 <- data.frame (out)
	out1$date <- data_sub$date
	out1$DO <- data_sub$DO
	
	max_y <- ceiling ( max (out1$Cnow, out1$DO, na.rm = TRUE ))
	min_y <- floor ( min (out1$Cnow, out1$DO, na.rm = TRUE ))
	
	plot(out1$date, out1$DO, ylim = c( min_y, max_y))
	points(out1$date, out1$Cnow, col = 'grey50', pch = 2)
	legend ('topright', c ('Observed', 'Simulated'), pch = c ( 1,2), col = c ( 'black','grey50'), lwd = c ( 2,2), lty = c ( NA,NA))


	try (out1$date <- data_sub$date)
	try (out1$date1 <- data_sub$date1)
	try (out1$daynight <- data_sub$daynight)
	try (out1$daynight1 <- data_sub$daynight1)

	return (out1)
	}




































ModelDODayNight02 <- function ( indata )
	{
	library (deSolve)
	library (O2)


	stopifnot ( TRUE %in% (c ( 'Bo0','PARraw','ModelledLight' )  %in% names (indata)))
	stopifnot ( c ( 'date', 'date1', 'ER','Ks','DO_diff','DO_deficit', 'Temp', 'CSat') %in% names (indata))


	data (O2_sol)

	setwd ( dirdmp )

	#indata <- indata[indata$daynight1 == c_night,]
	#plot ( indata$date, indata$DO )
	
	
	times_1 <- indata$date
	plot (times_1)
	times_1_num <- as.numeric (times_1)
	#set first time as 0
	times_1_num <- times_1_num - times_1_num[1] + 0
	#divide by median
	times_1_num <- times_1_num / (summary(diff(times_1_num))[3])
	#shift all by 1
	times_1_num <- times_1_num + 1



	max_DO_obs <- max (indata$DO)
	min_DO_obs <- min (indata$DO)


	DO_initial = indata$DO[1]

	if ( c ( 'ModelledLight' )  %in% names (indata))
		{
		Bo0 = as.numeric ( indata$ModelledLight )
		Bo0[is.na(Bo0)] = 0
		#remove negative values of light
		Bo0 = pmax (0, Bo0)
		}



	ReAer.env <- new.env()
	
	
	ReAer.env$Phot <- Jassby1
	ReAer.env$Resp <- Resp2
	ReAer.env$Gas <- Gas2
	ReAer1 <- ReAer


	ode.ReAer <- ode

	
	ReAer.env$input_O2 <- approxfun ( times_1_num, indata$CSat, rule = 1)
	ReAer.env$input_Ks <- approxfun ( times_1_num, indata$Ks, rule = 1)
	ReAer.env$input_ER <- approxfun ( times_1_num, indata$ER, rule = 1)
	ReAer.env$input_T <- approxfun ( times_1_num, indata$Temp, rule = 1)
	ReAer.env$input_light <- approxfun ( times_1_num, Bo0, rule = 1)
	ReAer.env$input_Pmax <- approxfun ( times_1_num, indata$Pmax, rule = 1)
	ReAer.env$input_Ik <- approxfun ( times_1_num, indata$I_k, rule = 1)


	ReAer.env$photo_params <- quote ( list ( 'light' = light, 'Pmax' = Pmax, 'I_k' = I_k))
	ReAer.env$resp_params <- quote ( list ( 'ER' = ER) )
	ReAer.env$gas_params <- quote ( list ( 'Ks' = Ks, 'C_sat' = C_sat,'Cnow' = Cnow ) )
	ReAer.env$indata <- indata

	environment(ReAer1) <- ReAer.env
	environment(ode.ReAer) <- ReAer.env
	
	print (ls(ReAer.env))
	
	
	y_ini <- c (Cnow = DO_initial)


	out <- ode.ReAer(y = y_ini, times = times_1_num, func = ReAer1, parms = NULL)
	out1 <- data.frame (out)
	out1$date <- indata$date
	out1$DO <- indata$DO
	
	max_y <- ceiling ( max (out1$Cnow, out1$DO, na.rm = TRUE ))
	min_y <- floor ( min (out1$Cnow, out1$DO, na.rm = TRUE ))
	
	plot(out1$date, out1$DO, ylim = c( min_y, max_y))
	points(out1$date, out1$Cnow, col = 'grey50', pch = 2)
	legend ('topright', c ('Observed', 'Simulated'), pch = c ( 1,2), col = c ( 'black','grey50'), lwd = c ( 2,2), lty = c ( NA,NA))

	return (out1)
	}






















































































































ModelDODayNight01 <- function ( indata, inPhot = DO_Ebble_2013_fittedParams )
	{
	library (deSolve)
	library (O2)

	stopifnot ( c ( 'date', 'date1','PARraw','ER','Ks','DO_diff','DO_deficit', 'Temp', 'CSat') %in% names (indata))

	data (O2_sol)

	setwd ( dirdmp )

	#indata <- indata[indata$daynight1 == c_night,]
	#plot ( indata$date, indata$DO )
	indata <- merge (indata, inPhot, by = 'date1' )
	print (head ( indata,3))
	print (summary ( indata$I_k,3))

	
	times_1 <- indata$date
	plot (times_1)
	times_1_num <- as.numeric (times_1)
	#set first time as 0
	times_1_num <- times_1_num - times_1_num[1] + 0
	#divide by median
	times_1_num <- times_1_num / (summary(diff(times_1_num))[3])
	#shift all by 1
	times_1_num <- times_1_num + 1



	max_DO_obs <- max (indata$DO)
	min_DO_obs <- min (indata$DO)


	DO_initial = indata$DO[1]

	Bo0 = as.numeric ( indata$PARraw )
	Bo0[is.na(Bo0)] = 0
	#remove negative values of light
	Bo0 = pmax (0, Bo0)



	ReAer.env <- new.env()
	
	
	ReAer.env$Phot <- Jassby1
	ReAer.env$Resp <- Resp2
	ReAer.env$Gas <- Gas2
	ReAer1 <- ReAer


	ode.ReAer <- ode

	
	ReAer.env$input_O2 <- approxfun ( times_1_num, indata$CSat, rule = 1)
	ReAer.env$input_Ks <- approxfun ( times_1_num, indata$Ks, rule = 1)
	ReAer.env$input_ER <- approxfun ( times_1_num, indata$ER, rule = 1)
	ReAer.env$input_T <- approxfun ( times_1_num, indata$Temp, rule = 1)
	ReAer.env$input_light <- approxfun ( times_1_num, Bo0, rule = 1)
	ReAer.env$input_Pmax <- approxfun ( times_1_num, indata$Pmax, rule = 1)
	ReAer.env$input_Ik <- approxfun ( times_1_num, indata$I_k, rule = 1)


	ReAer.env$photo_params <- quote ( list ( 'light' = light, 'Pmax' = Pmax, 'I_k' = I_k))
	ReAer.env$resp_params <- quote ( list ( 'ER' = ER) )
	ReAer.env$gas_params <- quote ( list ( 'Ks' = Ks, 'C_sat' = C_sat,'Cnow' = Cnow ) )
	ReAer.env$indata <- indata

	environment(ReAer1) <- ReAer.env
	environment(ode.ReAer) <- ReAer.env
	
	print (ls(ReAer.env))
	
	
	y_ini <- c (Cnow = DO_initial)


	out <- ode.ReAer(y = y_ini, times = times_1_num, func = ReAer1, parms = NULL)
	out1 <- data.frame (out)
	out1$date <- indata$date
	out1$DO <- indata$DO
	
	max_y <- ceiling ( max (out1$Cnow, out1$DO, na.rm = TRUE ))
	min_y <- floor ( min (out1$Cnow, out1$DO, na.rm = TRUE ))
	
	plot(out1$date, out1$DO, ylim = c( min_y, max_y))
	points(out1$date, out1$Cnow, col = 'grey50', pch = 2)
	legend ('topright', c ('Observed', 'Simulated'), pch = c ( 1,2), col = c ( 'black','grey50'), lwd = c ( 2,2), lty = c ( NA,NA))

	try (out1$date <- indata$date)
	try (out1$date1 <- indata$date1)
	try (out1$daynight <- indata$daynight)
	try (out1$daynight1 <- indata$daynight1)
	return (out1)
	}


#x1 <- MakeERKsTimeSeries ('Ebble_CE1_2013_04_25', 'Ebble_CE1_2013_04_25_ER_Ks')
#xss <- ModelDODayNight (indata = x1)



#x1 <- MakeERKsTimeSeries ('Ebble_CE1_2013_08_08', 'Ebble_CE1_2013_08_08_ER_Ks')
#xss <- ModelDODayNight (indata = x1)



























































































































































































































































ModelDODayNight <- function ( indata, inPhot = DO_Ebble_2013_fittedParams )
	{
	library (deSolve)
	library (O2)

	stopifnot ( c ( 'date', 'date1','PARraw','ER','Ks','DO_diff','DO_deficit', 'Temp', 'CSat') %in% names (indata))

	data (O2_sol)

	setwd ( dirdmp )

	#indata <- indata[indata$daynight1 == c_night,]
	#plot ( indata$date, indata$DO )
	indata <- merge (indata, inPhot, by = 'date1' )
	print (head ( indata,3))
	print (summary ( indata$I_k,3))

	
	times_1 <- indata$date
	plot (times_1)
	times_1_num <- as.numeric (times_1)
	times_1_num <- 1:length(times_1_num)

	#times_1 <- as.numeric (indata$date)


	max_DO_obs <- max (indata$DO)
	min_DO_obs <- min (indata$DO)


	DO_initial = indata$DO[1]

	Bo0 = as.numeric ( indata$PARraw )
	Bo0[is.na(Bo0)] = 0
	#remove negative values of light
	Bo0 = pmax (0, Bo0)



	ReAer.env <- new.env()
	
	
	ReAer.env$Phot <- Jassby1
	ReAer.env$Resp <- Resp2
	ReAer.env$Gas <- Gas2
	ReAer1 <- ReAer


	ode.ReAer <- ode

	
	ReAer.env$input_O2 <- approxfun ( times_1_num, indata$CSat, rule = 1)
	ReAer.env$input_Ks <- approxfun ( times_1_num, indata$Ks, rule = 1)
	ReAer.env$input_ER <- approxfun ( times_1_num, indata$ER, rule = 1)
	ReAer.env$input_T <- approxfun ( times_1_num, indata$Temp, rule = 1)
	ReAer.env$input_light <- approxfun ( times_1_num, Bo0, rule = 1)
	ReAer.env$input_Pmax <- approxfun ( times_1_num, indata$Pmax, rule = 1)
	ReAer.env$input_Ik <- approxfun ( times_1_num, indata$I_k, rule = 1)


	ReAer.env$photo_params <- quote ( list ( 'light' = light, 'Pmax' = Pmax, 'I_k' = I_k))
	ReAer.env$resp_params <- quote ( list ( 'ER' = ER) )
	ReAer.env$gas_params <- quote ( list ( 'Ks' = Ks, 'C_sat' = C_sat,'Cnow' = Cnow ) )
	ReAer.env$indata <- indata

	environment(ReAer1) <- ReAer.env
	environment(ode.ReAer) <- ReAer.env
	
	print (ls(ReAer.env))
	
	
	y_ini <- c (Cnow = DO_initial)


	out <- ode.ReAer(y = y_ini, times = times_1_num, func = ReAer1, parms = NULL)
	out1 <- data.frame (out)
	out1$date <- indata$date
	out1$DO <- indata$DO
	
	max_y <- ceiling ( max (out1$Cnow, out1$DO, na.rm = TRUE ))
	min_y <- floor ( min (out1$Cnow, out1$DO, na.rm = TRUE ))
	
	plot(out1$date, out1$DO, ylim = c( min_y, max_y))
	points(out1$date, out1$Cnow, col = 'grey50', pch = 2)
	legend ('topright', c ('Observed', 'Simulated'), pch = c ( 1,2), col = c ( 'black','grey50'), lwd = c ( 2,2), lty = c ( NA,NA))

	return (out1)
	}


#x1 <- MakeERKsTimeSeries ('Ebble_CE1_2013_04_25', 'Ebble_CE1_2013_04_25_ER_Ks')
#xss <- ModelDODayNight (indata = x1)



#x1 <- MakeERKsTimeSeries ('Ebble_CE1_2013_08_08', 'Ebble_CE1_2013_08_08_ER_Ks')
#xss <- ModelDODayNight (indata = x1)
