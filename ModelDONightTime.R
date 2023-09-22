

ModelDOSingleNightArrhVariableTime <- function (indata, ER20, K20, theta_ER, theta_K, DO_initial = NA, n_mins = 5, l_keepoutput = TRUE, l_plot = FALSE)
	{

	library (deSolve)
	try(library (O2))


	#only modelling one night, therefore ok
	c_night <- as.character(unique(indata$daynight1))
	stopifnot(length(c_night)==1)
	

	secs_in_min = 60
	time_step = n_mins * secs_in_min

	d_dates <- seq(head(indata$date,1), tail(indata$date,1), by = time_step)
	
	indata01 <- data.frame('date' = d_dates)
	
	banana <- with(indata, approxfun(date,DO))
	indata01$DO <- banana(d_dates)
	banana <- with(indata, approxfun(date,Temp))
	indata01$Temp <- banana(d_dates)
	banana <- with(indata, approxfun(date,DO_deficit))
	indata01$DO_deficit <- banana(d_dates)
	banana <- with(indata, approxfun(date,CSat))
	indata01$CSat <- banana(d_dates)


	DO_diff <- with(indata01, embed(DO,2))
	DO_diff <- (DO_diff[,1] - DO_diff[,2])

	DO <- with(indata01, embed(DO,2))
	DO <- rowMeans(DO)

	Temp <- with(indata01, embed(Temp,2))
	Temp <- rowMeans(Temp)

	DO_deficit <- with(indata01, embed(DO_deficit,2))
	DO_deficit <- rowMeans(DO_deficit)
	
	CSat <- with(indata01, embed(CSat,2))
	CSat <- rowMeans(CSat)

	d_dates1 <- indata01$date + time_step/2
	d_dates1 <- head(d_dates1, -1)
	
	DO <- with(indata01, embed(DO,2))
	DO <- rowMeans(DO)
	
	indata02 <- data.frame('date' = d_dates1)
	indata02$date1 <- as.Date(indata02$date)
	
	indata02$DO <- DO
	indata02$Temp <- Temp
	indata02$DO_deficit <- DO_deficit
	indata02$DO_diff <- DO_diff
	indata02$CSat <- CSat
	

	#reassign indata
	indata <- indata02
	

	stopifnot ( c ( 'date', 'date1','DO_diff','DO_deficit', 'Temp', 'CSat') %in% names (indata))
	#stopifnot ( names(in_params) == c('ER','Ks'))
	#c_night,]
	#indata <- indata[indata$daynight1 == c_night,]
	#indata <- data_sub

	if(is.na(DO_initial))
		{
		#this removes the need to consider whether the first, second etc.. DO values are missing values
		DO_initial = max(indata$DO, na.rm = TRUE)
		}
	

	#remove initial NAs
	#n_ctr = 0
	#while(is.na(DO_initial))
	#	{
	#	n_ctr = n_ctr + 1
	#	DO_initial = indata$DO[n_ctr]
	#	}
	
	#if(n_ctr > 1)
	#	{
	#	indata <- indata[-(1:n_ctr-1),]
	#	}


	times_1_num <- seq(1, length = dim(indata)[1], by = 1)

	#times_1_num <- as.numeric (times_1)
	#set first time as 0
	#times_1_num <- times_1_num - times_1_num[1] + 0
	#divide by median
	#times_1_num <- times_1_num / (summary(diff(times_1_num))[3])
	#shift all by 1
	#times_1_num <- times_1_num + 1



	max_DO_obs <- max (indata$DO)
	min_DO_obs <- min (indata$DO)




	ReAerNight.env <- new.env()
	
	
	ReAerNight.env$Resp <- RespTemp01
	ReAerNight.env$Gas <- GasTemp01
	ReAer1 <- ReAerNightArrh01


	ode.ReAer <- ode

	
	ReAerNight.env$input_O2 <- approxfun ( times_1_num, indata$CSat, rule = 1)
	ReAerNight.env$input_T <- approxfun ( times_1_num, indata$Temp, rule = 1)

	
	ReAerNight.env$resp_params <- quote ( list ( 'ER20' = ER20, 'Temp' = Temp, 'theta_ER' = theta_ER) )
	ReAerNight.env$gas_params <- quote ( list ( 'K20' = K20, 'C_sat' = C_sat,'Cnow' = Cnow, 'Temp' = Temp, 'theta_K' = theta_K ) )
	
	ReAerNight.env$indata <- indata

	environment(ReAer1) <- ReAerNight.env
	environment(ode.ReAer) <- ReAerNight.env

	print (ls(ReAerNight.env))
	
	
	y_ini <- c (Cnow = DO_initial)


	out <- ode.ReAer(y = y_ini, times = times_1_num, func = ReAer1, parms = NULL)
	out1 <- data.frame (out)
	out1$date <- indata$date
	out1$DO <- indata$DO
	#out1$daynight1 <- c_night
	
	
	
	if(l_plot)
		{
		max_y <- max (out1$Cnow, out1$DO, na.rm = TRUE )
		min_y <- min (out1$Cnow, out1$DO, na.rm = TRUE )
		
		plot(out1$date, out1$DO, ylim = c( min_y, max_y))
		points(out1$date, out1$Cnow, col = 'grey50', pch = 3)
		legend ('topright', c ('Observed', 'Simulated'), pch = c ( 1,2), col = c ( 'black','grey50'), lwd = c ( 2,2), lty = c ( NA,NA))
		}


	#RMSE_out <- (mean(with(out1, (Cnow - DO)^2, na.rm = TRUE))  ^ 0.5
	RMSE_out <- with(out1, mean((Cnow - DO)^2, na.rm = TRUE))  ^ 0.5


	out1 <- list(out1)
	names(out1) <- 'output'

	out1$RMSE <- c('ER20' = ER20 , 'K20' = K20, 'RMSE' = RMSE_out)

	#out1 = c('ER' = ER , 'Ks' = Ks, 'RMSE' = RMSE_out)

	
	DO_compare <- (DO_initial == max(indata$DO, na.rm = TRUE))
	#overwrite if all output is to be kept
	if(!l_keepoutput)
		{
		out1 = c('ER20' = ER20 , 'K20' = K20, 'RMSE' = RMSE_out, 'theta_ER' = theta_ER, 'theta_K' = theta_K, 'DO_initial' = 			DO_initial, 'DO_compare' = DO_compare, 'daynight1' = c_night)
		#out1$RMSE <- c('ER' = ER , 'Ks' = Ks, 'RMSE' = RMSE_out)
		}


	return (out1)	
	}



#library(mdot); data(GA2_Aug2015)
#data_sub <- GA2_Aug2015[GA2_Aug2015$daynight1 == 'night0250',]
#with(data_sub, plot(date,DO))
#model_night <- ModelDOSingleNightArrhenius (indata = data_sub, ER20, K20, theta_ER, theta_K, DO_initial = NA, l_keepoutput = TRUE, l_plot = TRUE)


#model_night <- ModelDOSingleNightArrhVariableTime (indata = data_sub, ER20 = -0.035, K20 = 0.015, theta_ER = 1, theta_K = 1.02, DO_initial = NA, l_keepoutput = TRUE, l_plot = TRUE)


#model_night <- ModelDOSingleNightArrhVariableTime (indata = data_sub, ER20 = -0.17, K20 = 0.075, theta_ER = 1, theta_K = 1.02, DO_initial = NA, l_keepoutput = TRUE, l_plot = TRUE)




















































ModelDOSingleNight_ERKTimeSeries <- function (indata, theta_ER, theta_K, DO_initial = NA, n_mins, l_keepoutput = TRUE)
	{

	library (deSolve)
	try(library (O2))
	
	#indata = data_sub_synthetic; theta_ER = 1; theta_K = 1
	#DO_initial = NA; n_mins = 1; l_keepoutput = TRUE

	#only modelling one night, therefore ok
	c_night <- as.character(unique(indata$daynight1))
	stopifnot(length(c_night)==1)
	

	secs_in_min = 60
	time_step = n_mins * secs_in_min

	d_dates <- seq(head(indata$date,1), tail(indata$date,1), by = time_step)
	
	indata01 <- data.frame('date' = d_dates)
	
	banana <- with(indata, approxfun(date,DO))
	indata01$DO <- banana(d_dates)

	banana <- with(indata, approxfun(date,Temp))
	indata01$Temp <- banana(d_dates)

	#banana <- with(indata, approxfun(date,DO_deficit))
	#indata01$DO_deficit <- banana(d_dates)

	banana <- with(indata, approxfun(date,CSat))
	indata01$CSat <- banana(d_dates)

	banana <- with(indata, approxfun(date,ER20))
	indata01$ER20 <- banana(d_dates)

	banana <- with(indata, approxfun(date,K20))
	indata01$K20 <- banana(d_dates)


	DO_diff <- with(indata01, embed(DO,2))
	DO_diff <- (DO_diff[,1] - DO_diff[,2])

	DO <- with(indata01, embed(DO,2))
	DO <- rowMeans(DO)

	Temp <- with(indata01, embed(Temp,2))
	Temp <- rowMeans(Temp)

	ER20 <- with(indata01, embed(ER20,2))
	ER20 <- rowMeans(ER20)

	K20 <- with(indata01, embed(K20,2))
	K20 <- rowMeans(K20)

	#DO_deficit <- with(indata01, embed(DO_deficit,2))
	#DO_deficit <- rowMeans(DO_deficit)
	
	CSat <- with(indata01, embed(CSat,2))
	CSat <- rowMeans(CSat)

	d_dates1 <- indata01$date + time_step/2
	d_dates1 <- head(d_dates1, -1)
	
	DO <- with(indata01, embed(DO,2))
	DO <- rowMeans(DO)
	
	indata02 <- data.frame('date' = d_dates1)
	indata02$date1 <- as.Date(indata02$date)
	
	indata02$DO <- DO
	indata02$Temp <- Temp
	#indata02$DO_deficit <- DO_deficit
	indata02$DO_diff <- DO_diff
	indata02$CSat <- CSat
	indata02$ER20 <- ER20
	indata02$K20 <- K20

	#reassign indata
	indata <- indata02
	

	#stopifnot ( c ( 'date', 'date1','DO_diff','DO_deficit', 'Temp', 'CSat') %in% names (indata))
	stopifnot ( c ( 'date', 'date1', 'Temp', 'CSat') %in% names (indata))

	if(is.na(DO_initial))
		{
		#this removes the need to consider whether the first, second etc.. DO values are missing values
		#DO_initial = max(indata$DO, na.rm = TRUE)
		DO_initial = indata$DO[1]
		if(is.na(DO_initial)) DO_initial = max(indata$DO, na.rm = TRUE)
		}
	
	times_1_num <- seq(1, length = dim(indata)[1], by = 1)



	max_DO_obs <- max (indata$DO)
	min_DO_obs <- min (indata$DO)


	ReAerNight.env <- new.env()
	
	
	ReAerNight.env$Resp <- RespTemp01
	ReAerNight.env$Gas <- GasTemp01
	
	#ReAerNightArrhRespTS is the fundamental name of the function
	ReAer1 <- ReAerNightArrhRespTS


	ode.ReAer <- ode

	
	ReAerNight.env$input_O2 <- approxfun ( times_1_num, indata$CSat, rule = 1)
	ReAerNight.env$input_T <- approxfun ( times_1_num, indata$Temp, rule = 1)
	ReAerNight.env$input_ER20 <- approxfun ( times_1_num, indata$ER20, rule = 1)
	ReAerNight.env$input_K20 <- approxfun ( times_1_num, indata$K20, rule = 1)
	
	
	
	ReAerNight.env$resp_params <- quote ( list ( 'ER20' = ER20, 'Temp' = Temp, 'theta_ER' = theta_ER) )
	ReAerNight.env$gas_params <- quote ( list ( 'K20' = K20, 'C_sat' = C_sat,'Cnow' = Cnow, 'Temp' = Temp, 'theta_K' = theta_K ) )
	
	ReAerNight.env$indata <- indata

	environment(ReAer1) <- ReAerNight.env
	environment(ode.ReAer) <- ReAerNight.env

	print (ls(ReAerNight.env))
	
	
	y_ini <- c (Cnow = DO_initial)


	out <- ode.ReAer(y = y_ini, times = times_1_num, func = ReAer1, parms = NULL)
	out1 <- data.frame (out)
	out1$date <- indata$date
	out1$DO <- indata$DO
	#out1$daynight1 <- c_night

	#RMSE_out <- (mean(with(out1, (Cnow - DO)^2, na.rm = TRUE))  ^ 0.5
	RMSE_out <- with(out1, mean((Cnow - DO)^2, na.rm = TRUE))  ^ 0.5


	out1 <- list(out1)
	names(out1) <- 'output'

	out1$RMSE <- c('ER20' = ER20 , 'K20' = K20, 'RMSE' = RMSE_out)

	#out1 = c('ER' = ER , 'Ks' = Ks, 'RMSE' = RMSE_out)

	
	DO_compare <- (DO_initial == max(indata$DO, na.rm = TRUE))
	#overwrite if all output is to be kept
	if(!l_keepoutput)
		{
		out1 = c('ER20' = ER20 , 'K20' = K20, 'RMSE' = RMSE_out, 'theta_ER' = theta_ER, 'theta_K' = theta_K, 'DO_initial' = 			DO_initial, 'DO_compare' = DO_compare, 'daynight1' = c_night)
		#out1$RMSE <- c('ER' = ER , 'Ks' = Ks, 'RMSE' = RMSE_out)
		}


	return (out1)	
	}





























































































ModelDOSingleNightArrhenius <- function (indata, ER20, K20, theta_ER, theta_K, DO_initial = NA, l_keepoutput = TRUE, l_plot = TRUE)
	{
	library (deSolve)
	library (O2)


	stopifnot ( c ( 'date', 'date1','DO_diff','DO_deficit', 'Temp', 'CSat') %in% names (indata))
	#stopifnot ( names(in_params) == c('ER','Ks'))
	#c_night,]
	#indata <- indata[indata$daynight1 == c_night,]
	#indata <- data_sub

	if(is.na(DO_initial))
		{
		#this removes the need to consider whether the first, second etc.. DO values are missing values
		DO_initial = max(indata$DO, na.rm = TRUE)
		}
	

	#remove initial NAs
	#n_ctr = 0
	#while(is.na(DO_initial))
	#	{
	#	n_ctr = n_ctr + 1
	#	DO_initial = indata$DO[n_ctr]
	#	}
	
	#if(n_ctr > 1)
	#	{
	#	indata <- indata[-(1:n_ctr-1),]
	#	}


	times_1 <- indata$date

	times_1_num <- as.numeric (times_1)
	#set first time as 0
	times_1_num <- times_1_num - times_1_num[1] + 0
	#divide by median
	times_1_num <- times_1_num / (summary(diff(times_1_num))[3])
	#shift all by 1
	times_1_num <- times_1_num + 1



	max_DO_obs <- max (indata$DO)
	min_DO_obs <- min (indata$DO)




	ReAerNight.env <- new.env()
	
	
	ReAerNight.env$Resp <- RespTemp01
	ReAerNight.env$Gas <- GasTemp01
	ReAer1 <- ReAerNightArrh01


	ode.ReAer <- ode

	
	ReAerNight.env$input_O2 <- approxfun ( times_1_num, indata$CSat, rule = 1)
	ReAerNight.env$input_T <- approxfun ( times_1_num, indata$Temp, rule = 1)

	
	ReAerNight.env$resp_params <- quote ( list ( 'ER20' = ER20, 'Temp' = Temp, 'theta_ER' = theta_ER) )
	ReAerNight.env$gas_params <- quote ( list ( 'K20' = K20, 'C_sat' = C_sat,'Cnow' = Cnow, 'Temp' = Temp, 'theta_K' = theta_K ) )
	
	ReAerNight.env$indata <- indata

	environment(ReAer1) <- ReAerNight.env
	environment(ode.ReAer) <- ReAerNight.env

	print (ls(ReAerNight.env))
	
	
	y_ini <- c (Cnow = DO_initial)


	out <- ode.ReAer(y = y_ini, times = times_1_num, func = ReAer1, parms = NULL)
	out1 <- data.frame (out)
	out1$date <- indata$date
	out1$DO <- indata$DO
	#out1$daynight1 <- c_night
	
	
	
	if(l_plot)
		{
		max_y <- max (out1$Cnow, out1$DO, na.rm = TRUE )
		min_y <- min (out1$Cnow, out1$DO, na.rm = TRUE )
		
		plot(out1$date, out1$DO, ylim = c( min_y, max_y))
		points(out1$date, out1$Cnow, col = 'grey50', pch = 3)
		legend ('topright', c ('Observed', 'Simulated'), pch = c ( 1,2), col = c ( 'black','grey50'), lwd = c ( 2,2), lty = c ( NA,NA))
		}


	#RMSE_out <- (mean(with(out1, (Cnow - DO)^2, na.rm = TRUE))  ^ 0.5
	RMSE_out <- with(out1, mean((Cnow - DO)^2, na.rm = TRUE))  ^ 0.5


	out1 <- list(out1)
	names(out1) <- 'output'

	out1$RMSE <- c('ER20' = ER20 , 'K20' = K20, 'RMSE' = RMSE_out)

	#out1 = c('ER' = ER , 'Ks' = Ks, 'RMSE' = RMSE_out)

	DO_compare <- (DO_initial == max(indata$DO, na.rm = TRUE))
	#overwrite if all output is to be kept
	if(!l_keepoutput)
		{
		out1 = c('ER20' = ER20 , 'K20' = K20, 'RMSE' = RMSE_out, 'theta_ER' = theta_ER, 'theta_K' = theta_K, 'DO_initial' = DO_initial, 'DO_compare' = DO_compare, 'daynight1' = as.character(unique(indata$daynight1)))
		#out1$RMSE <- c('ER' = ER , 'Ks' = Ks, 'RMSE' = RMSE_out)
		}


	return (out1)	
	}




#library(mdot); data(GA2_Aug2015)
#data_sub <- GA2_Aug2015[GA2_Aug2015$daynight1 == 'night0250',]
#with(data_sub, plot(date,DO))
#model_night <- ModelDOSingleNightArrhenius (indata = data_sub, ER20, K20, theta_ER, theta_K, DO_initial = NA, l_keepoutput = TRUE, l_plot = TRUE)

#model_night <- ModelDOSingleNightArrhenius (indata = data_sub, ER20 = -0.04, K20 = 0.02, theta_ER = 1, theta_K = 1.02, DO_initial = NA, l_keepoutput = TRUE, l_plot = TRUE)

#model_night <- ModelDOSingleNightArrhenius (indata = data_sub, ER20 = -0.035, K20 = 0.015, theta_ER = 1, theta_K = 1.02, DO_initial = NA, l_keepoutput = TRUE, l_plot = TRUE)
































































































ModelDOSingleNightRMSE01FreeInitial <- function (indata, in_params, DO_initial = NA, l_keepoutput = TRUE, l_plot = TRUE)
	{
	library (deSolve)
	library (O2)


	stopifnot ( c ( 'date', 'date1','DO_diff','DO_deficit', 'Temp', 'CSat') %in% names (indata))
	stopifnot ( names(in_params) == c('ER','Ks'))

	data (O2_sol)
	setwd ( dirdmp )

	#c_night,]
	#indata <- indata[indata$daynight1 == c_night,]
	#indata <- data_sub


	times_1 <- indata$date
	
	if(l_plot)
		{
		plot (times_1)
		}

	times_1_num <- as.numeric (times_1)
	#set first time as 0
	times_1_num <- times_1_num - times_1_num[1] + 0
	#divide by median
	times_1_num <- times_1_num / (summary(diff(times_1_num))[3])
	#shift all by 1
	times_1_num <- times_1_num + 1



	max_DO_obs <- max (indata$DO)
	min_DO_obs <- min (indata$DO)


	if(is.na(DO_initial))
		{
		DO_initial = indata$DO[1]
		}


	ReAerNight.env <- new.env()
	
	
	ReAerNight.env$Resp <- Resp2
	ReAerNight.env$Gas <- Gas2
	ReAer1 <- ReAerNight01


	ode.ReAer <- ode

	
	ReAerNight.env$input_O2 <- approxfun ( times_1_num, indata$CSat, rule = 1)
	ReAerNight.env$input_T <- approxfun ( times_1_num, indata$Temp, rule = 1)

	ER = as.numeric(in_params$ER)
	Ks = as.numeric(in_params$Ks)
	
	ReAerNight.env$resp_params <- quote ( list ( 'ER' = ER) )
	ReAerNight.env$gas_params <- quote ( list ( 'Ks' = Ks, 'C_sat' = C_sat,'Cnow' = Cnow ) )
	ReAerNight.env$indata <- indata

	environment(ReAer1) <- ReAerNight.env
	environment(ode.ReAer) <- ReAerNight.env

	print (ls(ReAerNight.env))
	
	
	y_ini <- c (Cnow = DO_initial)


	out <- ode.ReAer(y = y_ini, times = times_1_num, func = ReAer1, parms = NULL)
	out1 <- data.frame (out)
	out1$date <- indata$date
	out1$DO <- indata$DO
	#out1$daynight1 <- c_night
	
	
	
	if(l_plot)
		{
		max_y <- max (out1$Cnow, out1$DO, na.rm = TRUE )
		min_y <- min (out1$Cnow, out1$DO, na.rm = TRUE )
		
		plot(out1$date, out1$DO, ylim = c( min_y, max_y))
		points(out1$date, out1$Cnow, col = 'grey50', pch = 3)
		legend ('topright', c ('Observed', 'Simulated'), pch = c ( 1,2), col = c ( 'black','grey50'), lwd = c ( 2,2), lty = c ( NA,NA))
		}


	#RMSE_out <- (mean(with(out1, (Cnow - DO)^2, na.rm = TRUE))  ^ 0.5
	RMSE_out <- with(out1, mean((Cnow - DO)^2, na.rm = TRUE))  ^ 0.5


	out1 <- list(out1)
	names(out1) <- 'output'

	out1$RMSE <- c('ER' = ER , 'Ks' = Ks, 'RMSE' = RMSE_out)

	#out1 = c('ER' = ER , 'Ks' = Ks, 'RMSE' = RMSE_out)

	#overwrite if all output is to be kept
	if(!l_keepoutput)
		{
		out1 = c('ER' = ER , 'Ks' = Ks, 'RMSE' = RMSE_out)
		#out1$RMSE <- c('ER' = ER , 'Ks' = Ks, 'RMSE' = RMSE_out)
		}


	return (out1)	
	}



#model_night <- ModelDOSingleNightRMSE01(indata = data_sub, in_params = in_params, l_keepoutput = FALSE, l_plot = FALSE)
#model_night <- ModelDOSingleNightRMSE01FreeInitial(indata = data_sub, in_params = in_params, l_keepoutput = FALSE, l_plot = FALSE)






































































ModelDOSingleNightRMSE01 <- function (indata, in_params, l_keepoutput = TRUE, l_plot = TRUE)
	{
	library (deSolve)
	library (O2)


	stopifnot ( c ( 'date', 'date1','DO_diff','DO_deficit', 'Temp', 'CSat') %in% names (indata))
	stopifnot ( names(in_params) == c('ER','Ks'))

	data (O2_sol)
	setwd ( dirdmp )

	#c_night,]
	#indata <- indata[indata$daynight1 == c_night,]
	#indata <- data_sub


	times_1 <- indata$date
	
	if(l_plot)
		{
		plot (times_1)
		}

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


	ReAerNight.env <- new.env()
	
	
	ReAerNight.env$Resp <- Resp2
	ReAerNight.env$Gas <- Gas2
	ReAer1 <- ReAerNight01


	ode.ReAer <- ode

	
	ReAerNight.env$input_O2 <- approxfun ( times_1_num, indata$CSat, rule = 1)
	ReAerNight.env$input_T <- approxfun ( times_1_num, indata$Temp, rule = 1)

	ER = as.numeric(in_params$ER)
	Ks = as.numeric(in_params$Ks)
	
	ReAerNight.env$resp_params <- quote ( list ( 'ER' = ER) )
	ReAerNight.env$gas_params <- quote ( list ( 'Ks' = Ks, 'C_sat' = C_sat,'Cnow' = Cnow ) )
	ReAerNight.env$indata <- indata

	environment(ReAer1) <- ReAerNight.env
	environment(ode.ReAer) <- ReAerNight.env

	print (ls(ReAerNight.env))
	
	
	y_ini <- c (Cnow = DO_initial)


	out <- ode.ReAer(y = y_ini, times = times_1_num, func = ReAer1, parms = NULL)
	out1 <- data.frame (out)
	out1$date <- indata$date
	out1$DO <- indata$DO
	#out1$daynight1 <- c_night
	
	
	
	if(l_plot)
		{
		max_y <- max (out1$Cnow, out1$DO, na.rm = TRUE )
		min_y <- min (out1$Cnow, out1$DO, na.rm = TRUE )
		
		plot(out1$date, out1$DO, ylim = c( min_y, max_y))
		points(out1$date, out1$Cnow, col = 'grey50', pch = 3)
		legend ('topright', c ('Observed', 'Simulated'), pch = c ( 1,2), col = c ( 'black','grey50'), lwd = c ( 2,2), lty = c ( NA,NA))
		}


	#RMSE_out <- (mean(with(out1, (Cnow - DO)^2, na.rm = TRUE))  ^ 0.5
	RMSE_out <- with(out1, mean((Cnow - DO)^2, na.rm = TRUE))  ^ 0.5


	out1 <- list(out1)
	names(out1) <- 'output'

	out1$RMSE <- c('ER' = ER , 'Ks' = Ks, 'RMSE' = RMSE_out)

	#out1 = c('ER' = ER , 'Ks' = Ks, 'RMSE' = RMSE_out)

	#overwrite if all output is to be kept
	if(!l_keepoutput)
		{
		out1 = c('ER' = ER , 'Ks' = Ks, 'RMSE' = RMSE_out)
		#out1$RMSE <- c('ER' = ER , 'Ks' = Ks, 'RMSE' = RMSE_out)
		}


	return (out1)	
	}



#model_night <- ModelDOSingleNightRMSE01(indata = data_sub, in_params = in_params, l_keepoutput = FALSE, l_plot = FALSE)

















































ModelDONightTime01 <- function (c_night = 'night0002', indata )
	{
	library (deSolve)
	library (O2)

	stopifnot ( c ( 'date', 'date1','PARraw','ER','Ks','DO_diff','DO_deficit', 'Temp', 'CSat') %in% names (indata))

	data (O2_sol)

	setwd ( dirdmp )

	indata <- indata[indata$daynight1 == c_night,]
	plot ( indata$date, indata$DO )


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


	ReAer.env <- new.env()
	
	
	ReAer.env$Phot <- function (light) 0
	ReAer.env$Resp <- Resp2
	ReAer.env$Gas <- Gas2
	ReAer1 <- ReAer


	ode.ReAer <- ode

	
	ReAer.env$input_O2 <- approxfun ( times_1_num, indata$CSat, rule = 1)
	ReAer.env$input_Ks <- approxfun ( times_1_num, indata$Ks, rule = 1)
	ReAer.env$input_ER <- approxfun ( times_1_num, indata$ER, rule = 1)
	ReAer.env$input_T <- approxfun ( times_1_num, indata$Temp, rule = 1)
	ReAer.env$input_light <- approxfun ( times_1_num, Bo0, rule = 1)
	
	#always returns zero
	ReAer.env$input_Pmax <- function (times_1_num) 0
	ReAer.env$input_Ik <- function (times_1_num) 0


	ReAer.env$photo_params <- quote ( list ( 'light' = light ))
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
	out1$daynight1 <- c_night
	
	
	max_y <- max (out1$Cnow, out1$DO, na.rm = TRUE )
	min_y <- min (out1$Cnow, out1$DO, na.rm = TRUE )
	
	plot(out1$date, out1$DO, ylim = c( min_y, max_y))
	points(out1$date, out1$Cnow, col = 'grey50', pch = 3)
	legend ('topright', c ('Observed', 'Simulated'), pch = c ( 1,2), col = c ( 'black','grey50'), lwd = c ( 2,2), lty = c ( NA,NA))

	return (out1)
	}


#x1 <- MakeERKsTimeSeries ('Ebble_CE1_2013_04_25', 'Ebble_CE1_2013_04_25_ER_Ks')
#xss <- ModelDONightTime(indata = x1, c_night = 'night0002')
#xss <- ModelDONightTime(indata = x1, c_night = 'night0003')


#x1 <- MakeERKsTimeSeries ('Ebble_CE1_2013_08_08', 'Ebble_CE1_2013_08_08_ER_Ks')
#xss <- ModelDONightTime(indata = x1, c_night = 'night0002')
#xss <- ModelDONightTime(indata = x1, c_night = 'night0003')




























































































































ModelDONightTime <- function (c_night = 'night0002', indata )
	{
	library (deSolve)
	library (O2)

	stopifnot ( c ( 'date', 'date1','PARraw','ER','Ks','DO_diff','DO_deficit', 'Temp', 'CSat') %in% names (indata))

	data (O2_sol)

	setwd ( dirdmp )

	indata <- indata[indata$daynight1 == c_night,]
	plot ( indata$date, indata$DO )


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


	ReAer.env <- new.env()
	
	
	ReAer.env$Phot <- function (light) 0
	ReAer.env$Resp <- Resp2
	ReAer.env$Gas <- Gas2
	ReAer1 <- ReAer


	ode.ReAer <- ode

	
	ReAer.env$input_O2 <- approxfun ( times_1_num, indata$CSat, rule = 1)
	ReAer.env$input_Ks <- approxfun ( times_1_num, indata$Ks, rule = 1)
	ReAer.env$input_ER <- approxfun ( times_1_num, indata$ER, rule = 1)
	ReAer.env$input_T <- approxfun ( times_1_num, indata$Temp, rule = 1)
	ReAer.env$input_light <- approxfun ( times_1_num, Bo0, rule = 1)
	
	#always returns zero
	ReAer.env$input_Pmax <- function (times_1_num) 0
	ReAer.env$input_Ik <- function (times_1_num) 0


	ReAer.env$photo_params <- quote ( list ( 'light' = light ))
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
	out1$daynight1 <- c_night
	
	
	max_y <- max (out1$Cnow, out1$DO, na.rm = TRUE )
	min_y <- min (out1$Cnow, out1$DO, na.rm = TRUE )
	
	plot(out1$date, out1$DO, ylim = c( min_y, max_y))
	points(out1$date, out1$Cnow, col = 'grey50', pch = 3)
	legend ('topright', c ('Observed', 'Simulated'), pch = c ( 1,2), col = c ( 'black','grey50'), lwd = c ( 2,2), lty = c ( NA,NA))

	return (out1)
	}


#x1 <- MakeERKsTimeSeries ('Ebble_CE1_2013_04_25', 'Ebble_CE1_2013_04_25_ER_Ks')
#xss <- ModelDONightTime(indata = x1, c_night = 'night0002')
#xss <- ModelDONightTime(indata = x1, c_night = 'night0003')


#x1 <- MakeERKsTimeSeries ('Ebble_CE1_2013_08_08', 'Ebble_CE1_2013_08_08_ER_Ks')
#xss <- ModelDONightTime(indata = x1, c_night = 'night0002')
#xss <- ModelDONightTime(indata = x1, c_night = 'night0003')
