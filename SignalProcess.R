
DOMove <- function (indata, n_move = 20)
	{
	library(parker)
	indata$DO <- MoveAv (indata$DO, n_1 = n_move)
	

	#differences for regression analysis
	indata$DO_deficit = with ( indata, CSat - DO )
	indata$DO_diff = c (NA, diff (indata$DO))
	indata$DO_diff_lag = c ( diff (indata$DO), NA)
	indata$DO_diff_mean = ( indata$DO_diff + indata$DO_diff_lag ) / 2
	indata$CSat_diff = c (NA, diff (indata$CSat))
	indata$Temp_diff = c (NA, diff (indata$Temp))
	#DO_diff = c (diff (DO_change$DO), NA)


	indata$DO_diff_diff = c(NA, diff(indata$DO_diff))
	indata$DO_diff_diff_lag = c ( diff (indata$DO), NA)

	outdata <- indata[c('date','date1','DO','CSat','Temp','DO_deficit', 'DO_diff', 'DO_diff_lag', 'DO_diff_mean', 'CSat_diff', 'Temp_diff', 'DO_diff_diff', 'DO_diff_diff_lag', 'daynight', 'daynight1', 'Sunrise', 'Sunset', 'SolarNoon')]
	
	return(outdata)
	}








































DOMoveWithTemp <- function (indata, n_move = 15, l_calccsat = TRUE)
	{
	library(parker)
	library(O2)
	data(O2_sol)
	
	indata$DO.raw <- indata$DO
	indata$CSat.raw <- indata$CSat
	
	
	indata$DO <- try(MoveAv (indata$DO, n_1 = n_move))
	indata$Temp <- try(MoveAv (indata$Temp, n_1 = n_move))

	#this takes along time and anyway the result (CSat moving average) is little different from the raw CSat (
	if(l_calccsat)
		{
		
		indata$CSat <- NA
		indata$CSat <- CalcCsat(indata)


		ratio <- with(indata, CSat/CSat_raw)
		x_ndx <- indata$CSat
		x_ndx[] <- FALSE
		
		x_ndx[ratio < 0.98 | ratio > 1.02] <- TRUE
		print(table(x_ndx))
		indata$CSat[x_ndx] <- NA
		}

	
	#differences for regression analysis
	indata$DO_deficit = with ( indata, CSat - DO )
	indata$DO_diff = c (NA, diff (indata$DO))
	indata$DO_diff_lag = c ( diff (indata$DO), NA)
	indata$DO_diff_mean = ( indata$DO_diff + indata$DO_diff_lag ) / 2
	indata$CSat_diff = c (NA, diff (indata$CSat))
	indata$Temp_diff = c (NA, diff (indata$Temp))
	#DO_diff = c (diff (DO_change$DO), NA)


	indata$DO_diff_diff = c(NA, diff(indata$DO_diff))
	indata$DO_diff_diff_lag = c ( diff (indata$DO), NA)


	outdata <- indata[c('date','date1','DO','CSat','Temp','DO_deficit', 'DO_diff', 'DO_diff_lag', 'DO_diff_mean', 'CSat_diff', 'Temp_diff', 'DO_diff_diff', 'DO_diff_diff_lag', 'daynight', 'daynight1', 'Sunrise', 'Sunset', 'SolarNoon', 'DO.raw','CSat.raw')]
	
	if(l_calccsat){
		outdata <- try(indata[c('date','date1','DO','CSat','CSat_raw','Temp','DO_deficit', 'DO_diff', 'DO_diff_lag', 'DO_diff_mean', 'CSat_diff', 'Temp_diff', 'DO_diff_diff', 'DO_diff_diff_lag', 'daynight', 'daynight1', 'Sunrise', 'Sunset', 'SolarNoon')])
		}
	
	return(outdata)
	}


#xxxx <- DOMoveWithTemp(indata)
























































DOMoveLocfit <- function (indata, n_tol = 0.001)
	{
	library(locfit)
	library(parker)
	
	
	smoothedDO <- with(indata, locfit(DO ~ lp(date, deg = 3, nn = n_tol)))
	indata$DO <- fitted(smoothedDO)
	
	
	#differences for regression analysis
	indata$DO_deficit = with ( indata, CSat - DO )
	indata$DO_diff = c (NA, diff (indata$DO))
	indata$DO_diff_lag = c ( diff (indata$DO), NA)
	indata$DO_diff_mean = ( indata$DO_diff + indata$DO_diff_lag ) / 2
	indata$CSat_diff = c (NA, diff (indata$CSat))
	indata$Temp_diff = c (NA, diff (indata$Temp))
	#DO_diff = c (diff (DO_change$DO), NA)


	indata$DO_diff_diff = c(NA, diff(indata$DO_diff))
	indata$DO_diff_diff_lag = c ( diff (indata$DO), NA)

	outdata <- indata[c('date','date1','DO','CSat','Temp','DO_deficit', 'DO_diff', 'DO_diff_lag', 'DO_diff_mean', 'CSat_diff', 'Temp_diff', 'DO_diff_diff', 'DO_diff_diff_lag', 'daynight', 'daynight1', 'Sunrise')]
	
	return(outdata)
	}






