
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

	outdata <- indata[c('date','date1','DO','CSat','Temp','DO_deficit', 'DO_diff', 'DO_diff_lag', 'DO_diff_mean', 'CSat_diff', 'Temp_diff', 'DO_diff_diff', 'DO_diff_diff_lag', 'daynight', 'daynight1', 'Sunrise')]
	
	return(outdata)
	}
