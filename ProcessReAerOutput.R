
ProcessReAerOutput <- function ( indata )
	{
	#indata <- out_ModelDayNight

	indata$CSat <- try(indata$C_sat)
	indata$DO <- try(indata$Cnow)
	try (indata$Temp <- indata$T)



	#differences for regression analysis
	indata$DO_deficit = with ( indata, CSat - DO )
	indata$DO_diff = c (NA, diff (indata$DO))
	indata$DO_diff_lag = c ( diff (indata$DO), NA)
	indata$DO_diff_mean =  ( indata$DO_diff + indata$DO_diff_lag ) / 2
	indata$CSat_diff =  c (NA, diff (indata$CSat))
	indata$Temp_diff =  c (NA, diff (indata$Temp))
	#DO_diff = c (diff (DO_change$DO), NA)


	indata$DO_diff_diff = c(NA, diff(indata$DO_diff))
	indata$DO_diff_diff_lag = c ( diff (indata$DO), NA)

	indata$mv_avg_diff <- with ( indata, filter (DO_diff, rep(1/10,10), sides=2))

	return(indata)
	}

#x222 <- ProcessReAerOutput(out_ModelDayNight)
