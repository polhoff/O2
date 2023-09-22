
FitProductionCurveMulti <- function ( indata1, indata2, which_return = 1, lower_Pmax = 0.01, upper_Pmax = 0.1, lower_Ik = 20, upper_Ik = 500 )
	{
	library(rovelli)
	data_temp <- do.call ( data, list (indata1))
	print ( head (data_temp))
	n_dates <- unique ( as.Date ( data_temp$date))
	parameter_df <- as.data.frame (matrix (nrow = length (n_dates), ncol = 3))
	
	for ( i in 1:length (n_dates) )
		{
		#compute O2 production from photosynthesis
		#data01 <- PreProcessDOPhot (c_indata1 = 'Ebble_CE1_2013_08_08', c_indata2 = 'ER_Ks_timeseries')
		#data01 <- MakeERKsTimeSeries ('Ebble_CE1_2013_08_08', 'Ebble_CE1_2013_08_08_ER_Ks')
		data01 <- MakeERKsTimeSeries ( indata1, indata2 )

		data02 <- CutDataDataSetByTime (indata = data01, timeinterval = 120, n_date = n_dates[i],  lon = -1.92392, lat = 51.02816)
		
		data03 <- CalcDOPhot (data02)
		with ( data03, plot (  PARraw, DO_Phot ))
		
		x_fit <- FitProductionCurve ( data02, which_return = 1, lower_Pmax = 0.01, upper_Pmax = 0.1, lower_Ik = 20, upper_Ik = 500 )
		print (x_fit)
		
		parameter_df[i,2:3] <- x_fit
		}
	
	parameter_df[,1] <- n_dates
	names (parameter_df) <- c ('date1','Pmax','I_k')
	return (parameter_df)
	}


#FitProductionCurveMulti ( indata1 = 'Ebble_CE1_2013_08_08', indata2 = 'Ebble_CE1_2013_08_08_ER_Ks')
