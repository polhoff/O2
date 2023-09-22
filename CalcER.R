
CalcERSingleNight <- function (indata, which_return)
	{
	#ensure no stragglers, i.e. clean slate
	indata$ER <- NA
	
	#indata = inputToPhot
	#if (l_missinglight) indata$PARraw = NA
	#stopifnot ( c ('DO_diff', 'DO_deficit', 'PARraw', 'ER', 'Ks', 'date') %in% names (indata) )
	stopifnot ( c ('DO_diff', 'DO_deficit', 'ER', 'Ks', 'date') %in% names (indata) )
	stopifnot ( which_return %in% c('mean', 'median', 'dataset'))
	
	
	aeration <- indata$Ks * indata$DO_deficit 
	#respiration <- indata$ER
	
	indata$ER <- indata$DO_diff - aeration
	indata$aeration <- aeration

	mean.ER <- with(indata, mean(ER, na.rm = TRUE))
	median.ER <- with(indata, median(ER, na.rm = TRUE))


	c_vars <- c('date', 'date1', 'DO', 'CSat', 'Sunrise','Sunset','daynight', 'daynight1', 'DO_diff', 'DO_deficit', 'ER', 'Ks')
	x_ndx <- c_vars %in% names(indata)

	indata <- indata[c_vars[x_ndx]]

	if(which_return == 'mean')
		{
		return (mean.ER)
		}

	if(which_return == 'median')
		{
		return (median.ER)
		}
			
	if(which_return == 'dataset')
		{
		return (indata)
		}
	}


#CalcERSingleNight (indata, l_missinglight = TRUE, which_return = 1)


CalcERTimeSeries <- function (indata, which_return)
	{
	
	#night time only
	indata <- indata[indata$daynight == 'night',]
	
	#see how many nights there are
	c_nights <- unique(indata$daynight1)
	
	n_dim01 <- length(c_nights)
	
	out_matrix <- matrix(nrow = n_dim01, ncol = 3)
	out_df <- as.data.frame(out_matrix)
	names(out_df) <- c('daynight1','date1','ER')
	out_df$daynight1 <- c_nights
	out_df$date1 <- as.Date(out_df$date1)
	out_df$ER <- as.numeric(out_df$ER)



	for(i in 1:n_dim01)
		{
		data_sub <- indata[indata$daynight1 == c_nights[i],]
		
		
		ER <- CalcERSingleNight (data_sub, which_return)

		out_df$date1[i] <- tail(data_sub$date1,1)
		out_df$ER[i] <- ER
		}
	
	return (out_df)
	}

#x_test <- CalcERTimeSeries (ER_Ks_timeseries, which_return = 'median')
