
SplitProductionFunctionByTimeAfterSunrise <- function ( indata, timeinterval = 60, n_date, lon, lat, SiteCode, upperPmax = 0.2, upperIk = 1000 )
	{

	#n_date = as.Date ('2013-04-26'); indata = data_new; timeinterval = 60; SiteCode = 'CE1'
	
	if (missing (lon) | missing (lat))
		{
		lon <- try (LonLatSite ( SiteCode )['lon'])
		lat <- try (LonLatSite ( SiteCode )['lat'])
		}

	#stopifnot ( FALSE )
	stopifnot ( !missing (indata) | !missing (n_date) | !missing (lon) | !missing (lat))
	stopifnot(c("DO_diff", "DO_deficit", "PARraw", "ER", "Ks", "date", "date1") %in% names(indata))
	
	library (sun)

	#one day only
	indata <- indata[indata$date1 == n_date,]
	
	day_night <- DayNight ( indata$date, lon, lat )

	#datime only
	indata <- indata[day_night == 'day', ]

	sunrise <- Sunrise (indata$date, lon, lat )
	sunset <- Sunset (indata$date, lon, lat )
	
	#split_times <- seq ( sunrise, sunset + 60*60, by = (timeinterval * 60))
	split_times <- seq ( sunrise, sunset + (timeinterval*60), by = (timeinterval * 60))
	
	intervals <- cut(indata$date, split_times)
	intervals01 <- unique (intervals)
	c_intervals01 <- as.character (intervals01)


	param_matrix <- as.data.frame ( matrix (nrow = length (intervals01), ncol = 4))


	for ( i in 1:length(intervals01) )
		{
		data_sub <- indata[intervals == intervals01[i], ]
		param_matrix[i,1] = c_intervals01[i]
		param_matrix[i,2] = c_intervals01[i+1]
		#param_matrix[i,3:4] = as.matrix (try (FitProductionCurve ( indata = data_sub, which_return = 1 )))
		param_matrix[i,3:4] = try (FitProductionCurve ( indata = data_sub, upper_Pmax = upperPmax, upper_Ik = upperIk, which_return = 1 ))
		#assign ( paste ('x_', Labels1()[i], sep = ''), x)
		}
	
	
	names (param_matrix) = c ( 'StartPeriod','EndPeriod','Pmax','Ik')
	param_matrix$StartPeriod = as.POSIXct (param_matrix$StartPeriod)
	param_matrix$EndPeriod = as.POSIXct (param_matrix$EndPeriod)
	param_matrix$EndPeriod[i] = sunset

	
	#numeric value in seconds
	param_matrix$Duration =  as.numeric(param_matrix$EndPeriod) - as.numeric (param_matrix$StartPeriod)
	param_matrix$MidPeriod =  param_matrix$StartPeriod + param_matrix$Duration / 2
	#param_matrix$MidPeriod =  (param_matrix$EndPeriod + param_matrix$StartPeriod) / 2
	
	param_matrix <- param_matrix[ c ( 'StartPeriod', 'EndPeriod', 'Duration', 'MidPeriod', 'Pmax', 'Ik')]


	#warning (param_matrix$Pmax > 0.5 * upperPmax )
	if ( max (param_matrix$Pmax) > 0.9 * upperPmax |  max (param_matrix$Ik) > 0.9 * upperIk )
		{
		print ('WARNING!!!!!!!!!!!!!!!!!!!!')
		print ('Examine function parameters upperPmax and upperIk. One is probably too low.')
		print ('Examine function parameters upperPmax and upperIk. One is probably too low.')
		print ('Examine function parameters upperPmax and upperIk. One is probably too low.')
		print ('Examine function parameters upperPmax and upperIk. One is probably too low.')
		}


	return (param_matrix)
	}
