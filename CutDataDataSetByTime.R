
CutDataDataSetByTime <- function ( indata, timeinterval = 60, n_date, lon, lat, SiteCode )
	{

	#n_date = as.Date ('2013-04-26'); indata = data_new; timeinterval = 60; SiteCode = 'CE1'
	
	if (missing (lon) | missing (lat))
		{
		lon <- try (LonLatSite ( SiteCode )['lon'])
		lat <- try (LonLatSite ( SiteCode )['lat'])
		}

	#stopifnot ( FALSE )
	stopifnot ( !missing (indata) | !missing (n_date) | !missing (lon) | !missing (lat))
	stopifnot ( class (n_date) == 'Date' )
	stopifnot(c("DO_diff", "DO_deficit", "PARraw", "ER", "Ks", "date", "date1") %in% names(indata))
	stopifnot ( n_date %in% indata$date1 )
	
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

	indata$splitpoint <- intervals
	
	return (indata)
	}
