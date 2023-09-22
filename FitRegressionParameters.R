

FitRegressionParameters <- function ( c_indata, n_quality = 2 )
	{
	#indata <- Ebble_CE1_2013_04_25
	#library (mdot); data (Minidot_02); indata <- Minidot_02; n_quality = 14; c_indata <- 'Minidot_02'

	#dirscr <- paste ( dirtop, '/DO/rovscript/', sep = '')

	
	library (parker)
	data (parker)
	
	do.call ( data, list ( c_indata ))
	indata <- get (c_indata)
	
	
	if ( !is.null(indata$qualityFilter))
		{
		indata <- indata[indata$qualityFilter > n_quality,]
		}


	print ( head (indata))
	print ( str (indata))

	#max_y <- max ( indata$Cnow, indata$DO, na.rm = T )
	#min_y <- min ( indata$Cnow, indata$DO, na.rm = T )


	x_date = unique ( as.Date (indata$date1))

	

	x_nights = unique ( indata$daynight1 )
	x_nights = x_nights[grep ('night', x_nights)]
	
	m_coeffs = matrix (nrow = length (x_nights), ncol = 3 )
	

	for ( i in 1:length (x_nights) )
		{
		
		#i = 1
		x_ndx = x_nights[i]
		
		data_sub_ndx = which ( indata$daynight1 == x_ndx )
		
		data_sub = indata[data_sub_ndx, ]
		
		x_ndx1 = which ( !is.na (data_sub$DO_deficit))
		
		data_sub = data_sub[x_ndx1, ]
		
		#with ( data_sub, plot ( DO_deficit, DO_change ))
		n_cor <- cor(data_sub$DO_diff,data_sub$DO_deficit)
		
		summary ( data_sub$DO_diff )
		summary ( data_sub$DO_deficit )
		fit <- try ( lm ( data_sub$DO_diff ~ data_sub$DO_deficit ))

		n_inter <-  as.numeric (fit[[1]][1])
		n_slope <-  as.numeric (fit[[1]][2])


		m_coeffs[i,1] <- n_inter
		m_coeffs[i,2] <- n_slope
		m_coeffs[i,3] <- n_cor
		}


	#Ebble_CE1_2013_04_25
	x_temp <- data.frame ( x_date[1:length (x_nights)], x_nights, m_coeffs )
	names (x_temp) <- c ('date1', 'daynight1','ER','Ks', 'correlation')
	assign ( paste (c_indata, '_ER_Ks_for', sep = ''), x_temp)






































	m_coeffs = matrix (nrow = length (x_nights), ncol = 3 )


	for ( i in 1:length (x_nights) )
		{
		
		#i = 1
		x_ndx = x_nights[i]
		data_sub_ndx = which ( indata$daynight1 == x_ndx )
		data_sub = indata[data_sub_ndx, ]
		x_ndx1 = which ( !is.na (data_sub$DO_deficit))
		data_sub = data_sub[x_ndx1, ]
		
		#with ( data_sub, plot ( DO_deficit, DO_change ))
		n_cor <- cor(data_sub$DO_diff_mean,data_sub$DO_deficit)
		fit <- try ( lm ( data_sub$DO_diff_mean ~ data_sub$DO_deficit ))

		
		n_inter <-  as.numeric (fit[[1]][1])
		n_slope <-  as.numeric (fit[[1]][2])


		m_coeffs[i,1] <- n_inter
		m_coeffs[i,2] <- n_slope
		m_coeffs[i,3] <- n_cor
		}


	x_temp <- data.frame ( x_date[1:length (x_nights)], x_nights, m_coeffs )
	names (x_temp) <- c ('date1', 'daynight1','ER','Ks', 'correlation')
	assign ( paste (c_indata, '_ER_Ks_mid', sep = ''), x_temp)

























































	m_coeffs = matrix (nrow = length (x_nights), ncol = 3 )


	for ( i in 1:length (x_nights) )
		{
		
		#i = 1
		x_ndx = x_nights[i]
		data_sub_ndx = which ( indata$daynight1 == x_ndx )
		data_sub = indata[data_sub_ndx, ]
		x_ndx1 = which ( !is.na (data_sub$DO_deficit))
		data_sub = data_sub[x_ndx1, ]
		
		#with ( data_sub, plot ( DO_deficit, DO_change ))
		n_cor <- cor(data_sub$DO_diff_lag,data_sub$DO_deficit)
		fit <- try ( lm ( data_sub$DO_diff_lag ~ data_sub$DO_deficit ))

		
		n_inter <-  as.numeric (fit[[1]][1])
		n_slope <-  as.numeric (fit[[1]][2])


		m_coeffs[i,1] <- n_inter
		m_coeffs[i,2] <- n_slope
		m_coeffs[i,3] <- n_cor
		}


	x_temp <- data.frame ( x_date[1:length (x_nights)], x_nights, m_coeffs )
	names (x_temp) <- c ('date1', 'daynight1','ER','Ks', 'correlation')
	assign ( paste (c_indata, '_ER_Ks_lag', sep = ''), x_temp)







	DailyData_ER_Ks <- MakeDailyData(indata)
	


	x_ls <- ls() [grep ( ls(), pattern = '_ER_Ks' )]
	print ( x_ls )

	setwd (dirdmp)
	save (list =  x_ls, file =  paste ( c_indata, '_ER_Ks.rda', sep = ''))
}


#FitRegressionParameters ( c_indata = 'Ebble_CE1_2013_04_25' )
#FitRegressionParameters ( c_indata = 'Ebble_CE1_2013_08_08' )
























































































































FitRegressionParametersUseLib <- function ( c_indata, c_library, n_quality = 2 )
	{
	#indata <- Ebble_CE1_2013_04_25
	#library (mdot); data (Minidot_02); indata <- Minidot_02; n_quality = 14; c_indata <- 'Minidot_02'

	#dirscr <- paste ( dirtop, '/DO/rovscript/', sep = '')
	
	
	library (parker)
	data (parker)
	
	indata <- GetIndata (c_indata, c_library)
	
	
	if ( !is.null(indata$qualityFilter))
		{
		indata <- indata[indata$qualityFilter > n_quality,]
		}


	print ( head (indata))
	print ( str (indata))

	#max_y <- max ( indata$Cnow, indata$DO, na.rm = T )
	#min_y <- min ( indata$Cnow, indata$DO, na.rm = T )


	x_date = unique ( as.Date (indata$date1))

	

	x_nights = unique ( indata$daynight1 )
	x_nights = x_nights[grep ('night', x_nights)]
	
	m_coeffs = matrix (nrow = length (x_nights), ncol = 3 )
	

	for ( i in 1:length (x_nights) )
		{
		
		#i = 1
		x_ndx = x_nights[i]
		
		data_sub_ndx = which ( indata$daynight1 == x_ndx )
		
		data_sub = indata[data_sub_ndx, ]
		
		x_ndx1 = which ( !is.na (data_sub$DO_deficit))
		
		data_sub = data_sub[x_ndx1, ]
		
		#with ( data_sub, plot ( DO_deficit, DO_change ))
		n_cor <- cor(data_sub$DO_diff,data_sub$DO_deficit)
		
		summary ( data_sub$DO_diff )
		summary ( data_sub$DO_deficit )
		fit <- try ( lm ( data_sub$DO_diff ~ data_sub$DO_deficit ))

		n_inter <-  as.numeric (fit[[1]][1])
		n_slope <-  as.numeric (fit[[1]][2])


		m_coeffs[i,1] <- n_inter
		m_coeffs[i,2] <- n_slope
		m_coeffs[i,3] <- n_cor
		}


	#Ebble_CE1_2013_04_25
	x_temp <- data.frame ( x_date[1:length (x_nights)], x_nights, m_coeffs )
	names (x_temp) <- c ('date1', 'daynight1','ER','Ks', 'correlation')
	assign ( paste (c_indata, '_ER_Ks_for', sep = ''), x_temp)






































	m_coeffs = matrix (nrow = length (x_nights), ncol = 3 )


	for ( i in 1:length (x_nights) )
		{
		
		#i = 1
		x_ndx = x_nights[i]
		data_sub_ndx = which ( indata$daynight1 == x_ndx )
		data_sub = indata[data_sub_ndx, ]
		x_ndx1 = which ( !is.na (data_sub$DO_deficit))
		data_sub = data_sub[x_ndx1, ]
		
		#with ( data_sub, plot ( DO_deficit, DO_change ))
		n_cor <- cor(data_sub$DO_diff_mean,data_sub$DO_deficit)
		fit <- try ( lm ( data_sub$DO_diff_mean ~ data_sub$DO_deficit ))

		
		n_inter <-  as.numeric (fit[[1]][1])
		n_slope <-  as.numeric (fit[[1]][2])


		m_coeffs[i,1] <- n_inter
		m_coeffs[i,2] <- n_slope
		m_coeffs[i,3] <- n_cor
		}


	x_temp <- data.frame ( x_date[1:length (x_nights)], x_nights, m_coeffs )
	names (x_temp) <- c ('date1', 'daynight1','ER','Ks', 'correlation')
	assign ( paste (c_indata, '_ER_Ks_mid', sep = ''), x_temp)

























































	m_coeffs = matrix (nrow = length (x_nights), ncol = 3 )


	for ( i in 1:length (x_nights) )
		{
		
		#i = 1
		x_ndx = x_nights[i]
		data_sub_ndx = which ( indata$daynight1 == x_ndx )
		data_sub = indata[data_sub_ndx, ]
		x_ndx1 = which ( !is.na (data_sub$DO_deficit))
		data_sub = data_sub[x_ndx1, ]
		
		#with ( data_sub, plot ( DO_deficit, DO_change ))
		n_cor <- cor(data_sub$DO_diff_lag,data_sub$DO_deficit)
		fit <- try ( lm ( data_sub$DO_diff_lag ~ data_sub$DO_deficit ))

		
		n_inter <-  as.numeric (fit[[1]][1])
		n_slope <-  as.numeric (fit[[1]][2])


		m_coeffs[i,1] <- n_inter
		m_coeffs[i,2] <- n_slope
		m_coeffs[i,3] <- n_cor
		}


	x_temp <- data.frame ( x_date[1:length (x_nights)], x_nights, m_coeffs )
	names (x_temp) <- c ('date1', 'daynight1','ER','Ks', 'correlation')
	assign ( paste (c_indata, '_ER_Ks_lag', sep = ''), x_temp)





















	x_ls <- ls() [grep ( ls(), pattern = '_ER_Ks' )]
	print ( x_ls )

	setwd (dirdmp)
	save (list =  x_ls, file =  paste ( c_indata, '_ER_Ks.rda', sep = ''))
}


#FitRegressionParameters ( c_indata = 'Ebble_CE1_2013_04_25' )
#FitRegressionParameters ( c_indata = 'Ebble_CE1_2013_08_08' )







































































































































































































































































































































FitRegressionParametersDataSet <- function ( indata, c_name, n_quality = 2 )
	{
	#indata <- Ebble_CE1_2013_04_25
	#library (mdot); data (Minidot_02); indata <- Minidot_02; n_quality = 14; c_indata <- 'Minidot_02'

	#dirscr <- paste ( dirtop, '/DO/rovscript/', sep = '')
	
	#library(mdot)
	library (parker)
	data (parker)
	
	
	
	
	
	if ( !is.null(indata$qualityFilter))
		{
		indata <- indata[indata$qualityFilter > n_quality,]
		}


	print ( head (indata))
	print ( str (indata))

	#max_y <- max ( indata$Cnow, indata$DO, na.rm = T )
	#min_y <- min ( indata$Cnow, indata$DO, na.rm = T )


	x_date = unique ( as.Date (indata$date1))

	

	x_nights = unique ( indata$daynight1 )
	x_nights = x_nights[grep ('night', x_nights)]
	
	m_coeffs = matrix (nrow = length (x_nights), ncol = 3 )
	

	for ( i in 1:length (x_nights) )
		{
		
		#i = 1
		x_ndx = x_nights[i]
		
		data_sub_ndx = which ( indata$daynight1 == x_ndx )
		
		data_sub = indata[data_sub_ndx, ]
		
		x_ndx1 = which ( !is.na (data_sub$DO_deficit))
		
		data_sub = data_sub[x_ndx1, ]
		
		#with ( data_sub, plot ( DO_deficit, DO_change ))
		n_cor <- cor(data_sub$DO_diff,data_sub$DO_deficit)
		
		summary ( data_sub$DO_diff )
		summary ( data_sub$DO_deficit )
		fit <- try ( lm ( data_sub$DO_diff ~ data_sub$DO_deficit ))

		n_inter <-  as.numeric (fit[[1]][1])
		n_slope <-  as.numeric (fit[[1]][2])


		m_coeffs[i,1] <- n_inter
		m_coeffs[i,2] <- n_slope
		m_coeffs[i,3] <- n_cor
		}


	#Ebble_CE1_2013_04_25
	x_temp <- data.frame ( x_date[1:length (x_nights)], x_nights, m_coeffs )
	names (x_temp) <- c ('date1', 'daynight1','ER','Ks', 'correlation')
	assign ( paste (c_name, '_ER_Ks_for', sep = ''), x_temp)






































	m_coeffs = matrix (nrow = length (x_nights), ncol = 3 )


	for ( i in 1:length (x_nights) )
		{
		
		#i = 1
		x_ndx = x_nights[i]
		data_sub_ndx = which ( indata$daynight1 == x_ndx )
		data_sub = indata[data_sub_ndx, ]
		x_ndx1 = which ( !is.na (data_sub$DO_deficit))
		data_sub = data_sub[x_ndx1, ]
		
		#with ( data_sub, plot ( DO_deficit, DO_change ))
		n_cor <- cor(data_sub$DO_diff_mean,data_sub$DO_deficit)
		fit <- try ( lm ( data_sub$DO_diff_mean ~ data_sub$DO_deficit ))

		
		n_inter <-  as.numeric (fit[[1]][1])
		n_slope <-  as.numeric (fit[[1]][2])


		m_coeffs[i,1] <- n_inter
		m_coeffs[i,2] <- n_slope
		m_coeffs[i,3] <- n_cor
		}


	x_temp <- data.frame ( x_date[1:length (x_nights)], x_nights, m_coeffs )
	names (x_temp) <- c ('date1', 'daynight1','ER','Ks', 'correlation')
	assign ( paste (c_name, '_ER_Ks_mid', sep = ''), x_temp)

























































	m_coeffs = matrix (nrow = length (x_nights), ncol = 3 )


	for ( i in 1:length (x_nights) )
		{
		
		#i = 1
		x_ndx = x_nights[i]
		data_sub_ndx = which ( indata$daynight1 == x_ndx )
		data_sub = indata[data_sub_ndx, ]
		x_ndx1 = which ( !is.na (data_sub$DO_deficit))
		data_sub = data_sub[x_ndx1, ]
		
		#with ( data_sub, plot ( DO_deficit, DO_change ))
		n_cor <- cor(data_sub$DO_diff_lag,data_sub$DO_deficit)
		fit <- try ( lm ( data_sub$DO_diff_lag ~ data_sub$DO_deficit ))

		
		n_inter <-  as.numeric (fit[[1]][1])
		n_slope <-  as.numeric (fit[[1]][2])


		m_coeffs[i,1] <- n_inter
		m_coeffs[i,2] <- n_slope
		m_coeffs[i,3] <- n_cor
		}


	x_temp <- data.frame ( x_date[1:length (x_nights)], x_nights, m_coeffs )
	names (x_temp) <- c ('date1', 'daynight1','ER','Ks', 'correlation')
	assign ( paste (c_name, '_ER_Ks_lag', sep = ''), x_temp)





















	x_ls <- ls() [grep ( ls(), pattern = '_ER_Ks' )]
	print ( x_ls )

	setwd (dirdmp)
	save (list =  x_ls, file =  paste ( c_indata, '_ER_Ks.rda', sep = ''))
}


#FitRegressionParametersDataSet ( c_indata = 'Ebble_CE1_2013_04_25' )
#FitRegressionParametersDataSet ( c_indata = 'Ebble_CE1_2013_08_08' )




















































































#added temperature as output

FitRegressionParametersDataSet01 <- function ( indata, c_name, which_return )
	{
	#indata <- Ebble_CE1_2013_04_25
	#library (mdot); data (Minidot_02); indata <- Minidot_02; n_quality = 14; c_indata <- 'Minidot_02'

	#dirscr <- paste ( dirtop, '/DO/rovscript/', sep = '')
	
	#library(mdot)
	library (parker)
	data (parker)


	print ( head (indata))
	print ( str (indata))

	#max_y <- max ( indata$Cnow, indata$DO, na.rm = T )
	#min_y <- min ( indata$Cnow, indata$DO, na.rm = T )


	x_date = unique ( as.Date (indata$date1))

	

	x_nights = unique ( indata$daynight1 )
	x_nights = x_nights[grep ('night', x_nights)]
	
	m_coeffs = matrix (nrow = length (x_nights), ncol = 5 )
	

	for ( i in 1:length (x_nights) )
		{
		
		#i = 1
		x_ndx = x_nights[i]
		
		data_sub_ndx = which ( indata$daynight1 == x_ndx )
		
		data_sub = indata[data_sub_ndx, ]
		
		x_ndx1 = which ( !is.na (data_sub$DO_deficit))
		
		data_sub = data_sub[x_ndx1, ]
		
		#with ( data_sub, plot ( DO_deficit, DO_change ))
		n_cor <- cor(data_sub$DO_diff,data_sub$DO_deficit)
		
		summary ( data_sub$DO_diff )
		summary ( data_sub$DO_deficit )
		fit <- try ( lm ( data_sub$DO_diff ~ data_sub$DO_deficit ))

		n_inter <-  as.numeric (fit[[1]][1])
		n_slope <-  as.numeric (fit[[1]][2])


		m_coeffs[i,1] <- n_inter
		m_coeffs[i,2] <- n_slope
		m_coeffs[i,3] <- n_cor
		m_coeffs[i,4] <- mean (data_sub$Temp, na.rm = TRUE)
		m_coeffs[i,5] <- median (data_sub$Temp, na.rm = TRUE)
	
		}


	#Ebble_CE1_2013_04_25
	x_temp <- data.frame ( x_date[1:length (x_nights)], x_nights, m_coeffs )
	names (x_temp) <- c ('date1', 'daynight1','ER','Ks', 'correlation', 'mean.Temp','median.Temp')
	assign ( paste (c_name, '_ER_Ks_for', sep = ''), x_temp)

	if(which_return == 1) return_data = x_temp




































	m_coeffs = matrix (nrow = length (x_nights), ncol = 5 )


	for ( i in 1:length (x_nights) )
		{
		
		#i = 1
		x_ndx = x_nights[i]
		data_sub_ndx = which ( indata$daynight1 == x_ndx )
		data_sub = indata[data_sub_ndx, ]
		x_ndx1 = which ( !is.na (data_sub$DO_deficit))
		data_sub = data_sub[x_ndx1, ]
		
		#with ( data_sub, plot ( DO_deficit, DO_change ))
		n_cor <- cor(data_sub$DO_diff_mean,data_sub$DO_deficit)
		fit <- try ( lm ( data_sub$DO_diff_mean ~ data_sub$DO_deficit ))

		
		n_inter <-  as.numeric (fit[[1]][1])
		n_slope <-  as.numeric (fit[[1]][2])


		m_coeffs[i,1] <- n_inter
		m_coeffs[i,2] <- n_slope
		m_coeffs[i,3] <- n_cor
		m_coeffs[i,4] <- mean (data_sub$Temp, na.rm = TRUE)
		m_coeffs[i,5] <- median (data_sub$Temp, na.rm = TRUE)
	
		}


	x_temp <- data.frame ( x_date[1:length (x_nights)], x_nights, m_coeffs )
	names (x_temp) <- c ('date1', 'daynight1','ER','Ks', 'correlation', 'mean.Temp','median.Temp')
	assign ( paste (c_name, '_ER_Ks_mid', sep = ''), x_temp)

	if(which_return == 2) return_data = x_temp























































	m_coeffs = matrix (nrow = length (x_nights), ncol = 5 )


	for ( i in 1:length (x_nights) )
		{
		
		#i = 1
		x_ndx = x_nights[i]
		data_sub_ndx = which ( indata$daynight1 == x_ndx )
		data_sub = indata[data_sub_ndx, ]
		x_ndx1 = which ( !is.na (data_sub$DO_deficit))
		data_sub = data_sub[x_ndx1, ]
		
		#with ( data_sub, plot ( DO_deficit, DO_change ))
		n_cor <- cor(data_sub$DO_diff_lag,data_sub$DO_deficit)
		fit <- try ( lm ( data_sub$DO_diff_lag ~ data_sub$DO_deficit ))

		
		n_inter <-  as.numeric (fit[[1]][1])
		n_slope <-  as.numeric (fit[[1]][2])


		m_coeffs[i,1] <- n_inter
		m_coeffs[i,2] <- n_slope
		m_coeffs[i,3] <- n_cor
		m_coeffs[i,4] <- mean (data_sub$Temp, na.rm = TRUE)
		m_coeffs[i,5] <- median (data_sub$Temp, na.rm = TRUE)
	
		}


	x_temp <- data.frame ( x_date[1:length (x_nights)], x_nights, m_coeffs )
	names (x_temp) <- c ('date1', 'daynight1','ER','Ks', 'correlation', 'mean.Temp','median.Temp')
	assign ( paste (c_name, '_ER_Ks_lag', sep = ''), x_temp)

	if(which_return == 3) return_data = x_temp



















	return (return_data)
	
}


#FitRegressionParametersDataSet01 ( c_indata = 'Ebble_CE1_2013_04_25' )
#FitRegressionParametersDataSet01 ( c_indata = 'Ebble_CE1_2013_08_08' )
























































































#wrapper

FitRegressionParametersDataSetStd <- function (indata, dawn_cutoff = 60 )
	{
	dawn_cutoff <- dawn_cutoff
	indata <- indata
	outdata <- FitRegressionParametersDataSetStd02(indata, dawn_cutoff)
	return (outdata)
	}









































































































FitRegressionParametersDataSetStd01 <- function (indata)
	{
	#indata <- Ebble_CE1_2013_04_25
	#library (mdot); data (Minidot_02); indata <- Minidot_02; n_quality = 14; c_indata <- 'Minidot_02'

	#dirscr <- paste ( dirtop, '/DO/rovscript/', sep = '')
	
	#library(mdot)
	library (parker)
	data (parker)


	indata_sub <- NormalizeIndata(indata)

	x_date = unique ( as.Date (indata_sub$date1))
	

	x_nights = unique ( indata_sub$daynight1 )
	x_nights = x_nights[grep ('night', x_nights)]
	
	m_coeffs = matrix (nrow = length (x_nights), ncol = 5 )
	

	for ( i in 1:length (x_nights) )
		{
		
		#i = 1
		x_ndx = x_nights[i]
		
		data_sub_ndx = which ( indata_sub$daynight1 == x_ndx )
		
		data_sub = indata_sub[data_sub_ndx, ]
		
		x_ndx1 = which ( !is.na (data_sub$DO_deficit_mean))
		
		data_sub = data_sub[x_ndx1, ]
		
		#with ( data_sub, plot ( DO_deficit, DO_change ))
		n_cor <- cor(data_sub$DO_diff_std,data_sub$DO_deficit_mean)
		
		fit <- try ( lm ( data_sub$DO_diff_std ~ data_sub$DO_deficit_mean ))

		n_inter <-  as.numeric (fit[[1]][1])
		n_slope <-  as.numeric (fit[[1]][2])


		m_coeffs[i,1] <- n_inter
		m_coeffs[i,2] <- n_slope
		m_coeffs[i,3] <- n_cor
		m_coeffs[i,4] <- mean (data_sub$Temp, na.rm = TRUE)
		m_coeffs[i,5] <- median (data_sub$Temp, na.rm = TRUE)
	
		}


	#Ebble_CE1_2013_04_25
	outdata <- data.frame ( x_date[1:length (x_nights)], x_nights, m_coeffs )
	names (outdata) <- c ('date1', 'daynight1','ER','Ks', 'correlation', 'mean.Temp','median.Temp')

	return (outdata)
	
}


#FitRegressionParametersDataSetStd ( c_indata = 'Ebble_CE1_2013_04_25' )
#FitRegressionParametersDataSetStd ( c_indata = 'Ebble_CE1_2013_08_08' )















































































































FitRegressionParametersDataSetStd02 <- function (indata, dawn_cutoff = 60)
	{

	#this differs from FitRegressionParametersDataSetStd01 by cutting of the last hour before dawn
	#and therefore hopefully getting rid of spurious data
	
	#indata <- Ebble_CE1_2013_04_25
	#library (mdot); data (Minidot_02); indata <- Minidot_02; n_quality = 14; c_indata <- 'Minidot_02'

	#dirscr <- paste ( dirtop, '/DO/rovscript/', sep = '')
	
	#library(mdot)
	library (parker)
	data (parker)


	indata_sub <- NormalizeIndata(indata)
	
	indata_sub$dawn <- indata_sub$daynight
	dawn_ndx <- with (indata_sub, difftime(Sunrise, date, units = 'mins'))
	#dawn_ndx <- dawn_ndx <= 0 & dawn_ndx > -dawn_cutoff
	dawn_ndx <- dawn_ndx < dawn_cutoff & indata_sub$date < indata_sub$Sunrise
	indata_sub$dawn[dawn_ndx] <- 'dawn'

	#return (indata_sub)
	#}
	
	#xxx <- FitRegressionParametersDataSetStd02(xx, 120)
	#xxx[1:2000, c('date','dawn', 'Sunrise', 'daynight')]
	#xxx[1:2000, c('date')]

	x_date = unique ( as.Date (indata_sub$date1))
	

	x_nights = unique ( indata_sub$daynight1 )
	x_nights = x_nights[grep ('night', x_nights)]
	
	m_coeffs = matrix (nrow = length (x_nights), ncol = 6 )
	

	for ( i in 1:length (x_nights) )
		{
		
		#i = 1
		x_ndx = x_nights[i]
		
		data_sub_ndx = which ( indata_sub$daynight1 == x_ndx )
		
		data_sub = indata_sub[data_sub_ndx, ]
		#exclude dawn
		data_sub = data_sub[data_sub$dawn != 'dawn',]
		
		x_ndx1 = which ( !is.na (data_sub$DO_deficit_mean))
		
		data_sub = data_sub[x_ndx1, ]
		
		#with ( data_sub, plot ( DO_deficit, DO_change ))
		n_cor <- cor(data_sub$DO_diff_std,data_sub$DO_deficit_mean)
		n_samplesize <- length(data_sub$DO_diff_std)

		fit <- try ( lm ( data_sub$DO_diff_std ~ data_sub$DO_deficit_mean ))

		n_inter <-  as.numeric (fit[[1]][1])
		n_slope <-  as.numeric (fit[[1]][2])


		m_coeffs[i,1] <- n_inter
		m_coeffs[i,2] <- n_slope
		m_coeffs[i,3] <- n_cor
		m_coeffs[i,4] <- mean (data_sub$Temp, na.rm = TRUE)
		m_coeffs[i,5] <- median (data_sub$Temp, na.rm = TRUE)
		m_coeffs[i,6] <- n_samplesize

		}


	#Ebble_CE1_2013_04_25
	outdata <- data.frame ( x_date[1:length (x_nights)], x_nights, m_coeffs )
	names (outdata) <- c ('date1', 'daynight1','ER','Ks', 'correlation', 'mean.Temp','median.Temp', 'SampleSize')

	return (outdata)
	
}


#FitRegressionParametersDataSetStd ( c_indata = 'Ebble_CE1_2013_04_25' )
#FitRegressionParametersDataSetStd ( c_indata = 'Ebble_CE1_2013_08_08' )







































































FitRegressionParametersSampled <- function (indata, dawn_cutoff = 60, sample_fraction = 0.5)
	{

	#this differs from FitRegressionParametersDataSetStd01 by cutting of the last hour before dawn
	#and therefore hopefully getting rid of spurious data
	
	#indata <- Ebble_CE1_2013_04_25
	#library (mdot); data (Minidot_02); indata <- Minidot_02; n_quality = 14; c_indata <- 'Minidot_02'

	#dirscr <- paste ( dirtop, '/DO/rovscript/', sep = '')
	
	#library(mdot)
	library (parker)
	data (parker)


	indata_sub <- NormalizeIndata(indata)
	
	indata_sub$dawn <- indata_sub$daynight
	dawn_ndx <- with (indata_sub, difftime(Sunrise, date, units = 'mins'))
	#dawn_ndx <- dawn_ndx <= 0 & dawn_ndx > -dawn_cutoff
	dawn_ndx <- dawn_ndx < dawn_cutoff & indata_sub$date < indata_sub$Sunrise
	indata_sub$dawn[dawn_ndx] <- 'dawn'

	#return (indata_sub)
	#}
	
	#xxx <- FitRegressionParametersDataSetStd02(xx, 120)
	#xxx[1:2000, c('date','dawn', 'Sunrise', 'daynight')]
	#xxx[1:2000, c('date')]

	x_date = unique ( as.Date (indata_sub$date1))
	

	x_nights = unique ( indata_sub$daynight1 )
	x_nights = x_nights[grep ('night', x_nights)]
	
	m_coeffs = matrix (nrow = length (x_nights), ncol = 6 )
	

	for ( i in 1:length (x_nights) )
		{
		
		#i = 5
		x_ndx = x_nights[i]
		
		data_sub_ndx = which ( indata_sub$daynight1 == x_ndx )
		
		data_sub = indata_sub[data_sub_ndx, ]
		#exclude dawn
		data_sub = data_sub[data_sub$dawn != 'dawn',]
		
		x_ndx1 = which ( !is.na (data_sub$DO_deficit_mean))

		
		data_sub = data_sub[x_ndx1, ]

		n_dim01 <- dim(data_sub)[1]
		n_samplesize <- floor(sample_fraction * n_dim01)

		pick_ndx <- sample(1:n_dim01, n_samplesize)
		data_sub <- data_sub[pick_ndx,]
		
		
		#with ( data_sub, plot ( DO_deficit, DO_change ))
		#with ( data_sub, plot (DO_deficit_mean, DO_diff_std))
		
		n_cor <- cor(data_sub$DO_diff_std,data_sub$DO_deficit_mean)
		fit <- try ( lm ( data_sub$DO_diff_std ~ data_sub$DO_deficit_mean ))

		n_inter <-  as.numeric (fit[[1]][1])
		n_slope <-  as.numeric (fit[[1]][2])


		m_coeffs[i,1] <- n_inter
		m_coeffs[i,2] <- n_slope
		m_coeffs[i,3] <- n_cor
		m_coeffs[i,4] <- mean (data_sub$Temp, na.rm = TRUE)
		m_coeffs[i,5] <- median (data_sub$Temp, na.rm = TRUE)
		m_coeffs[i,6] <- n_samplesize
		}

	#Ebble_CE1_2013_04_25
	outdata <- data.frame ( x_date[1:length (x_nights)], x_nights, m_coeffs )
	names (outdata) <- c ('date1', 'daynight1','ER','Ks', 'correlation', 'mean.Temp','median.Temp', 'SampleSize')

	return (outdata)
	
}


#FitRegressionParametersDataSetStd ( c_indata = 'Ebble_CE1_2013_04_25' )
#FitRegressionParametersDataSetStd ( c_indata = 'Ebble_CE1_2013_08_08' )
























































FitRegressionParametersSampled15Minute <- function (indata, dawn_cutoff = 60, sample_fraction = 0.5)
	{

	#this differs from FitRegressionParametersDataSetStd01 by cutting of the last hour before dawn
	#and therefore hopefully getting rid of spurious data
	
	#indata <- Ebble_CE1_2013_04_25
	#library (mdot); data (Minidot_02); indata <- Minidot_02; n_quality = 14; c_indata <- 'Minidot_02'

	#dirscr <- paste ( dirtop, '/DO/rovscript/', sep = '')
	
	#library(mdot)
	library (parker)
	data (parker)


	#indata_sub <- NormalizeIndata(indata)
	indata_sub <- indata
	
	indata_sub$dawn <- indata_sub$daynight
	dawn_ndx <- with (indata_sub, difftime(Sunrise, date, units = 'mins'))
	#dawn_ndx <- dawn_ndx <= 0 & dawn_ndx > -dawn_cutoff
	dawn_ndx <- dawn_ndx < dawn_cutoff & indata_sub$date < indata_sub$Sunrise
	indata_sub$dawn[dawn_ndx] <- 'dawn'

	#return (indata_sub)
	#}
	
	#xxx <- FitRegressionParametersDataSetStd02(xx, 120)
	#xxx[1:2000, c('date','dawn', 'Sunrise', 'daynight')]
	#xxx[1:2000, c('date')]

	x_date = unique ( as.Date (indata_sub$date1))
	

	x_nights = unique ( indata_sub$daynight1 )
	x_nights = x_nights[grep ('night', x_nights)]
	
	m_coeffs = matrix (nrow = length (x_nights), ncol = 6 )
	

	for ( i in 1:length (x_nights) )
		{
		
		#i = 5
		x_ndx = x_nights[i]
		
		data_sub_ndx = which ( indata_sub$daynight1 == x_ndx )
		
		data_sub = indata_sub[data_sub_ndx, ]
		#exclude dawn
		data_sub = data_sub[data_sub$dawn != 'dawn',]
		
		x_ndx1 = which ( !is.na (data_sub$DO15_deficit))

		
		data_sub = data_sub[x_ndx1, ]

		n_dim01 <- dim(data_sub)[1]
		n_samplesize <- floor(sample_fraction * n_dim01)

		pick_ndx <- sample(1:n_dim01, n_samplesize)
		data_sub <- data_sub[pick_ndx,]
		
		
		#with ( data_sub, plot ( DO_deficit, DO_change ))
		#with ( data_sub, plot (DO_deficit_mean, DO_diff_std))
		
		n_cor <- cor(data_sub$DO15_diff,data_sub$DO15_deficit)
		fit <- try ( lm ( data_sub$DO15_diff ~ data_sub$DO15_deficit ))

		n_inter <-  as.numeric (fit[[1]][1])
		n_slope <-  as.numeric (fit[[1]][2])


		m_coeffs[i,1] <- n_inter
		m_coeffs[i,2] <- n_slope
		m_coeffs[i,3] <- n_cor
		m_coeffs[i,4] <- mean (data_sub$Temp, na.rm = TRUE)
		m_coeffs[i,5] <- median (data_sub$Temp, na.rm = TRUE)
		m_coeffs[i,6] <- n_samplesize
		}

	#Ebble_CE1_2013_04_25
	outdata <- data.frame ( x_date[1:length (x_nights)], x_nights, m_coeffs )
	names (outdata) <- c ('date1', 'daynight1','ER','Ks', 'correlation', 'mean.Temp','median.Temp', 'SampleSize')

	return (outdata)
	
}


#FitRegressionParametersDataSetStd ( c_indata = 'Ebble_CE1_2013_04_25' )
#FitRegressionParametersDataSetStd ( c_indata = 'Ebble_CE1_2013_08_08' )









































































#assign date from within matrix, not by adding a column to the data.frame


FitRegressionParametersDataSetStd03 <- function (indata, dawn_cutoff = 60)
	{

	#this differs from FitRegressionParametersDataSetStd01 by cutting of the last hour before dawn
	#and therefore hopefully getting rid of spurious data
	
	#indata <- Ebble_CE1_2013_04_25
	#library (mdot); data (Minidot_02); indata <- Minidot_02; n_quality = 14; c_indata <- 'Minidot_02'

	#dirscr <- paste ( dirtop, '/DO/rovscript/', sep = '')
	
	#library(mdot)
	library (parker)
	data (parker)


	indata_sub <- NormalizeIndata(indata)
	
	indata_sub$dawn <- indata_sub$daynight
	dawn_ndx <- with (indata_sub, difftime(Sunrise, date, units = 'mins'))
	#dawn_ndx <- dawn_ndx <= 0 & dawn_ndx > -dawn_cutoff
	dawn_ndx <- dawn_ndx < dawn_cutoff & indata_sub$date < indata_sub$Sunrise
	indata_sub$dawn[dawn_ndx] <- 'dawn'

	#return (indata_sub)
	#}
	
	#xxx <- FitRegressionParametersDataSetStd02(xx, 120)
	#xxx[1:2000, c('date','dawn', 'Sunrise', 'daynight')]
	#xxx[1:2000, c('date')]

	x_date = unique ( as.Date (indata_sub$date1))
	

	x_nights = unique ( indata_sub$daynight1 )
	x_nights = x_nights[grep ('night', x_nights)]
	
	m_coeffs = matrix (nrow = length (x_nights), ncol = 7 )
	

	for ( i in 1:length (x_nights) )
		{
		
		#i = 1
		x_ndx = x_nights[i]
		
		data_sub_ndx = which ( indata_sub$daynight1 == x_ndx )
		
		data_sub = indata_sub[data_sub_ndx, ]
		#exclude dawn
		data_sub = data_sub[data_sub$dawn != 'dawn',]
		
		x_ndx1 = which ( !is.na (data_sub$DO_deficit_mean))
		
		data_sub = data_sub[x_ndx1, ]
		
		#with ( data_sub, plot ( DO_deficit, DO_change ))
		n_cor <- cor(data_sub$DO_diff_std,data_sub$DO_deficit_mean)
		n_samplesize <- length(data_sub$DO_diff_std)

		fit <- try ( lm ( data_sub$DO_diff_std ~ data_sub$DO_deficit_mean ))

		n_inter <-  as.numeric (fit[[1]][1])
		n_slope <-  as.numeric (fit[[1]][2])


		m_coeffs[i,1] <- n_inter
		m_coeffs[i,2] <- n_slope
		m_coeffs[i,3] <- n_cor
		m_coeffs[i,4] <- mean (data_sub$Temp, na.rm = TRUE)
		m_coeffs[i,5] <- median (data_sub$Temp, na.rm = TRUE)
		m_coeffs[i,6] <- n_samplesize
		m_coeffs[i,7] <- tail(data_sub$date1[!is.na(data_sub$date1)],1)

		}


	#Ebble_CE1_2013_04_25
	outdata <- data.frame ( x_nights, m_coeffs )
	names (outdata) <- c ('daynight1','ER','Ks', 'correlation', 'mean.Temp','median.Temp', 'SampleSize', 'date1' )
	outdata$date1 <- as.Date(outdata$date1, origin = '1970-01-01')

	#check dates on final anal
	n_tails <- tail(outdata$date1,2)
	if(n_tails[1] ==  n_tails[2])
		{
		#remove last row because duplicated date
		outdata <- outdata[1:(dim(outdata)[1] - 1),]
		}

	return (outdata)
	
}


#FitRegressionParametersDataSetStd ( c_indata = 'Ebble_CE1_2013_04_25' )
#FitRegressionParametersDataSetStd ( c_indata = 'Ebble_CE1_2013_08_08' )











































































































































































#this function calculates ER and Ks using moving averages for DO and temperature

FitRegressionParametersDataSet02 <- function (indata, varname_suffix = '15', dawn_cutoff = 60, 	dusk_cutoff = 10)
	{

	#this method is based on the paper by 
	#Uehlinger, U. (2006). Annual cycle and inter‐annual variability of gross primary production and ecosystem respiration in a floodprone river during a 15‐year period. Freshwater Biology, 51(5), 938-950.
	
	#uses regression method to calculate the slope, sums ER component and divides by timestep
	#this differs from FitRegressionParametersDataSetStd01 by cutting of the last hour before dawn
	
	
	#indata <- Ebble_CE1_2013_04_25
	#library (mdot); data (Minidot_02); indata <- Minidot_02; n_quality = 14; c_indata <- 'Minidot_02'

	#dirscr <- paste ( dirtop, '/DO/rovscript/', sep = '')
	#varname_suffix = '15'; dawn_cutoff = 60; 	dusk_cutoff = 10

	#this method is based on the paper by 
	
	#library(mdot)
	library (parker)
	data (parker)


	#DO15_deficit;DO15_diff;DO15_diff_lag;DO15_diff_mean;CSat15_diff;Sunrise       
	#select names
	#c_names <- paste ('DO', varname_suffix, c('_deficit','_diff','_diff_lag','_diff_mean'), sep = '')
	#c_names <- c (c_names, paste ('CSat', varname_suffix, c('','_diff'), sep = ''))
	#c_names <- c (c_names, paste ('Temp', varname_suffix, sep = ''))
	#c_names <- c (c_names, c('Sunrise', 'Sunset'))


	c_names <- paste ('DO', varname_suffix, c('_deficit','_diff_mean'), sep = '')
	c_names <- c (c_names, paste ('Temp', varname_suffix, sep = ''))
	c_names <- c (c_names, c('Sunrise', 'Sunset', 'date', 'date1', 'daynight1'))
	#c_names


	indata_sub <- indata[,c_names]
	#rename for consistency
	names(indata_sub) <- c('DO_deficit','DO_diff','Temp','Sunrise','Sunset','date','date1', 'daynight1')
	
	#AS1_Aug2015_5min
	
	#indata_sub <- NormalizeIndata(indata)
	
	indata_sub$dawn <- indata_sub$daynight
	dawn_ndx <- with (indata_sub, difftime(Sunrise, date, units = 'mins'))
	#dawn_ndx <- dawn_ndx <= 0 & dawn_ndx > -dawn_cutoff
	dawn_ndx <- dawn_ndx < dawn_cutoff & indata_sub$date < indata_sub$Sunrise
	indata_sub$dawn[dawn_ndx] <- 'dawn'


	#dusk_cutoff is default ten minutes
	dusk_ndx <- with (indata_sub, difftime(Sunset, date, units = 'mins'))
	#dawn_ndx <- dawn_ndx <= 0 & dawn_ndx > -dawn_cutoff
	dusk_ndx <- dusk_ndx > dusk_cutoff & indata_sub$date > indata_sub$Sunset
	indata_sub$dawn[dusk_ndx] <- 'dusk'

	
	
	#xxx <- FitRegressionParametersDataSetStd02(xx, 120)
	#xxx[1:2000, c('date','dawn', 'Sunrise', 'daynight')]
	#xxx[1:2000, c('date')]

	x_date = unique ( as.Date (indata_sub$date1))
	

	x_nights = unique ( indata_sub$daynight1 )
	x_nights = x_nights[grep ('night', x_nights)]
	
	m_coeffs = matrix (nrow = length (x_nights), ncol = 6 )
	

	for ( i in 1:length (x_nights) )
		{
		
		#i = 1
		x_ndx = x_nights[i]
		
		data_sub_ndx = which ( indata_sub$daynight1 == x_ndx )
		
		data_sub = indata_sub[data_sub_ndx, ]
		#exclude dawn
		data_sub = data_sub[data_sub$dawn != 'dawn',]
		
		x_ndx1 = which ( !is.na (data_sub$DO_deficit))
		
		data_sub = data_sub[x_ndx1, ]
		
		#with ( data_sub, plot ( DO_deficit, DO_change ))
		n_cor <- cor(data_sub$DO_diff,data_sub$DO_deficit)
		n_samplesize <- length(data_sub$DO_diff)

		fit <- try ( lm ( data_sub$DO_diff ~ data_sub$DO_deficit ))

		n_inter <-  as.numeric (fit[[1]][1])
		n_slope <-  as.numeric (fit[[1]][2])


		m_coeffs[i,1] <- n_inter
		m_coeffs[i,2] <- n_slope
		m_coeffs[i,3] <- n_cor
		m_coeffs[i,4] <- mean (data_sub$Temp, na.rm = TRUE)
		m_coeffs[i,5] <- median (data_sub$Temp, na.rm = TRUE)
		m_coeffs[i,6] <- n_samplesize

		}


	#Ebble_CE1_2013_04_25
	outdata <- data.frame ( x_date[1:length (x_nights)], x_nights, m_coeffs )
	names (outdata) <- c ('date1', 'daynight1','ER','Ks', 'correlation', 'mean.Temp','median.Temp', 'SampleSize')

	return (outdata)
	
}


#FitRegressionParametersDataSetStd ( c_indata = 'Ebble_CE1_2013_04_25' )
#FitRegressionParametersDataSetStd ( c_indata = 'Ebble_CE1_2013_08_08' )



































































#this function calculates ER and Ks using moving averages for DO and temperature
#based on FitRegressionParametersDataSet02
#no default setting for varname_suffix
#no default cutoffs
#exclude NAs from lm() calculation
#added 3 columns to the output

FitRegressionParametersDataSet10 <- function (indata, varname_suffix, dawn_cutoff, 	dusk_cutoff)
	{

	#this method is based on the paper by 
	#Uehlinger, U. (2006). Annual cycle and inter‐annual variability of gross primary production and ecosystem respiration in a floodprone river during a 15‐year period. Freshwater Biology, 51(5), 938-950.
	
	#uses regression method to calculate the slope, sums ER component and divides by timestep
	#this differs from FitRegressionParametersDataSetStd01 by cutting of the last hour before dawn
	
	
	#indata <- Ebble_CE1_2013_04_25
	#library (mdot); data (Minidot_02); indata <- Minidot_02; n_quality = 14; c_indata <- 'Minidot_02'

	#dirscr <- paste ( dirtop, '/DO/rovscript/', sep = '')
	#varname_suffix = '15'; dawn_cutoff = 60; 	dusk_cutoff = 10

	#this method is based on the paper by 
	
	#library(mdot)
	library (parker)
	data (parker)


	#DO15_deficit;DO15_diff;DO15_diff_lag;DO15_diff_mean;CSat15_diff;Sunrise       
	#select names
	#c_names <- paste ('DO', varname_suffix, c('_deficit','_diff','_diff_lag','_diff_mean'), sep = '')
	#c_names <- c (c_names, paste ('CSat', varname_suffix, c('','_diff'), sep = ''))
	#c_names <- c (c_names, paste ('Temp', varname_suffix, sep = ''))
	#c_names <- c (c_names, c('Sunrise', 'Sunset'))


	c_names <- paste ('DO', varname_suffix, c('', '_deficit','_diff_mean'), sep = '')
	c_names <- c (c_names, paste ('Temp', varname_suffix, sep = ''))
	c_names <- c (c_names, c('Sunrise', 'Sunset', 'date', 'date1', 'daynight1'))
	#c_names


	indata_sub <- indata[,c_names]
	#rename for consistency
	names(indata_sub) <- c('DO','DO_deficit','DO_diff','Temp','Sunrise','Sunset','date','date1', 'daynight1')
	
	#AS1_Aug2015_5min
	
	#indata_sub <- NormalizeIndata(indata)
	
	indata_sub$dawn <- indata_sub$daynight
	dawn_ndx <- with (indata_sub, difftime(Sunrise, date, units = 'mins'))
	#dawn_ndx <- dawn_ndx <= 0 & dawn_ndx > -dawn_cutoff
	dawn_ndx <- dawn_ndx < dawn_cutoff & indata_sub$date < indata_sub$Sunrise
	indata_sub$dawn[dawn_ndx] <- 'dawn'


	#dusk_cutoff is default ten minutes
	dusk_ndx <- with (indata_sub, difftime(Sunset, date, units = 'mins'))
	#dawn_ndx <- dawn_ndx <= 0 & dawn_ndx > -dawn_cutoff
	dusk_ndx <- dusk_ndx > dusk_cutoff & indata_sub$date > indata_sub$Sunset
	indata_sub$dawn[dusk_ndx] <- 'dusk'

	
	
	#xxx <- FitRegressionParametersDataSetStd02(xx, 120)
	#xxx[1:2000, c('date','dawn', 'Sunrise', 'daynight')]
	#xxx[1:2000, c('date')]

	x_date = unique ( as.Date (indata_sub$date1))
	

	x_nights = unique ( indata_sub$daynight1 )
	x_nights = x_nights[grep ('night', x_nights)]
	
	m_coeffs = matrix (nrow = length (x_nights), ncol = 9 )
	

	for ( i in 1:length (x_nights) )
		{
		
		#i = 1
		x_ndx = x_nights[i]
		
		data_sub_ndx = which ( indata_sub$daynight1 == x_ndx )
		
		data_sub = indata_sub[data_sub_ndx, ]
		#exclude dawn
		data_sub = data_sub[data_sub$dawn != 'dawn',]
		
		x_ndx1 = which ( !is.na (data_sub$DO_deficit))
		
		time_steps <- with(data_sub, difftime(c(date,as.POSIXct(NA)), c(as.POSIXct(NA),date), units = 'secs'))
		#remove last element
		time_steps <- rev(rev(time_steps)[-1])
		time_interval <- with(data_sub, difftime(tail(date,1),head(date,1), unit = 'mins'))

		
		data_sub = data_sub[x_ndx1, ]
		
		#included because zero elements was booting out of analysis
		if(dim(data_sub)[1] > 0)
			{
			#with ( data_sub, plot ( DO_deficit, DO_change ))
			#n_cor <- cor(data_sub$DO_diff,data_sub$DO_deficit, na.rm = TRUE)
			n_cor <- cor(data_sub$DO_diff,data_sub$DO_deficit, use = 'complete.obs')
			n_samplesize <- length(data_sub$DO_diff)

			fit <- try(lm(data_sub$DO_diff ~ data_sub$DO_deficit, na.exclude = TRUE))


			n_inter <-  as.numeric (fit[[1]][1])
			n_slope <-  as.numeric (fit[[1]][2])

			aeration_component <- with (data_sub, n_slope * DO_deficit)
			respiration_component <- with (data_sub, DO_diff - aeration_component)
			
			#remove aeration_component[1] and respiration_component[1] because they are differences
			#and first element will be missing
			respiration_component01 <- mean(respiration_component, na.rm = TRUE)
			respiration_component02 <- median(respiration_component, na.rm = TRUE)


			DO_change <- with(data_sub, tail(DO,1) - head(DO,1))
			n_respiration <- (DO_change - sum(aeration_component, na.rm = TRUE)) / as.numeric(time_interval)
			

			m_coeffs[i,1] <- n_inter
			m_coeffs[i,2] <- n_slope
			m_coeffs[i,3] <- try(n_respiration)
			m_coeffs[i,4] <- respiration_component01
			m_coeffs[i,5] <- respiration_component02
			m_coeffs[i,6] <- n_cor
			m_coeffs[i,7] <- mean (data_sub$Temp, na.rm = TRUE)
			m_coeffs[i,8] <- median (data_sub$Temp, na.rm = TRUE)
			m_coeffs[i,9] <- n_samplesize

			}
		}

	#Ebble_CE1_2013_04_25
	outdata <- data.frame ( x_date[1:length (x_nights)], x_nights, m_coeffs )
	names (outdata) <- c ('date1', 'daynight1','ER','Ks', 'Respiration', 'ER.mean', 'ER.median', 'correlation', 'mean.Temp','median.Temp', 'SampleSize')

	return (outdata)
	
}


#FitRegressionParametersDataSet10 ( data_sub_May_Manta, varname_suffix = '', dawn_cutoff = 0, dusk_cutoff = 0)











































































#this function calculates ER and Ks using moving averages for DO and temperature
#but using the method of Uehlinger

FitRegressionParametersDataSetUehl01 <- function (indata, varname_suffix = '15', dawn_cutoff = 60, 	dusk_cutoff = 10)
	{

	#this method is based on the paper by 
	#Uehlinger, U. (2006). Annual cycle and inter‐annual variability of gross primary production and ecosystem respiration in a floodprone river during a 15‐year period. Freshwater Biology, 51(5), 938-950.
	
	#uses regression method to calculate the slope, sums ER component and divides by timestep
	#this differs from FitRegressionParametersDataSetStd01 by cutting of the last hour before dawn
	
	
	#indata <- Ebble_CE1_2013_04_25
	#library (mdot); data (Minidot_02); indata <- Minidot_02; n_quality = 14; c_indata <- 'Minidot_02'

	#dirscr <- paste ( dirtop, '/DO/rovscript/', sep = '')
	#varname_suffix = '15'; dawn_cutoff = 60; 	dusk_cutoff = 10

	#this method is based on the paper by 
	
	#library(mdot)
	library (parker)
	data (parker)


	#DO15_deficit;DO15_diff;DO15_diff_lag;DO15_diff_mean;CSat15_diff;Sunrise       
	#select names
	#c_names <- paste ('DO', varname_suffix, c('_deficit','_diff','_diff_lag','_diff_mean'), sep = '')
	#c_names <- c (c_names, paste ('CSat', varname_suffix, c('','_diff'), sep = ''))
	#c_names <- c (c_names, paste ('Temp', varname_suffix, sep = ''))
	#c_names <- c (c_names, c('Sunrise', 'Sunset'))


	c_names <- paste ('DO', varname_suffix, c('', '_deficit','_diff_mean'), sep = '')
	c_names <- c (c_names, paste ('Temp', varname_suffix, sep = ''))
	c_names <- c (c_names, c('Sunrise', 'Sunset', 'date', 'date1', 'daynight1'))
	#c_names


	indata_sub <- indata[,c_names]
	#rename for consistency
	names(indata_sub) <- c('DO','DO_deficit','DO_diff','Temp','Sunrise','Sunset','date','date1', 'daynight1')
	
	#AS1_Aug2015_5min
	
	#indata_sub <- NormalizeIndata(indata)
	
	indata_sub$dawn <- indata_sub$daynight
	dawn_ndx <- with (indata_sub, difftime(Sunrise, date, units = 'mins'))
	#dawn_ndx <- dawn_ndx <= 0 & dawn_ndx > -dawn_cutoff
	dawn_ndx <- dawn_ndx < dawn_cutoff & indata_sub$date < indata_sub$Sunrise
	indata_sub$dawn[dawn_ndx] <- 'dawn'


	#dusk_cutoff is default ten minutes
	dusk_ndx <- with (indata_sub, difftime(Sunset, date, units = 'mins'))
	#dawn_ndx <- dawn_ndx <= 0 & dawn_ndx > -dawn_cutoff
	dusk_ndx <- dusk_ndx > dusk_cutoff & indata_sub$date > indata_sub$Sunset
	indata_sub$dawn[dusk_ndx] <- 'dusk'

	
	
	#xxx <- FitRegressionParametersDataSetStd02(xx, 120)
	#xxx[1:2000, c('date','dawn', 'Sunrise', 'daynight')]
	#xxx[1:2000, c('date')]

	x_date = unique ( as.Date (indata_sub$date1))
	

	x_nights = unique ( indata_sub$daynight1 )
	x_nights = x_nights[grep ('night', x_nights)]
	
	m_coeffs = matrix (nrow = length (x_nights), ncol = 10 )
	

	for ( i in 1:length (x_nights) )
		{
		
		#i = 1
		x_ndx = x_nights[i]
		
		data_sub_ndx = which ( indata_sub$daynight1 == x_ndx )
		
		data_sub = indata_sub[data_sub_ndx, ]
		#exclude dawn
		data_sub = data_sub[data_sub$dawn != 'dawn',]
		
		x_ndx1 = which ( !is.na (data_sub$DO_deficit))
		
		data_sub = data_sub[x_ndx1, ]
		
		time_steps <- with(data_sub, difftime(c(date,as.POSIXct(NA)), c(as.POSIXct(NA),date), units = 'secs'))
		#remove last element
		time_steps <- rev(rev(time_steps)[-1])
		time_interval <- with(data_sub, difftime(tail(date,1),head(date,1), unit = 'mins'))


		stopifnot(median(time_steps, na.rm = TRUE) == mean(time_steps, na.rm = TRUE))
		
		
		#with ( data_sub, plot ( DO_deficit, DO_change ))
		n_cor <- cor(data_sub$DO_diff,data_sub$DO_deficit)
		n_samplesize <- length(data_sub$DO_diff)

		fit <- try ( lm ( data_sub$DO_diff ~ data_sub$DO_deficit ))

		n_inter <-  as.numeric (fit[[1]][1])
		n_slope <-  as.numeric (fit[[1]][2])

		aeration_component <- with (data_sub, n_slope * DO_deficit)
		respiration_component <- with (data_sub, DO_diff - aeration_component)
		
		#remove aeration_component[1] and respiration_component[1] because they are differences
		#and first element will be missing
		respiration_component01 <- mean(respiration_component, na.rm = TRUE)
		respiration_component02 <- median(respiration_component, na.rm = TRUE)
		
		
		
		DO_change <- with(data_sub, tail(DO,1) - head(DO,1))
		n_respiration <- (DO_change - sum(aeration_component, na.rm = TRUE)) / as.numeric(time_interval)
		

		m_coeffs[i,1] <- n_inter
		m_coeffs[i,2] <- n_slope
		m_coeffs[i,3] <- n_respiration
		m_coeffs[i,4] <- respiration_component01
		m_coeffs[i,5] <- respiration_component02
		m_coeffs[i,6] <- n_cor
		m_coeffs[i,7] <- mean (data_sub$Temp, na.rm = TRUE)
		m_coeffs[i,8] <- median (data_sub$Temp, na.rm = TRUE)
		m_coeffs[i,9] <- n_samplesize
		

		}


	#Ebble_CE1_2013_04_25
	outdata <- data.frame ( x_date[1:length (x_nights)], x_nights, m_coeffs )
	names (outdata) <- c ('date1', 'daynight1','ER','Ks', 'Respiration', 'ER.mean', 'ER.median', 'correlation', 'mean.Temp','median.Temp', 'SampleSize')

	return (outdata)
	
}


#FitRegressionParametersDataSetStd ( c_indata = 'Ebble_CE1_2013_04_25' )
#FitRegressionParametersDataSetStd ( c_indata = 'Ebble_CE1_2013_08_08' )


































































#this function calculates ER and Ks using moving averages for DO and temperature
#but using the method of Uehlinger

FitRegParametersDataSetTempMthdA01 <- function (indata, varname_suffix = '15', dawn_cutoff = 60, dusk_cutoff = 10, seq_length = 30)
	{
	
	#attempt to fit Arrhenius relationship
	
	#this method is based on the paper by 
	#Uehlinger, U. (2006). Annual cycle and inter‐annual variability of gross primary production and ecosystem respiration in a floodprone river during a 15‐year period. Freshwater Biology, 51(5), 938-950.
	
	#uses regression method to calculate the slope, sums ER component and divides by timestep
	#this differs from FitRegressionParametersDataSetStd01 by cutting of the last hour before dawn
	
	
	#indata <- Ebble_CE1_2013_04_25
	#library (mdot); data (Minidot_02); indata <- Minidot_02; n_quality = 14; c_indata <- 'Minidot_02'

	#dirscr <- paste ( dirtop, '/DO/rovscript/', sep = '')
	#indata <- Ebble_CE1_2013_04_25; varname_suffix = '15'; dawn_cutoff = 60; 	dusk_cutoff = 10; seq_length = 30

	#this method is based on the paper by 

	#library(mdot)
	library (parker)
	data (parker)


	#DO15_deficit;DO15_diff;DO15_diff_lag;DO15_diff_mean;CSat15_diff;Sunrise       
	#select names
	#c_names <- paste ('DO', varname_suffix, c('_deficit','_diff','_diff_lag','_diff_mean'), sep = '')
	#c_names <- c (c_names, paste ('CSat', varname_suffix, c('','_diff'), sep = ''))
	#c_names <- c (c_names, paste ('Temp', varname_suffix, sep = ''))
	#c_names <- c (c_names, c('Sunrise', 'Sunset'))


	c_names <- paste ('DO', varname_suffix, c('', '_deficit','_diff_mean'), sep = '')
	c_names <- c (c_names, paste ('Temp', varname_suffix, sep = ''))
	c_names <- c (c_names, c('Sunrise', 'Sunset', 'date', 'date1', 'daynight1'))
	#c_names


	indata_sub <- indata[,c_names]
	#rename for consistency
	names(indata_sub) <- c('DO','DO_deficit','DO_diff','Temp','Sunrise','Sunset','date','date1', 'daynight1')
	
	#AS1_Aug2015_5min
	
	#indata_sub <- NormalizeIndata(indata)
	
	indata_sub$dawn <- indata_sub$daynight
	dawn_ndx <- with (indata_sub, difftime(Sunrise, date, units = 'mins'))
	#dawn_ndx <- dawn_ndx <= 0 & dawn_ndx > -dawn_cutoff
	dawn_ndx <- dawn_ndx < dawn_cutoff & indata_sub$date < indata_sub$Sunrise
	indata_sub$dawn[dawn_ndx] <- 'dawn'


	#dusk_cutoff is default ten minutes
	dusk_ndx <- with (indata_sub, difftime(Sunset, date, units = 'mins'))
	#dawn_ndx <- dawn_ndx <= 0 & dawn_ndx > -dawn_cutoff
	dusk_ndx <- dusk_ndx > dusk_cutoff & indata_sub$date > indata_sub$Sunset
	indata_sub$dawn[dusk_ndx] <- 'dusk'

	
	
	#xxx <- FitRegressionParametersDataSetStd02(xx, 120)
	#xxx[1:2000, c('date','dawn', 'Sunrise', 'daynight')]
	#xxx[1:2000, c('date')]

	x_date = unique ( as.Date (indata_sub$date1))
	

	x_nights = unique ( indata_sub$daynight1 )
	x_nights = x_nights[grep ('night', x_nights)]
	
	m_coeffs = matrix (nrow = length (x_nights), ncol = 10 )
	
	out_list <- list()

	for ( i in 1:length (x_nights) )
		{
		
		#i = 1
		x_ndx = x_nights[i]
		
		data_sub_ndx = which ( indata_sub$daynight1 == x_ndx )
		
		data_sub = indata_sub[data_sub_ndx, ]
		#exclude dawn
		data_sub = data_sub[data_sub$dawn != 'dawn',]
		
		x_ndx1 = which ( !is.na (data_sub$DO_deficit))
		
		data_sub = data_sub[x_ndx1, ]
		
		time_steps <- with(data_sub, difftime(c(date,as.POSIXct(NA)), c(as.POSIXct(NA),date), units = 'secs'))
		#remove last element
		time_steps <- rev(rev(time_steps)[-1])
		time_interval <- with(data_sub, difftime(tail(date,1),head(date,1), unit = 'mins'))


		stopifnot(median(time_steps, na.rm = TRUE) == mean(time_steps, na.rm = TRUE))
		


		
		
		#split one night into multiple time segments
		n_indices <- seq (1,dim(data_sub)[1], by = seq_length)
		n_indices <- embed(n_indices,2)
		#switch columns as conceptually easier
		n_indices <- n_indices[,c(2,1)]
		n_indices[,1] <- n_indices[,1]+1
		n_indices[1,1] <- n_indices[1,1]-1

		
		m_coeffs01 <- matrix (nrow = dim(n_indices)[1], ncol = 10 )

		for(j in 1:dim(n_indices)[1])
			{
			
			data_sub_slice <- data_sub[n_indices[j,1]:n_indices[j,2],]
			time_interval <- with(data_sub_slice, difftime(tail(date,1),head(date,1), unit = 'mins'))

			
			n_cor <- cor(data_sub_slice$DO_diff,data_sub_slice$DO_deficit)
			n_samplesize <- length(data_sub_slice$DO_diff)

			fit <- try ( lm ( data_sub_slice$DO_diff ~ data_sub_slice$DO_deficit ))

			n_inter <-  as.numeric (fit[[1]][1])
			n_slope <-  as.numeric (fit[[1]][2])

			aeration_component <- with (data_sub_slice, n_slope * DO_deficit)
			respiration_component <- with (data_sub_slice, DO_diff - aeration_component)
			
			#remove aeration_component[1] and respiration_component[1] because they are differences
			#and first element will be missing
			respiration_component01 <- mean(respiration_component, na.rm = TRUE)
			respiration_component02 <- median(respiration_component, na.rm = TRUE)
			
			
			
			DO_change <- with(data_sub_slice, tail(DO,1) - head(DO,1))
			n_respiration <- (DO_change - sum(aeration_component, na.rm = TRUE)) / as.numeric(time_interval)
			

			m_coeffs01[j,1] <- n_inter
			m_coeffs01[j,2] <- n_slope
			m_coeffs01[j,3] <- n_respiration
			m_coeffs01[j,4] <- respiration_component01
			m_coeffs01[j,5] <- respiration_component02
			m_coeffs01[j,6] <- n_cor
			m_coeffs01[j,7] <- mean (data_sub_slice$Temp, na.rm = TRUE)
			m_coeffs01[j,8] <- median (data_sub_slice$Temp, na.rm = TRUE)
			m_coeffs01[j,9] <- n_samplesize
			}

		m_coeffs01 <- data.frame(m_coeffs01)
		names(m_coeffs01) <- c ('ER','Ks', 'Respiration', 'ER.mean', 'ER.median', 'correlation', 'mean.Temp','median.Temp', 'SampleSize')
		outdata <- list( x_nights[i], x_date[i], m_coeffs01 )
		names(outdata) <- c('Night','Date','Data')

		#outdata <- list( outdata)

		#assign(

		out_list <- append(out_list, outdata)
		}

	return (out_list)
	
}


#FitRegressionParametersDataSetStd ( c_indata = 'Ebble_CE1_2013_04_25' )
#FitRegressionParametersDataSetStd ( c_indata = 'Ebble_CE1_2013_08_08' )


































































































#this function calculates ER and Ks using moving averages for DO and temperature
#but using the method of Uehlinger

FitRegParametersDataSetTempMthdB01 <- function (indata, varname_suffix = '15', dawn_cutoff = 60, dusk_cutoff = 10, seq_length = 30)
	{
	
	#attempt to fit Arrhenius relationship
	
	#this method is based on the paper by 
	#Uehlinger, U. (2006). Annual cycle and inter‐annual variability of gross primary production and ecosystem respiration in a floodprone river during a 15‐year period. Freshwater Biology, 51(5), 938-950.
	
	#uses regression method to calculate the slope, sums ER component and divides by timestep
	#this differs from FitRegressionParametersDataSetStd01 by cutting of the last hour before dawn
	
	
	#indata <- Ebble_CE1_2013_04_25
	#library (mdot); data (Minidot_02); indata <- Minidot_02; n_quality = 14; c_indata <- 'Minidot_02'

	#dirscr <- paste ( dirtop, '/DO/rovscript/', sep = '')
	#indata <- Ebble_CE1_2013_04_25; varname_suffix = '15'; dawn_cutoff = 60; 	dusk_cutoff = 10; seq_length = 30

	#this method is based on the paper by 

	#library(mdot)
	library (parker)
	data (parker)

	
	try(rm(outdata_all))
	

	#DO15_deficit;DO15_diff;DO15_diff_lag;DO15_diff_mean;CSat15_diff;Sunrise       
	#select names
	#c_names <- paste ('DO', varname_suffix, c('_deficit','_diff','_diff_lag','_diff_mean'), sep = '')
	#c_names <- c (c_names, paste ('CSat', varname_suffix, c('','_diff'), sep = ''))
	#c_names <- c (c_names, paste ('Temp', varname_suffix, sep = ''))
	#c_names <- c (c_names, c('Sunrise', 'Sunset'))


	c_names <- paste ('DO', varname_suffix, c('', '_deficit','_diff_mean'), sep = '')
	c_names <- c (c_names, paste ('Temp', varname_suffix, sep = ''))
	c_names <- c (c_names, c('Sunrise', 'Sunset', 'date', 'date1', 'daynight1'))
	#c_names


	indata_sub <- indata[,c_names]
	#rename for consistency
	names(indata_sub) <- c('DO','DO_deficit','DO_diff','Temp','Sunrise','Sunset','date','date1', 'daynight1')
	
	#AS1_Aug2015_5min
	
	#indata_sub <- NormalizeIndata(indata)
	
	indata_sub$dawn <- indata_sub$daynight
	dawn_ndx <- with (indata_sub, difftime(Sunrise, date, units = 'mins'))
	#dawn_ndx <- dawn_ndx <= 0 & dawn_ndx > -dawn_cutoff
	dawn_ndx <- dawn_ndx < dawn_cutoff & indata_sub$date < indata_sub$Sunrise
	indata_sub$dawn[dawn_ndx] <- 'dawn'


	#dusk_cutoff is default ten minutes
	dusk_ndx <- with (indata_sub, difftime(Sunset, date, units = 'mins'))
	#dawn_ndx <- dawn_ndx <= 0 & dawn_ndx > -dawn_cutoff
	dusk_ndx <- dusk_ndx > dusk_cutoff & indata_sub$date > indata_sub$Sunset
	indata_sub$dawn[dusk_ndx] <- 'dusk'

	
	
	#xxx <- FitRegressionParametersDataSetStd02(xx, 120)
	#xxx[1:2000, c('date','dawn', 'Sunrise', 'daynight')]
	#xxx[1:2000, c('date')]

	x_date = unique ( as.Date (indata_sub$date1))
	

	x_nights = unique ( indata_sub$daynight1 )
	x_nights = x_nights[grep ('night', x_nights)]
	
	m_coeffs = matrix (nrow = length (x_nights), ncol = 10 )
	
	
	for ( i in 1:length (x_nights) )
		{
		
		#i = 1
		x_ndx = x_nights[i]
		
		data_sub_ndx = which ( indata_sub$daynight1 == x_ndx )
		
		data_sub = indata_sub[data_sub_ndx, ]
		#exclude dawn
		data_sub = data_sub[data_sub$dawn != 'dawn',]
		
		x_ndx1 = which ( !is.na (data_sub$DO_deficit))
		
		data_sub = data_sub[x_ndx1, ]
		
		time_steps <- with(data_sub, difftime(c(date,as.POSIXct(NA)), c(as.POSIXct(NA),date), units = 'secs'))
		#remove last element
		time_steps <- rev(rev(time_steps)[-1])
		time_interval <- with(data_sub, difftime(tail(date,1),head(date,1), unit = 'mins'))


		stopifnot(median(time_steps, na.rm = TRUE) == mean(time_steps, na.rm = TRUE))
		


		
		
		#split one night into multiple time segments
		n_indices <- seq (1,dim(data_sub)[1], by = seq_length)
		n_indices <- embed(n_indices,2)
		#switch columns as conceptually easier
		n_indices <- n_indices[,c(2,1)]
		n_indices[,1] <- n_indices[,1]+1
		n_indices[1,1] <- n_indices[1,1]-1

		
		m_coeffs01 <- matrix (nrow = dim(n_indices)[1], ncol = 10 )

		for(j in 1:dim(n_indices)[1])
			{
			
			data_sub_slice <- data_sub[n_indices[j,1]:n_indices[j,2],]
			time_interval <- with(data_sub_slice, difftime(tail(date,1),head(date,1), unit = 'mins'))

			
			n_cor <- cor(data_sub_slice$DO_diff,data_sub_slice$DO_deficit)
			n_samplesize <- length(data_sub_slice$DO_diff)

			fit <- try ( lm ( data_sub_slice$DO_diff ~ data_sub_slice$DO_deficit ))

			n_inter <-  as.numeric (fit[[1]][1])
			n_slope <-  as.numeric (fit[[1]][2])

			aeration_component <- with (data_sub_slice, n_slope * DO_deficit)
			respiration_component <- with (data_sub_slice, DO_diff - aeration_component)
			
			#remove aeration_component[1] and respiration_component[1] because they are differences
			#and first element will be missing
			respiration_component01 <- mean(respiration_component, na.rm = TRUE)
			respiration_component02 <- median(respiration_component, na.rm = TRUE)
			
			
			
			DO_change <- with(data_sub_slice, tail(DO,1) - head(DO,1))
			n_respiration <- (DO_change - sum(aeration_component, na.rm = TRUE)) / as.numeric(time_interval)
			

			m_coeffs01[j,1] <- n_inter
			m_coeffs01[j,2] <- n_slope
			m_coeffs01[j,3] <- n_respiration
			m_coeffs01[j,4] <- respiration_component01
			m_coeffs01[j,5] <- respiration_component02
			m_coeffs01[j,6] <- n_cor
			m_coeffs01[j,7] <- mean (data_sub_slice$Temp, na.rm = TRUE)
			m_coeffs01[j,8] <- median (data_sub_slice$Temp, na.rm = TRUE)
			m_coeffs01[j,9] <- n_samplesize
			}

		outdata <- data.frame(m_coeffs01)
		names(outdata) <- c ('ER','Ks', 'Respiration', 'ER.mean', 'ER.median', 'correlation', 'mean.Temp','median.Temp', 'SampleSize')
		
		outdata$Night <- x_nights[i]
		outdata$date1 <- x_date[i]
		
		if(!exists('outdata_all'))
			{
			outdata_all <- outdata
			}

		
		if(exists('outdata_all'))
			{
			outdata_all <- rbind(outdata_all, outdata)
			}

		}

	return (outdata_all)
	
}


#FitRegressionParametersDataSetStd ( c_indata = 'Ebble_CE1_2013_04_25' )
#FitRegressionParametersDataSetStd ( c_indata = 'Ebble_CE1_2013_08_08' )




































#this function calculates ER and Ks using the original data and calculates moving averages on the original data
#not yet used

FitRegressionParametersDataSetMoveAv01 <- function (indata, n_moveaverage = 15, dawn_cutoff = 60, 	dusk_cutoff = 10)
	{

	#this method is based on the paper by 
	#Uehlinger, U. (2006). Annual cycle and inter‐annual variability of gross primary production and ecosystem respiration in a floodprone river during a 15‐year period. Freshwater Biology, 51(5), 938-950.
	
	#uses regression method to calculate the slope, sums ER component and divides by timestep
	#this differs from FitRegressionParametersDataSetStd01 by cutting of the last hour before dawn
	
	
	#indata <- Ebble_CE1_2013_04_25
	#library (mdot); data (Minidot_02); indata <- Minidot_02; n_quality = 14; c_indata <- 'Minidot_02'

	#dirscr <- paste ( dirtop, '/DO/rovscript/', sep = '')
	
	#library(mdot)
	library (parker)
	data (parker)


	#DO15_deficit;DO15_diff;DO15_diff_lag;DO15_diff_mean;CSat15_diff;Sunrise       
	#select names
	#c_names <- paste ('DO', varname_suffix, c('_deficit','_diff','_diff_lag','_diff_mean'), sep = '')
	#c_names <- c (c_names, paste ('CSat', varname_suffix, c('','_diff'), sep = ''))
	#c_names <- c (c_names, paste ('Temp', varname_suffix, sep = ''))
	#c_names <- c (c_names, c('Sunrise', 'Sunset'))


	c_names <- paste ('DO', c('_deficit','_diff_mean'), sep = '')
	c_names <- c (c_names, paste ('Temp', sep = ''))
	c_names <- c (c_names, c('Sunrise', 'Sunset', 'date1', 'daynight1'))
	#c_names


	indata_sub <- indata[,c_names]
	#rename for consistency
	names(indata_sub) <- c('DO_deficit','DO_diff','Temp','Sunrise','Sunset','date1', 'daynight1')
	
	#AS1_Aug2015_5min
	
	#indata_sub <- NormalizeIndata(indata)
	
	indata_sub$dawn <- indata_sub$daynight
	dawn_ndx <- with (indata_sub, difftime(Sunrise, date, units = 'mins'))
	#dawn_ndx <- dawn_ndx <= 0 & dawn_ndx > -dawn_cutoff
	dawn_ndx <- dawn_ndx < dawn_cutoff & indata_sub$date < indata_sub$Sunrise
	indata_sub$dawn[dawn_ndx] <- 'dawn'


	#dusk_cutoff is default ten minutes
	dusk_ndx <- with (indata_sub, difftime(Sunset, date, units = 'mins'))
	#dawn_ndx <- dawn_ndx <= 0 & dawn_ndx > -dawn_cutoff
	dusk_ndx <- dusk_ndx > dusk_cutoff & indata_sub$date > indata_sub$Sunset
	indata_sub$dawn[dusk_ndx] <- 'dusk'

	
	
	#xxx <- FitRegressionParametersDataSetStd02(xx, 120)
	#xxx[1:2000, c('date','dawn', 'Sunrise', 'daynight')]
	#xxx[1:2000, c('date')]

	x_date = unique ( as.Date (indata_sub$date1))
	

	x_nights = unique ( indata_sub$daynight1 )
	x_nights = x_nights[grep ('night', x_nights)]
	
	m_coeffs = matrix (nrow = length (x_nights), ncol = 6 )
	

	for ( i in 1:length (x_nights) )
		{
		
		#i = 1
		x_ndx = x_nights[i]
		
		data_sub_ndx = which ( indata_sub$daynight1 == x_ndx )
		
		data_sub = indata_sub[data_sub_ndx, ]
		#exclude dawn
		data_sub = data_sub[data_sub$dawn != 'dawn',]
		
		x_ndx1 = which ( !is.na (data_sub$DO_deficit))
		
		data_sub = data_sub[x_ndx1, ]
		
		#with ( data_sub, plot ( DO_deficit, DO_change ))
		n_cor <- cor(data_sub$DO_diff,data_sub$DO_deficit)
		n_samplesize <- length(data_sub$DO_diff)

		fit <- try ( lm ( data_sub$DO_diff ~ data_sub$DO_deficit ))

		n_inter <-  as.numeric (fit[[1]][1])
		n_slope <-  as.numeric (fit[[1]][2])


		m_coeffs[i,1] <- n_inter
		m_coeffs[i,2] <- n_slope
		m_coeffs[i,3] <- n_cor
		m_coeffs[i,4] <- mean (data_sub$Temp, na.rm = TRUE)
		m_coeffs[i,5] <- median (data_sub$Temp, na.rm = TRUE)
		m_coeffs[i,6] <- n_samplesize

		}


	#Ebble_CE1_2013_04_25
	outdata <- data.frame ( x_date[1:length (x_nights)], x_nights, m_coeffs )
	names (outdata) <- c ('date1', 'daynight1','ER','Ks', 'correlation', 'mean.Temp','median.Temp', 'SampleSize')

	return (outdata)
	
}


#FitRegressionParametersDataSetStd ( c_indata = 'Ebble_CE1_2013_04_25' )
#FitRegressionParametersDataSetStd ( c_indata = 'Ebble_CE1_2013_08_08' )













































































































































































FitRegressionParametersCiavatta <- function (indata, dawn_cutoff = 60)
	{
		
	#Ciavatta et al., Ecological Modelling 212 (2008), pp.28-36
	#this differs from FitRegressionParametersDataSetStd01 by cutting of the last hour before dawn
	#and therefore hopefully getting rid of spurious data
	
	#indata <- Ebble_CE1_2013_04_25
	#library (mdot); data (Minidot_02); indata <- Minidot_02; n_quality = 14; c_indata <- 'Minidot_02'

	#dirscr <- paste ( dirtop, '/DO/rovscript/', sep = '')
	
	#library(mdot)
	library (parker)
	data (parker)


	indata_sub <- NormalizeIndata(indata)
	
	indata_sub$dawn <- indata_sub$daynight
	dawn_ndx <- with (indata_sub, difftime(Sunrise, date, units = 'mins'))
	#dawn_ndx <- dawn_ndx <= 0 & dawn_ndx > -dawn_cutoff
	dawn_ndx <- dawn_ndx < dawn_cutoff & indata_sub$date < indata_sub$Sunrise
	indata_sub$dawn[dawn_ndx] <- 'dawn'

	#return (indata_sub)
	#}
	
	#xxx <- FitRegressionParametersDataSetStd02(xx, 120)
	#xxx[1:2000, c('date','dawn', 'Sunrise', 'daynight')]
	#xxx[1:2000, c('date')]

	x_date = unique ( as.Date (indata_sub$date1))
	

	x_nights = unique ( indata_sub$daynight1 )
	x_nights = x_nights[grep ('night', x_nights)]
	
	m_coeffs = matrix (nrow = length (x_nights), ncol = 6 )
	

	for ( i in 1:length (x_nights) )
		{
		
		#i = 1
		x_ndx = x_nights[i]
		
		data_sub_ndx = which ( indata_sub$daynight1 == x_ndx )
		
		data_sub = indata_sub[data_sub_ndx, ]
		#exclude dawn
		data_sub = data_sub[data_sub$dawn != 'dawn',]
		
		x_ndx1 = which ( !is.na (data_sub$DO_deficit_mean))
		
		data_sub = data_sub[x_ndx1, ]
		DO_lagged <- embed(data_sub$DO,2)
		
		
		#with ( data_sub, plot ( DO_deficit, DO_change ))
		n_cor <- cor(DO_lagged[,2],DO_lagged[,1])
		n_samplesize <- length(DO_lagged[,1])

		fit <- try ( lm ( DO_lagged[,1] ~ DO_lagged[,2] ))

		CSat_mean <- mean(data_sub$DO, na.rm = TRUE)

		n_inter <-  as.numeric (fit[[1]][1])
		n_slope <-  as.numeric (fit[[1]][2])
		
		ks <- -(log(n_slope))
		ER <- -ks * (n_inter - CSat_mean * (1-n_slope))/(1-n_slope)


		m_coeffs[i,1] <- ER
		m_coeffs[i,2] <- ks
		m_coeffs[i,3] <- n_cor
		m_coeffs[i,4] <- mean (data_sub$Temp, na.rm = TRUE)
		m_coeffs[i,5] <- median (data_sub$Temp, na.rm = TRUE)
		m_coeffs[i,6] <- n_samplesize
		}


	#Ebble_CE1_2013_04_25
	outdata <- data.frame ( x_date[1:length (x_nights)], x_nights, m_coeffs )
	names (outdata) <- c ('date1', 'daynight1','ER','Ks', 'correlation', 'mean.Temp','median.Temp', 'SampleSize')

	return (outdata)
	
}


#FitRegressionParametersDataSetStd ( c_indata = 'Ebble_CE1_2013_04_25' )
#FitRegressionParametersDataSetStd ( c_indata = 'Ebble_CE1_2013_08_08' )
#library(O2); library(parker); 
#xx <- FitRegressionParametersCiavatta (GetIndata('Ebble_CE1_2014_08_21_md','mdot.data'))
#xx <- xx[xx$Ks < 0.02,]
#plot(xx$Ks, xx$ER)
#plot(xx$date1, xx$ER)




























































































































































































































































































































































































































































































FitRegressionParametersIterate <- function ( c_dataset, c_library, c_name, n_limit = 0.007, n_quality = 2, metric_type = 1 )
	{
	#indata <- Ebble_CE1_2013_04_25
	#c_library <- mdot
	#library (mdot); data (Minidot_02); indata <- Minidot_02; n_quality = 14; c_indata <- 'Minidot_02'
	

	#dirscr <- paste ( dirtop, '/DO/rovscript/', sep = '')
	
	#library(mdot)
	library (parker)
	data (parker)
	
	do.call ( library, list (c_library))
	
	DataSets <- parker::ListData (c_library)
	rownumber <- which (DataSets[,1] == c_dataset)
	
	#return (rownumber)
	indata <- parker::LoadData (c_library, rownumber)

	
	if ( !is.null(indata$qualityFilter))
		{
		indata <- indata[indata$qualityFilter > n_quality,]
		}

	print ( str (indata))
	print ( head (indata,2))
	
	#plot (indata$date, indata$DO) 


	#max_y <- max ( indata$Cnow, indata$DO, na.rm = T )
	#min_y <- min ( indata$Cnow, indata$DO, na.rm = T )


	x_date = unique ( as.Date (indata$date1))

	

	x_nights = unique ( indata$daynight1 )
	x_nights = x_nights[grep ('night', x_nights)]
	

	m_coeffs = matrix (nrow = length (x_nights), ncol = 4 )

		for ( i in 1:length (x_nights) )
		#for ( i in 1:10 )
			{
			
			#i = 1
			x_ndx = x_nights[i]
			
			data_sub_ndx = which ( indata$daynight1 == x_ndx )
			
			data_sub = indata[data_sub_ndx, ]
			
			x_ndx1 = which ( !is.na (data_sub$DO_deficit))
			
			data_sub = data_sub[x_ndx1, ]
			
			
			x_ndx2 <- x_ndx1
			l_test <- TRUE
			while (l_test)
				{
				data_sub <- data_sub[x_ndx2,]
				#with ( data_sub, plot ( DO_deficit, DO_change ))
				n_cor <- cor(data_sub$DO_diff,data_sub$DO_deficit)
				
				summary ( data_sub$DO_diff )
				summary ( data_sub$DO_deficit )
				fit <- try ( lm ( data_sub$DO_diff ~ data_sub$DO_deficit ))
				
				n_fitted <- fitted.values (fit)
				#x_fit <- predict (fit, data_sub$DO_deficit)
				n_res <- residuals(fit)
				n_res_proportion <- (as.numeric (n_res) - as.numeric(n_fitted)) / as.numeric(n_fitted)
				print (n_res_proportion)
				print (summary (n_res_proportion))
				
				#Wait()
				plot (data_sub$DO_deficit, data_sub$DO_diff)
				points (data_sub$DO_deficit, n_fitted, col = 'red')
				
				#print (n_res)
				#print (n_res_proportion)
				
				if (metric_type == 1) {x_ndx2 <- (abs (n_res) < n_limit)}
				if (metric_type == 2) {x_ndx2 <- (abs (n_res_proportion) < n_limit)}
				print (FALSE %in% x_ndx2)
				
				Wait()
				plot (data_sub$DO_deficit, data_sub$DO_diff)
				if ( !(FALSE %in% x_ndx2)) l_test <- FALSE
				}
			
			plot (data_sub$DO_deficit, data_sub$DO_diff, col = 'green')
			points (data_sub$DO_deficit, n_fitted, col = 'red')
			Wait(0.2)
			
			n_inter <-  as.numeric (fit[[1]][1])
			n_slope <-  as.numeric (fit[[1]][2])


			m_coeffs[i,1] <- n_inter
			m_coeffs[i,2] <- n_slope
			m_coeffs[i,3] <- n_cor
			m_coeffs[i,4] <- dim(data_sub)[1]
			}

	DailyData <- MakeDailyData(indata)
	#DailyData <- MakeDailyData(indata)

	#Ebble_CE1_2013_04_25
	x_temp <- data.frame ( x_date[1:length (x_nights)], x_nights, m_coeffs )
	names (x_temp) <- c ('date1', 'daynight1','ER','Ks', 'correlation', 'N')
	x_temp <- merge (x_temp, DailyData, by = 'date1')
	assign ( paste (c_dataset, '_ER_Ks', n_limit, sep = ''), x_temp)


	x_ls <- ls() [grep ( ls(), pattern = '_ER_Ks' )]
	print ( x_ls )

	setwd (dirdmp)
	save (list =  x_ls, file =  paste ( c_dataset, '_ER_Ks', n_limit,'.rda', sep = ''))

	#return (n_res_proportion)
	return (x_temp)
	}



#FitRegressionParametersIterate ( c_dataset = 'Nadder_GN1_2014-08-20_md', c_library = 'mdot.data')
#FitRegressionParametersIterate ( c_dataset = 'Nadder_GN1_2014-08-20_md', c_library = 'mdot.data', n_limit = 0.007)
#FitRegressionParametersIterate ( c_dataset = 'Nadder_GN1_2014-08-20_md', c_library = 'mdot.data', n_limit = 20, metric_type = 2)
#regressed <- FitRegressionParametersIterate ( c_dataset = 'Nadder_GN1_2014-08-20_md', c_library = 'mdot.data', n_limit = 0.007, metric_type = 1)



#regressed01 <- FitRegressionParametersIterate ( c_dataset = 'Ebble_CE1_2014-08-21_md', c_library = 'mdot.data', n_limit = 0.007, metric_type = 1)
#regressed02 <- FitRegressionParametersIterate ( c_dataset = 'Ebble_CE1_2014-08-21_md', c_library = 'mdot.data', n_limit = 5, metric_type = 1)
#regressed03 <- FitRegressionParametersIterate ( c_dataset = 'Ebble_CE1_2014-08-21_md', c_library = 'mdot.data', n_limit = 50, metric_type = 2)


#regressed01 <- regressed01[1:41,]
#regressed02 <- regressed02[1:41,]
#regressed03 <- regressed03[1:41,]

#with (regressed01, plot ( date1, ER, type = 'b', ylim = c (-0.02, -0.00)))
#with (regressed02, points ( date1, ER, type = 'b', col = 'red'))
#with (regressed03, points ( date1, ER, type = 'b', col = 'blue'))

































































































































































































FitRegressionParametersSingleDF <- function ( c_indata, n_quality = 2 )
	{
	#indata <- Ebble_CE1_2013_04_25
	#library (mdot); data (Minidot_02); indata <- Minidot_02; n_quality = 14; c_indata <- 'Minidot_02'

	#dirscr <- paste ( dirtop, '/DO/rovscript/', sep = '')
	
	
	library (parker)
	data (parker)
	
	do.call ( data, list ( c_indata ))
	indata <- get (c_indata)
	
	
	if ( !is.null(indata$qualityFilter))
		{
		indata <- indata[indata$qualityFilter > n_quality,]
		}


	print ( head (indata))
	print ( str (indata))

	#max_y <- max ( indata$Cnow, indata$DO, na.rm = T )
	#min_y <- min ( indata$Cnow, indata$DO, na.rm = T )


	x_date = unique ( as.Date (indata$date1))

	

	x_nights = unique ( indata$daynight1 )
	x_nights = x_nights[grep ('night', x_nights)]
	
	m_coeffs = matrix (nrow = length (x_nights), ncol = 3 )
	

	for ( i in 1:length (x_nights) )
		{
		
		#i = 1
		x_ndx = x_nights[i]
		
		data_sub_ndx = which ( indata$daynight1 == x_ndx )
		
		data_sub = indata[data_sub_ndx, ]
		
		x_ndx1 = which ( !is.na (data_sub$DO_deficit))
		
		data_sub = data_sub[x_ndx1, ]
		
		#with ( data_sub, plot ( DO_deficit, DO_change ))
		n_cor <- cor(data_sub$DO_diff,data_sub$DO_deficit)
		
		summary ( data_sub$DO_diff )
		summary ( data_sub$DO_deficit )
		fit <- try ( lm ( data_sub$DO_diff ~ data_sub$DO_deficit ))

		n_inter <-  as.numeric (fit[[1]][1])
		n_slope <-  as.numeric (fit[[1]][2])


		m_coeffs[i,1] <- n_inter
		m_coeffs[i,2] <- n_slope
		m_coeffs[i,3] <- n_cor
		}


	#Ebble_CE1_2013_04_25
	x_temp <- data.frame ( x_date[1:length (x_nights)], x_nights, m_coeffs )
	names (x_temp) <- c ('date1', 'daynight1','ER','Ks', 'correlation')
	assign ( paste (c_indata, '_ER_Ks_for', sep = ''), x_temp)






































	m_coeffs = matrix (nrow = length (x_nights), ncol = 3 )


	for ( i in 1:length (x_nights) )
		{
		
		#i = 1
		x_ndx = x_nights[i]
		data_sub_ndx = which ( indata$daynight1 == x_ndx )
		data_sub = indata[data_sub_ndx, ]
		x_ndx1 = which ( !is.na (data_sub$DO_deficit))
		data_sub = data_sub[x_ndx1, ]
		
		#with ( data_sub, plot ( DO_deficit, DO_change ))
		n_cor <- cor(data_sub$DO_diff_mean,data_sub$DO_deficit)
		fit <- try ( lm ( data_sub$DO_diff_mean ~ data_sub$DO_deficit ))

		
		n_inter <-  as.numeric (fit[[1]][1])
		n_slope <-  as.numeric (fit[[1]][2])


		m_coeffs[i,1] <- n_inter
		m_coeffs[i,2] <- n_slope
		m_coeffs[i,3] <- n_cor
		}


	x_temp <- data.frame ( x_date[1:length (x_nights)], x_nights, m_coeffs )
	names (x_temp) <- c ('date1', 'daynight1','ER','Ks', 'correlation')
	assign ( paste (c_indata, '_ER_Ks_mid', sep = ''), x_temp)

























































	m_coeffs = matrix (nrow = length (x_nights), ncol = 3 )


	for ( i in 1:length (x_nights) )
		{
		
		#i = 1
		x_ndx = x_nights[i]
		data_sub_ndx = which ( indata$daynight1 == x_ndx )
		data_sub = indata[data_sub_ndx, ]
		x_ndx1 = which ( !is.na (data_sub$DO_deficit))
		data_sub = data_sub[x_ndx1, ]
		
		#with ( data_sub, plot ( DO_deficit, DO_change ))
		n_cor <- cor(data_sub$DO_diff_lag,data_sub$DO_deficit)
		fit <- try ( lm ( data_sub$DO_diff_lag ~ data_sub$DO_deficit ))

		
		n_inter <-  as.numeric (fit[[1]][1])
		n_slope <-  as.numeric (fit[[1]][2])


		m_coeffs[i,1] <- n_inter
		m_coeffs[i,2] <- n_slope
		m_coeffs[i,3] <- n_cor
		}


	x_temp <- data.frame ( x_date[1:length (x_nights)], x_nights, m_coeffs )
	names (x_temp) <- c ('date1', 'daynight1','ER','Ks', 'correlation')
	assign ( paste (c_indata, '_ER_Ks_lag', sep = ''), x_temp)





















	x_ls <- ls() [grep ( ls(), pattern = '_ER_Ks' )]
	print ( x_ls )

	setwd (dirdmp)
	save (list =  x_ls, file =  paste ( c_indata, '_ER_Ks.rda', sep = ''))
}


#FitRegressionParametersSingleDF ( c_indata = 'Ebble_CE1_2013_04_25' )
#FitRegressionParametersSingleDF ( c_indata = 'Ebble_CE1_2013_08_08' )










































































































































































































FitRegressionParametersModelled <- function (indata)
	{
	#indata <- Ebble_CE1_2013_04_25
	#library (mdot); data (Minidot_02); indata <- Minidot_02; n_quality = 14; c_indata <- 'Minidot_02'

	#dirscr <- paste ( dirtop, '/DO/rovscript/', sep = '')
	
	#library(mdot)
	library (parker)
	data (parker)

	indata <- ProcessReAerOutput (indata)

	x_date = unique ( as.Date (indata$date1))
	

	x_nights = unique ( indata_sub$daynight1 )
	x_nights = x_nights[grep ('night', x_nights)]
	
	m_coeffs = matrix (nrow = length (x_nights), ncol = 5 )

	

	for ( i in 1:length (x_nights) )
		{
		
		#i = 1
		x_ndx = x_nights[i]
		
		data_sub_ndx = which ( indata_sub$daynight1 == x_ndx )
		
		data_sub = indata_sub[data_sub_ndx, ]
		
		x_ndx1 = which ( !is.na (data_sub$DO_deficit_mean))
		
		data_sub = data_sub[x_ndx1, ]
		
		#with ( data_sub, plot ( DO_deficit, DO_change ))
		n_cor <- cor(data_sub$DO_diff_std,data_sub$DO_deficit_mean)
		
		fit <- try ( lm ( data_sub$DO_diff_std ~ data_sub$DO_deficit_mean ))

		n_inter <-  as.numeric (fit[[1]][1])
		n_slope <-  as.numeric (fit[[1]][2])


		m_coeffs[i,1] <- n_inter
		m_coeffs[i,2] <- n_slope
		m_coeffs[i,3] <- n_cor
		m_coeffs[i,4] <- mean (data_sub$Temp, na.rm = TRUE)
		m_coeffs[i,5] <- median (data_sub$Temp, na.rm = TRUE)
	
		}


	#Ebble_CE1_2013_04_25
	outdata <- data.frame ( x_date[1:length (x_nights)], x_nights, m_coeffs )
	names (outdata) <- c ('date1', 'daynight1','ER','Ks', 'correlation', 'mean.Temp','median.Temp')

	return (outdata)
	
}


#FitRegressionParametersDataSetStd ( c_indata = 'Ebble_CE1_2013_04_25' )
#FitRegressionParametersDataSetStd ( c_indata = 'Ebble_CE1_2013_08_08' )





















































































FitRegressionParameters_v01 <- function ( c_indata )
	{
	#indata <- Ebble_CE1_2013_04_25
	#dirscr <- paste ( dirtop, '/DO/rovscript/', sep = '')

	library (parker)
	data (parker)
	
	do.call ( data, list ( c_indata ))
	indata <- get (c_indata)

	print ( head (indata))
	print ( str (indata))

	#max_y <- max ( indata$Cnow, indata$DO, na.rm = T )
	#min_y <- min ( indata$Cnow, indata$DO, na.rm = T )


	x_date = unique ( as.Date (indata$date1))

	

	x_nights = unique ( indata$daynight1 )
	x_nights = x_nights[grep ('night', x_nights)]
	
	m_coeffs = matrix (nrow = length (x_nights), ncol = 3 )
	

	for ( i in 1:length (x_nights) )
		{
		
		#i = 1
		x_ndx = x_nights[i]
		
		data_sub_ndx = which ( indata$daynight1 == x_ndx )
		
		data_sub = indata[data_sub_ndx, ]
		
		x_ndx1 = which ( !is.na (data_sub$DO_deficit))
		
		data_sub = data_sub[x_ndx1, ]
		
		#with ( data_sub, plot ( DO_deficit, DO_change ))
		n_cor <- cor(data_sub$DO_diff,data_sub$DO_deficit)
		
		summary ( data_sub$DO_diff )
		summary ( data_sub$DO_deficit )
		fit <- try ( lm ( data_sub$DO_diff ~ data_sub$DO_deficit ))

		n_inter <-  as.numeric (fit[[1]][1])
		n_slope <-  as.numeric (fit[[1]][2])


		m_coeffs[i,1] <- n_inter
		m_coeffs[i,2] <- n_slope
		m_coeffs[i,3] <- n_cor
		}

	#Ebble_CE1_2013_04_25
	x_temp <- data.frame ( x_date[1:length (x_nights)], x_nights, m_coeffs )
	names (x_temp) <- c ('date1', 'daynight1','ER','Ks', 'correlation')
	assign ( paste (c_indata, '_ER_Ks_for', sep = ''), x_temp)






































	m_coeffs = matrix (nrow = length (x_nights), ncol = 3 )


	for ( i in 1:length (x_nights) )
		{
		
		#i = 1
		x_ndx = x_nights[i]
		data_sub_ndx = which ( indata$daynight1 == x_ndx )
		data_sub = indata[data_sub_ndx, ]
		x_ndx1 = which ( !is.na (data_sub$DO_deficit))
		data_sub = data_sub[x_ndx1, ]
		
		#with ( data_sub, plot ( DO_deficit, DO_change ))
		n_cor <- cor(data_sub$DO_diff_mean,data_sub$DO_deficit)
		fit <- try ( lm ( data_sub$DO_diff_mean ~ data_sub$DO_deficit ))

		
		n_inter <-  as.numeric (fit[[1]][1])
		n_slope <-  as.numeric (fit[[1]][2])


		m_coeffs[i,1] <- n_inter
		m_coeffs[i,2] <- n_slope
		m_coeffs[i,3] <- n_cor
		}


	x_temp <- data.frame ( x_date[1:length (x_nights)], x_nights, m_coeffs )
	names (x_temp) <- c ('date1', 'daynight1','ER','Ks', 'correlation')
	assign ( paste (c_indata, '_ER_Ks_mid', sep = ''), x_temp)

























































	m_coeffs = matrix (nrow = length (x_nights), ncol = 3 )


	for ( i in 1:length (x_nights) )
		{
		
		#i = 1
		x_ndx = x_nights[i]
		data_sub_ndx = which ( indata$daynight1 == x_ndx )
		data_sub = indata[data_sub_ndx, ]
		x_ndx1 = which ( !is.na (data_sub$DO_deficit))
		data_sub = data_sub[x_ndx1, ]
		
		#with ( data_sub, plot ( DO_deficit, DO_change ))
		n_cor <- cor(data_sub$DO_diff_lag,data_sub$DO_deficit)
		fit <- try ( lm ( data_sub$DO_diff_lag ~ data_sub$DO_deficit ))

		
		n_inter <-  as.numeric (fit[[1]][1])
		n_slope <-  as.numeric (fit[[1]][2])


		m_coeffs[i,1] <- n_inter
		m_coeffs[i,2] <- n_slope
		m_coeffs[i,3] <- n_cor
		}


	x_temp <- data.frame ( x_date[1:length (x_nights)], x_nights, m_coeffs )
	names (x_temp) <- c ('date1', 'daynight1','ER','Ks', 'correlation')
	assign ( paste (c_indata, '_ER_Ks_lag', sep = ''), x_temp)










	x_ls <- ls() [grep ( ls(), pattern = '_ER_Ks' )]
	print ( x_ls )

	setwd (dirdmp)
	save (list =  x_ls, file =  paste ( c_indata, '_ER_Ks.rda', sep = ''))
}


#FitRegressionParameters ( c_indata = 'Ebble_CE1_2013_04_25' )
#FitRegressionParameters ( c_indata = 'Ebble_CE1_2013_08_08' )





































































NormalizeIndata <- function(indata)
	{
	
	DO_diff_std <- embed (indata$DO,2)
	DO_diff_std <- DO_diff_std[,1] - DO_diff_std[,2]
	
	
	CSat_diff_std <- embed (indata$CSat,2)
	CSat_diff_std <- CSat_diff_std[,1] - CSat_diff_std[,2]
	

	time_diff <- embed (as.ts(indata[,c('date')]),2)
	#gives time difference in seconds
	time_diff <- time_diff[,1] - time_diff[,2]
	time_diff <- time_diff / 60
	#gives time difference in minutes
	indata$time_diff <- c(NA, time_diff)

	#standardize DO change over the time interval
	indata$DO_diff_std <- c (NA, DO_diff_std / time_diff)
	indata$CSat_diff_std <- c (NA, CSat_diff_std / time_diff)

	
	indata$DO_deficit_mean <- c(NA, rowMeans(embed(indata$CSat - indata$DO, 2)))
	indata$Temp_mean <- c(NA, rowMeans(embed(indata$Temp, 2)))

	return(indata)
	}
	
#x <- NormalizeIndata(indata); head(x)
