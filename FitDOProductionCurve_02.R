
CalcDOPhot <- function (indata, l_missinglight = FALSE)
	{
	
	#indata = inputToPhot
	if (l_missinglight) indata$PARraw = NA
	stopifnot ( c ('DO_diff', 'DO_deficit', 'PARraw', 'ER', 'Ks', 'date') %in% names (indata) )
	
	
	aeration <- indata$Ks * indata$DO_deficit 
	respiration <- indata$ER
	
	indata$DO_Phot <- indata$DO_diff - respiration - aeration
	indata$aeration <- aeration

	x <- indata [c ('date', 'DO_Phot', 'PARraw', 'DO_diff', 'DO_deficit','ER', 'Ks', 'aeration')]
	x <- try ( indata [c ('date', 'DO_Phot', 'PARraw', 'DO_diff', 'DO_deficit','ER', 'Ks', 'aeration')] )
	
	if ( !is.null(indata$splitpoint))
		{
		x <- try ( indata [c ('date', 'DO_Phot', 'PARraw', 'DO_diff', 'DO_deficit','ER', 'Ks','aeration', 'splitpoint')] )
		}

	return ( x )
	}











































CalcDOPhot_v01_old <- function (indata)
	{
	stopifnot ( c ('DO_diff', 'DO_deficit', 'PARraw', 'ER', 'Ks', 'date') %in% names (indata) )

	aeration <- indata$Ks * indata$DO_deficit 
	respiration <- indata$ER
	
	indata$DO_Phot <- indata$DO_diff - respiration - aeration

	
	x <- try ( indata [c ('date', 'DO_Phot', 'PARraw', 'DO_diff', 'DO_deficit','ER', 'Ks')] )
	
	if ( !is.null(indata$splitpoint))
		{
		x <- try ( indata [c ('date', 'DO_Phot', 'PARraw', 'DO_diff', 'DO_deficit','ER', 'Ks', 'splitpoint')] )
		}

	return ( x )
	}
























































































FitProductionCurve <- function (indata, which_return = 1, lower_Pmax = 0.01, upper_Pmax = 0.2, lower_Ik = 10, upper_Ik = 500 )
	{

	#library (parker)

	stopifnot ( c ('DO_diff', 'DO_deficit', 'PARraw', 'ER', 'Ks') %in% names (indata) )
	stopifnot ( which_return %in% c (1,2))

	max_light <- max (indata$PARraw, na.rm = TRUE)
	min_light <- min (indata$PARraw, na.rm = TRUE)
	light_input = 1:floor(max_light)

		
	Pmax_possible <- seq ( lower_Pmax, upper_Pmax, by = 0.005)
	Pmax_possible <- seq ( lower_Pmax, upper_Pmax, by = 0.001)

	Ik_possible <- seq ( lower_Ik, upper_Ik, by = 10 )


	param_matrix <- expand.grid ( Pmax_possible, Ik_possible )
	names (param_matrix) <- c ('Pmax','I_k')

	MSE <- rep ( NA, dim(param_matrix)[1])


	PARraw_adj <- pmax (1,indata$PARraw)
	PARraw_adj <- pmin (PARraw_adj, floor(max_light))


	aeration <- indata$Ks * indata$DO_deficit 
	respiration <- indata$ER

	
	DO_Phot <- indata$DO_diff - respiration - aeration


	for ( i in 1:dim(param_matrix)[1])
	#for ( i in 1:100)
		{
		Pmax <- param_matrix[i,1]
		I_k <- param_matrix[i,2]

		x3 <- Jassby1 (light_input, Pmax, I_k)
		banana <- approxfun ( light_input, x3 )
		
		out <- banana ( PARraw_adj )
		MSE[i] <- sum ((DO_Phot - out) ^ 2, na.rm = T)
		}


	x_ndx <- which.min (MSE)
	print ( 'min(MSE) is...........' )
	print (MSE [x_ndx])
	print (x_ndx)


	print ( 'and params are .....' )
	print (param_matrix[x_ndx,])


	#pngfilename = 'Rovelli_fitPmax.png'
	#l_png = T
	#l_png = F
	
	setwd (tempdir())

	png(filename = paste ('Rovelli_fitPmax', Sys.time(),'.png', sep = ''))
	par ( mar = c ( 7,7,6,4))

	plot(indata$PARraw, DO_Phot, main = 'Dioxygen produced by photosynthesis, River Ebble, 2013', ylab = 'Dioxygen (mg per litre per minute)', xlab = 'PAR (micromoles photons per metre squared per second)', cex.axis = 1.6, cex.lab = 1.6, cex.main = 1.6, ylim = c ( -0.02, 0.1))
	#points(indata$PARav, indata$DO_Phot, col = 'red')
	points ( light_input, x3, col = 'gray70', pch = 2, type = 'l', lwd = 8 )
	dev.off()
	#PngOff()
	#l_png = F


	#x_list <- list ( param_matrix[x_ndx,], indata$DO_Phot ) 
	if (which_return == 1)
		{
		return (param_matrix[x_ndx,])
		}
	
	
	x = list ( param_matrix[x_ndx,], DO_Phot)
	names(x) <- c ( 'params', 'DO_Phot')
	
	if (which_return == 2)
		{
		return (x)
		}
	
	}






































































FitProductionCurve_old <- function (indata, which_return = 1, lower_Pmax = 0.01, upper_Pmax = 0.1, lower_Ik = 20, upper_Ik = 500 )
	{

	#library (parker)

	stopifnot ( c ('DO_diff', 'DO_deficit', 'PARraw', 'ER', 'Ks') %in% names (indata) )
	stopifnot ( which_return %in% c (1,2))

	max_light <- max (indata$PARraw)
	min_light <- min (indata$PARraw)
	light_input = 1:floor(max_light)

		
	Pmax_possible <- seq ( lower_Pmax, upper_Pmax, by = 0.001)
	Ik_possible <- seq ( lower_Ik, upper_Ik, by = 10 )


	param_matrix <- expand.grid ( Pmax_possible, Ik_possible )
	names (param_matrix) <- c ('Pmax','I_k')

	MSE <- rep ( NA, dim(param_matrix)[1])


	PARraw_adj <- pmax (1,indata$PARraw)
	PARraw_adj <- pmin (PARraw_adj, floor(max_light))


	aeration <- indata$Ks * indata$DO_deficit 
	respiration <- indata$ER

	
	DO_Phot <- indata$DO_diff - respiration - aeration


	for ( i in 1:dim(param_matrix)[1])
	#for ( i in 1:100)
		{
		Pmax <- param_matrix[i,1]
		I_k <- param_matrix[i,2]

		x3 <- Jassby1 (light_input, Pmax, I_k)
		banana <- approxfun ( light_input, x3 )
		
		out <- banana ( PARraw_adj )
		MSE[i] <- sum ((DO_Phot - out) ^ 2, na.rm = T)
		}


	x_ndx <- which.min (MSE)
	print ( 'min(MSE) is...........' )
	print (MSE [x_ndx])
	print (x_ndx)


	print ( 'and params are .....' )
	print (param_matrix[x_ndx,])


	#pngfilename = 'Rovelli_fitPmax.png'
	#l_png = T
	#l_png = F
	
	setwd (tempdir())

	png(filename = paste ('Rovelli_fitPmax', Sys.time(),'.png', sep = ''))
	par ( mar = c ( 7,7,6,4))

	plot(indata$PARraw, DO_Phot, main = 'Dioxygen produced by photosynthesis, River Ebble, 2013', ylab = 'Dioxygen (mg per litre per minute)', xlab = 'PAR (micromoles photons per metre squared per second)', cex.axis = 1.6, cex.lab = 1.6, cex.main = 1.6, ylim = c ( -0.02, 0.1))
	#points(indata$PARav, indata$DO_Phot, col = 'red')
	points ( light_input, x3, col = 'gray70', pch = 2, type = 'l', lwd = 8 )
	dev.off()
	#PngOff()
	#l_png = F


	#x_list <- list ( param_matrix[x_ndx,], indata$DO_Phot ) 
	if (which_return == 1)
		{
		return (param_matrix[x_ndx,])
		}
	
	
	x = list ( param_matrix[x_ndx,], DO_Phot)
	names(x) <- c ( 'params', 'DO_Phot')
	
	if (which_return == 2)
		{
		return (x)
		}
	
	}





































































PreProcessDOPhot <- function (c_indata1, c_indata2, c_library = 'rovelli' )
	{
	
	do.call ( library, list (c_library))

	do.call ( data, list (c_indata1))
	do.call ( data, list (c_indata2))

	a1 <- get ( c_indata1 )
	a2 <- get ( c_indata2 )

	data_new <- merge (a1, a2, by = 'date')
	data_new$date1 <- try (data_new$date1.x)

	return (data_new)
	}



#data01 <- PreProcessDOPhot (c_indata1 = 'Ebble_Eddy', c_indata2 = 'Ebble_ER_Ks_timeseries')
#data02 <- CutDataDataSetByTime (indata = data01, timeinterval = 120, n_date = as.Date ('2013-04-26'),  lon = -1.92392, lat = 51.02816)
#data03 <- CalcDOPhot (data02)










































































PreProcessDOPhot01 <- function (indata1, indata2, c_library = 'rovelli' )
	{
	do.call ( library, list (c_library))
	
	#assume data.frame
	a1 <- indata1
	a2 <- indata2

	if ( class(indata1) == 'character' )
		{
		do.call ( data, list (indata1))
		a1 <- get ( indata1 )
		}
	
	if ( class(indata2) == 'character' )
		{
		do.call ( data, list (indata2))
		a2 <- get ( indata2 )
		}

	data_new <- merge (a1, a2, by = 'date')
	if (try ( class (data_new$date1.x)) == 'Date' )
		{
		data_new$date1 <- try (data_new$date1.x)
		}
	
	return (data_new)
	}


#data01 <- PreProcessDOPhot01 ( indata1 = 'Ebble_CE1_2013_04_25', indata2 = ER_Ks_timeseries )
#data02 <- CutDataDataSetByTime (indata = data01, timeinterval = 120, n_date = as.Date ('2013-04-26'),  lon = -1.92392, lat = 51.02816)
#data03 <- CalcDOPhot (data02)
