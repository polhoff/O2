
PlotDO <- function ( infile, c_library, n_quality)
	{
	do.call ( library, list (c_library))
	do.call ( data, list (infile))
	#indata <- do.call ( data, list (infile))
	#indata <- do.call ( data, list (infile, package = c_library))
	
	indata <- get (infile)
	
	print (head (indata, 2))
	
	par (mar = c (7,7,4,4))
	par (cex.axis = 1.6)
	par (cex.lab = 1.6)

	x_ndx <- 1:dim(indata)[1]
	if ( with (indata, exists('qualityFilter')))
		{
		x_ndx <- with (indata, qualityFilter > n_quality)
		}

	par (mar = c (7,7,4,4))
	par (cex.axis = 1.6)
	par (cex.lab = 1.6)

	plot ( indata$date[x_ndx], indata$DO[x_ndx], xlab = 'Date', ylab = 'DO (mg per litre)' )
	}

#PlotDO ( 'Minidot_03' )
#PlotDO ( 'Minidot_04' )
#PlotDO ( 'Ebble_CE1_2013_04_25', c_library = 'rovelli' )
#PlotDO ( 'Ebble_CE1_2013_08_08', c_library = 'rovelli.data', n_quality = 2 )
#ListData ('mdot.data')
#PlotDO ( 'Sem_AS1_2014-08-21_md', c_library = 'mdot.data', n_quality = 3 )
#PlotDO ( 'Ebble_CE1_2014-08-21_md', c_library = 'mdot.data', n_quality = 3 )
#PlotDO ( 'DO_Avon', c_library = 'avon' )


















































































































































PlotDODaily <- function ( infile, c_library, n_quality)
	{
	
	library (plyr)
	do.call ( library, list (c_library))
	do.call ( data, list (infile))
	#indata <- do.call ( data, list (infile))
	#indata <- do.call ( data, list (infile, package = c_library))
	
	indata <- get (infile)
	
	print (head (indata, 2))
	
	par (mar = c (7,7,4,4))
	par (cex.axis = 1.6)
	par (cex.lab = 1.6)

	x_ndx <- 1:dim(indata)[1]
	if ( with (indata, exists('qualityFilter')))
		{
		x_ndx <- with (indata, qualityFilter > n_quality)
		}



	c_var <- 'DO'
	#data_sub_day <-  ddply (indata[c ( c_var, 'date1')], .(date1), summarize, max = max(get (c_var), na.rm = TRUE), mean = mean ( get(c_var), na.rm = TRUE), min = min (get(c_var), na.rm = TRUE))
	data_sub_day <-  ddply (indata[c ( 'DO', 'date1')], .(date1), summarize, max = max(DO, na.rm = TRUE), mean = mean (DO, na.rm = TRUE), min = min (DO, na.rm = TRUE))
	print ( head (data_sub_day))

	
	max_y = max ( data_sub_day [c ('max','min','mean')], na.rm = TRUE)
	min_y = min ( data_sub_day [c ('max','min','mean')], na.rm = TRUE)


	par (mar = c (7,7,4,4))
	par (cex.axis = 1.6)
	par (cex.lab = 1.6)



	plot ( data_sub_day$date1, data_sub_day$max, xlab = 'Date', ylab = 'DO (mg per litre)', ylim = c (min_y, max_y) )
	points ( data_sub_day$date1, data_sub_day$mean, col = 'red')
	points ( data_sub_day$date1, data_sub_day$min, col = 'grey50')
	}


#ListData ('mdot.data')
#PlotDODaily ( 'Sem_AS1_2014-08-21_md', c_library = 'mdot.data', n_quality = 3 )
#PlotDODaily ( 'DO_Avon', c_library = 'avon' )
#PlotDODaily ( 'Ebble_CE1_2014-08-21_md', c_library = 'mdot.data', n_quality = 3 )






























































































































PlotDOPhot <- function ( indata )
	{

	par (mar = c (7,7,4,4))
	par (cex.axis = 1.6)
	par (cex.lab = 1.6)

	#with ( indata, plot (  PARraw, DO_Phot, xlab = expression ( paste( 'PAR ', mu,'moles m ^{-2}', sep = '')), ylab = 'DO (mg per litre)' ))
	with ( indata, plot (  PARraw, DO_Phot, xlab = expression ( PAR~~(mu~moles~~photons~~m^{-2}~~per~~'unit'~~'time')), ylab = 'DO (mg per litre)' ))
	#expression (Area~~(km^{2})) )
	
	}

#PlotDOPhot (data03)




















































PlotER <- function ( infile, c_library = 'mdot')
	{

	do.call ( data, list (infile))
	name_data <- paste (infile, '_for', sep = '')
	indata <- get (name_data)
	
	par (mar = c (7,9,4,4))
	par (cex.axis = 1.6)
	par (cex.lab = 1.6)

	plot ( indata$date1, indata$ER, xlab = 'Date', ylab = 'net DO from ER (-ve) \n and GW influx (mg per litre) \n per unit time' )
	}


#PlotER ( 'Minidot_02_ER_Ks' )
#PlotER ( 'Minidot_03_ER_Ks' )
#PlotER ( 'Minidot_04_ER_Ks' )
#PlotER ( 'Minidot_05_ER_Ks' )
#PlotER ( 'Minidot_06_ER_Ks' )
#PlotER ( 'Minidot_07_ER_Ks' )
#PlotER ( 'Minidot_08_ER_Ks' )









































PlotKs <- function ( infile, c_library = 'mdot')
	{

	do.call ( data, list (infile))
	name_data <- paste (infile, '_for', sep = '')
	indata <- get (name_data)
	
	par (mar = c (7,7,4,4))
	par (cex.axis = 1.6)
	par (cex.lab = 1.6)

	plot ( indata$date1, indata$Ks, xlab = 'Date', ylab = 'Aeration constant per unit time' )
	}


#PlotKs ( 'Minidot_02_ER_Ks' )
#PlotKs ( 'Minidot_03_ER_Ks' )
#PlotKs ( 'Minidot_04_ER_Ks' )
#PlotKs ( 'Minidot_05_ER_Ks' )
#PlotKs ( 'Minidot_06_ER_Ks' )
#PlotKs ( 'Minidot_07_ER_Ks' )
#PlotKs ( 'Minidot_08_ER_Ks' )


















































PlotERvsKs <- function ( infile, c_library = 'mdot')
	{

	do.call ( data, list (infile))
	name_data <- paste (infile, '_for', sep = '')
	indata <- get (name_data)
	
	par (mar = c (7,7,4,4))
	par (cex.axis = 1.6)
	par (cex.lab = 1.6)

	plot ( indata$Ks, indata$ER, ylab = 'ER and GW response', xlab = 'Aeration constant per unit time' )
	}


#PlotERvsKs ( 'Minidot_02_ER_Ks' )
#PlotERvsKs ( 'Minidot_03_ER_Ks' )
#PlotERvsKs ( 'Minidot_04_ER_Ks' )
#PlotERvsKs ( 'Minidot_05_ER_Ks' )
#PlotERvsKs ( 'Minidot_06_ER_Ks' )
#PlotERvsKs ( 'Minidot_07_ER_Ks' )
#PlotERvsKs ( 'Minidot_08_ER_Ks' )


