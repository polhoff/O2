

CalcCsat03 <- function (input) 
	{
		
	stopifnot(class (input) == 'matrix' | class (input) == 'data.frame'  )
	stopifnot('Temp' %in% names (input))
	stopifnot('AbsPres_kPa' %in% names (input))
	
	Temp = input[ ,c ('Temp')]
    pressure = input[ , c ('AbsPres_kPa')]
    
    #Temp = input$Temp
    #pressure = input$Pressure

    stopifnot(Temp >= 0 & Temp < 40 | is.na (Temp))
    stopifnot(pressure >= 90 & pressure < 108 | is.na (pressure))
    floorTemp = floor(Temp)
    floorpressure = floor(pressure)
    c_pressure = paste("kPa", as.matrix(floorpressure), sep = "")
    a1 = match(as.matrix(floorTemp), O2_sol[, c("Temp")])
    b1 = match(c_pressure, names(O2_sol))

    c1 = paste ( 'O2_sol', '[', a1, ',', b1, ']', sep = '')
    d1 = paste ( 'O2_sol', '[', a1+1, ',', b1, ']', sep = '')
    e1 = paste ( 'O2_sol', '[', a1, ',', b1+1, ']', sep = '')


    GetCsat <- function(x)		{		try (eval(parse(text=x)))		}


    O2_mg1 = sapply(X = c1, FUN=GetCsat)
    O2_mg2 = sapply(X = d1, FUN=GetCsat)
	O2_mg3 = sapply(X = e1, FUN=GetCsat)


    O2_mg = try((O2_mg2 - O2_mg1) * (Temp - floorTemp) + O2_mg1)
    pressure_adj = try((O2_mg3 - O2_mg1) * (pressure - floorpressure))
    
    O2_mg = O2_mg + pressure_adj
    return(O2_mg)
	}

#CalcCsat03 (input)





































































CalcCsat <- function (input) 
	{
		
	stopifnot(class (input) == 'matrix' | class (input) == 'data.frame'  )
	stopifnot('Temp' %in% names (input))
	stopifnot('AbsPres_kPa' %in% names (input))
	stopifnot('date' %in% names (input) | 'date1' %in% names (input))

	
		
    Temp = input[ ,c ('Temp')]
    pressure = input[ , c ('AbsPres_kPa')]
    indate = try (input[ c ('date1')])
    indate = try (input[ c ('date')])
    CSat_all <- indate
    #assign empty values to vector of required length
	CSat_all[] <- NA


    stopifnot(Temp >= 0 & Temp < 40 | is.na (Temp))
    stopifnot(pressure >= 90 & pressure < 108 | is.na (pressure))

	Temp[is.na(Temp)] <- 39.9
	pressure[is.na(pressure)] <- 90.1
	pressure[is.na(pressure)] <- 90.1

	#x_ndx <- !is.na(Temp) & !is.na(pressure)
	#Temp <- Temp[x_ndx]
	#pressure <- pressure[x_ndx]
	

    floorTemp = floor(Temp)
    floorpressure = floor(pressure)
    c_pressure = paste("kPa", as.matrix(floorpressure), sep = "")
    a1 = try (match(as.matrix(floorTemp), O2_sol[, c("Temp")]))
    b1 = try (match(c_pressure, names(O2_sol)))

    c1 = paste ( 'O2_sol', '[', a1, ',', b1, ']', sep = '')
    d1 = paste ( 'O2_sol', '[', a1+1, ',', b1, ']', sep = '')
    e1 = paste ( 'O2_sol', '[', a1, ',', b1+1, ']', sep = '')


    GetCsat <- function(x)		{		try (eval(parse(text=x)))		}


    O2_mg1 = sapply(X = c1, FUN=GetCsat)
    #O2_mg1 = try (sapply(X = c1, FUN=function(x) return(eval(parse(text=x)))))
    O2_mg2 = sapply(X = d1, FUN=GetCsat)
	O2_mg3 = sapply(X = e1, FUN=GetCsat)

    O2_mg = (O2_mg2 - O2_mg1) * (Temp - floorTemp) + O2_mg1
    pressure_adj = (O2_mg3 - O2_mg1) * (pressure - floorpressure)


    O2_mg = O2_mg + pressure_adj
    
	#CSat_all[x_ndx] <- O2_mg

	
	O2_mg[O2_mg <- 7] <- NA
	return (O2_mg)
    #return(CSat_all)
	}

#xx <- CalcCsat02 (input)




















































CalcCsat_arc1 <- function (input) 
	{
		
	stopifnot(class (input) == 'matrix' | class (input) == 'data.frame'  )
    Temp = input[,1]
    pressure = input[,2]

    stopifnot(Temp >= 0 & Temp < 40 | is.na (Temp))
    stopifnot(pressure >= 90 & pressure < 108 | is.na (pressure))
    floorTemp = floor(Temp)
    floorpressure = floor(pressure)
    c_pressure = paste("kPa", as.matrix(floorpressure), sep = "")
    a1 = match(as.matrix(floorTemp), O2_sol[, c("Temp")])
    b1 = match(c_pressure, names(O2_sol))

    c1 = paste ( 'O2_sol', '[', a1, ',', b1, ']', sep = '')
    d1 = paste ( 'O2_sol', '[', a1+1, ',', b1, ']', sep = '')
    e1 = paste ( 'O2_sol', '[', a1, ',', b1+1, ']', sep = '')


    GetCsat <- function(x)		{		try (eval(parse(text=x)))		}


    O2_mg1 = sapply(X = c1, FUN=GetCsat)
    O2_mg2 = sapply(X = d1, FUN=function(x) return(eval(parse(text=x))))

    O2_mg = (O2_mg2 - O2_mg1) * (Temp - floorTemp) + O2_mg1


    O2_mg1 = sapply(X = c1, FUN=function(x) return(eval(parse(text=x))))
    O2_mg2 = sapply(X = e1, FUN=function(x) return(eval(parse(text=x))))

    pressure_adj = (O2_mg2 - O2_mg1) * (pressure - floorpressure)

    O2_mg = O2_mg + pressure_adj
    return(O2_mg)
	}

#CalcCsat1 (input)


















































CalcCsat02 <- function (input) 
	{
		
	stopifnot(class (input) == 'matrix' | class (input) == 'data.frame'  )
	stopifnot('Temp' %in% names (input))
	stopifnot('AbsPres_kPa' %in% names (input))
	stopifnot('date' %in% names (input) | 'date1' %in% names (input))

	
		
    Temp = input[ ,c ('Temp')]
    pressure = input[ , c ('AbsPres_kPa')]
    indate = try (input[ c ('date1')])
    indate = try (input[ c ('date')])
    CSat_all <- indate
    #assign empty values to vector of required length
	CSat_all[] <- NA


    stopifnot(Temp >= 0 & Temp < 40 | is.na (Temp))
    stopifnot(pressure >= 90 & pressure < 108 | is.na (pressure))

	Temp[is.na(Temp)] <- 39.9
	pressure[is.na(pressure)] <- 90.1
	pressure[is.na(pressure)] <- 90.1

	#x_ndx <- !is.na(Temp) & !is.na(pressure)
	#Temp <- Temp[x_ndx]
	#pressure <- pressure[x_ndx]
	

    floorTemp = floor(Temp)
    floorpressure = floor(pressure)
    c_pressure = paste("kPa", as.matrix(floorpressure), sep = "")
    a1 = try (match(as.matrix(floorTemp), O2_sol[, c("Temp")]))
    b1 = try (match(c_pressure, names(O2_sol)))

    c1 = paste ( 'O2_sol', '[', a1, ',', b1, ']', sep = '')
    d1 = paste ( 'O2_sol', '[', a1+1, ',', b1, ']', sep = '')
    e1 = paste ( 'O2_sol', '[', a1, ',', b1+1, ']', sep = '')
    
    O2_mg1 = try (sapply(X = c1, FUN=function(x) return(eval(parse(text=x)))))
    O2_mg2 = try (sapply(X = d1, FUN=function(x) return(eval(parse(text=x)))))

    O2_mg = (O2_mg2 - O2_mg1) * (Temp - floorTemp) + O2_mg1


    O2_mg1 = try (sapply(X = c1, FUN=function(x) return(eval(parse(text=x)))))
    O2_mg2 = try (sapply(X = e1, FUN=function(x) return(eval(parse(text=x)))))

    pressure_adj = (O2_mg2 - O2_mg1) * (pressure - floorpressure)

    O2_mg = O2_mg + pressure_adj
    
	#CSat_all[x_ndx] <- O2_mg

	
	O2_mg[O2_mg <- 7] <- NA
	return (O2_mg)
    #return(CSat_all)
	}

#xx <- CalcCsat02 (input)













































































































CalcCsat1 <- function (input) 
	{
		
	stopifnot(class (input) == 'matrix' | class (input) == 'data.frame'  )
    Temp = input[,1]
    pressure = input[,2]

    stopifnot(Temp >= 0 & Temp < 40 | is.na (Temp))
    stopifnot(pressure >= 90 & pressure < 108 | is.na (pressure))
    floorTemp = floor(Temp)
    floorpressure = floor(pressure)
    c_pressure = paste("kPa", as.matrix(floorpressure), sep = "")
    a1 = match(as.matrix(floorTemp), O2_sol[, c("Temp")])
    b1 = match(c_pressure, names(O2_sol))

    c1 = paste ( 'O2_sol', '[', a1, ',', b1, ']', sep = '')
    d1 = paste ( 'O2_sol', '[', a1+1, ',', b1, ']', sep = '')
    e1 = paste ( 'O2_sol', '[', a1, ',', b1+1, ']', sep = '')
    
    O2_mg1 = sapply(X = c1, FUN=function(x) return(eval(parse(text=x))))
    O2_mg2 = sapply(X = d1, FUN=function(x) return(eval(parse(text=x))))

    O2_mg = (O2_mg2 - O2_mg1) * (Temp - floorTemp) + O2_mg1


    O2_mg1 = sapply(X = c1, FUN=function(x) return(eval(parse(text=x))))
    O2_mg2 = sapply(X = e1, FUN=function(x) return(eval(parse(text=x))))

    pressure_adj = (O2_mg2 - O2_mg1) * (pressure - floorpressure)

    O2_mg = O2_mg + pressure_adj
    return(O2_mg)
	}

#CalcCsat1 (input)










































#diravon = paste ( dirtop, '/avon/', sep = '' )
#dir02 = paste ( diravon, '/O2/', sep = '' )

#O2_sol = read.csv ( paste ( dir02, 'O2_solubility.csv', sep = '' ), header = T)

#CalcCsat <- function ( Temp, pressure )
CalcCsat_arc <- function ( input )
	{

	Temp = input[1]
	pressure = input[2]

	stopifnot(Temp >= 0 & Temp < 40 | is.na (Temp))
    stopifnot(pressure >= 90 & pressure < 108 | is.na (pressure))
    
	#Temp = 34.5
	#pressure = 100.5
	
	floorTemp = floor (Temp)
	floorpressure = floor (pressure)
	
	
	c_pressure = paste ( 'kPa', floorpressure, sep = '' )
	
	
	a1 = match ( floorTemp, O2_sol[ , c('Temp')] )
	b1 =  which ( names(O2_sol) == c_pressure)
	
	
	O2_mg1 = O2_sol[a1,b1]
	O2_mg2 = O2_sol[a1+1,b1]
	
	O2_mg =  (O2_mg2 -  O2_mg1) * (Temp - floorTemp) + O2_mg1
	
	O2_mg1 = O2_sol[a1,b1]
	O2_mg2 = O2_sol[a1,b1 + 1]

	# pressure - floorpressure will always be between 0 and 1
	# therefore this is a proportion
	pressure_adj = (O2_mg2 -  O2_mg1) * (pressure - floorpressure)
	
	O2_mg = O2_mg + pressure_adj
	
	return (O2_mg)
	}

#save (O2_sol, file = 'O2_sol.rda')








































#CalcCsat <- function ( Temp, pressure )
CalcCsatSingleValue <- function ( n_temp, n_pressure )
	{
	data(O2_sol)
	
	#n_temp = 8.248356; n_pressure = 101.805833333333
	
	Temp = n_temp
	pressure = n_pressure

	stopifnot(Temp >= 0 & Temp < 40 | is.na (Temp))
    stopifnot(pressure >= 90 & pressure < 108 | is.na (pressure))
    
	#Temp = 34.5
	#pressure = 100.5
	
	floorTemp = floor (Temp)
	floorpressure = floor (pressure)
	
	
	c_pressure = paste ( 'kPa', floorpressure, sep = '' )
	
	
	a1 = match ( floorTemp, O2_sol[ , c('Temp')] )
	b1 =  which ( names(O2_sol) == c_pressure)
	
	
	O2_mg1 = O2_sol[a1,b1]
	O2_mg2 = O2_sol[a1+1,b1]
	
	O2_mg =  (O2_mg2 -  O2_mg1) * (Temp - floorTemp) + O2_mg1
	
	O2_mg1 = O2_sol[a1,b1]
	O2_mg2 = O2_sol[a1,b1 + 1]

	# pressure - floorpressure will always be between 0 and 1
	# therefore this is a proportion
	pressure_adj = (O2_mg2 -  O2_mg1) * (pressure - floorpressure)
	
	O2_mg = O2_mg + pressure_adj
	
	return (O2_mg)
	}

#save (O2_sol, file = 'O2_sol.rda')

#CalcCsatSingleValue(23.6949,101.805833333333)
#8.511341
