
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

	6.181479

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
