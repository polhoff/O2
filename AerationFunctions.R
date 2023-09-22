
Gas1 <- function ( Ks, C_sat, Cnow, beta1, Temp, Temp0 )
	{
	rate_gas <- 	Ks * ( C_sat - Cnow )
	exp1 <- 	exp ( beta1 * ( Temp - Temp0 ) )
	x <-  rate_gas * exp1
	return (x)
	}


Gas2 <- function ( Ks, C_sat, Cnow )
	{
	rate_gas <- 	Ks * ( C_sat - Cnow )
	#exp1 <- 	exp ( beta1 * ( Temp - Temp0 ) )
	#x <-  rate_gas * exp1
	return (rate_gas)
	}


Gas3 <- function (Ks, C_sat, Cnow) 
	{
	print (paste ('Ks...', Ks))
	print (C_sat)
	print (Cnow)
    rate_gas <- Ks * (C_sat - Cnow)
    return(rate_gas)
	}


GasTemp01 <- function ( K20, C_sat, Cnow, Temp, theta_K )
	{
	Ks <- K20 * theta_K ^ (Temp - 20)
	rate_gas <- 	Ks * ( C_sat - Cnow )
	#exp1 <- 	exp ( beta1 * ( Temp - Temp0 ) )
	#x <-  rate_gas * exp1
	return (rate_gas)
	}

#GasTemp01 (4,10,9,16,1.02)
#GasTemp01 (4,10,9,16,1.05)
#GasTemp01 (4,10,8,16,1.02)
#GasTemp01 (4,10,8,16,1.10)
