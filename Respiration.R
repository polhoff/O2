


#ER is already negative as a result of the correlation (intercept on y axis
Resp2 <- function ( ER )
	{
	x <- ER
	}



RespTemp01 <- function ( ER20, Temp, theta_ER )
	{
	ER <- ER20 * theta_ER ^ (Temp - 20)
	#exp1 <- 	exp ( beta1 * ( Temp - Temp0 ) )
	#x <-  rate_gas * exp1
	return (ER)
	}

#RespTemp01 (4,16,1.05)
#RespTemp01 (4,16,1)
