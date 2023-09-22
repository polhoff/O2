

#(Webb et al., 1974)
Webb <- function ( light, Pmax, I_k )
	{
	#I_k was set above and is constant
	y  = light /  I_k 
	x  =  Pmax * ( 1 - exp(-y))
	}




#(Jassby and Platt, 1976)
Jassby1 <- function ( light, Pmax, I_k )
	{
	#I_k was set above and is constant
	y  = light /  I_k
	x  =  Pmax * tanh (y)
	}



#zero for nighttime
NighttimePhot <- function ( light )
	{
	x <- 0
	}

