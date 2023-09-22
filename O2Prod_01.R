

#ReAer <- function(t, y, parms, envir = env1)
ReAer <- function(t, y, parms)
	{
	#with(as.list(c(parms, x)),
	with (as.list(c(y, parms)),
		{
		C_sat <- input_O2 (t)
		Temp <- input_T (t)
		ER <- input_ER (t)
		Ks <- input_Ks (t)
		Pmax <- input_Pmax (t)
		I_k <- input_Ik (t)
		#light <- PAR_fraction * input_light (t)
		#note that PAR fraction will have been built into the respose between DO and light
		#see FitDOProductionCurve_Rovelli.R
		light <- input_light (t)
		#force NA to zero
		light[is.na(light)] <- 0
		#guard against negative values
		light <- max (light, 0)


		
		#stopifnot ( !is.na (C_sat))
		#stopifnot ( !is.na (Temp))
		#stopifnot ( !is.na (ER))
		#stopifnot ( !is.na (Ks))
		#stopifnot ( !is.na (light))
		#stopifnot ( !is.na (P1))

		#temporary bodges
		if  (is.na (C_sat)) (C_sat = summary(indata$CSat)[3])
		if  (is.na (Temp)) (Temp = summary(indata$Temp)[3])
		if  (is.na (ER)) (ER = summary(indata$ER)[3])
		if  (is.na (Ks)) (Ks = summary(indata$Ks)[3])
		if  (is.na (light)) (light = summary(indata$Bo0)[3])
		
		
		
		stopifnot ( !is.na (ER))
		stopifnot ( !is.na (Ks))
		stopifnot ( !is.na (light))





		Ks = max ( 0, Ks )
		#ER always negative
		ER = min ( 0, ER )
		

		dPhot 	<- do.call ( 'Phot',  eval ( photo_params ))
		dResp 	<-  do.call ( 'Resp',  eval ( resp_params ))
		dGas	 <-  do.call ( 'Gas', eval ( gas_params ))
		
		
		#divide by 1000 because milligrams to grams
		#...but not necessary because mg per litre is g per m cubed
		#dCnow  = Ks * ( C_sat - Cnow ) / 1000 * exp ( beta1 * ( Temp - Temp0 ) )
		#dCnow  = Ks * ( C_sat - Cnow )  * exp ( beta1 * ( Temp - Temp0 ) )
		dCnow  = dGas + dResp + dPhot

		#Cnow = min (Cnow, 3 * C_sat)
		#Cnow = max (Cnow, 0)
		

		list ( dCnow, 'Phot' = dPhot, 'Resp' = dResp, 'Gas' = dGas, 'C_sat' = C_sat, 'Cdiff' = C_sat - Cnow, 'Cpercent' = Cnow / C_sat,  'light' = light, 'T' =  Temp, 'ER'  = ER,  'Pmax'  = Pmax)

		}
		)
	}
















































		

#ReAer <- function(t, y, parms, envir = env1)
ReAerNight01 <- function(t, y, parms)
	{
	#with(as.list(c(parms, x)),
	with (as.list(c(y, parms)),
		{
		C_sat <- input_O2 (t)
		Temp <- input_T (t)
		#ER <- input_ER (t)
		#Ks <- input_Ks (t)
		#Pmax <- input_Pmax (t)
		#I_k <- input_Ik (t)
		#light <- PAR_fraction * input_light (t)
		#note that PAR fraction will have been built into the respose between DO and light
		#see FitDOProductionCurve_Rovelli.R
		#light <- input_light (t)
		#force NA to zero
		#light[is.na(light)] <- 0
		#guard against negative values
		#light <- max (light, 0)


		
		#stopifnot ( !is.na (C_sat))
		#stopifnot ( !is.na (Temp))
		#stopifnot ( !is.na (ER))
		#stopifnot ( !is.na (Ks))
		#stopifnot ( !is.na (light))
		#stopifnot ( !is.na (P1))

		#temporary bodges
		if  (is.na (C_sat)) (C_sat = summary(indata$CSat)[3])
		if  (is.na (Temp)) (Temp = summary(indata$Temp)[3])
		if  (is.na (ER)) (ER = summary(indata$ER)[3])
		if  (is.na (Ks)) (Ks = summary(indata$Ks)[3])
		#if  (is.na (light)) (light = summary(indata$Bo0)[3])
		
		
		
		stopifnot ( !is.na (ER))
		stopifnot ( !is.na (Ks))
		#stopifnot ( !is.na (light))





		Ks = max ( 0, Ks )
		#ER always negative
		ER = min ( 0, ER )
		

		#dPhot 	<- do.call ( 'Phot',  eval ( photo_params ))
		dResp 	<-  do.call ( 'Resp',  eval ( resp_params ))
		dGas	 <-  do.call ( 'Gas', eval ( gas_params ))
		
		
		#divide by 1000 because milligrams to grams
		#...but not necessary because mg per litre is g per m cubed
		#dCnow  = Ks * ( C_sat - Cnow ) / 1000 * exp ( beta1 * ( Temp - Temp0 ) )
		#dCnow  = Ks * ( C_sat - Cnow )  * exp ( beta1 * ( Temp - Temp0 ) )
		#dCnow  = dGas + dResp + dPhot
		dCnow  = dGas + dResp

		#Cnow = min (Cnow, 3 * C_sat)
		#Cnow = max (Cnow, 0)
		

		list ( dCnow,'Resp' = dResp, 'Gas' = dGas, 'C_sat' = C_sat, 'Cdiff' = C_sat - Cnow, 'Cpercent' = Cnow / C_sat, 'T' =  Temp, 'ER'  = ER)

		}
		)
	}





























































#ReAer <- function(t, y, parms, envir = env1)
ReAerNightArrh01 <- function(t, y, parms)
	{
	#with(as.list(c(parms, x)),
	with (as.list(c(y, parms)),
		{
		C_sat <- input_O2 (t)
		Temp <- input_T (t)

		#temporary bodges
		if  (is.na (C_sat)) (C_sat = summary(indata$CSat)[3])
		if  (is.na (Temp)) (Temp = summary(indata$Temp)[3])
		#if  (is.na (ER)) (ER = summary(indata$ER)[3])
		#if  (is.na (Ks)) (Ks = summary(indata$Ks)[3])
		#if  (is.na (light)) (light = summary(indata$Bo0)[3])

		
		#stopifnot ( !is.na (ER))
		#stopifnot ( !is.na (Ks))
		#stopifnot ( !is.na (light))


		#Ks = max ( 0, Ks )
		#ER always negative
		#ER = min ( 0, ER )
		

		#dPhot 	<- do.call ( 'Phot',  eval ( photo_params ))
		dResp 	<-  do.call ( 'Resp',  eval ( resp_params ))
		dGas	 <-  do.call ( 'Gas', eval ( gas_params ))
		
		
		#divide by 1000 because milligrams to grams
		#...but not necessary because mg per litre is g per m cubed
		#dCnow  = Ks * ( C_sat - Cnow ) / 1000 * exp ( beta1 * ( Temp - Temp0 ) )
		#dCnow  = Ks * ( C_sat - Cnow )  * exp ( beta1 * ( Temp - Temp0 ) )
		#dCnow  = dGas + dResp + dPhot
		dCnow  = dGas + dResp

		#Cnow = min (Cnow, 3 * C_sat)
		#Cnow = max (Cnow, 0)
		

		list ( dCnow,'Resp' = dResp, 'Gas' = dGas, 'C_sat' = C_sat, 'Cdiff' = C_sat - Cnow, 'Cpercent' = Cnow / C_sat, 'T' =  Temp, 'ER20'  = ER20, 'K20' = K20)

		}
		)
	}


























































#ReAer <- function(t, y, parms, envir = env1)
#reaeration function using time series of R and K
ReAerNightArrhRespTS <- function(t, y, parms)
	{
	#with(as.list(c(parms, x)),
	with (as.list(c(y, parms)),
		{
		C_sat <- input_O2 (t)
		Temp <- input_T (t)
		ER20 <- input_ER20 (t)
		K20 <- input_K20 (t)


		#temporary bodges
		if  (is.na (C_sat)) (C_sat = summary(indata$CSat)[3])
		if  (is.na (Temp)) (Temp = summary(indata$Temp)[3])
		#if  (is.na (ER)) (ER = summary(indata$ER)[3])
		#if  (is.na (Ks)) (Ks = summary(indata$Ks)[3])
		#if  (is.na (light)) (light = summary(indata$Bo0)[3])

		
		#stopifnot ( !is.na (ER))
		#stopifnot ( !is.na (Ks))
		#stopifnot ( !is.na (light))


		#Ks = max ( 0, Ks )
		#ER always negative
		#ER = min ( 0, ER )
		

		#dPhot 	<- do.call ( 'Phot',  eval ( photo_params ))
		dResp 	<-  do.call ( 'Resp',  eval ( resp_params ))
		dGas	 <-  do.call ( 'Gas', eval ( gas_params ))
		
		
		#divide by 1000 because milligrams to grams
		#...but not necessary because mg per litre is g per m cubed
		#dCnow  = Ks * ( C_sat - Cnow ) / 1000 * exp ( beta1 * ( Temp - Temp0 ) )
		#dCnow  = Ks * ( C_sat - Cnow )  * exp ( beta1 * ( Temp - Temp0 ) )
		#dCnow  = dGas + dResp + dPhot
		dCnow  = dGas + dResp

		#Cnow = min (Cnow, 3 * C_sat)
		#Cnow = max (Cnow, 0)
		

		list ( dCnow,'Resp' = dResp, 'Gas' = dGas, 'C_sat' = C_sat, 'Cdiff' = C_sat - Cnow, 'Cpercent' = Cnow / C_sat, 'T' =  Temp, 'ER20'  = ER20, 'K20' = K20)

		}
		)
	}
