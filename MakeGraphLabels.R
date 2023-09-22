
MakeGraphLabels <- function (dirout)
	{
	DO_label_sim_timeless <- expression(paste ('Simulated DO  (', g~O[2]~m^{-3}, ' )'))

	DO_label_timeless <- expression(paste ('DO  (', g~O[2]~m^{-3}, ' )'))
	DO_label_minute <- expression(paste ('DO  (', g~O[2]~m^{-3}~minute^{-1}, ' )'))
	DO_label_hour <- expression(paste ('DO  (', g~O[2]~m^{-3}~hour^{-1}, ' )'))
	DO_label_day <- expression(paste ('DO  (', g~O[2]~m^{-3}~day^{-1}, ' )'))
	
	DO_label_day_raw <- expression(paste ('DO  (', g~O[2]~m^{-3}~day^{-1}, ' ) (raw data)'))
	DO_label_day_15 <- expression(paste ('DO  (', g~O[2]~m^{-3}~day^{-1}, ' ) (15 minute data)'))

	GPP_label_minute <- expression(paste ('GPP  (', g~O[2]~m^{-3}~minute^{-1}, ' )'))
	GPP_label_hour <- expression(paste ('GPP  (', g~O[2]~m^{-3}~hour^{-1}, ' )'))
	GPP_label_day <- expression(paste ('GPP  (', g~O[2]~m^{-3}~day^{-1}, ' )'))


	DO_deficit_label <-  expression(paste('DO deficit (', g~O[2]~m^{-3}, ' )'))
	DO_change_label <-  expression(paste (Delta, ' DO (', g~O[2]~m^{-3}, ' )'))
	DO_saturation_label <- expression(paste('DO at saturation (', g~O[2]~m^{-3}, ' )'))
	DO_generic_label_hour <- expression(~(g~O[2]~m^{-3}~hour^{-1}))
	DO_median_deficit_label <-  expression(paste('Median DO deficit (', g~O[2]~m^{-3}, ' )'))
	Regression_quotient_label <-  expression(paste('Regression quotient (', g~O[2]~m^{-3}, ' )'))

	
	#Regression_quotient_label_2line <-  expression(paste('Regression quotient \n (', g~O[2]~m^{-3}, ' )'))
	#Regression_quotient_label_3line <-  expression(paste('Regression \n quotient \n (', g~O[2]~m^{-3}, ' )'))

	Regression_quotient_label_2line <-  expression(atop('Regression quotient', paste('(', g~O[2]~m^{-3}, ' )')))
	Regression_quotient_label_3line <-  expression(atop('Regression \n quotient', paste('(', g~O[2]~m^{-3}, ' )')))

	DO_median_deficit_label_2line <-  expression(atop('Median DO deficit', paste('(', g~O[2]~m^{-3}, ' )')))
	
	
	
	
	
	
	Hornberger_KellyRatio <- expression(paste('Ratio (R to k) (', g~O[2]~m^{-3}, ' )'))
	DO_deficitTo_HKRatio <- 'Ratio of DO deficit to regression quotient'
	#DO_deficitTo_HKRatio <- 'Ratio of DO deficit to ER/Ks quotient'
	
	
	
	r_label_timeless_Manta <- expression(paste ('R  (', g~O[2]~m^{-3}, ' ) Manta sonde'))
	r_label_timeless_mdot <- expression(paste ('R  (', g~O[2]~m^{-3}, ' ) MiniDOT'))
	
	
	r_label_hour_Manta <- expression(paste ('R  (', g~O[2]~m^{-3}~hour^{-1}, ' ) Manta sonde'))
	r_label_hour_mdot <- expression(paste ('R  (', g~O[2]~m^{-3}~hour^{-1}, ' ) MiniDOT'))
	
	r_label_timeless <- expression(paste ('R  (', g~O[2]~m^{-3}, ' )'))
	r_label_minute <- expression(paste ('R  (', g~O[2]~m^{-3}~minute^{-1}, ' )'))
	r_label_hour <- expression(paste ('R  (', g~O[2]~m^{-3}~hour^{-1}, ' )'))
	r_label_day <- expression(paste ('R  (', g~O[2]~m^{-3}~day^{-1}, ' )'))


	r_label_hour_Hornberger <- expression(paste ('R  (', g~O[2]~m^{-3}~hour^{-1}, ' ) Nighttime regression'))
	r_label_hour_CurveFit <- expression(paste ('R  (', g~O[2]~m^{-3}~hour^{-1}, ' ) Curve fitting'))
	

	k_label_hour_Hornberger <- expression(paste ('k  (', hour^{-1}, ') Nighttime regression'))
	k_label_hour_CurveFit <- expression(paste ('k  (', hour^{-1}, ') Curve fitting'))


	k_label_timeless <- 'k'
	k_label_minute <- expression(paste ('k  (', minute^{-1}, ' )'))
	k_label_hour <- expression(paste ('k  (', hour^{-1}, ' )'))
	k_label_day <- expression(paste ('k  (', day^{-1}, ' )'))

	k_label_hour_Manta <- expression(paste ('k  (', hour^{-1}, ' ) Manta sonde'))
	k_label_hour_mdot <- expression(paste ('k  (', hour^{-1}, ' ) MiniDOT'))
	

	DO_label_sim_timeless_mg <- expression(paste ('Simulated DO  (', mg~O[2]~litre^{-1}, ' )'))

	DO_label_timeless_mg <- expression(paste ('DO  (', mg~O[2]~litre^{-1}, ' )'))
	DO_label_minute_mg <- expression(paste ('DO  (', mg~O[2]~litre^{-1}~minute^{-1}, ' )'))
	DO_label_hour_mg <- expression(paste ('DO  (', mg~O[2]~litre^{-1}~hour^{-1}, ' )'))
	DO_label_day_mg <- expression(paste ('DO  (', mg~O[2]~litre^{-1}~day^{-1}, ' )'))


	r_label_timeless_mg <- expression(paste ('R  (', mg~O[2]~litre^{-1}, ' )'))
	r_label_minute_mg <- expression(paste ('R  (', mg~O[2]~litre^{-1}~minute^{-1}, ' )'))
	r_label_hour_mg <- expression(paste ('R  (', mg~O[2]~litre^{-1}~hour^{-1}, ' )'))
	r_label_day_mg <- expression(paste ('R  (', mg~O[2]~litre^{-1}~day^{-1}, ' )'))


	DO_deficit_label_mg <-  expression(paste('DO deficit (', mg~O[2]~litre^{-1}, ' )'))
	DO_change_label_mg <-  expression(paste (Delta, ' DO (', mg~O[2]~litre^{-1}, ' )'))
	DO_saturation_label_mg <- expression(paste('DO at saturation (', mg~O[2]~litre^{-1}, ' )'))
	DO_generic_label_hour_mg <- expression(~(mg~O[2]~litre^{-1}~hour^{-1}))
	DO_median_deficit_label_mg <- expression(paste('Median DO deficit (', mg~O[2]~litre^{-1}, ' )'))
	
	DeltaDO <-  expression(paste (Delta, ' DO'))

	Hornberger_KellyRatio_mg <- expression(paste('Ratio (R to k) (', mg~O[2]~litre^{-1}, ' )'))
	

	Temp_degC <- expression(Temperature~(degree~C))
	Temp_degC_noBrack <- expression(Temperature~degree~C)
	
	PAR_label_second <- expression(paste ('PAR (', mu~mol~photons~m^{-2}~s^{-1}, ' )'))
	ETSW_label_second <- expression(paste ('ETSW (', Watts~m^{-2}, ' )'))



	x = list(DO_label_timeless, DO_label_minute, DO_label_hour, DO_label_day, DO_label_day_raw, DO_label_day_15, DO_deficit_label, DO_change_label, DO_saturation_label, DO_generic_label_hour, r_label_timeless, r_label_minute, r_label_hour, r_label_day, DO_label_timeless_mg, DO_label_minute_mg, DO_label_hour_mg, DO_label_day_mg, r_label_timeless_mg, r_label_minute_mg, r_label_hour_mg, r_label_day_mg, DO_deficit_label_mg, DO_change_label_mg, DO_saturation_label_mg, DO_generic_label_hour_mg,  k_label_timeless, k_label_minute, k_label_hour, k_label_day, PAR_label_second, ETSW_label_second, DO_label_sim_timeless, DO_label_sim_timeless_mg, GPP_label_day, GPP_label_hour, GPP_label_minute, Temp_degC, Temp_degC_noBrack, r_label_timeless_Manta, r_label_timeless_mdot, r_label_hour_Manta, r_label_hour_mdot, k_label_hour_Manta, k_label_hour_mdot, r_label_hour_Hornberger, r_label_hour_CurveFit, k_label_hour_Hornberger, k_label_hour_CurveFit, Hornberger_KellyRatio_mg, Hornberger_KellyRatio, DO_median_deficit_label_mg, DO_median_deficit_label, DO_deficitTo_HKRatio, Regression_quotient_label, DeltaDO, Regression_quotient_label_2line, Regression_quotient_label_3line, DO_median_deficit_label_2line
	)





	names(x) <- c('DO_label_timeless', 'DO_label_minute', 'DO_label_hour', 'DO_label_day', 'DO_label_day_raw', 'DO_label_day_15', 'DO_deficit_label', 'DO_change_label', 'DO_saturation_label', 'DO_generic_label_hour', 'r_label_timeless', 'r_label_minute', 'r_label_hour', 'r_label_day', 'DO_label_timeless_mg', 'DO_label_minute_mg', 'DO_label_hour_mg', 'DO_label_day_mg', 'r_label_timeless_mg', 'r_label_minute_mg', 'r_label_hour_mg', 'r_label_day_mg', 'DO_deficit_label_mg', 'DO_change_label_mg', 'DO_saturation_label_mg', 'DO_generic_label_hour_mg', 'k_label_timeless', 'k_label_minute', 'k_label_hour', 'k_label_day', 'PAR_label_second', 'ETSW_label_second', 'DO_label_sim_timeless', 'DO_label_sim_timeless_mg', 'GPP_label_day', 'GPP_label_hour', 'GPP_label_minute', 'Temp_degC', 'Temp_degC_noBrack', 'r_label_timeless_Manta', 'r_label_timeless_mdot', 'r_label_hour_Manta', 'r_label_hour_mdot', 'k_label_hour_Manta', 'k_label_hour_mdot', 'r_label_hour_Hornberger', 'r_label_hour_CurveFit', 'k_label_hour_Hornberger', 'k_label_hour_CurveFit', 'Hornberger_KellyRatio_mg', 'Hornberger_KellyRatio', 'DO_median_deficit_label_mg', 'DO_median_deficit_label', 'DO_deficitTo_HKRatio', 'Regression_quotient_label', 'DeltaDO', 'Regression_quotient_label_2line', 'Regression_quotient_label_3line', 'DO_median_deficit_label_2line'
	)



	save(list = c('DO_label_timeless', 'DO_label_minute', 'DO_label_hour', 'DO_label_day', 'DO_label_day_raw', 'DO_label_day_15', 'DO_deficit_label', 'DO_change_label', 'DO_saturation_label', 'DO_generic_label_hour', 'r_label_timeless', 'r_label_minute', 'r_label_hour', 'r_label_day', 'DO_label_timeless_mg', 'DO_label_minute_mg', 'DO_label_hour_mg', 'DO_label_day_mg', 'r_label_timeless_mg', 'r_label_minute_mg', 'r_label_hour_mg', 'r_label_day_mg', 'DO_deficit_label_mg', 'DO_change_label_mg', 'DO_saturation_label_mg', 'DO_generic_label_hour_mg', 'k_label_timeless', 'k_label_minute', 'k_label_hour', 'k_label_day',  'PAR_label_second', 'ETSW_label_second', 'DO_label_sim_timeless', 'DO_label_sim_timeless_mg', 'GPP_label_day', 'GPP_label_hour', 'GPP_label_minute', 'Temp_degC', 'Temp_degC_noBrack', 'r_label_timeless_Manta', 'r_label_timeless_mdot', 'r_label_hour_Manta', 'r_label_hour_mdot', 'k_label_hour_Manta', 'k_label_hour_mdot','r_label_hour_Hornberger', 'r_label_hour_CurveFit', 'k_label_hour_Hornberger', 'k_label_hour_CurveFit','Hornberger_KellyRatio_mg', 'Hornberger_KellyRatio', 'DO_median_deficit_label_mg', 'DO_median_deficit_label', 'DO_deficitTo_HKRatio', 'Regression_quotient_label', 'DeltaDO', 'Regression_quotient_label_2line', 'Regression_quotient_label_3line', 'DO_median_deficit_label_2line'
	 ), file = paste(dirout, 'DO_graph_labels.rda', sep= ''))

	return(x)
	}


#library(st); data(st)
#MakeGraphLabels(dirdmp)



PostProcessGraphLabels <- function(x = MakeGraphLabels(dirout = '/home/sjparker/sp/dump/'))
	{
	c_names <- names(x)

	for ( i in 1:length(x))
		{
		x1 <- unlist(x[i])
		x2 <- x[[i]]
		x3 <- names(x1)
		assign(x3,x2, env = .GlobalEnv)
		}
	return(x)	
	}


#library(st); data(st)
#rm(list = ls())
#y1 <- PostProcessGraphLabels()
#y1 <- PostProcessGraphLabels(dirout = dirdmp)














































MakeGraphLabelsBquote <- function (dirout)
	{
	DO_label_sim_timeless <- expression(paste ('Simulated DO  (', g~O[2]~m^{-3}, ' )'))

	DO_label_timeless <- expression(paste ('DO  (', g~O[2]~m^{-3}, ' )'))
	DO_label_minute <- expression(paste ('DO  (', g~O[2]~m^{-3}~minute^{-1}, ' )'))
	DO_label_hour <- expression(paste ('DO  (', g~O[2]~m^{-3}~hour^{-1}, ' )'))
	DO_label_day <- expression(paste ('DO  (', g~O[2]~m^{-3}~day^{-1}, ' )'))
	
	DO_label_day_raw <- expression(paste ('DO  (', g~O[2]~m^{-3}~day^{-1}, ' ) (raw data)'))
	DO_label_day_15 <- expression(paste ('DO  (', g~O[2]~m^{-3}~day^{-1}, ' ) (15 minute data)'))

	GPP_label_minute <- expression(paste ('GPP  (', g~O[2]~m^{-3}~minute^{-1}, ' )'))
	GPP_label_hour <- expression(paste ('GPP  (', g~O[2]~m^{-3}~hour^{-1}, ' )'))
	GPP_label_day <- expression(paste ('GPP  (', g~O[2]~m^{-3}~day^{-1}, ' )'))


	DO_deficit_label <-  expression(paste('DO deficit (', g~O[2]~m^{-3}, ' )'))
	DO_change_label <-  expression(paste (Delta, ' DO (', g~O[2]~m^{-3}, ' )'))
	DO_saturation_label <- expression(paste('DO at saturation (', g~O[2]~m^{-3}, ' )'))
	DO_generic_label_hour <- expression(~(g~O[2]~m^{-3}~hour^{-1}))

	
	r_label_timeless_Manta <- expression(paste ('R  (', g~O[2]~m^{-3}, ' )'))
	r_label_timeless_mdot <- expression(paste ('R  (', g~O[2]~m^{-3}~minute^{-1}, ' )'))
	r_label_hour <- expression(paste ('R  (', g~O[2]~m^{-3}~hour^{-1}, ' )'))
	r_label_day <- expression(paste ('R  (', g~O[2]~m^{-3}~day^{-1}, ' )'))


	k_label_timeless <- 'k'
	k_label_minute <- expression(paste ('k  (', minute^{-1}, ' )'))
	k_label_hour <- expression(paste ('k  (', hour^{-1}, ' )'))
	k_label_day <- expression(paste ('k  (', day^{-1}, ' )'))
	

	DO_label_sim_timeless_mg <- expression(paste ('Simulated DO  (', mg~O[2]~litre^{-1}, ' )'))

	DO_label_timeless_mg <- expression(paste ('DO  (', mg~O[2]~litre^{-1}, ' )'))
	DO_label_minute_mg <- expression(paste ('DO  (', mg~O[2]~litre^{-1}~minute^{-1}, ' )'))
	DO_label_hour_mg <- expression(paste ('DO  (', mg~O[2]~litre^{-1}~hour^{-1}, ' )'))
	DO_label_day_mg <- expression(paste ('DO  (', mg~O[2]~litre^{-1}~day^{-1}, ' )'))


	r_label_timeless_mg <- expression(paste ('R  (', mg~O[2]~litre^{-1}, ' )'))
	r_label_minute_mg <- expression(paste ('R  (', mg~O[2]~litre^{-1}~minute^{-1}, ' )'))
	r_label_hour_mg <- expression(paste ('R  (', mg~O[2]~litre^{-1}~hour^{-1}, ' )'))
	r_label_day_mg <- expression(paste ('R  (', mg~O[2]~litre^{-1}~day^{-1}, ' )'))


	DO_deficit_label_mg <-  expression(paste('DO deficit (', mg~O[2]~litre^{-1}, ' )'))
	DO_change_label_mg <-  expression(paste (Delta, ' DO (', mg~O[2]~litre^{-1}, ' )'))
	DO_saturation_label_mg <- expression(paste('DO at saturation (', mg~O[2]~litre^{-1}, ' )'))
	DO_generic_label_hour_mg <- expression(~(mg~O[2]~litre^{-1}~hour^{-1}))

	DeltaDO <-  expression(paste (Delta, ' DO'))

	Temp_degC <- expression(Temperature~(degree~C))
	Temp_degC_noBrack <- expression(Temperature~degree~C)
	
	PAR_label_second <- expression(paste ('PAR (', mu~mol~photons~m^{-2}~s^{-1}, ' )'))
	ETSW_label_second <- expression(paste ('ETSW (', Watts~m^{-2}, ' )'))



	x = list(DO_label_timeless, DO_label_minute, DO_label_hour, DO_label_day, DO_label_day_raw, DO_label_day_15, DO_deficit_label, DO_change_label, DO_saturation_label, DO_generic_label_hour, r_label_timeless, r_label_minute, r_label_hour, r_label_day, DO_label_timeless_mg, DO_label_minute_mg, DO_label_hour_mg, DO_label_day_mg, r_label_timeless_mg, r_label_minute_mg, r_label_hour_mg, r_label_day_mg, DO_deficit_label_mg, DO_change_label_mg, DO_saturation_label_mg, DO_generic_label_hour_mg,  k_label_timeless, k_label_minute, k_label_hour, k_label_day, PAR_label_second, ETSW_label_second, DO_label_sim_timeless, DO_label_sim_timeless_mg, GPP_label_day, GPP_label_hour, GPP_label_minute, Temp_degC, Temp_degC_noBrack, DeltaDO)



	names(x) <- c('DO_label_timeless', 'DO_label_minute', 'DO_label_hour', 'DO_label_day', 'DO_label_day_raw', 'DO_label_day_15', 'DO_deficit_label', 'DO_change_label', 'DO_saturation_label', 'DO_generic_label_hour', 'r_label_timeless', 'r_label_minute', 'r_label_hour', 'r_label_day', 'DO_label_timeless_mg', 'DO_label_minute_mg', 'DO_label_hour_mg', 'DO_label_day_mg', 'r_label_timeless_mg', 'r_label_minute_mg', 'r_label_hour_mg', 'r_label_day_mg', 'DO_deficit_label_mg', 'DO_change_label_mg', 'DO_saturation_label_mg', 'DO_generic_label_hour_mg', 'k_label_timeless', 'k_label_minute', 'k_label_hour', 'k_label_day', 'PAR_label_second', 'ETSW_label_second', 'DO_label_sim_timeless', 'DO_label_sim_timeless_mg', 'GPP_label_day', 'GPP_label_hour', 'GPP_label_minute', 'Temp_degC', 'Temp_degC_noBrack', 'DeltaDO')



	save(list = c('DO_label_timeless', 'DO_label_minute', 'DO_label_hour', 'DO_label_day', 'DO_label_day_raw', 'DO_label_day_15', 'DO_deficit_label', 'DO_change_label', 'DO_saturation_label', 'DO_generic_label_hour', 'r_label_timeless', 'r_label_minute', 'r_label_hour', 'r_label_day', 'DO_label_timeless_mg', 'DO_label_minute_mg', 'DO_label_hour_mg', 'DO_label_day_mg', 'r_label_timeless_mg', 'r_label_minute_mg', 'r_label_hour_mg', 'r_label_day_mg', 'DO_deficit_label_mg', 'DO_change_label_mg', 'DO_saturation_label_mg', 'DO_generic_label_hour_mg', 'k_label_timeless', 'k_label_minute', 'k_label_hour', 'k_label_day',  'PAR_label_second', 'ETSW_label_second', 'DO_label_sim_timeless', 'DO_label_sim_timeless_mg', 'GPP_label_day', 'GPP_label_hour', 'GPP_label_minute', 'Temp_degC', 'Temp_degC_noBrack', 'DeltaDO'), file = paste(dirout, 'DO_graph_labels.rda', sep= ''))

	return(x)
	}

#MakeGraphLabels()


PostProcessGraphLabelsBquote <- function(x = MakeGraphLabels(dirout = '/home/sjparker/sp/dump/'))
	{
	c_names <- names(x)

	for ( i in 1:length(x))
		{
		x1 <- unlist(x[i])
		x2 <- x[[i]]
		x3 <- names(x1)
		assign(x3,x2, env = .GlobalEnv)
		}
	return(x)	
	}


#library(st); data(st)
#rm(list = ls())
#y1 <- PostProcessGraphLabels()
