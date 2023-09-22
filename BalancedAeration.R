
BalancedAeration01 <- function(indata, analysisperiod = c(50,70), d_date, Ks = 0.01, l_dawnanalysis = TRUE, l_outputdatasetonly = FALSE, l_resultsonly = FALSE )
	{
	#indata <- Ebble_CE1_2013_04_25; analysisperiod = c(50,70); d_date = as.Date('2013-04-26'); Ks = 0.01; l_dawnanalysis = TRUE

	#indata <- Ebble_CE1_2013_04_25; analysisperiod = c(50,70); d_date = as.Date('2013-04-27'); Ks = 0.01; l_dawnanalysis = TRUE

	data_sub <- indata[indata$date1 == d_date,]
	data_sub <- data_sub[,c('date','TimeAfterMidnight','TimeAfterSolarNoon','DO','DO_deficit','CSat','PARraw','PARav')]
	#TimeAfterSunrise <- c(NA,with(data_sub,difftime(date, Sunrise, units = 'mins')))
	#TimeAfterSunset <- c(NA,with(data_sub,difftime(date, Sunset, units = 'mins')))
	
	
	#include > 0.5 because of time anomaly at around midnight
	morning_ndx <- with(data_sub, TimeAfterMidnight > 0.5 & TimeAfterMidnight < 12)
	#evening_ndx <- with(data_sub, TimeAfterSolarNoon > (2*60) & TimeAfterSolarNoon < (10*60))
	evening_ndx <- with(data_sub, TimeAfterSolarNoon > 2 & TimeAfterSolarNoon < 10)
	
	
	
	if(l_dawnanalysis) data_sub <- data_sub[morning_ndx,]
	if(!l_dawnanalysis) data_sub <- data_sub[evening_ndx,]

	#this analysis assumes a timestep of 1 minute
	#using 1 minute timestep the default aeration rate above is 0.01 per minute
	start_date <- data_sub$date[!is.na(data_sub$date)][1]

	outputs <- matrix(nrow = length(analysisperiod), ncol = 3)
	outputs <- data.frame(outputs)
	names(outputs) <- c('DO_change','TimeInterval','SummedPAR')

	for (i in 1:2)
		{
		endpoint <- analysisperiod[i]
		#endpoint <- 70
		base_vector <- 1:endpoint

		#matrix for indices of all minute points in the day in blocks of 60 or thirty etc...
		m_indices <- matrix(nrow = endpoint, ncol = 1440)
		#m_indices <- matrix(nrow = endpoint, ncol = 1440)

		#initialize
		#each column of m_indices is an index that will select a subset of 50 minutes in a day
		out <- base_vector
		for (j in 2:1440)
			{
			out <- out + 1
			m_indices[,j] <- out
			}

		#make m_Csat the same dimensions as m_indices
		m_CSat <- m_indices
		m_CSat[] <- NA
		
		
		#populate m_Csat with the DO deficit at that point in the day
		for (j in 1:dim(m_CSat)[2])
			{
			m_CSat[,j] <- data_sub$DO_deficit[m_indices[,j]]
			}
			

		#populate m_Csat with the DO deficit at that point in the day
		aeration <- Ks * m_CSat
		aeration_allposs <- aeration
		
		aeration <- colSums(aeration)
		aeration_orig <- aeration
		#square aeration so that I can find the minimum
		aeration <- (aeration ^ 2) ^ 0.5
		aer_ndx <- which.min(aeration)
		aeration_orig <- aeration_orig[aer_ndx]
		aeration_singlecolumn <- aeration_allposs[,aer_ndx]
		
		
		data_sub01 <- data_sub[m_indices[,aer_ndx],]
		data_sub01 <- data_sub01[,c('date', 'DO','CSat','DO_deficit', 'PARraw', 'PARav')]
		data_sub01$Aeration <- aeration_singlecolumn
		
		
		#abs() because if it's evening, will be a decrease
		#.....although does not work
		#outputs$DO_change[i] <- abs(tail(data_sub01$DO,1) - head(data_sub01$DO,1))
		outputs$DO_change[i] <- tail(data_sub01$DO,1) - head(data_sub01$DO,1)
		
		
		
		time_diff <- difftime(tail(data_sub01$date,1),head(data_sub01$date,1), units = 'mins')
		outputs$TimeInterval[i] <- time_diff
		outputs$SummedPAR[i] <- with(data_sub01, mean(PARraw, na.rm = TRUE)) * as.numeric(time_diff)
		outputs$NetAeration[i] <- aeration_orig
		outputs$MeanAeration[i] <- aeration_orig / as.numeric(time_diff)

		assign(paste('data_period.0', i, sep = ''), data_sub01)

		#assign(paste('DO_change_', i, sep = ''), tail(data_sub01$DO,1) - head(data_sub01$DO,1))
		#assign(paste('time_diff_', i, sep = ''), difftime(tail(data_sub01$date,1),head(data_sub01$date,1), units = 'mins'))
		#assign(paste('PAR_sum_', i, sep = ''), with(data_sub01, mean(PARraw, na.rm = TRUE)) * as.numeric(time_diff01))
		}

	#attach(outputs)
	part1 <- with(outputs, TimeInterval[1] * DO_change[2] - TimeInterval[2] * DO_change[1])
	part2 <- with(outputs, TimeInterval[1] * SummedPAR[2] - TimeInterval[2] * SummedPAR[1])
	
	disparity.DOchange <- with(outputs,DO_change[2] - DO_change[1])
	disparity.PAR <- with(outputs,SummedPAR[2] - SummedPAR[1])
	
	n1 <- analysisperiod[1]
	n2 <- analysisperiod[2]
	
	n2.to.n1 <- n2/n1
	
	
	PIslope.numerator <- outputs$DO_change[2] - n2.to.n1 * outputs$DO_change[1]
	PIslope.denominator <- outputs$SummedPAR[2] - n2.to.n1 * outputs$SummedPAR[1]
	
	PIslope <- PIslope.numerator / PIslope.denominator
	
	
	ER <- (outputs$DO_change[1] - PIslope * outputs$SummedPAR[1]) / n1
	
	
	
	#k <- part1 / part2
	#ER1 <- with(outputs, (DO_change[1] - k*SummedPAR[1]) / TimeInterval[1])
	#ER2 <- with(outputs, (DO_change[2] - k*SummedPAR[2]) / TimeInterval[2])
	
	#ER <- round(mean(ER1,ER2),4)

	Pmax <- 0.045
	I_k <- Pmax / PIslope

	outputs	<- list('Determinants' = outputs)
	outputs$parameters = list('PIslope' = PIslope, 'ER' = ER, 'Dawn' = l_dawnanalysis, 'Date' = head(data_sub01$date,1), 'Pmax (assumed value)' = Pmax, 'I_k (given Pmax)' = I_k)

	#output result only
	if(!l_resultsonly){outputs$data_period.01 = data_period.01; outputs$data_period.02 = data_period.02}
	
	#output dataset only
	if(l_outputdatasetonly) outputs <- list(data_period.01, data_period.02)
		
	return(outputs)
	}

#library(rovelli); indata <- get(data(Ebble_CE1_2013_04_25)); d_date <- as.Date('2013-04-26')
#BalancedAeration01 (indata, analysisperiod = c(50,70), as.Date('2013-04-26'), Ks = 0.01)
#BalancedAeration01 (indata, analysisperiod = c(50,70), as.Date('2013-04-26'), Ks = 0.01, l_dawnanalysis = FALSE)
#BalancedAeration01 (indata, analysisperiod = c(50,70), as.Date('2013-04-26'), Ks = 0.01, l_dawnanalysis = FALSE, l_resultsonly = TRUE)
#BalancedAeration01 (indata, analysisperiod = c(50,70), as.Date('2013-04-25'), Ks = 0.01, l_dawnanalysis = FALSE)
#BalancedAeration01 (indata, analysisperiod = c(50,70), as.Date('2013-04-26'), Ks = 0.01,l_lateday = TRUE)






















































BalancedAeration <- function(indata, analysisperiod = c(50,70), d_date, Ks = 0.01, l_halfday = TRUE, l_lateday = FALSE, UpToMinutesAfterDawn = 60)
	{
	
	#indata <- Ebble_CE1_2013_04_25; analysisperiod = c(50,70); d_date = as.Date('2013-04-26'); Ks = 0.01;l_halfday = TRUE; l_lateday = FALSE; UpToMinutesAfterDawn = 60



	data_sub <- indata[indata$date1 == d_date,]

	#dawn analysis
	if(l_halfday & !l_lateday)
		{
		TimeAfterSunrise <- c(NA,with(data_sub,difftime(date, Sunrise, units = 'mins')))
		#less than 6 hours after sunrise
		data_sub <- data_sub[TimeAfterSunrise < UpToMinutesAfterDawn,]
		}

	#dusk analysis
	if(l_lateday)
		{
		TimeAfterSunrise <- c(NA,with(data_sub,difftime(date, Sunrise, units = 'mins')))
		#less than 6 hours after sunrise
		data_sub <- data_sub[TimeAfterSunrise > UpToMinutesAfterDawn,]
		}

	#this analysis assumes a timestep of 1 minute
	#using 1 minute timestep the default aeration rate above is 0.01 per minute
	start_date <- data_sub$date[!is.na(data_sub$date)][1]

	outputs <- matrix(nrow = length(analysisperiod), ncol = 3)
	outputs <- data.frame(outputs)
	names(outputs) <- c('DO_change','TimeInterval','SummedPAR')

	for (i in 1:2)
		{
		endpoint <- analysisperiod[i]
		#endpoint <- 70
		base_vector <- 1:endpoint

		#matrix for indices of all minute points in the day in blocks of 60 or thirty etc...
		m_indices <- matrix(nrow = endpoint, ncol = 1440)
		#m_indices <- matrix(nrow = endpoint, ncol = 1440)

		#initialize
		#each column of m_indices is an index that will select a subset of 50 minutes in a day
		out <- base_vector
		for (j in 2:1440)
			{
			out <- out + 1
			m_indices[,j] <- out
			}

		#make m_Csat the same dimensions as m_indices
		m_CSat <- m_indices
		m_CSat[] <- NA
		
		
		#populate m_Csat with the DO deficit at that point in the day
		for (j in 1:dim(m_CSat)[2])
			{
			m_CSat[,j] <- data_sub$DO_deficit[m_indices[,j]]
			}
			

		#populate m_Csat with the DO deficit at that point in the day
		aeration <- Ks * m_CSat
		aeration <- colSums(aeration)
		#square aeration so that I can find the minimum
		aeration <- (aeration ^ 2) ^ 0.5
		aer_ndx <- which.min(aeration)

		data_sub01 <- data_sub[m_indices[,aer_ndx],]

		outputs$DO_change[i] <- tail(data_sub01$DO,1) - head(data_sub01$DO,1)
		
		time_diff <- difftime(tail(data_sub01$date,1),head(data_sub01$date,1), units = 'mins')
		outputs$TimeInterval[i] <- time_diff
		outputs$SummedPAR[i] <- with(data_sub01, mean(PARraw, na.rm = TRUE)) * as.numeric(time_diff)


		#assign(paste('DO_change_', i, sep = ''), tail(data_sub01$DO,1) - head(data_sub01$DO,1))
		#assign(paste('time_diff_', i, sep = ''), difftime(tail(data_sub01$date,1),head(data_sub01$date,1), units = 'mins'))
		#assign(paste('PAR_sum_', i, sep = ''), with(data_sub01, mean(PARraw, na.rm = TRUE)) * as.numeric(time_diff01))
		}

	#attach(outputs)
	part1 <- with(outputs, TimeInterval[1] * DO_change[2] - TimeInterval[2] * DO_change[1])
	part2 <- with(outputs, TimeInterval[1] * SummedPAR[2] - TimeInterval[2] * SummedPAR[1])
	
	k <- part1 / part2
	ER1 <- with(outputs, (DO_change[1] - k*SummedPAR[1]) / TimeInterval[1])
	ER2 <- with(outputs, (DO_change[2] - k*SummedPAR[2]) / TimeInterval[2])
	
	ER <- round(mean(ER1,ER2),4)

	outputs	<- list(outputs)
	outputs$parameters = c('k' = k, 'ER' = ER)
	
	return(outputs)
	}

#library(rovelli)
#indata <- get(data(Ebble_CE1_2013_04_25)); d_date <- as.Date('2013-04-26')
#BalancedAeration (indata, analysisperiod = c(50,70), as.Date('2013-04-26'), Ks = 0.01)
#BalancedAeration (indata, analysisperiod = c(50,70), as.Date('2013-04-26'), Ks = 0.01,l_lateday = TRUE)




































































PlotBalancedAeration01 <- function(indata)
	{
	data_period.01 <- indata$data_period.01
	data_period.02 <- indata$data_period.02
	
	data.all <- rbind(data_period.01,data_period.02)
		
	with(data.all, plot(date, DO, type = 'n'))
	with(data.all, points(date, CSat, pch = 16, cex = 0.4, col = 'grey70', type = 'l', lwd = 2))
	
	with(data_period.01, points(date, DO, pch = 1))
	with(data_period.02, points(date, DO, pch = 4))
	}


#x_out <- BalancedAeration01 (indata, analysisperiod = c(50,70), as.Date('2013-04-26'), Ks = 0.01, l_dawnanalysis = FALSE, l_resultsonly = FALSE)

#PlotBalancedAeration01(x_out)


















































PlotBalAer.DO <- function(indata)
	{
	data_period.01 <- indata$data_period.01
	data_period.02 <- indata$data_period.02
	
	data.all <- rbind(data_period.01,data_period.02)
		
	with(data.all, plot(date, DO, type = 'n'))
	with(data.all, points(date, CSat, pch = 16, cex = 0.4, col = 'grey70', type = 'l', lwd = 2))
	
	with(data_period.01, points(date, DO, pch = 1))
	with(data_period.02, points(date, DO, pch = 4))
	}


#x_out <- BalancedAeration01 (indata, analysisperiod = c(50,70), as.Date('2013-04-26'), Ks = 0.01, l_dawnanalysis = FALSE, l_resultsonly = FALSE)

#PlotBalAer.DO(x_out)
























































PlotBalAer.Aer <- function(indata)
	{
	data_period.01 <- indata$data_period.01
	data_period.02 <- indata$data_period.02
	
	data.all <- rbind(data_period.01,data_period.02)
		
	with(data.all, plot(date, Aeration, type = 'n'))
	with(data.all, points(date, CSat, pch = 16, cex = 0.4, col = 'grey70', type = 'l', lwd = 2))
	abline(0,0)
	
	with(data_period.01, points(date, Aeration, pch = 1))
	with(data_period.02, points(date, Aeration, pch = 4))
	}


#x_out <- BalancedAeration01 (indata, analysisperiod = c(50,70), as.Date('2013-04-26'), Ks = 0.01, l_dawnanalysis = FALSE, l_resultsonly = FALSE)

#PlotBalAer.Aer(x_out)

