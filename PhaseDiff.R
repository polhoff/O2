
PhaseDiffCcf <- function(x, Var1, Var2, bin = 15, n_bins = 48, l_daytime = FALSE, l_detrend = TRUE)
	{

	#48 bins at 15 minutes per bin is 12 hours
	
	#x = phase_chalk; Var1 = 'DOEbble'; Var2 = 'DOWylye'; bin = 15; n_bins = 48; l_daytime = TRUE; l_detrend = TRUE
	#x = phase_chalk; Var1 = 'DOEbble'; Var2 = 'DOWylye'; bin = 15; n_bins = 48; l_daytime = FALSE; l_detrend = TRUE
	stopifnot(class(x) == 'data.frame')

	
	if(l_daytime)
		{
		#x[x$daynight == 'day', c(Var1,Var2)] <- NA
		x[x$daynight == 'day', Var1] <- NA
		x[x$daynight == 'day', Var2] <- NA
		}

	
	x1 <- with(x, get(Var1))
	x2 <- with(x, get(Var2))

	output <- ccf(x1,x2, na.action = na.pass)

	
	}


















































PhaseDiff <- function(x, Var1, Var2, bin = 15, n_bins = 48, l_daytime = FALSE, l_detrend = TRUE, l_plot = FALSE)
	{

	#48 bins at 15 minutes per bin is 12 hours
	
	#x = phase_chalk; Var1 = 'DOEbble'; Var2 = 'DOWylye'; bin = 15; n_bins = 48; l_daytime = TRUE; l_detrend = TRUE
	#x = phase_chalk; Var1 = 'DOEbble'; Var2 = 'DOWylye'; bin = 15; n_bins = 48; l_daytime = FALSE; l_detrend = TRUE
	stopifnot(class(x) == 'data.frame')

	
	if(l_daytime)
		{
		#x[x$daynight == 'day', c(Var1,Var2)] <- NA
		x[x$daynight == 'day', Var1] <- NA
		x[x$daynight == 'day', Var2] <- NA
		}

	
	x1 <- with(x, get(Var1))
	x2 <- with(x, get(Var2))

	if(l_detrend)
		{
		x1.mean <- mean(x1, na.rm = TRUE)
		x2.mean <- mean(x2, na.rm = TRUE)
		
		x1 <- x1 - x1.mean
		x2 <- x2 - x2.mean
		}
		
		
	lag_indices <- seq(1, length = n_bins, by = bin)
	#lag_indices <- seq(1, length = 3, by = bin)
	
	
	data_diff_list1 <- list()
	
	for(i in lag_indices)
	
		{
		data_sub1 <- x1[i:length(x1)]
		data_sub2 <- x2[1:length(data_sub1)]
		
			
		data_diff <- data_sub1 - data_sub2	

		data_diff_list1 <- append(data_diff_list1, list(data_diff))
		}
	
	names(data_diff_list1) <- lag_indices-1
	data_diff_list1 <- data_diff_list1[-1]
	
	
	
	
	
	
	
	#lag in other direction
	#lag in other direction
	#lag in other direction
	#lag in other direction
	#lag in other direction
	#lag in other direction
	#lag in other direction
	#lag in other direction
	#lag in other direction
	#lag in other direction
	#lag in other direction
	#lag in other direction
	#lag in other direction
	#lag in other direction
	#lag in other direction
	#lag in other direction
	
	
	
	data_diff_list2 <- list()
	
	for(i in lag_indices)
	
		{
		data_sub1 <- x2[i:length(x2)]
		data_sub2 <- x1[1:length(data_sub1)]
		
			
		data_diff <- data_sub2 - data_sub1
		data_diff_list2 <- append(data_diff_list2, list(data_diff))
		}


	names(data_diff_list2) <- paste('-',lag_indices-1, sep = '')
	data_diff_list2 <- data_diff_list2[-1]

	data_diff_list2 <- rev(data_diff_list2)
	
	
	data_no_lag <- list(x1 - x2)
	names(data_no_lag) <- '0'
	#output <- list(data_diff_list2, data_no_lag, data_diff_list1)
	
	output <- append(data_diff_list2, data_no_lag)
	output <- append(output, data_diff_list1)
	
	if(l_plot) boxplot(output)
	
	return(output)
	
	}


