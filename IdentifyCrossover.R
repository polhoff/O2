
IdentifyCrossover <- function ( indata, c_var )
	{
	
	c_labels <- c('negative','minuschange','spurious','minuschange','pluschange','spurious','pluschange','positive')
	
	test_data <- c(NA, indata[,c_var], NA)
	xx <- try(embed(test_data,3))
	
	xx1 <- xx
	
	xx1[,1] <- xx[,1] > 0
	xx1[,2] <- xx[,2] > 0
	xx1[,3] <- xx[,3] > 0
	
	x_test <- xx1[,3] + xx1[,2]*2 + xx1[,1]*4
	#shift from 0,7 to 1,8
	x_test <- x_test + 1
	
	x_label <- c_labels[x_test]
	
	x_test <- data.frame(x_test)
	x_test$label <- as.factor(x_label)
	
	
	x_test <- data.frame(x_test)
	}


#indata <- Ebble_CE1_2014_08_21_md
#data_moved <- DOMove(indata,20)
#c_var = 'DO_diff'
#m_cross <- IdentifyCrossover (data_moved, c_var)
#table(m_cross)
#dim(m_cross)
#dim(Ebble_CE1_2014_08_21_md)
#tail(m_cross,20)
#tail(data_moved$DO_diff,20)
