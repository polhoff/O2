
BuildO2 <- function(n_ver)
	{
	
	try (library(roxygen2))
	try (library(tools))
	try (library(inlinedocs))
	try (library(devtools))


	dirtop <- "/home/sjparker/sp/"

	dirpackagetop     <-  paste( dirtop, "Rpackage/", sep = "")
	packagename <- "O2"

	dirpackage <- paste(dirpackagetop, packagename, sep = "")


	setwd ( dirpackagetop)


	c_check <-  paste ( "R CMD check      ", dirpackage )
	system (c_check)
 
	c_build <-  paste ( "R CMD build --resave-data      ", dirpackage )
	system (c_build)

	install_string <- paste ( 'install.packages ( paste ( dirdmp, "/", packagename, "_", n_ver, ".tar.gz", sep = ""), type = "source" )')

	return (install_string)
	}


#x1 <- BuildO2 (1.1)
#install.packages ( 'O2_1.1.tar.gz', type = "source" )
