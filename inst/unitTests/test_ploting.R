
test.openBabelPlot <- function() {
	#if(! ChemmineR:::.haveOB()) 
		DEACTIVATED("this test requires ChemmineOB, but not installed")
	data(sdfsample)
	openBabelPlot(sdfsample[1])
}
