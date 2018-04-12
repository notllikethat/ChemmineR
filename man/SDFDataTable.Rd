\name{SDFDataTable}
\alias{SDFDataTable}
\title{
	SDF Data Table
}
\description{
	Creates and HTML DataTable showing the compound image along with the fields 
	in the compound data block. Using a browser, this table can be filtered and paged,
	among other things.

	This uses the DT library to create the DataTable.
}
\usage{
	SDFDataTable(sdfset)
}
\arguments{
	\item{sdfset}{ An \code{SDFSet} object }
}
\value{
	Returns the result of the \code{datatable} function from the \code{DT} library.
	An HTML file can be created from this value by calling the \code{saveWidget} function
	on it.
}
\references{
	DT library: https://rstudio.github.io/DT/
	DataTables javascript library: https://datatables.net/
}
\author{
	Kevin Horan
}
\examples{
	library(ChemmineR)
	data(sdfsample)

	# this will open a browser to display the result
	x=SDFDataTable(sdfsample[1:3]) 

	# if no GUI is available or you want to save the HTML result:
	saveWidget(x,"output.html")

	
}
\keyword{datatable,sdfset,compounds,display}
