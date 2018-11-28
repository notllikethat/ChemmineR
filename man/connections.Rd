\name{connections}
\alias{DUD}
\alias{DrugBank}
\title{
	Database Connections
}
\description{
	Get a connection to one of the pre-build compound databases.
	The DrugBank database is distributed in the ChemmineDrugs package.

	The DUD database will be downloaded the first time it is called.
	It will download a 1.8GB zipped file which will expand to abut 9GB.
	A directory to store the database in can be passed to the \code{DUD()} function.

}
\usage{
DUD(destinationDir=".")

DrugBank()
}
\arguments{
  \item{destinationDir}{
	  The directory to store the downloaded DUD database in.
	}
}

\value{
	A connection object to the ether the DUD or DrugBank database.
	This object must be passed to other functions which make use of the connection.
}
\author{
	Kevin Horan
}

\examples{
	dbConn = DrugBank()
}
