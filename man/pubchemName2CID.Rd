\name{pubchemName2CID}
\alias{pubchemName2CID}
\title{
	Translate compound name to pubchem compound id
}
\description{
	Takes any compound name and queries pubchem to find its pubchem id (CID).
}
\usage{
pubchemName2CID(name)
}

\arguments{
  \item{name}{
	  Any compound name, used to query pubchem to find the compound. 
}
}
\value{
	The result is the pubchem compound id. If the name is not found, NA will be returned.
}
\references{
PubChem PUG REST: https://pubchem.ncbi.nlm.nih.gov/pug_rest/PUG_REST_Tutorial.html

}
\author{
	Kevin Horan
}

\examples{
\dontrun{
## fetch 2 compounds from PubChem
cid <- pubchemName2CID("CHEMBL460363")
}
}

\keyword{ utilities }
