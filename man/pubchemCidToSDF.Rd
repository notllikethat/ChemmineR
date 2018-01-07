\name{pubchemCidToSDF}
\alias{pubchemCidToSDF}
\title{
Import Compounds from PubChem
}
\description{
Accepts one or more PubChem compound ids
and downloads the corresponding compounds from PubChem Power User Gateway (PUG)
returning results in an \code{SDFset} container. 
}
\usage{
pubchemCidToSDF(cids)
}

\arguments{
  \item{cids}{
  	  A \code{numeric} object which contains one or more PubChem cids
}
}
\value{
\item{SDFset}{ for details see ?"SDFset-class"}
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
compounds <- pubchemCidToSDF(c(111,123))}
}

\keyword{ utilities }
