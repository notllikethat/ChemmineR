\name{pubchemSDFSearch}
\alias{pubchemSDFSearch}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
PubChem Similarity (Fingerprint) Search 
}
\description{
Accepts one \code{SDFset} container
and performs a  similarity PubChem fingerprint search, returning 
hits in an \code{SDFset} container.  If the input object
contains multiple items, only the first is used as a query.
}
\usage{
pubchemSDFSearch(sdf)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{sdf}{
  	  A \code{SDFset} object which contains one compound
}
}
\value{
\item{SDFset}{ for details see ?"SDFset-class"}
}
\references{
PubChem PUG REST: https://pubchem.ncbi.nlm.nih.gov/pug_rest/PUG_REST_Tutorial.html 

SMILES Format: http://en.wikipedia.org/wiki/Chemical_file_format#SMILES
}
\author{
	Kevin Horan
}

\examples{
\dontrun{
## get a sample compound
data(sdfsample); sdfset <- sdfsample[2]
## search a compound on PubChem
compounds <- pubchemSDFSearch(sdfset)}
}

\keyword{ utilities }
