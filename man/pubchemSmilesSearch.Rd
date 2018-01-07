\name{pubchemSmilesSearch}
\alias{pubchemSmilesSearch}
\title{
PubChem Similarity (Fingerprint) SMILES Search 
}
\description{
Accepts one SMILE string or \code{SMIset} container
and performs a  PubChem fingerprint search, returning 
hits in an \code{SDFset} container.  If the input object
contains multiple items, only the first is used as a query.
}
\usage{
pubchemSmilesSearch(smiles)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{smiles}{
  	  A \code{SMIset} object which contains one compound, or a SMILES string
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
compounds <- pubchemSmilesSearch(sdfset)}
}

\keyword{ utilities }
