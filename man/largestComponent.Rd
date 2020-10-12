\name{largestComponent}
\alias{largestComponent}
\title{
	Largest Component	
}
\description{
	If a single compound in an SDF file contains more than one disconnected
	component, this function will return an SDF with only the largest component,
	removing all other components. This will be applied to each SDF in the given
	SDFset.
}
\usage{
	largestComponent(sdfSet)
}
\arguments{
  \item{sdfSet}{
	  any SDFset object.
	}
}
\value{
	a new SDFset containing only single component compounds.
}
\author{
	Kevin Horan
}
\examples{
	\dontrun{
		sdf = smiles2sdf(c("Cl.CCC1C2CC3C4C5(CC(C2C5O)N3C1O)C6=CC=CC=C6N4C	TEST"))
		lg = largestComponent(sdf)
	}

}
