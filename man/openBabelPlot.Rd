\name{openBabelPlot}
\alias{openBabelPlot}
\alias{openBabelPlot}
\title{
	Plot compound structures
}
\description{
	Plots compound structure(s) for molecules stored in SDF and SDFset containers.
}
\usage{
	openBabelPlot(sdfset, height=600, noHbonds = TRUE, regenCoords=FALSE)
}

\arguments{
	\item{sdfset}{
		Object of class \code{SDFset} 
	}
	\item{height}{
		The height of the image in pixels. The generated image is always square, so this will also
		be the width. 
	}
	\item{noHbonds}{
		If \code{TRUE}, then the C-hydrogens and their bonds - explicitly defined in an SDF - are excluded from the plot.
	}
	\item{regenCoords}{
		If ChemmineOB is installed and this option is TRUE, then Open
		Babel will be used to re-generate the 2D coords for each
		compound before plotting it. This often results in a nicer
		layout. If you want to save the results of the coord
		re-generation, call the \code{\link{regenerateCoords}} function
		first yourself and save the result.
	}
}
\details{
	The function \code{openBablePlot} depicts a 2D compound structure based
	on the XY-coordinates specified in the atom block of an SDF.
	If more than one compound is given in the SDFset, they will be arranged in a grid layout.
} 
\author{
	Kevin Horan
}

\seealso{
	\code{sdf.visualize}
}
\examples{
	\dontrun{
	## Import SDFset sample set
	data(sdfsample)
	(sdfset <- sdfsample)

	## Plot single compound structure
	openBabelPlot(sdfset[1])

	## Plot several compounds structures
	openBabelPlot(sdfset[1:4])
	}

}

\keyword{ aplot  }
\keyword{ utilities }
