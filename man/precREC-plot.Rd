\docType{methods}
\name{precREC-plot}
\alias{plot,precREC,ANY-method}

\title{Plot method for precREC.}

\description{plot achieved precision versus recall using adjusted p-values (padj) and known true positives (labels).}

\usage{
\S4method{plot}{precREC,ANY}(x, y, add=FALSE, legend=list(), \ldots)
}

\arguments{

\item{x}{An object of \code{precREC}.}

\item{y}{not used.}

\item{add}{logical. If 'TRUE', plots are added to current one.}

\item{legend}{list or NULL. Add legends to plots. It is a 'list' including all the argments from function \code{legend} (see help(legend)). If it is NULL, do not add legends to plots.}

\item{\ldots}{Optional(see details on \code{\link{precREC}}).}
}

\details{See the detials of \code{\link{precREC}}.   
}

\value{Invisibly, an S4 object of class \code{precREC-class}. 
}


\author{Xiaobei Zhou and Mark D. Robinson}


\keyword{precREC}