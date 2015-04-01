\docType{methods}
\name{rocX-plot}
\alias{plot,rocX,ANY-method}

\title{Plot method for rocX}

\description{Produce a ROC plot augmented with X point for \code{\link{rocX}} object.}

\usage{
\S4method{plot}{rocX,ANY}(x, y, add=FALSE, legend=list(), \dots)
}

\arguments{

\item{x}{An object of \code{\link{rocX}}.}

\item{y}{Not used.}

\item{add}{Logical. If 'TRUE', plots are added to current one.}

\item{legend}{A list or NULL. Add legends to plots.}

\item{\dots}{Optional.}
}

\details{See the details of \code{\link{rocX}}.   
}

\value{Invisibly, an S4 object of class \code{\link{rocX}}. 
}


\author{Xiaobei Zhou and Mark D. Robinson}


\keyword{rocX}
