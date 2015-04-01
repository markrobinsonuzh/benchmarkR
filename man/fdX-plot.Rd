\docType{methods}
\name{fdX-plot}
\alias{plot,fdX,ANY-method}

\title{Plot method for fdX}

\description{Plot false discoveries versus the number of selected positive value of object \code{\link{SimResults}} with a threshold.}

\usage{
\S4method{plot}{fdX,ANY}(x, y, add=FALSE, legend=list(), \dots)
}

\arguments{

\item{x}{An object of \code{fdX}.}

\item{y}{Not used.}

\item{add}{Logical. If 'TRUE', plots are added to current one.}

\item{legend}{A list or NULL. Add legends to plots. }

\item{\ldots}{Optional(see details on \code{\link{powerFDR}}).}
}

\details{See the detials of \code{\link{fdX}}.   
}

\value{Invisibly, an S4 object of class \code{\link{fdX}}. 
}


\author{Xiaobei Zhou and Mark D. Robinson}


\keyword{dfX}