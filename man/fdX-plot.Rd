\docType{methods}
\name{fdX-plot}
\alias{plot,fdX,ANY-method}

\title{Plot method for fdX.}

\description{Plot false discoveries versus the number of selected positive value of object \code{SimResults} with a threshold.}

\usage{
\S4method{plot}{fdX,ANY}(x, y, add=FALSE, legend=list(), \dots)
}

\arguments{

\item{x}{An object of \code{fdX}.}

\item{y}{not used.}

\item{add}{logical. If 'TRUE', plots are added to current one.}

\item{legend}{list or NULL. Add legends to plots. It is a 'list' including all the argments from function 'legend' (see help(legend)). If it is NULL, do not add legends to plots.}

\item{\dots}{Optional arguments from 'par()' 'cexX', 'pchX', 'colX', 'pchX' are special parameters of threshold X for plot. 'fprCutoff' is the cutoff of fpr to domaine 'xlim' of the plot.).}
}

\details{See the detials of \code{\link{rocX}}.   
}

\value{An S4 object of class \code{fdX-class}. 
}


\author{Xiaobei Zhou and Mark D. Robinson}


\keyword{dfX}