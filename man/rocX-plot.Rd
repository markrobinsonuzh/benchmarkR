\docType{methods}
\name{rocX-plot}
\alias{plot,rocX,ANY-method}

\title{Plot method for rocX.}

\description{Produce the ROC plot with threshold X (fdr) using the features of ROCR for \code{rocX} object.}

\usage{
\S4method{plot}{rocX,ANY}(x, y, add=FALSE, legend=list(), \dots)
}

\arguments{

\item{x}{An object of \code{rocX}.}

\item{y}{Not used.}

\item{add}{Logical. If 'TRUE', plots are added to current one.}

\item{legend}{iist or NULL. Add legends to plots. It is a 'list' including all the argments from function 'legend' (see help(legend)). If it is NULL, do not add legends to plots.}

\item{\dots}{Optional arguments from 'par()' 'cexX', 'pchX', 'colX', 'pchX' are special parameters of threshold X for plot. 'fprCutoff' is the cutoff of fpr to domaine 'xlim' of the plot.).}
}

\details{See the detials of \code{\link{rocX}}.   
}

\value{An S4 object of class \code{rocX}. 
}


\author{Xiaobei Zhou and Mark D. Robinson}


\keyword{rocX}