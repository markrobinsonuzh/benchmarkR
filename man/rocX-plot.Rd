\docType{methods}
\name{rocX-plot}
\alias{plot,rocX,ANY-method}

\title{Plot method for rocX.}

\description{Produce the ROC plot with threshold X (fdr) using the features of ROCR for 'rocX' object.}

\usage{
\S4method{plot}{rocX,ANY}(x, y, add=FALSE, \dots)
}

\arguments{

\item{x}{An object of 'rocX'.}

\item{y}{not used.}

\item{add}{logical. If 'TRUE', plots are added to current one.}

\item{\dots}{Optional arguments from 'par()' 'cexX', 'pchX', 'colX', 'pchX' are special parameters of threshold X for plot. 'fprCutoff' is the cutoff of fpr to domaine 'xlim' of the plot.).}
}

\details{See the detials of \code{\link{rocX}}.   
}

\value{An S4 object of class "rocX". 
}


\author{Xiaobei Zhou and Mark D. Robinson}


\keyword{rocX}