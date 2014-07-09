\docType{methods}
\name{rocXList-plot}
\alias{plot,rocXList,ANY-method}

\title{Plot method for rocXList.}

\description{Produce a series of ROC plots with threshold X (fdr) using the features of ROCR for 'rocXList' object.}

\usage{
\S4method{plot}{rocXList,ANY}(x, y, add=FALSE, addFun=NULL, addFunLocation=NULL, \dots)
}

\arguments{

\item{x}{An object of 'rocXList'.}

\item{y}{not used.}

\item{add}{logical. If 'TRUE', plots are added to current one.}

\item{addFun}{additional function for each iteration plot.}

\item{addFunLocation}{Location of 'addFun'. '1' means add and '0' means skip 'addFun' on each iteration.}

\item{\dots}{Optional arguments from 'par()' 'cexX', 'pchX', 'colX', 'pchX' are special parameters of threshold X for plot. 'fprCutoff' is the cutoff of fpr to domaine 'xlim' of the plot.).}
}

\details{See the detials of \code{\link{rocX}}.   
}

\value{An S4 object of class "rocX". 
}


\author{Xiaobei Zhou and Mark D. Robinson}

 

\keyword{rocX}