\docType{methods}
\name{rocXList-plot}
\alias{plot,rocXList,ANY-method}

\title{Plot method for rocXList}

\description{Produce a series of ROC plots with threshold X (fdr) using the features of ROCR for 'rocXList' object.}

\usage{
\S4method{plot}{rocXList,ANY}(x, y, add=FALSE, addFun=NULL, addFunLocation=NULL, legend=list(), \dots)
}

\arguments{

\item{x}{An object of \code{\link{rocXList}}.}

\item{y}{Not used.}

\item{add}{Logical. If 'TRUE', plots are added to current one.}

\item{addFun}{Additional function for each iteration plot.}

\item{addFunLocation}{Location of 'addFun'. '1' means add and '0' means skip 'addFun' on each iteration.}

\item{legend}{A list or NULL. Add legends to plots.}

\item{\dots}{Optional.}
}

\details{See the detials of \code{\link{rocX}}.   
}

\value{An S4 object of class\code{\link{rocX}}. 
}


\author{Xiaobei Zhou and Mark D. Robinson}

 

\keyword{rocX}
