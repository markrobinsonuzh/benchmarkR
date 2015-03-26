\docType{methods}
\name{powerFDR-plot}
\alias{plot,powerFDR,ANY-method}

\title{Plot method for powerFDR.}

\description{plot achieved false discovery rate versus true positive rate using adjusted p-values (padj) and known true positives (labels).}

\usage{
\S4method{plot}{powerFDR,ANY}(x, y, add=FALSE, legend=list(), \ldots)
}

\arguments{

\item{x}{An object of \code{powerFDR}.}

\item{y}{not used.}

\item{add}{logical. If 'TRUE', plots are added to current one.}

\item{legend}{list or NULL. Add legends to plots. It is a 'list' including all the argments from function 'legend' (see help(legend)). If it is NULL, do not add legends to plots.}

\item{\ldots}{Optional arguments from 'par()' 'cexX', 'pchX', 'colX', 'pchX', 'ltyX' and ''pointType'' are special parameters of threshold X for plot.}
}

\details{See the detials of \code{\link{powerFDR}}.   
}

\value{Invisibly, an S4 object of class \code{powerFDR-class}. 
}


\author{Charity Law, Xiaobei Zhou and Mark D. Robinson}


\keyword{powerFDR}