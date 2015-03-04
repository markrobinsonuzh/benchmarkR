\docType{methods}
\name{fdX}
\alias{fdX}
\title{False discovery plot.}

\description{Plot false discoveries versus the number of top rank feature with a cutoff when false discovery rate (FDR) are under certain control. }

\usage{
fdX(object=NULL, stratify=NULL, thresholdX=0.05, transformation = "1-x", plot=TRUE, \ldots)
}

\arguments{

\item{object}{An object of 'SimResults'.}

\item{stratify}{A stratify factor (Optional arguments. Currently not available).}

\item{thresholdX}{Numeric value of the p-value threshold (cutoff). \code{threshold.X=0.05}(FDR = 0.05).}

\item{transformation}{The transformation of score (score.X). The default setting "1-x" means "1-pval(padj, thresholdX)". Note that another useful transformation is "-log10(x)".} 
 

\item{plot}{Logical, whether plot or not.}

\item{\ldots}{Optional arguments from 'plot'. Note that logical parameter 'add', should power-fdr value(s) be added to current plot. Graphical parameters from \code{par} such as 'col', 'cex' can be used for the figure. 'cexX', 'pchX', 'colX', 'pchX' are special parameters of X point on the figure(See details below.).}
}

\details{false discoveries is calculated by \code{padj} and \code{labels} of object of \code{SimResults}. X point in \code{fdX} displays the actual false discoveries number versus number of top rank feature given a threshold (e.g., FDR=0.05).  
}



\seealso{ \code{\link{SimResults}}
}

\author{Xiaobei Zhou and Mark D. Robinson}

\examples{
library(benchmarkR)
data(Pickrell)
pval <- do.call("cbind", Pickrell$pval)
padj <- do.call("cbind", Pickrell$padj)
labels <- rep(0, nrow(pval))
labels[Pickrell$indDE] <- 1
re <- SimResults(pval=pval, padj=padj, 
         labels=labels)
fdX(re)
}  

\keyword{fdX, SimResults}