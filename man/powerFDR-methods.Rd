\docType{methods}
\name{powerFDR-methods}
\alias{powerFDR}
\alias{powerFDR-methods}
\alias{powerFDR,SimResults,missing-method}
\alias{powerFDR,missing,ANY-method}
\title{Power (TPR) versus achieved false discovery rate (FDR) plot}
\description{Calculate and plot power (TPR) versus achieved false discovery rate (FDR) using adjusted p-values (padj) and known true positives (labels).}
\usage{
powerFDR(object, padj, \ldots)

\S4method{powerFDR}{SimResults,missing}(object, threshold=c(0.01, 0.05, 0.1), plot=TRUE, \ldots)
\S4method{powerFDR}{missing,ANY}(padj, labels, threshold=c(0.01, 0.05, 0.1), plot=TRUE, \ldots)

}
\arguments{
  \item{object}{An object of \code{\link{SimResults}}.}
  \item{padj}{A vector or matrix containing the adjusted p-value.}
  \item{labels}{A numeric vector indicating positive with a 1, and negative with a 0.}
  \item{threshold}{Numeric value of the p-value threshold (cutoff), or vector of multiple p-value threshold, used to determine the significance of tests.}
  \item{plot}{Logical, should power-fdr values be plotted.}
  \item{\ldots}{Optional arguments for plotting (see below for more details). 
	\itemize{
	    \item \code{col}, \code{cex} and other general graphical parameters from \code{\link{par}} can be directly used into the figure.	
  	    \item \code{point.type} are special parameters of power-fdr values (see details below).
            \item \code{lwd.threshold}, \code{lty.threshold} and \code{col.threshold} are special parameters referred to the lines drawn for \code{threshold} (see details below). 
  	    \item \code{add}, logical, whether the new plot added to current one or not.
            \item \code{legend}, a list or NULL, is used to add legends to plots (see details below).}
}  

}

\details{
The function \code{powerFDR} calculates the true positive rate and false discovery rate given a particular threshold, control FDR. Several thresholds can be assessed simultaneously (e.g., \code{threshold=c(0.01,0.02,0.03,0.04,0.05)}).

Most of the graphical parameters such as \code{col}, \code{cex}, \code{pch} and etc from \code{\link{par}}, can be directly passed to \code{plot} for power-fdr values. \code{point.type}, letter indicating how power-fdr values should be plotted: "b" for both points and lines; "p" for points only; and "l" for lines only. \code{col.line} and \code{lwd.line}, \code{col} and \code{lwd} of line connecting power-fdr points, if \code{point.type} is either "b" or "l". \code{lwd.threshold}, \code{lty.threshold} and \code{col.threshold} are \code{lwd}, \code{lty} and \code{col} referred to the lines drawn for 'threshold'. 
 
The FDR is considered to be controlled if the false discovery rate falls below the threshold it is assessed at, and power-fdr points are filled-in if plotted. power-fdr points are unfilled, or empty, if the FDR is not controlled and is equal to or greater than the cutoff it is assessed at. Note that the fill-unfill option is only compatible with \code{pch} values of 21 to 25. Visually, this means that for a given threshold, a point (of the same color) will be filled if it falls left of the threshold line, and will be unfilled if it sits right of the threshold line.

To increase the flexibility of plots, you can use argument \code{add}. If \code{add=TRUE}, plots are added to current one combining different \code{rocX} curves together into one figure. 

\code{legend} is a list including all the arguments from function \code{legend} (see help(legend)). If it is NULL, do not add legends to plots.
   
}



\value{
Invisibly, an S4 object of class \code{\link{powerFDR-class}} containing  the threshold value(s) assessed, and the associated false discovery rate(s), FDR, and true positive rates, TPR.}

\seealso{ \code{\link{SimResults}}, \code{\link{rocX}}
}

\author{Charity Law, Xiaobei Zhou and Mark D. Robinson}
\examples{
# example1
data(Pickrell)
re <- SimResults(pval=Pickrell$pval, labels=Pickrell$labels)
p <- powerFDR(re, plot=TRUE)
p <- powerFDR(re, plot=TRUE, legend=NULL)
plot(p, cex=2, pch=c(23,25), col=1:3, main="powerFDR plot", 
     lwd.line=2, cex.axis=1.5, col.threshold=4, lwd.threshold=2)

# example2
data(calibration)
padj <- calibration$padj
labels <- calibration$labels
powerFDR(padj=padj, labels=labels, xlim=c(0,.3))
}
\keyword{powerFDR, SimResults}
