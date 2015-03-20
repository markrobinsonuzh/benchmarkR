\docType{methods}
\name{powerFDR-methods}
\alias{powerFDR}
\alias{powerFDR-methods}
\alias{powerFDR,SimResults,missing-method}
\alias{powerFDR,missing,ANY-method}
\title{ower (TPR) versus achieved false discovery rate (FDR) plot}
\description{Calculate and plot power (TPR) versus achieved false discovery rate (FDR) using adjusted p-values (padj) and known true positives (labels).}
\usage{
powerFDR(object, padj, \ldots)

\S4method{powerFDR}{SimResults,missing}(object, threshold=c(0.01, 0.05, 0.1), plot=TRUE, \ldots)
\S4method{powerFDR}{missing,ANY}(padj, labels, threshold=c(0.01, 0.05, 0.1), plot=TRUE, \ldots)

}
\arguments{
  \item{object}{An object of \code{\link{SimResults}}.}
  \item{padj}{A vector, matrix containing the p-adjusted-value.}
  \item{labels}{A numeric vector indicating positive with a 1, and negative with a 0.}
  \item{threshold}{Numeric value of the p-value threshold (cutoff), or vector of multiple p-value threshold, used to determine the significance of tests.}
  \item{plot}{Logical, should power-fdr values be plotted.}
  \item{\ldots}{graphical parameters such as 'col', 'cex' and etc from \code{\link{par}}. 'lty.threshold' and 'point.type' (see details below). and logical parameter 'add', should power-fdr value(s) be added to current plot. 'legend' is used to add legends to plots (see details below).}
}

\details{
The function \code{powerFDR} calculates the true positive rate and false discovery rate given a particular threshold, control FDR. Several thresholds can be assessed simultaneously (e.g., threshold=c(0.01,0.02,0.03,0.04,0.05)).

Most of the graphical parameters such as 'col', 'cex', 'pch' and etc from \code{\link{par}}, can be directly passed to \code{plot} for power-fdr values. 'point.type', letter indicating how power-fdr values should be plotted: "b" for both points and lines; "p" for points only; and "l" for lines only. 'col.line' and 'lwd.line, col and lwd of line connecting power-fdr points, if 'point.type' is either "b" or "l". 'lwd.threshold', 'lty.threshold' and 'col.threshold' are lwd, lty and col referred to the lines drawn for 'threshold'. 
 
The FDR is considered to be controlled if the false discovery rate falls below the threshold it is assessed at, and power-fdr points are filled-in if plotted. power-fdr points are unfilled, or empty, if the FDR is not controlled and is equal to or greater than the cutoff it is assessed at. Note that the fill-unfill option is only compatible with 'pch' values of 21 to 25. Visually, this means that for a given threshold, a point (of the same color) will be filled if it falls left of the threshold line, and will be unfilled if it sits right of the threshold line.

To increase the flexibility of plots, you can use argument 'add'. If 'add=TRUE', plots are added to current one combining different \code{rocX} curves together into one figure. 

'legend' is a list including all the argments from function \code{legend} (see help(legend)). If it is NULL, do not add legends to plots.
   
}



\value{
An S4 object of class \code{\link{powerFDR-class}} containing  the threshold value(s) assessed, and the associated false discovery rate(s), FDR, and true positive rates, TPR.}

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
col <- c("red", "blue", "green")
# P-values from Method 1
padj <- runif(100)
padj <- sort(padj)
labels <- numeric(100)
labels[1:10] <- 1
powerFDR(padj=padj, labels=labels, threshold=c(0.05, 0.1, 0.5), col=col, plot=TRUE, xlim=c(0,1))
# P-values from Method 2 with larger p-values
padj <- runif(100, min=0.02)
padj <- sort(padj)
powerFDR(padj=padj, labels=labels, threshold=c(0.05, 0.1, 0.5), col=col, add=TRUE, pch=22)
legend("bottomright", pch=c(21, 22), legend=c("Method 1", "Method 2"), pt.bg="black")
}
\keyword{powerFDR, SimResults}
