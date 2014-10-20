\docType{methods}
\name{powerFDR}
\alias{powerFDR}
\title{powerFDR constructor.}
\description{Calculate and plot power (true positive rate) versus achieved false discovery rate using adjusted p-values (padj) and known true positives (labels).}
\usage{
powerFDR(object, threshold=c(0.01, 0.05, 0.1), plot=TRUE, \ldots)
}
\arguments{
  \item{object}{an object of 'SimResults' or adjusted p-values(padj).}
  \item{threshold}{numeric value of the p-value threshold (cutoff), or vector of multiple p-value threshold, used to determine the significance of tests.}
  \item{plot}{logical, should power-fdr values be plotted.}
  \item{\ldots}{ graphical parameters such as 'col', 'cex', 'lty.threshold' and 'pointType' (see 'Details'). and logical parameter 'add', should power-fdr value(s) be added to current plot.}
}

\details{
Typical usages are

powerFDR(object="SimResults", \ldots)

powerFDR(object="vector", labels="numeric", add=FALSE, \ldots)

powerFDR(object="matrix", labels="numeric", add=FALSE, \ldots)

The first form secifies the object of 'SimResults' (see \code{\link{SimResults}}).

The second and last forms are directly working with a vector of (adjust) p-values or a matrix containing p-values from multiple methods. 'labels' is vector containing the true class labels. It must be numeric only containing '0' or '1'. '1' is positive and '0' is negative. In this case,'add' can be used to merge the power-fdr values to currently existing plot. If 'TRUE', the values are added to current plots.

The function 'powerFDR' calculates the true positive rate and false discovery rate given a particular threshold, control FDR. Several thresholds can be assessed simultaneously (e.g., threshold=c(0.01,0.02,0.03,0.04,0.05)).

Most of the graphical parameters such as 'col', 'cex', 'pch', \ldots from 'par', can be directly passed to 'plot' for power-fdr values. 'pointType', letter indicating how power-fdr values should be plotted: "b" for both points and lines; "p" for points only; and "l" for lines only. 'col.line' and 'lwd.line, col and lwd of line connecting power-fdr points, if 'pointType' is either "b" or "l". 'lwd.threshold', 'lty.threshold' and 'col.threshold' are lwd, lty and col referred to the lines drawn for 'threshold'. 
 
The FDR is considered to be controlled if the false discovery rate falls below the threshold it is assessed at, and power-fdr points are filled-in if plotted. power-fdr points are unfilled, or empty, if the FDR is not controlled and is equal to or greater than the cutoff it is assessed at. Note that the fill-unfill option is only compatible with 'pch' values of 21 to 25. Visually, this means that for a given threshold, a point (of the same color) will be filled if it falls left of the threshold line, and will be unfilled if it sits right of the threshold line.   
}



\value{
An S4 object of class "powerFDR" containing  the threshold value(s) assessed, and the associated false discovery rate(s), FDR, and true positive rates, TPR.}

\seealso{ \code{\link{SimResults}}, \code{\link{rocX}}
}

\author{Charity Law, Xiaobei Zhou and Mark D. Robinson}
\examples{
library(benchmarkR)
######example1, object="SimResults"###############################
data(rocX)
pval <- do.call("cbind", dat_rocX$pval)
padj <- do.call("cbind", dat_rocX$padj)
labels <- rep(0, nrow(pval))
labels[dat_rocX$indDE] <- 1
re <- SimResults(pval=pval, padj=padj, labels=labels)
p <- powerFDR(re, plot=TRUE)
plot(p, cex=2, pch=c(23,25), col=1:3, main="powerFDR plot", 
     lwd.line=2, cex.axis=1.5, col.threshold=4, lwd.threshold=2)
######example2, object="vector"###################################
col <- c("red", "blue", "green")
# P-values from Method 1
pvalues <- runif(100)
pvalues <- sort(pvalues)
labels <- numeric(100)
labels[1:10] <- 1
powerFDR(pvalues, labels=labels, threshold=c(0.05, 0.1, 0.5), col=col, plot=TRUE, xlim=c(0,1))
# P-values from Method 2 with larger p-values
pvalues <- runif(100, min=0.02)
pvalues <- sort(pvalues)
powerFDR(pvalues, labels=labels, threshold=c(0.05, 0.1, 0.5), col=col, add=TRUE, pch=22)
# Legend
legend("bottomright", pch=c(21, 22), legend=c("Method 1", "Method 2"), pt.bg="black")
}
\keyword{powerFDR, SimResults}
