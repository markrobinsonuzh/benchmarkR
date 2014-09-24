\docType{methods}
\name{powerFDR}
\alias{powerFDR}
\title{powerFDR constructor.}
\description{Calculate and plot achieved false discovery rate versus true positive rate using adjusted p-values (padj) and known true positives (labels).}
\usage{
powerFDR(object, thresholdX=c(0.01, 0.05, 0.1), plot=FALSE, \ldots)
}
\arguments{
  \item{object}{An object of 'SimResults'.}
  \item{thresholdX}{The threshold of score (score.X). Here thresholdX is (control) FDR.}
  \item{plot}{logical, should powerFDR be plotted.}
  \item{\ldots}{Optional arguments from 'plot'. There are several special arguments different from "par()". 'add', logical, should powerFDR be added to current plot. 'colX', color of powerFDR point(s). 'colX' can be a vector, if multiple 'thresholdX' values are assessed. 'pchX', type of powerFDR points(s) (21 to 25 are recommended.). 'cexX', numeric value for magnification of point(s).'lwdX', positive numeric value indicating the width of lines to be drawn. The assigned width applies to lines drawn for the thresholdX, as well as for powerFDR lines if there is more than one thresholdX value. 'ltyX', line type drawn for thresholdX. 'pointType', letter indicating how powerFDR values should be plotted: "b" for both points and lines; "p" for points only; and "l" for lines only.}
}

\details{
This function calculates the false discovery rate and true positive rate given a particular cutoff, control FDR. Several cutoffs can be assessed simultaneously.
 
The error rate is considered to be controlled if the false discovery rate falls below the cutoff (thresholdX) it is assessed at, and powerFDR points are filled-in if plotted. powerFDR points are unfilled, or empty, if the false discovery rate is not controlled and is equal to or greater than the cutoff it is assessed at. Note that the fill-unfill option is only compatible with 'pchX' values of 21 to 25. Visually, this means that for a given cutoff, a point (of the same color) will be filled if it falls left of the cutoff line, and will be unfilled if it sits right of the cutoff line.   
}



\value{
An S4 object of class "powerFDR" containing false discovery rate (fdr), true positive rate (tpr) and the thresholdX value (control FDR) assessed.}


\seealso{ \code{\link{SimResults}}, \code{\link{rocX}}
}

\author{Charity Law, Xiaobei Zhou and Mark D. Robinson}
\examples{
library(benchmarkR)
data(rocX)
pval <- do.call("cbind", dat_rocX$pval)
padj <- do.call("cbind", dat_rocX$padj)
labels <- rep(0, nrow(pval))
labels[dat_rocX$indDE] <- 1
re <- SimResults(pval=pval, padj=padj, labels=labels)
######example1###############################################################################
p <- powerFDR(re, plot=TRUE)
plot(p, lwd=3, cexX=2, pchX=23:25, colX=1:3, main="powerFDR plot", cex.axis=1.5)
}
\keyword{powerFDR, SimResults}
