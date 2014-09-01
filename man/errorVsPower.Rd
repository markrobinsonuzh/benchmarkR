\docType{methods}
\name{errorVsPower}
\alias{errorVsPower}
\title{Error versus power}
\description{Calculate and plot achieved false discovery rate versus true positive rate using adjusted (or raw) p-values and known true positives.}
\usage{
errorVsPower(pvalue, posctrl, id=NULL, cutoff=0.05, plot=FALSE, add.points=FALSE, col="black", pch=21, cex=1.3, lwd=2, xlim=c(0,1), ylim=c(0,1), points.type="b", line.col="black", cutoff.lty=3, print=TRUE, \dots)
}
\arguments{
  \item{pvalue}{\code{numeric} vector containing adjusted or raw p-values}
  \item{posctrl}{\code{character} vector containing identifiers for positive controls. Alternatively, a \code{logical} vector of length \code{length(pvalue)} can be supplied.}
  \item{id}{\code{character} vector of length \code{lengt(pvalue)} containing identifiers associated with p-values. \code{id} is not required if \code{posctrl} is a \code{logical} vector of length \code{length(pvalue)}}
  \item{cutoff}{\code{numeric} value of the p-value cutoff, or vector of multiple p-value cutoffs, used to determine the significance of tests.}
  \item{plot}{\code{logical}, should error-power value(s) be plotted?}
  \item{add.points}{\code{logical}, should error-power value(s) be added to current plot?}
  \item{col}{color of error-power point(s). \code{col} can be a vector, if multiple \code{cutoff} values are assessed. \code{col} is also applied to lines drawn for the \code{cutoff} when \code{plot=TRUE}.}
  \item{pch}{type of error-power points(s). Values of 21 to 25 are recommended as this allows fill-unfill options for point(s) to indicate successful or unsuccessful error control (see details below).}
  \item{cex}{\code{numeric} value for magnification of point(s). Default set to 1.3.}
  \item{lwd}{positive \code{numeric} value indicating the width of lines to be drawn. The assigned width applies to lines drawn for the\code{cutoff}, as well as for error-power lines if there is more than one cutoff value.}
  \item{xlim}{range of values displayed along the x-axis when \code{plot=TRUE}. The default is set to \code{c(0,1)}, giving the full-range of values.}
  \item{ylim}{range of values displayed along the y-axis when \code{plot=TRUE}. The default is set to \code{c(0,1)}, giving the full-range of values.}
  \item{points.type}{letter indicating how error-power values should be plotted -- \code{"b"} for both points and lines; \code{"p"} for points only; and \code{"l"} for lines only.}
  \item{line.col}{color of line connecting error-power points, if \code{points.type} is either \code{"b"} or \code{"l"}.} 
  \item{cutoff.lty}{line type drawn for \code{cutoff}.}
  \item{\print}{\code{logical}, should false discovery and true postive rates be printed?}
  \item{\dots}{other optional arguments to be passed to \code{plot}.}
}

\value{
A \code{data.frame} object containing the \code{cutoff} value(s) assessed, and the associated false discovery rate(s), \code{FDR}, and true positive rates, \code{TPR}.
}

\details{
This function calculates the false discovery rate (number of significant features that are not in\code{posctrl} out of all significant features) for a particular cutoff. The true postive rate (number of \code{posctrl} features that are significant out of all \code{posctrl} features) is also calculated. Several cutoffs can be assessed simultaneously. 

The error rate is considered to be controlled if the false discovery rate falls below the cutoff it is assessed at, and error-power points are filled-in if plotted. Error-power points are unfilled, or empty, if the false discovery rate is not controlled and is equal to or greater than the cutoff it is assessed at. Note that the fill-unfill option is only compatible with \code{pch} values of 21 to 25. Visually, this means that for a given cutoff, a point (of the same color) will be filled if it falls left of the cutoff line, and will be unfilled if it sits right of the cutoff line.   
}

\seealso{
}

\author{Charity Law}
\examples{
col <- c("red", "blue", "green")
# P-values from Method 1
	pvalues <- runif(100)
	pvalues <- sort(pvalues)
	posctrl <- as.character(1:10)
	id <- as.character(1:length(pvalues))
	errorVsPower(pvalues, posctrl, id, cutoff=c(0.05, 0.1, 0.5), col=col, plot=TRUE)
# P-values from Method 2 with larger p-values
	pvalues <- runif(100, min=0.02)
	pvalues <- sort(pvalues)
	errorVsPower(pvalues, posctrl, id, cutoff=c(0.05, 0.1, 0.5), col=col, add.points=TRUE, pch=22)
# Legend
legend("bottomright", pch=c(21, 22), legend=c("Method 1", "Method 2"), pt.bg="black")
}
\keyword{error, power, fd, falsediscovery, truepositive}
