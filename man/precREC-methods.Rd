\docType{methods}
\name{precREC-methods}
\alias{precREC}
\alias{precREC-methods}
\alias{precREC,SimResults,missing-method}
\alias{precREC,missing,ANY-method}
\alias{precREC,SimResultsList,missing-method}
\title{Achieved precision versus recall plot}
\description{Calculate and plot achieved precision versus recall using adjusted p-values (padj) and known true positives (labels).}
\usage{
precREC(object, padj, \ldots)

\S4method{precREC}{SimResults,missing}(object, threshold=c(0.99, 0.95, 0.9),  transformation="1-x", plot=TRUE, \ldots)
\S4method{precREC}{missing,ANY}(padj, labels, threshold=c(0.99, 0.95, 0.9),  transformation="1-x", plot=TRUE, \ldots)
\S4method{precREC}{SimResultsList,missing}(object, threshold=c(0.99, 0.95, 0.9),  transformation="1-x", plot=TRUE, \ldots)

}
\arguments{
  \item{object}{An object of \code{\link{SimResults}}.}
  \item{padj}{A vector or matrix containing the adjusted p-value.}
  \item{labels}{A numeric vector indicating class labels (positives with 1, negatives with 0).  For more details, see \code{?ROCR::prediction}.}
  \item{threshold}{Numeric value of the adjusted p-value threshold (cutoff), or vector of multiple thresholds, used to determine the significance of tests.}
  \item{transformation}{The transformation of \code{pval} and \code{padj}. The default setting "1-x" means \code{1-pval(padj)}. Note that another useful transformation is "-log10(x)".} 
  \item{plot}{Logical, should precision-recall values be plotted.}
  \item{\ldots}{Optional arguments for plotting (see below for more details). 
	\itemize{
	    \item \code{col}, \code{cex} and other general graphical parameters from \code{\link{par}} can be directly used into the figure.	
  	    \item \code{point.type} are special parameters of precision-recall values (see details below).
            \item \code{lwd.threshold}, \code{lty.threshold} and \code{col.threshold} are special parameters referred to the lines drawn for \code{threshold} (see details below). 
  	    \item \code{add}, logical, whether the new plot added to current one or not.
            \item \code{legend}, a list or NULL, is used to add legends to plots (see details below).}
}  

}

\details{
The function \code{precREC} calculates the achieved precision (positive predictive value (PPV) or 1-FDR) and recall (true positive rate (TPR)) given a particular threshold, control precision. Several thresholds can be assessed simultaneously (e.g., \code{threshold=c(0.99,0.98,0.97,0.96,0.95)}).


Most of the graphical parameters such as \code{col}, \code{cex}, \code{pch} and etc from \code{\link{par}}, can be directly passed to \code{plot} for precision-recall values. \code{point.type}, letter indicating how precision-recall values should be plotted: "b" for both points and lines; "p" for points only; and "l" for lines only. \code{col.line} and \code{lwd.line}, \code{col} and \code{lwd} of line connecting precision-recall points, if \code{point.type} is either "b" or "l". \code{lwd.threshold}, \code{lty.threshold} and \code{col.threshold} are \code{lwd}, \code{lty} and \code{col} referred to the lines drawn for 'threshold'. 
 
The precision is considered to be controlled if the precision (1-FDR) falls above the threshold it is assessed at, and precision-recall points are filled-in. precision-recall points are unfilled, or empty, if the precision is not controlled and is equal to or less than the cutoff it is assessed at. Note that the fill-unfill option is only compatible with \code{pch} values of 21 to 25. Visually, this means that for a given threshold, a point (of the same color) will be filled if it falls left of the threshold line, and will be unfilled if it sits to the right of the threshold line.

To increase the flexibility of plots, you can use argument \code{add}. If \code{add=TRUE}, plots are added to current one combining different \code{rocX} curves together into one figure. 

\code{legend} is a list including all the arguments from function \code{legend} (see help(legend)). If it is NULL, do not add legends to plots.
   
If input is an \code{\link{SimResultsList}} object, the average performance would be plotted (see example 3). 
}



\value{
Invisibly, an S4 object of class \code{\link{precREC-class}} containing the threshold value(s) assessed, and the associated precision, achieved precision, PREC, and true positive rates, TPR.}

\seealso{ \code{\link{SimResults}}, \code{\link{rocX}}
}

\author{Xiaobei Zhou and Mark D. Robinson}
\examples{
# example1
data(Pickrell)
re <- SimResults(pval=Pickrell$pval, labels=Pickrell$labels)
p <- precREC(re, plot=TRUE)
p <- precREC(re, plot=TRUE, legend=NULL)
plot(p, cex=2, pch=c(23,25), col=1:3, main="precREC plot", 
     lwd.line=2, cex.axis=1.5, col.threshold=4, lwd.threshold=2)

# example2
data(calibration)
padj <- calibration$padj
labels <- calibration$labels
precREC(padj=padj, labels=labels, xlim=c(0,.3))


# example3
data(PickrellList)
rel <- SimResultsList()
for(i in 1:5)
    rel[[i]] <- SimResults(pval=PickrellList[[i]]$pval, labels=PickrellList[[i]]$labels)
powerFDR(rel)

}
\keyword{precREC, SimResults}
