\docType{methods}
\name{rocX}
\alias{rocX}
\alias{rocX,SimResults-method}
\title{rocX constructor.}

\description{Produce an object of 'rocX' and a simple ROC plot with threshold X (fdr) using the features of ROCR}

\usage{
\S4method{rocX}{SimResults}(object, thresholdX=0.05, plot=TRUE, \dots)
}

\arguments{

\item{object}{An object of 'SimResults'.}

\item{thresholdX}{The threshold of score (score.X). Note that if score is 1 - P_adjust_value, "threshold.X" can be set at 0.95 (FDR = 0.05).} 

\item{plot}{logical, if 'plot' is TRUE, an ROC plot would be produced when rocX object is created ('cexX', 'pchX', 'colX', 'pchX' are special parameters of threshold X for plot. 'fprCutoff' is the cutoff of fpr to domaine 'xlim' of the plot.).}

\item{\dots}{Optional arguments from 'par()'.}
}

\details{\code{rocX} will call 'prediction' and 'performance' from package 'ROCR' by Pvalue (pval) and labels from the object of 'SimResults'. Additionally, the value of tpr and fpr corresponding to the threshold X will be calculated by P adjusted value (padj) from the object of 'SimResults'.   
}

\value{An S4 object of class "rocX". 
}

\seealso{ \code{\link{SimResults}}
}

\author{Xiaobei Zhou}

\examples{
library(benchmarkR)
data(rocX)
pval <- do.call("cbind", dat_rocX$pval)
padj <- do.call("cbind", dat_rocX$padj)
labels <- rep(0, nrow(pval))
labels[dat_rocX$indDE] <- 1
re <- SimResults(pval=pval, padj=padj, labels=labels)
roc1 <- rocX(re, plot=TRUE)
###rocX can use all the args of par() and special args (i.e., pchX, colX, cexX, lwdX) ###
roc2 <- rocX(re, plot = TRUE, lty = 1:2,col = 1:2, lwd = 2, main = "roc", cex.lab = 2, cex.main = 3, colX = 2:3, pchX = 2:3, lwdX =5:6, fprCutoff = 0.6)
###you can also re-plot it###
plot(roc1, lwd = 3)
}  

\keyword{rocX, SimResults}