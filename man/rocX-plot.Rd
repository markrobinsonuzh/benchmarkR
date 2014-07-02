\docType{methods}
\name{rocX-plot}
\alias{plot}
\alias{plot,rocX,missing-method}

\title{Plot method for rocX.}

\description{Produce the ROC plot with threshold X (fdr) using the features of ROCR for 'rocX' object.}

\usage{
\S4method{plot}{rocX,missing}(x, y, \dots)
}

\arguments{

\item{x}{An object of 'rocX'.}

\item{y}{not used.}

\item{\dots}{Optional arguments from 'par()' 'cexX', 'pchX', 'colX', 'pchX' are special parameters of threshold X for plot. 'fprCutoff' is the cutoff of fpr to domaine 'xlim' of the plot.).}
}

\details{See the detials of \code{\link{rocX}}.   
}

\value{An S4 object of class "rocX". 
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

\keyword{rocX}