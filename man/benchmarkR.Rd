\docType{methods}
\name{benchmarkR}
\alias{benchmarkR}
\title{Visualized benchmark plot.}

\description{Produce a set of performance assessment plots for benchmarking}

\usage{
benchmarkR(object, stratify=NULL, name.panel=c("a", "b", "c"), \ldots, cex.panel=1.5)

#benchmarkR(pval=pval, padj=padj, labels=labels, stratify=NULL, name.panel=c("a", "b", "c"), ..., cex.panel=1.5)#

#''pval'', ''padj'' and ''labels'' can be directly as arguments without an object of ''SimResults''#
}

\arguments{

\item{object}{An object of 'SimResults'.}

\item{stratify}{A stratify factor (Optional arguments. Currently not available).}

\item{name.panel}{A list containing the sub-panel names. It follows the following order: c("rocX", "fdX", "powerFDR")}

\item{\ldots}{Optional arguments from plot. General graphical parameters from \code{par} such as 'col', 'cex' can be used for all the sub-figure. 'cexX', 'pchX', 'colX', 'pchX' are special parameters of X point for \code{rocX} and \code{fdX}. 'lty.threshold' and 'pointType' are for \code{powerFDR}.}

\item{cex.panel}{A numerical value giving the amount by plotting sub-panel text.}

}

\details{\code{benchmarkR} produce a visualized benchmark result combining with three plots: \code{rocX} (topleft) \code{fdX} (bottomleft) and \code{powerFDR} (right).
}


\seealso{ \code{\link{SimResults}}, \code{\link{rocX}}, \code{\link{fdX}}, \code{\link{powerFDR}} 
}

\author{Xiaobei Zhou and Mark D. Robinson}

\examples{
library(benchmarkR)
######Pickrell simulation#################
data(Pickrell)
pval <- do.call("cbind", Pickrell$pval)
padj <- do.call("cbind", Pickrell$padj)
labels <- rep(0, nrow(pval))
labels[Pickrell$indDE] <- 1
re <- SimResults(pval=pval, padj=padj, 
         labels=labels)
benchmarkR(re)
##with different range of xlim##
benchmarkR(re, 
     xlim=list(c(0,0.6), c(0,500), c(0,0.2)))
##remove one of legends##
benchmarkR(re, 
     legend=list("bottomleft", NULL, "bottomright"))
##with different lwd and cex##
benchmarkR(re, cex=list(1,2,3), 
     lwd=list(3,3,3))
dev.off()
######ABRF-AvsB#########################
data(ABRF)
reAB <- SimResults(pval=pvalAB, 
       padj=padjAB, labels=labelAB)
benchmarkR(reAB)
dev.off()
}  

\keyword{benchmarkR, SimResults, rocX, fdX, powerFDR}