\docType{methods}
\name{benchmarkR}
\alias{benchmarkR}
\title{visualized benchmark plot.}

\description{Produce a visualized benchmark plot combining with 'rocX' fdX and powerFDR.}

\usage{
benchmarkR(object, \ldots, cex.panel=1.5)
}

\arguments{

\item{object}{An object of 'SimResults'.}

\item{\ldots}{Optional arguments from 'plot'.}

\item{cex.panel}{A numerical value giving the amount by plotting panel text.}

}

\details{\code{benchmarkR} will produce a visualized benchmark plot combining with 'rocX' fdX and powerFDR: topleft(a) rocX, bottomleft(b) fdX and right(c) powerFDR   
}


\seealso{ \code{\link{SimResults}}, \code{\link{rocX}}, \code{\link{fdX}}, \code{\link{powerFDR}} 
}

\author{Xiaobei Zhou and Mark D. Robinson}

\examples{
library(benchmarkR)
######Pickrell simulation###############################################################################
data(Pickrell)
pval <- do.call("cbind", Pickrell$pval)
padj <- do.call("cbind", Pickrell$padj)
labels <- rep(0, nrow(pval))
labels[Pickrell$indDE] <- 1
re <- SimResults(pval=pval, padj=padj, labels=labels)
benchmarkR(re)
##with different range of xlim##
benchmarkR(re, xlim=list(c(0,0.6), c(0,500), c(0,0.2)))
##remove one of legends##
benchmarkR(re, legend=list("bottomleft", NULL, "bottomright"))
##with different lwd and cex##
benchmarkR(re, cex=list(1,2,3), lwd=list(3,3,3))
dev.off()
######ABRF-AvsB###############################################################################
data(ABRF)
reAB <- SimResults(pval=pvalAB, padj=padjAB, labels=labelAB)
benchmarkR(reAB)
dev.off()
}  

\keyword{benchmarkR, SimResults, rocX, fdX, powerFDR}