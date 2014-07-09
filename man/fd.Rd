\docType{methods}
\name{fd}
\alias{fd}
\title{false discovery plot.}

\description{Plot false discoveries versus the number of selected positive value of object 'SimResults'. }

\usage{
fd(object, \ldots)
}

\arguments{

\item{object}{An object of 'SimResults'.}

\item{\ldots}{Optional arguments from 'par()'.}
}

\details{false discoveries is calculated by 'padj' and 'labels' of object of 'SimResults'.   
}



\seealso{ \code{\link{SimResults}}
}

\author{Xiaobei Zhou and Mark D. Robinson}

\examples{
library(benchmarkR)
data(rocX)
pval <- do.call("cbind", dat_rocX$pval)
padj <- do.call("cbind", dat_rocX$padj)
labels <- rep(0, nrow(pval))
labels[dat_rocX$indDE] <- 1
re <- SimResults(pval=pval, padj=padj, labels=labels)
fd(re)
legend("topleft", col=c(1,4), c("edgeR", "limma"), lty=1, pch=1)
}  

\keyword{fd, SimResults}