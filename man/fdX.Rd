\docType{methods}
\name{fdX}
\alias{fdX}
\title{false discovery plot.}

\description{Plot false discoveries versus the number of selected positive value of object 'SimResults' with threshold X (fdr). }

\usage{
fdX(object, thresholdX=0.05, transformation = "1-x", plot=TRUE, \ldots)
}

\arguments{

\item{object}{An object of 'SimResults'.}

\item{thresholdX}{The threshold of score (score.X). Note that if score is P_adjust_value, "threshold.X" can be set at 0.05 (FDR = 0.05).}

\item{transformation}{The transformation of score (score.X). The default setting "1-x" means "1-pval(padj, thresholdX)". Note that another useful transformation is "-log10(x)".} 
 

\item{plot}{logical, if 'plot' is TRUE, an ROC plot would be produced when 'rocX' or 'rocXList' object is created ('cexX', 'pchX', 'colX', 'pchX' are special parameters of threshold X for plot.). 'addFun' and 'addFunLocation' See details below.}

\item{\ldots}{Optional arguments from 'plot'.}
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
fdX(re)
legend("topleft", col=c(1,4), c("edgeR", "limma"), lty=1, pch=1)
}  

\keyword{fdX, SimResults}