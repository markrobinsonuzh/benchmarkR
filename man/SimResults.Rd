
\name{SimResults}
\alias{SimResults}


\title{Object SimResults constructor}

\description{The \code{SimResults} is a S4 class object to store the p-value, labels and other necessary values for further analysis and evaluation.
}

\usage{
SimResults(pval=NULL, padj=NULL, labels=new("numeric"), stratify=NULL, padjMethod="BH", \ldots)
}

\arguments{

\item{pval}{A vector, matrix containing p-values from a classifier or a test.}

\item{padj}{A vector, matrix containing the adjust p-values (optional). If 'padj' is missing, its p-adjusted-value would be defaultly calculated by  Benjamini & Hochberg (BH) method or a certain defined adjust method (e.g., 'padjMethod=BY').}

\item{labels}{A numeric vector indicating positive with a 1, and negative with a 0.}

\item{stratify}{A stratify factor driving data (optional).}

\item{padjMethod}{The method to use for adjusting p-value, if 'padj' is NULL. \code{padjMethod} are available from c("holm", "hochberg", "hommel", "bonferroni", "BH", "BY", "fdr", "none")}

\item{\ldots}{\ldots}

}



\value{An S4 \code{\link{SimResults-class}} object.
}


\note{Selection observations can be done by:
	
	x <- SimResults()
	
	x[i,]
}


\author{Xiaobei Zhou and Mark D. Robinson.}

\examples{
library(benchmarkR)
set.seed(100)
# 'pval' can be a vector
re <- SimResults(pval=rnorm(2000), labels=sample(0:1, 2000, replace=TRUE))
# 'pval' can also be a matirx
pval <- matrix(runif(4000, 0, 0.8), 1000, 4)
padj <- matrix(runif(4000, 0, 0.8), 1000, 4)
re <- SimResults(pval=pval, padj=padj, labels=sample(0:1, 1000, replace=TRUE))
# matrix operators can be used for 'SimResults'
res <- re[1:100,] 
}

\keyword{SimResults}
