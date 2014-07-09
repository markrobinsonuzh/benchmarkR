


\name{SimResults}
\alias{SimResults}


\title{object SimResults constructor.}

\description{The 'SimResults' is a S4 class to store the Pvalue, true labels and other necessary values for further analysis and evaluation.
}

\usage{
SimResults(pval=NULL, padj=NULL, labels=new("numeric"), stratify=NULL, \ldots)
}

\arguments{

\item{pval}{A vector, matrix or NULL containing the Pvalue. if 'pval' is a vector, it would be transferred to a matrix.}

\item{padj}{A vector, matrix or NULL containing the P adjusted value (optional). If 'padj' is missing, its P adjusted value would be calculated by 'BH' method. if 'padj' is a vector, it would be transferred to a matrix.}

\item{labels}{A vector containing the true class labels. It must be numeric only containing '0' or '1'. '1' is positive and '0' is negative.}

\item{stratify}{A data.frame (optional arguments).}

\item{\ldots}{not used}

}



\value{An S4 "SimResults" object.
}


\author{Xiaobei Zhou and Mark D. Robinson.}

\examples{
library(benchmarkR)
##'pval' can be a vector
re <- SimResults(pval=rnorm(1000), labels=sample(0:1, 1000, replace=TRUE))
##'pval' can also be a matirx
re <- SimResults(pval=matrix(rnorm(1000), 250, 4), padj=matrix(rnorm(1000), 250, 4),  labels=sample(0:1, 250, replace=TRUE))
## matrix operators can be used for 'SimResults'
res <- re[1:100,] 
}

\keyword{SimResults}