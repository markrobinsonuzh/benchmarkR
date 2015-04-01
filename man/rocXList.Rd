


\name{rocXList}
\alias{rocXList}


\title{Object \code{rocXList} constructor}

\description{The object of \code{\link{rocXList-class}} class is a container for storing a collection of \code{\link{rocX-class}} objects.
}

\usage{
rocXList(\ldots)
}

\arguments{

\item{\ldots}{each element of \code{link{rocX}} object}

}



\value{An S4 \code{rocXList-class} object.
}


\author{Xiaobei Zhou and Mark D. Robinson.}

\examples{
library(benchmarkR)

re <- SimResults(pval=matrix(rnorm(1000), 250, 4), 
                 padj=matrix(rnorm(1000), 250, 4),  
                 labels=sample(0:1, 250, replace=TRUE))

# matrix operators can be used for 'SimResults'
res1 <- re[1:100,] 
res2 <- re[101:200,] 
r1 <- rocX(res1, plot=FALSE)
r2 <- rocX(res2, plot=FALSE)
roc <- rocXList(r1=r1, r2=r2)
}

\keyword{rocXList}