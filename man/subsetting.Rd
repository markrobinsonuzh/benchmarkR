\docType{methods}
\name{subsetting}
\alias{[,SimResults-method}

\title{Subset SimResults Object}
\description{
Extract a subset of a \code{\link{SimResults}}.}
\usage{
\S4method{[}{SimResults}(x, i, j)
}
\arguments{
\item{x}{object of class \code{\link{SimResults}}.}
\item{i,j}{elements to extract. \code{i} subsets the tags or genes while \code{j} subsets the libraries.}
}
\details{
'i,j' may take any values acceptable for the matrix components of an object of class \code{\link{SimResults}}.

}
\value{
An object of class \code{\link{SimResults}}, holding data from the specified subset of tags/genes and libraries.
}
\author{Xiaobei Zhou and Mark D. Robinson}
\seealso{
  \code{\link{SimResults}} in benchmarkR package.
}
\examples{
re <- SimResults(pval=matrix(rnorm(1000), 250, 4), 
                 padj=matrix(rnorm(1000), 250, 4),  
                 labels=sample(0:1, 250, replace=TRUE))
res <- re[1:100, 2]
}
\keyword{\code{SimResults}}
