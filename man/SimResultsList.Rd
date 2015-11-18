
\name{SimResultsList}
\alias{SimResultsList}


\title{Object SimResultsList constructor}

\description{The \code{SimResultsList} is a list to store multiple (\code{SimResults}) results. Each element should be one \code{\link{SimResults-class}} object.
}

\usage{
SimResultsList(...)
}

\arguments{

\item{...}{...}

}



\value{An S4 \code{\link{SimResultsList-class}} object.
}


\author{Xiaobei Zhou and Mark D. Robinson.}

\examples{
library(benchmarkR)
data(PickrellList)
rel <- SimResultsList()
for(i in 1:5)
    rel[[i]] <- SimResults(pval=PickrellList[[i]]$pval, labels=PickrellList[[i]]$labels)

rel
}

\keyword{SimResults}
