\docType{methods}
\name{show}
\alias{show}
\alias{show,SimResults-method}
\alias{show,rocX-method}
\title{Show method of objects}
\description{Show method for SimResults and rocX objects in benchmarkR}
\usage{
\S4method{show}{SimResults}(object)
\S4method{show}{rocX}(object)

}
\arguments{
  \item{object}{a SimResults or rocX object}
}

\details{

The \code{show} method of SimResults is directly used fucntion \code{printHead} from limma. It print only the leading elements or rows of components or slots which contain large quantities of data.

The \code{show} method of rocX is defined to use function \code{str} to 'summary' the features of data. 

}




\author{
Xiaobei Zhou
}
