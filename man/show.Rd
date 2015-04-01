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
  \item{object}{a \code{\link{SimResults-class}}, \code{\link{rocX-class}} \code{\link{fdX-class}} and \code{\link{powerFDR-class}} object}
}

\details{

The \code{show} method of SimResults is directly used function \code{printHead} from "limma". It print only the leading elements or rows of components or slots which contain large quantities of data.

The \code{show} method of rocX is defined to use function \code{\link{str}} to \code{summary} the features of data. 

}




\author{
Xiaobei Zhou and Mark D. Robinson
}
