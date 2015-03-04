
\docType{class}

\name{SimResults-class}
\alias{SimResults-class}


\title{Class \code{SimResults}.}

\description{An S4 Object class \code{SimResults} to store the p-value, labels and other necessary values for further analysis and evaluation.
}


\section{Objects from the class}{Objects can be created by using the \code{SimResults} function.
}



\section{Slots}{

\describe{
\item{pval}{A matrix containing the p-value.}

\item{padj}{A matrix containing the p-adjusted-value.}

\item{labels}{A numeric vector indicating positive with a 1, and negative with a 0.}

\item{stratify}{A stratify factor driving data (Optional arguments.).}

}

}





\seealso{ \code{\link{SimResults}} }


\author{Xiaobei Zhou and Mark D. Robinson.}


\keyword{SimResults}