
\docType{class}

\name{SimResults-class}
\alias{SimResults-class}


\title{Class \code{\link{SimResults}} }

\description{An S4 Object class \code{\link{SimResults}}  to store the p-values, labels and other necessary values for further analysis and evaluation.
}


\section{Objects from the class}{Objects can be created by using the \code{\link{SimResults}} function.
}



\section{Slots}{

\describe{
\item{pval}{A matrix containing p-values.}

\item{padj}{A matrix containing the adjusted p-values.}

\item{labels}{A numeric vector indicating positive with a 1, and negative with a 0.}

\item{stratify}{A stratify factor driving data (optional).}

}

}





\seealso{ \code{\link{SimResults}} }


\author{Xiaobei Zhou and Mark D. Robinson.}


\keyword{SimResults}
