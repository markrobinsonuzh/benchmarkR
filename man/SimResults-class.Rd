
\docType{class}

\name{SimResults-class}
\alias{SimResults-class}


\title{Class 'SimResults'.}

\description{An S4 Object class 'SimResults' to store the Pvalue, true labels and other necessary values for further analysis and evaluation.
}


\section{Objects from the Class}{Objects can be created by using the \code{SimResults} function.
}



\section{Slots}{

\describe{
\item{pval}{A matrix containing the Pvalue.}

\item{padj}{A matrix containing the P adjusted value.}

\item{labels}{A vector containing the true class labels. It must be numeric only containing '0' or '1'. '1' is positive and '0' is negative.}

\item{stratifyBy}{Optional arguments.}

}

}





\seealso{ \code{\link{SimResults}} }


\author{Robinson Group. First created by Xiaobei Zhou and Mark D. Robinson.}


\keyword{SimResults}