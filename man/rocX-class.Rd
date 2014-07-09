\name{rocX-class}
\alias{rocX-class}
\docType{class}
\title{rocX - class}

\description{
A simple S4 class for storing results of \code{rocX} for reproducible plots and re-analysis.
}



\section{Slots}{
  \describe{
    \item{\code{rocXelement}:}{A list contains: 'performance' from package 'ROCR' and 'threshold', which consits of the x-axis (fprX) and y-axis(tprX) corresponding to X.}
  }
}

\seealso{ \code{\link{rocX}} }


\author{Xiaobei Zhou and Mark D. Robinson}

\keyword{classes}

