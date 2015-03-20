\name{rocX-class}
\alias{rocX-class}
\docType{class}
\title{rocX - class}

\description{
A simple S4 class for storing results of \code{\link{rocX}} for reproducible plots and re-analysis.
}



\section{Slots}{
  \describe{
    \item{\code{element}:}{A list contains: \code{performance} from package \code{ROCR} and \code{threshold}, which consists of the x-axis (fprX) and y-axis(tprX) corresponding to X.}
  }
}

\seealso{ \code{\link{rocX}} }


\author{Xiaobei Zhou and Mark D. Robinson}

\keyword{classes}

