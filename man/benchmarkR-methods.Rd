\docType{methods}
\name{benchmarkR-methods}
\alias{benchmarkR}
\alias{benchmarkR-methods}
\alias{benchmarkR,SimResults,missing-method}
\alias{benchmarkR,missing,ANY-method}
\title{Wrapper function to create a multi-panel performance assessment plot}

\description{Produce a set of performance assessment plots for benchmarking.  A 3-panel 
plot is created with: an ROC curve (\code{\link{rocX}}), a false discovery (FD) plot (\code{\link{fdX}}) 
and a true-positive-rate versus achieved false-discovery plot (\code{\link{powerFDR}}).  
All plots show the augmented calibration information.}

\usage{
benchmarkR(object, pval, \ldots)	

\S4method{benchmarkR}{SimResults,missing}(object, name.panel=c("a", "b", "c"), \ldots, cex.panel=1.5)
\S4method{benchmarkR}{missing,ANY}(pval, padj=NULL, labels, name.panel=c("a", "b", "c"), \ldots, cex.panel=1.5)

}

\arguments{

\item{object}{An object of \code{\link{SimResults}}.}

\item{pval}{A vector, matrix containing p-values.}

\item{padj}{A vector, matrix containing adjusted p-values (optional). If 'padj' is missing, its p-adjusted-value would be defaultly calculated by  Benjamini & Hochberg (BH) method or a certain defined adjust method (e.g., 'padjMethod=BY'). For more details, see \code{\link{SimResults}}.}

\item{labels}{A numeric vector indicating positive with a 1, and negative with a 0.}

\item{name.panel}{A list containing the three sub-panel (label) names. Default is c("a", "b", "c").}

\item{\ldots}{Optional arguments for plotting (see below for more details). General graphical parameters from \code{\link{par}} such as 'col', 'cex' can be used for all the subfigures. 'cexX', 'pchX', 'colX', 'pchX' are special parameters for the X point (\code{rocX} and \code{fdX}). 'lty.threshold' and 'point.type' are for \code{powerFDR}.}

\item{cex.panel}{A numerical value giving the amount by plotting sub-panel text.}

}

\details{
\code{benchmarkR} produce a visualized benchmark result combining with three plots: \code{rocX} (topleft) \code{fdX} (bottomleft) and \code{powerFDR} (right).


Most of the graphical parameters such as 'col', 'cex', 'pch' and etc from \code{\link{par}}, can be directly passed into 'plot' for all the subfigures. They can be set up as a vector (e.g., 'col=1') sharing the same value for all the subfigures, or a list (e.g, 'col=list(1,2,3)' for different value of different subfigure. 'cexX', 'pchX', 'colX', 'pchX' are special parameters of X point (\code{rocX} and \code{fdX}). Their usage are similar with 'col', 'cex', 'pch'.

'name.panel' and 'cex.panel' are referring to the panel label but not title of each subfigure (e.g., name.panel=c("A", "B", "C"), cex.panel=2). The title of each subfigure can be done by 'main', such as 'main=list("roc curve", "false discovery plot", "power curve")'. The main title can be done by 'title' (e.g, 'title(main="benchmark result", outer=TRUE)'). For more details, see the examples below.  

}

\value{
A plot is created on the current graphics device.

}


\seealso{ \code{\link{SimResults}}, \code{\link{rocX}}, \code{\link{fdX}}, \code{\link{powerFDR}} 
}

\author{Xiaobei Zhou and Mark D. Robinson}

\examples{

data(Pickrell)
re <- SimResults(pval=Pickrell$pval, labels=Pickrell$labels)
benchmarkR(re)
benchmarkR(pval=Pickrell$pval, labels=Pickrell$labels)

# using different range of xlim
benchmarkR(re, xlim=list(c(0,0.6), c(0,500), c(0,0.2)))

# remove one of legends
benchmarkR(re, legend=list("bottomleft",NULL,"bottomright"))

# using different lwd and cex
benchmarkR(re, cex=list(1,2,3), lwd=list(3,3,3))

# using defined panel labels and panelcex
benchmarkR(re, name.panel=c("A", "B", "C"), cex.panel=2)

# using defined title of subfigures 
benchmarkR(re, main=list("roc curve", "false discovery plot", "power curve"))

# using defined main title 
benchmarkR(re)
title(main="benchmark result", outer=TRUE)
}  

\keyword{benchmarkR, SimResults, rocX, fdX, powerFDR}
