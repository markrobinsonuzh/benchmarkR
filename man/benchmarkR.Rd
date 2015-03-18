\docType{methods}
\name{benchmarkR}
\alias{benchmarkR}
\title{Wrapper function to create a multi-panel performance assessment plot}

\description{Produce a set of performance assessment plots for benchmarking.  A 3-panel 
plot is created with: an ROC curve (\code{\link{rocX}}), a false discovery (FD) plot (\code{\link{fdX}}) 
and a true-positive-rate versus achieved false-discovery plot (\code{\link{powerFDR}}).  
All plots show the augmented calibration information.}

\usage{
benchmarkR(object, stratify=NULL, name.panel=c("a", "b", "c"), \ldots, cex.panel=1.5)

#benchmarkR(pval=pval, padj=padj, labels=labels, stratify=NULL, name.panel=c("a", "b", "c"), ..., cex.panel=1.5)#
#''pval'', ''padj'' and ''labels'' can be directly as arguments without an object of ''SimResults''#
}

\arguments{

\item{object}{An object of 'SimResults'.}

\item{stratify}{A stratify factor (Optional arguments. Currently not available).}

\item{name.panel}{A list containing the three sub-panel names. Default is c("rocX", "fdX", "powerFDR").}

\item{\ldots}{Optional arguments for plotting (see below for more details). General graphical parameters from \code{\link{par}} such as 'col', 'cex' can be used for all the subfigures. 'cexX', 'pchX', 'colX', 'pchX' are special parameters for the X point (\code{rocX} and \code{fdX}). 'lty.threshold' and 'pointType' are for \code{powerFDR}.}

\item{cex.panel}{A numerical value giving the amount by plotting sub-panel text.}

}

\details{
\code{benchmarkR} produce a visualized benchmark result combining with three plots: \code{rocX} (topleft) \code{fdX} (bottomleft) and \code{powerFDR} (right).

Add a paragraph describing how 'col', 'main', etc. are interpreted, in terms of the 3 plots.

Add a paragraph to describe exactly what name.panel really means ... it's not the title, it's the panel label (e.g., a, b, c that would be used in a paper to label the subpanels of a figure).

}


\seealso{ \code{\link{SimResults}}, \code{\link{rocX}}, \code{\link{fdX}}, \code{\link{powerFDR}} 
}

\author{Xiaobei Zhou and Mark D. Robinson}

\examples{

data(Pickrell)
re <- SimResults(pval=Pickrell$pval, labels=Pickrell$labels)
benchmarkR(re)

# using different range of xlim
benchmarkR(re, xlim=list(c(0,0.6), c(0,500), c(0,0.2)))

# remove one of legends
benchmarkR(re, legend=list("bottomleft",NULL,"bottomright"))

# using different lwd and cex
benchmarkR(re, cex=list(1,2,3), lwd=list(3,3,3))

}  

\keyword{benchmarkR, SimResults, rocX, fdX, powerFDR}
