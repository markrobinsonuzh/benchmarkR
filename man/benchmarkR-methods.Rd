\docType{methods}
\name{benchmarkR-methods}
\alias{benchmarkR}
\alias{benchmarkR-methods}
\alias{benchmarkR,SimResults,missing-method}
\alias{benchmarkR,missing,ANY-method}
\title{Wrapper function to create a multi-panel performance assessment plot}

\description{Produces a set of performance assessment plots.  A 3-panel 
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

\item{pval}{A vector or matrix containing p-values.}

\item{padj}{A vector or matrix containing adjusted p-values (optional). If \code{padj} is missing, adjusted p-values are calculated using \code{p.adjust(..,method=padjMethod)}.  By default, \code{padjMethod="BH"}). For more details, see \code{\link{SimResults}} or \code{help("p.adjust")}.}

\item{labels}{A numeric vector indicating class labels (positives with 1, negatives with 0).  For more details, see \code{?ROCR::prediction}.}

\item{name.panel}{A list containing the three sub-panel (label) names. Default is c("a", "b", "c").}

\item{\ldots}{Optional arguments for plotting (see below for more details).
	\itemize{
	 \item \code{col}, \code{cex} and other general graphical parameters from \code{\link{par}} can be directly used into the figure.
	 \item\code{cexX}, \code{pchX}, \code{colX}, \code{pchX} are special parameters to control the plotting of the X point (\code{rocX} and \code{fdX}). 
	 \item \code{lty.threshold}, \code{lwd.threshold}, \code{col.threshold} and \code{point.type} are for \code{powerFDR}.}
}	 
	 
	 
\item{cex.panel}{A numerical value giving the amount by plotting sub-panel text.}

}

\details{
\code{benchmarkR} produces a visualized benchmark result combining with three plots: \code{rocX} (topleft) \code{fdX} (bottomleft) and \code{powerFDR} (right).


Most of the graphical parameters such as \code{col}, \code{cex}, \code{pch} and etc from \code{\link{par}}, can be directly passed into \code{plot} for all the subfigures. They can be set up as a vector (e.g., \code{col=1}) sharing the same value for all the subfigures, or a list (e.g, \code{col=list(1,2,3)}) for different value of different subfigure. \code{cexX}, \code{pchX}, \code{colX}, \code{pchX} are special parameters to control the plotting of the X point (\code{rocX} and \code{fdX}). Their usage is similar to \code{col}, \code{cex}, \code{pch}.

\code{name.panel} and \code{cex.panel} are referring to the panel label but not title of each subfigure (e.g., \code{name.panel=c("A", "B", "C")}, \code{cex.panel=2}). The title of each subfigure can be done by \code{main}, such as \code{main=list("roc curve", "false discovery plot", "power curve")}. The main title can be done by \code{title} (e.g, \code{title(main="benchmark result", outer=TRUE)}). For more details, see the examples below.  

}

\value{
A plot is created on the current graphics device.

}


\seealso{ \code{\link{SimResults}}, \code{\link{rocX}}, \code{\link{fdX}}, \code{\link{powerFDR}, \code{\link{p.adjust}}} 
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
