\docType{methods}
\name{fdX-methods}
\alias{fdX}
\alias{fdX-methods}
\alias{fdX,SimResults,missing-method}
\alias{fdX,missing,ANY-method}
\title{False discovery plot with threshold X}

\description{Plot false discoveries versus the number of top rank feature with a cutoff when false discovery rate (FDR) are under certain control. }

\usage{
fdX(object, pval, \ldots)

\S4method{fdX}{SimResults,missing}(object, thresholdX=0.05, transformation = "1-x", plot=TRUE, \ldots)
\S4method{fdX}{missing,ANY}(pval, padj=NULL, labels, thresholdX=0.05, transformation = "1-x", plot=TRUE, \ldots)

}

\arguments{

\item{object}{An object of \code{\link{SimResults}}.}

\item{pval}{A vector or matrix containing p-values.}

\item{padj}{A vector or matrix containing adjusted p-values (optional). If 'padj' is missing, its adjusted p-value would be defaultly calculated by  Benjamini & Hochberg (BH) method or a certain defined adjust method (e.g., 'padjMethod=BY'). For more details, see \code{\link{SimResults}}.}


\item{labels}{A numeric vector indicating positive with a 1, and negative with a 0.}

\item{thresholdX}{Numeric value of the p-value threshold (cutoff). (e.g., FDR = 0.05).}

\item{transformation}{The transformation of 'pval' and 'padj'. The default setting "1-x" means "1-pval(padj, thresholdX)". Note that another useful transformation is "-log10(x)".} 
 

\item{plot}{Logical, whether plot or not.}

\item{\ldots}{Optional arguments from 'plot'. General graphical parameters from \code{\link{par}} such as 'col', 'cex' can be directly used into the figure. Logical parameter 'add', whether the new plot added to current one or not. 'cexX', 'pchX', 'colX', 'pchX' are special parameters of X point on the figure. A list or NULL. 'legend' is used to add legends to plots (see details below). 'addFun' and 'addFunLocation' are special arguments for multiple-panel figure (see details below).}

}

\details{False discoveries are calculated according to the p-value rank (pval) and labels (from the object of \code{SimResults}). The X point displays the actual false discoveries number versus number of top ranked features given a threshold (e.g., FDR=0.05).

Most of the graphical parameters such as 'col', 'cex', 'pch' and etc from \code{\link{par}}, can be directly passed into 'plot' for all the subfigures. They can be set up as a vector (e.g., 'col=1') sharing the same value for all the subfigures, or a list (e.g, 'col=list(1,2,3)' for different value of different subfigure. 'cexX', 'pchX', 'colX', 'pchX' are special parameters of X point. Their usage is similar to 'col', 'cex', 'pch'. 

To increase the flexibility of plots, you can use argument 'add'. If 'add=TRUE', plots are added to current one combining different \code{rocX} curves together into one figure. 

'legend' is a list including all the argments from function \code{legend} (see help(legend)). If it is NULL, do not add legends to plots.}

\value{Invisibly, an S4 object of class \code{\link{fdX-class}}.
} 

\seealso{ \code{\link{SimResults}}
}

\author{Xiaobei Zhou and Mark D. Robinson}

\examples{
data(Pickrell)
re <- SimResults(pval=Pickrell$pval, labels=Pickrell$labels)
fdX(re)
fdX(pval=Pickrell$pval, labels=Pickrell$labels)

# removing legends
fdX(re, legend=NULL)

# controlling legend location
fdX(re, legend=list("topleft",cex=2))

# combining multiple plots 
fdX(re[,1], col=1, legend=NULL)
fdX(re[,2], col=2, add=TRUE, legend=NULL)
fdX(re[,3], col=3, add=TRUE, legend=NULL)

# setting X point 
fdX(re, pchX = 1:3, lwdX =4:6)
}  

\keyword{fdX, SimResults}
