\docType{methods}
\name{fdX-methods}
\alias{fdX}
\alias{fdX-methods}
\alias{fdX,SimResults,missing-method}
\alias{fdX,missing,ANY-method}
\title{False discovery plot augmented with X point}

\description{Plot cumulative number of false discoveries versus top ranked features with an X point marking the location of the method's FDR (at a chosen level). }

\usage{
fdX(object, pval, \ldots)

\S4method{fdX}{SimResults,missing}(object, thresholdX=0.05, transformation = "1-x", plot=TRUE, \ldots)
\S4method{fdX}{missing,ANY}(pval, padj=NULL, labels, thresholdX=0.05, transformation = "1-x", plot=TRUE, \ldots)

}

\arguments{

\item{object}{An object of \code{\link{SimResults}}.}

\item{pval}{A vector or matrix containing p-values.}

\item{padj}{A vector or matrix containing adjusted p-values (optional). If \code{padj} is missing, adjusted p-values are calculated using \code{p.adjust(..,method=padjMethod)}.  By default, \code{padjMethod="BH"}). For more details, see \code{\link{SimResults}} or \code{help("p.adjust")}.}


\item{labels}{A numeric vector indicating class labels (positives with 1, negatives with 0).  For more details, see \code{?ROCR::prediction}.}

\item{thresholdX}{Numeric value of the adjusted p-value threshold (cutoff). (e.g., FDR = 0.05).}

\item{transformation}{The transformation of \code{pval} and \code{padj}. The default setting "1-x" means \code{1-pval(padj)}. Note that another useful transformation is "-log10(x)".} 
 

\item{plot}{Logical, whether plot or not.}

\item{\ldots}{Optional arguments for plotting (see below for more details). 
	\itemize{
	    \item \code{col}, \code{cex} and other general graphical parameters from \code{\link{par}} can be directly used into the figure.	
	    \item \code{add}, logical, whether the new plot added to current one or not.  
	    \item \code{cexX}, \code{pchX}, \code{colX}, \code{pchX} are special parameters to control the plotting of the X point on the figure. 
            \item \code{legend}, a list or NULL, is used to add legends to plots (see details below). }
}    

}

\details{False discoveries are calculated according to the p-value rank (pval) and labels (from the object of \code{SimResults}). The X point displays the actual false discoveries number versus number of top ranked features given a threshold (e.g., FDR=0.05).

Most of the graphical parameters such as \code{col}, \code{cex}, \code{pch} and etc from \code{\link{par}}, can be directly passed into \code{plot} for all the subfigures. They can be set up as a vector (e.g., 'col=1') sharing the same value for all the subfigures, or a list (e.g, 'col=list(1,2,3)' for different value of different subfigure. \code{cexX}, \code{pchX}, \code{colX}, \code{pchX} are special parameters of X point. Their usage is similar to \code{col}, \code{cex}, \code{pch}. 

To increase the flexibility of plots, you can use argument \code{add}. If \code{add=TRUE}, plots are added to current one combining different \code{rocX} curves together into one figure. 

'legend' is a list including all the arguments from function \code{legend} (see help(legend)). If it is NULL, do not add legends to plots.}

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
