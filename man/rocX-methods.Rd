\docType{methods}
\name{rocX-methods}
\alias{rocX}
\alias{rocX-methods}
\alias{rocX,SimResults,missing-method}
\alias{rocX,missing,ANY-method}
\alias{rocX,SimResultsList,missing-method}
\title{ROC curve augmented with X point}

\description{Produce a ROC plot with an X point marking the location of the method's FDR (at a chosen level). }

\usage{
rocX(object, pval, \ldots)

\S4method{rocX}{SimResults,missing}(object, thresholdX=0.05, transformation = "1-x", plot=TRUE, \ldots)
\S4method{rocX}{missing,ANY}(pval, padj=NULL, labels, stratify=NULL, thresholdX=0.05, transformation = "1-x", plot=TRUE, \ldots)
\S4method{rocX}{SimResultsList,missing}(object, thresholdX=0.05, transformation = "1-x", plot=TRUE,typeLine="b", percentile=c(0,1), alpha=0.3, \ldots)

}

\arguments{

\item{object}{An object of \code{\link{SimResults}}.}

\item{pval}{A vector or matrix containing p-values.}

\item{padj}{A vector or matrix containing adjusted p-values (optional). If \code{padj} is missing, adjusted p-values are calculated using \code{p.adjust(..,method=padjMethod)}.  By default, \code{padjMethod="BH"}). For more details, see \code{\link{SimResults}} or \code{help("p.adjust")}.}

\item{labels}{A numeric vector indicating class labels (positives with 1, negatives with 0).  For more details, see \code{?ROCR::prediction}.}

\item{stratify}{(optional) Stratify factor driving data (see details below).}

\item{thresholdX}{Numeric value of the adjusted p-value threshold (cutoff).(e.g, FDR=0.05)}

\item{transformation}{The transformation of \code{pval} and \code{padj}. The default setting "1-x" means \code{1-pval(padj)}. Note that another useful transformation is "-log10(x)".} 
 

\item{plot}{Logical, whether plot or not.}

\item{typeLine}{Letter indicate how the average performance should be plotted: "ave" for average performance; "shape" for the defined interval (see percentile) and "b" for both average and defined interval. This argument is only referred to \code{\link{SimResultsList}}.}

\item{percentile}{Defined interval for "shape". This argument is only referred to \code{\link{SimResultsList}}.}

\item{alpha}{Modify colour transparency for "shape". This argument is only referred to \code{\link{SimResultsList}}.}

\item{\ldots}{Optional arguments for plotting (see below for more details). 
	\itemize{
	    \item \code{col}, \code{cex} and other general graphical parameters from \code{\link{par}} can be directly used into the figure.	
	    \item \code{add}, logical, whether the new plot added to current one or not.  
	    \item \code{cexX}, \code{pchX}, \code{colX}, \code{pchX} are special parameters to control the plotting of the X point on the figure (see details below). 
        \item \code{legend}, a list or NULL, is used to add legends to plots (see details below). 
        \item \code{addFun} and \code{addFunLocation} are special arguments for multiple-panel figure (see details below).}
}        
}

\details{\code{rocX} will call \code{prediction} and \code{performance} from package \code{ROCR} using p-value (pval) and labels (from the object of \code{SimResults}). Additionally, the X point showing true positive rate (TPR) versus false positve rate (PFR) corresponding to the threshold ('threholdX') will be calculated according to the adjusted p-value (padj). We introduced \code{rocX} to augment information (e.g., show the power at a selected FDR cutoff) available in standard ROC curves.

When \code{stratify} is NULL, function \code{rocX} would produce a \code{rocX-class} object and corresponding to a simple plot. 

When \code{stratify} is factor or numeric, it would produce a \code{rocXList}, which contains a series of collection of \code{rocX} splitted by different levels of \code{stratify} or range of data in \code{stratify} (by numGroups). For \code{rocXList}, multiple plots are made . During each iteration of plot, if you want to add some additional function, such as \code{abline}, you can use the argument 'addFun' and 'addFunLocation' providing a utility of adding a specified function into a specified location for a multi-panel figure (see example2 and example3). It is useful for plotting different subsets of data.     

Most of the graphical parameters such as \code{col}, \code{cex}, \code{pch} and etc from \code{\link{par}}, can be directly passed into \code{plot} for all the subfigures. They can be set up as a vector (e.g., \code{col=1}) sharing the same value for all the subfigures, or a list (e.g, \code{col=list(1,2,3)} for different value of different subfigure. \code{cexX}, \code{pchX}, \code{colX}, \code{pchX} are special parameters of X point. Their usage is similar to \code{col}, \code{cex}, \code{pch}. 

To increase the flexibility of plots, you can use argument 'add'. If 'add=TRUE', plots are added to current one combining different \code{rocX} curves together into one figure. 

\code{legend} is a list including all the argments from function \code{legend} (see help(legend)). If it is NULL, do not add legends to plots. For a multiple-panel figure containing several \code{rocX} curves, \code{location} can be used to determine whether to add legends on specific location (e.g., legend=list(location=c(1,0,0,0))).  
  
}

\value{Invisibly, an S4 object of class \code{\link{rocX-class}} or \code{\link{rocXList-class}}. 
}

\seealso{ \code{\link{SimResults}}, \code{\link{powerFDR}}
}

\author{Xiaobei Zhou and Mark D. Robinson}

\examples{
data(Pickrell)
re <- SimResults(pval=Pickrell$pval, labels=Pickrell$labels)

##3 example1
roc1 <- rocX(re, plot=TRUE)
# using arguments of par()
# using special argguments (pchX, colX, cexX, lwdX)
roc2 <- rocX(re, plot = TRUE, xlim=c(0,0.6), 
             lty = 1:3,col = 1:3, lwd = 2, 
             main = "roc", cex.lab = 2, 
             cex.main = 3, colX = 1:3, 
             pchX = 1:3, lwdX =4:6)
# re-plot 
plot(roc1, lwd = 3)
# add legends
plot(roc1, lwd = 3, legend=NULL)
plot(roc1, lwd = 3, 
     legend=list("center", c("edgeR", "voom"), 
     lwd=3, pch=1, lty=2))

## example2
# stratify as a factor 
f <- as.factor(sample(0:3, nrow(Pickrell$pval), replace=TRUE))
ref <- SimResults(pval=Pickrell$pval, padj=Pickrell$padj, 
          labels=Pickrell$labels, stratify=f)
par(mfrow=c(2,2))
roc3 <- rocX(ref, plot=TRUE)
plot(roc3)
plot(roc3, add=c(FALSE, TRUE, TRUE, TRUE))
fun1 <- 'abline(0,1, col=2)' 
par(mfrow=c(2,2))
plot(roc3, addFun=fun1)
loca1 <- c(1,0,1,0)
plot(roc3, addFun=fun1, addFunLocation=loca1)
fun2 <- 'abline(0,1, col=3)'
loca2 <- c(0,1,0,1)
plot(roc3, addFun=list(fun1, fun2), 
     addFunLocation=list(loca1, loca2))
plot(roc3, addFun=list(fun1, fun2), 
     addFunLocation=list(loca1, loca2), 
     main = list("a", "b", "c", "d"), 
     lwd = 1:3, col=1:3)
plot(roc3, legend=list(location=c(1,0,0,1)),
     addFun=list(fun1, fun2), 
     addFunLocation=list(loca1, loca2), 
     main = list("a", "b", "c", "d"), 
     lwd = 1:3, col=1:3)

## example3
# numeric stratify
n <- rnorm(nrow(Pickrell$pval), 0, 10)
ren <- SimResults(pval=Pickrell$pval, padj=Pickrell$padj, 
          labels=Pickrell$labels, stratify=n)
roc4 <- rocX(ren, plot=TRUE)
roc5 <- rocX(ren, plot=TRUE, numGroups=3)

## example4
# combining multiple plots
rl <- list()
s <- seq(0,4000,by=1000)
for(i in 1:4)
{
    rei <- re[(1+s[i]):s[i+1],]
    rl[[i]] <- rocX(rei, plot=FALSE)
}     
roc6 <- rocXList(rl)
par(mfrow=c(2,2))
plot(roc6, lwd=list(1,2,3,4))

}  

\keyword{rocX, SimResults}
