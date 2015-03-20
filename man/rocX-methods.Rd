\docType{methods}
\name{rocX-methods}
\alias{rocX}
\alias{rocX-methods}
\alias{rocX,SimResults,missing-method}
\alias{rocX,missing,ANY-method}
\title{ROC curve with threshold X}

\description{Produce a simple ROC plot with a cutoff when false discovery rate (FDR) are under certain control using the features of ROCR}

\usage{
rocX(object, pval, \ldots)

\S4method{rocX}{SimResults,missing}(object, thresholdX=0.05, transformation = "1-x", plot=TRUE, \ldots)
\S4method{rocX}{missing,ANY}(pval, padj=NULL, labels, stratify=NULL, thresholdX=0.05, transformation = "1-x", plot=TRUE, \ldots)

}

\arguments{

\item{object}{An object of \code{link{SimResults}}.}

\item{pval}{A vector, matrix containing the p-value.}

\item{padj}{A vector, matrix containing the p-adjusted-value (optional). If 'padj' is missing, its p-adjusted-value would be defaultly calculated by  Benjamini & Hochberg (BH) method or a certain defined adjust method (e.g., 'padjMethod=BY'). For more details please see \code{\link{SimResults}}.}

\item{labels}{A numeric vector indicating positive with a 1, and negative with a 0.}

\item{stratify}{(optional) Stratify factor driving data (see details below).}

\item{thresholdX}{Numeric value of the p-value threshold (cutoff).(e.g, FDR=0.05)}

\item{transformation}{The transformation of 'pval' and 'padj'. The default setting "1-x" means "1-pval(padj, thresholdX)". Note that another useful transformation is "-log10(x)".} 
 

\item{plot}{Logical, whether plot or not.}

\item{\ldots}{Optional arguments from 'plot'. General graphical parameters from \code{\link{par}} such as 'col', 'cex' can be directly used into the figure. Logical parameter 'add', whether the new plot added to current one or not. 'cexX', 'pchX', 'colX', 'pchX' are special parameters of X point on the figure. A list or NULL. 'legend' is used to add legends to plots (see details below). 'addFun' and 'addFunLocation' are special arguments for multiple-panel figure (see details below).}
}

\details{\code{rocX} will call \code{prediction} and \code{performance} from package \code{ROCR} by p-value (pval) and labels (from the object of \code{SimResults}). Additionally, the value (X point) of true positive rate (TPR) and false positve rate (PFR) corresponding to the threshold ('threholdX') will be calculated by p-adjusted-value (padj). The reason introduced \code{rocX} is that standard the ROC curve fails to show the information across different methods or methodology under the same condition. We want to present the information including precisely achieved TPR and FPR given a certain threshold across all the methods. 

When 'stratify' is NULL, function \code{rocX} would produce a \code{rocX-class} object and corresponding to a simple plot. 

When \code{stratify} is factor or numeric, it would produce a \code{rocXList}, which contains a series of collection of \code{rocX} splitted by different levels of \code{stratify} or range of data in \code{stratify} (by numGroups). For \code{rocXList}, multiple plots are made . During each iteration of plot, if you want to add some additional function, such as \code{abline}, you can use the argument 'addFun' and 'addFunLocation' providing a utility of adding a specified function into a specified location for a multi-panel figure (see example2 and example3). It is useful for plotting different subsets of data.     

Most of the graphical parameters such as 'col', 'cex', 'pch' and etc from \code{\link{par}}, can be directly passed into 'plot' for all the subfigures. They can be set up as a vector (e.g., 'col=1') sharing the same value for all the subfigures, or a list (e.g, 'col=list(1,2,3)' for different value of different subfigure. 'cexX', 'pchX', 'colX', 'pchX' are special parameters of X point. Their usage are similar with 'col', 'cex', 'pch'. 

To increase the flexibility of plots, you can use argument 'add'. If 'add=TRUE', plots are added to current one combining different \code{rocX} curves together into one figure. 

'legend' is a list including all the argments from function \code{legend} (see help(legend)). If it is NULL, do not add legends to plots. For a multiple-panel figure containing several \code{rocX} curves, 'location' can be used to determine whether to add legends on specific location (e.g., legend=list(location=c(1,0,0,0))).  
  
}

\value{An S4 object of class \code{\link{rocX-class}} or \code{\link{rocXList-class}}. 
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
res1 <- re[1:1000,] 
res2 <- re[1001:2000,]
res3 <- re[2001:3000,] 
res4 <- re[3001:4000,] 
r1 <- rocX(res1, plot=FALSE)
r2 <- rocX(res2, plot=FALSE)
r3 <- rocX(res3, plot=FALSE)
r4 <- rocX(res4, plot=FALSE) 
roc6 <- rocXList(r1=r1, r2=r2, r3=r3, r4=r4)
par(mfrow=c(2,2))
plot(roc6, lwd=list(1,2,3,4))

}  

\keyword{rocX, SimResults}