\docType{methods}
\name{rocX}
\alias{rocX}
\title{rocX constructor.}

\description{Produce an object of 'rocX' and a simple ROC plot with a cutoff when false discovery rate (FDR) are under certain control using the features of ROCR}

\usage{
rocX(object=NULL, stratify=NULL, thresholdX=0.05, transformation="1-x", plot=TRUE, \ldots)
}

\arguments{

\item{object}{An object of \code{SimResults}.}

\item{stratify}{A stratify factor driving data (Optional arguments. See details below.).}

\item{thresholdX}{Numeric value of the p-value threshold (cutoff). \code{threshold.X=0.05}(FDR = 0.05).}

\item{transformation}{The transformation of \code{pval} and \code{padj} in object of \code{SimResults}. The default setting "1-x" means "1-pval(padj, thresholdX)". Note that another useful transformation is "-log10(x)".} 
 

\item{plot}{Logical, whether plot or not.}

\item{\ldots}{Optional arguments from 'plot'. Note that logical parameter 'add', should power-fdr value(s) be added to current plot. Graphical parameters from \code{par} such as 'col', 'cex' can be used for the figure. 'cexX', 'pchX', 'colX', 'pchX' are special parameters of X point on the figure. 'addFun' and 'addFunLocation' are special arguments for multiple-panel figure (See details below).}
}

\details{\code{rocX} will call \code{prediction} and \code{performance} from package \code{ROCR} by p-value (pval) and labels from the object of \code{SimResults}. Additionally, the value (X point) of true positive rate (TPR) and false positve rate (PFR) corresponding to the threshold (\code{threholdX}) will be calculated by p-adjusted-value (padj) from the object of 'SimResults'. The reason introduced \code{rocX} is that standard the ROC curve fails to show the information across different methods or methodology under the same condition. We want to present the information including precisely achieved TPR and FPR given a certain threshold across all the methods. 

When \code{stratify} of \code{SimResults} object is NULL, function \code{rocX} would produce a \code{rocX-class} object and corresponding to a simple plot. 

When \code{stratify} is factor or numeric, it would produce a \code{rocXList}, which contains a series of collection of \code{rocX} splitted by different levels of \code{stratify} or range of data in \code{stratify} (by numGroups). For \code{rocXList}, multiple plots are made . During each iteration of plot, if you want to add some additional function, such as \code{legend} or \code{abline}, you can use the argument \code{addFun} and \code{addFunLocation} providing a utility of adding a specified function into a specified location for a multi-panel figure (see example2 and example3). It is useful for plotting different subsets of data.     

To increase the flexibility of plots, you can use argument \code{add}. If \code{add=TRUE}, plots are added to current one combining different \code{rocX} curves together into one figure. 
  
}

\value{An S4 object of class "rocX" or "rocXList". 
}

\seealso{ \code{\link{SimResults}}, \code{\link{powerFDR}}
}

\author{Xiaobei Zhou and Mark D. Robinson}

\examples{
library(benchmarkR)
data(Pickrell)
pval <- do.call("cbind", Pickrell$pval)
padj <- do.call("cbind", Pickrell$padj)
labels <- rep(0, nrow(pval))
labels[Pickrell$indDE] <- 1
re <- SimResults(pval=pval, padj=padj, 
                 labels=labels)
######example1####################################
roc1 <- rocX(re, plot=TRUE)
###rocX can use all the args of par()## 
###special args (i.e., pchX, colX, cexX, lwdX) ###
roc2 <- rocX(re, plot = TRUE, xlim=c(0,0.6), 
             lty = 1:2,col = 1:2, lwd = 2, 
             main = "roc", cex.lab = 2, 
             cex.main = 3, colX = 2:3, 
             pchX = 2:3, lwdX =5:6)
###you can also re-plot it###
plot(roc1, lwd = 3)
###you can also do not add legends
plot(roc1, lwd = 3, legend=NULL)
plot(roc1, lwd = 3, 
     legend=list("center", c("edgeR", "voom"), 
     lwd=3, pch=1, lty=2))
######example2######################################
f <- as.factor(sample(0:3, nrow(pval), replace=TRUE))
ref <- SimResults(pval=pval, padj=padj, 
          labels=labels, stratify=f)
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
     lwd = 1:2, col=c(1,3))
plot(roc3, legend=list(location=c(1,0,0,1)),
     addFun=list(fun1, fun2), 
     addFunLocation=list(loca1, loca2), 
     main = list("a", "b", "c", "d"), 
     lwd = c(1,3), col=c(1,2))

######example3######################################
n <- rnorm(nrow(pval), 0, 10)
ren <- SimResults(pval=pval, padj=padj, 
          labels=labels, stratify=n)
roc4 <- rocX(ren, plot=TRUE)
roc5 <- rocX(ren, plot=TRUE, numGroups=3)

######example4######################################
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
dev.off()
}  

\keyword{rocX, SimResults}