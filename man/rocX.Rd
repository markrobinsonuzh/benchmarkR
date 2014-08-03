\docType{methods}
\name{rocX}
\alias{rocX}
\title{rocX constructor.}

\description{Produce an object of 'rocX' and a simple ROC plot with threshold X (fdr) using the features of ROCR}

\usage{
rocX(object, thresholdX=0.05, plot=TRUE, \ldots)
}

\arguments{

\item{object}{An object of 'SimResults'.}

\item{thresholdX}{The threshold of score (score.X). Note that if score is 1 - P_adjust_value, "threshold.X" can be set at 0.95 (FDR = 0.05).} 

\item{plot}{logical, if 'plot' is TRUE, an ROC plot would be produced when 'rocX' or 'rocXList' object is created ('cexX', 'pchX', 'colX', 'pchX' are special parameters of threshold X for plot. 'fprCutoff' is the cutoff of fpr to domaine 'xlim' of the plot.). 'addFun' and 'addFunLocation' See details below.}

\item{\ldots}{Optional arguments from 'plot'.}
}

\details{\code{rocX} will call 'prediction' and 'performance' from package 'ROCR' by Pvalue (pval) and labels from the object of 'SimResults'. Additionally, the value of tpr and fpr corresponding to the threshold X will be calculated by P adjusted value (padj) from the object of 'SimResults'. The reason introduced 'rocX' is that standard 'roc' plot fails to show the information across different methods or methodology under the same condition (threshold of P-value). We want to present the information including precisely achieved tpr and fpr given a certain threshold across all the methods. 

When 'stratify' of 'SimResults' object is NULL, 'rocX' would produce a 'rocX' object and corresponding to a simple plot. 

When 'stratify' is factor or numeric, it would produce a 'rocXList', which contains a series of collection of 'rocX' splited by different levels of 'stratify' or range of data in 'stratify' (by numGroups). For 'rocXList', multiple plots are made . During each iteration of plot, if you want to add some additional function, such as 'legend' or 'abline', you can use argument 'addFun' and 'addFunLocation' (see example2 and example3). It is useful for plotting different subsets of data.     

To increase the flexibility of plots, you can use argument 'add'. If 'add'=TRUE, plots are added to current one. 
  
}

\value{An S4 object of class "rocX" or "rocXList". 
}

\seealso{ \code{\link{SimResults}}
}

\author{Xiaobei Zhou and Mark D. Robinson}

\examples{
library(benchmarkR)
data(rocX)
pval <- do.call("cbind", dat_rocX$pval)
padj <- do.call("cbind", dat_rocX$padj)
labels <- rep(0, nrow(pval))
labels[dat_rocX$indDE] <- 1
re <- SimResults(pval=pval, padj=padj, labels=labels)
######example1###############################################################################
roc1 <- rocX(re, plot=TRUE)
###rocX can use all the args of par() and special args (i.e., pchX, colX, cexX, lwdX) ###
roc2 <- rocX(re, plot = TRUE, lty = 1:2,col = 1:2, lwd = 2, main = "roc", cex.lab = 2, cex.main = 3, colX = 2:3, pchX = 2:3, lwdX =5:6, fprCutoff = 0.6)
###you can also re-plot it###
plot(roc1, lwd = 3)
###you can also do not add legends
plot(roc1, lwd = 3, legend=NULL)
plot(roc1, lwd = 3, legend=list("center", c("edgeR", "voom"), lwd=3, pch=1, lty=2))
######example2###############################################################################
f <- as.factor(sample(0:3, nrow(pval), replace=TRUE))
ref <- SimResults(pval=pval, padj=padj, labels=labels, stratify=f)
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
plot(roc3, addFun=list(fun1, fun2), addFunLocation=list(loca1, loca2))
plot(roc3, addFun=list(fun1, fun2), addFunLocation=list(loca1, loca2), main = list("a", "b", "c", "d"), lwd = 1:4, col=c(1,3))
plot(roc3, legend=list(location=c(1,0,0,1)),addFun=list(fun1, fun2), addFunLocation=list(loca1, loca2), main = list("a", "b", "c", "d"), lwd = 1:4, col=c(1,3))

######example3###############################################################################
n <- rnorm(nrow(pval), 0, 10)
ren <- SimResults(pval=pval, padj=padj, labels=labels, stratify=n)
roc4 <- rocX(ren, plot=TRUE)
roc5 <- rocX(ren, plot=TRUE, numGroups=3)

######example4###############################################################################
res1 <- re[1:250,] 
res2 <- re[251:500,]
res3 <- re[501:750,] 
res4 <- re[751:100,] 
r1 <- rocX(res1, plot=FALSE)
r2 <- rocX(res2, plot=FALSE)
r3 <- rocX(res3, plot=FALSE)
r4 <- rocX(res4, plot=FALSE) 
roc6 <- rocXList(r1=r1, r2=r2, r3=r3, r4=r4)
par(mfrow=c(2,2))
plot(roc6)
dev.off()
}  

\keyword{rocX, SimResults}