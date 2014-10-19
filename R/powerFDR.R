setClass("powerFDR", contains="BenchMetric")
#setClass("powerFDRList", contains="BenchMetricList")

setMethod("show","powerFDR",
function(object) 
##Define show of powerFDR
##Xiaobei Zhou
##Sep 2014.  Last modified 17 Sep 2014.
{
    cat("An object of class \"",class(object),"\n",sep="")
    cat("..............", "\n")
    cat(slotNames(object), ":", "\n")
    print(object@element)
})


powerFDR <- function(object,threshold=c(0.01, 0.05, 0.1), plot=TRUE, ...)
##Define powerFDR
##Xiaobei Zhou
##Sep 2014.  Last modified 15 Sep 2014.
        {
            if(class(object)=="SimResult")
                stratify <- object@stratify
            else
                stratify <- NULL  
            .powerFDR(object, stratify=stratify, threshold=threshold, plot=plot, ...)
        }



setGeneric(
##Define genetric of .powerFDR
##Xiaobei Zhou
##Sep 2014.  Last modified Sep 16 2014.
   ".powerFDR", 
   function(object, stratify, ...)   
   {
       standardGeneric(".powerFDR")
   }  
)


setMethod(
##Define .powerFDR for "SimResults"
##Xiaobei Zhou
##Sep 2014.  Last modified Sep 16 2014.
    .powerFDR,
    signature(object="SimResults", stratify="NULL"),
    function(object, stratify, threshold, plot=TRUE, ...)
    {
        l <- ncol(object@pval)
        if(l > 10) stop("the number of method cannot be larger than 10")    
        out <- new("powerFDR")
        for (i in 1:l)
            out@element[[i]] <- .powerFDRfun(padj=object@padj[, i], labels=object@labels,
                                              threshold=threshold) 
            names(out@element) <- colnames(object@pval)
            if(plot)
                plot(out, threshold=threshold, ...) 
        out
    } 
)



setMethod(
##Define .powerFDR for "pval" vector
##Xiaobei Zhou
##Sep 2014.  Last modified 19 Oct 2014.
    .powerFDR,
    signature(object="numeric", stratify="NULL"),
    function(object, labels, stratify, threshold, plot=TRUE, ...)
    {
        re <- SimResults(pval=object, padj=object, labels=labels)
        .powerFDR(re, stratify, threshold, plot=TRUE, ...)         
    } 
)


setMethod(
##Define .powerFDR for "pval" matrix
##Xiaobei Zhou
##Sep 2014.  Last modified 19 Oct 2014.
    .powerFDR,
    signature(object="matrix", stratify="NULL"),
    function(object, labels, stratify, threshold, plot=TRUE, ...)
    {
        re <- SimResults(pval=object, padj=object, labels=labels)
        .powerFDR(re, stratify, threshold, plot=TRUE, ...)         
    } 
)




.powerFDRfun <- function(padj, labels, threshold)
{ 
## Calculate false discovery rate and true positive rate.
	TPR <- NULL
	FDR <- NULL
	for (i in 1:length(threshold)) 
        {
	    pred <- labels[padj<threshold[i]]
            FDR <- c(FDR, 1-sum(pred)/length(pred))
	    TPR <- c(TPR, sum(pred)/sum(labels))
	}
        out <- cbind(threshold=threshold, FDR=FDR, TPR=TPR)
}


.powerFDRplot <- function(FDR, TPR, add=TRUE, threshold=c(0.01,0.05,0.1), pointType="b", ...)
{
    arglist <- c(lapply( as.list(environment()), eval ), list(...) ) 
    pointType <- match.arg(pointType, c("b","p","l"))
    bg <- arglist$col
    bg[FDR>threshold] <- "white" 
    arglistPlot <- .sarg(.slice.run(.getArgList("plot", arglist)), x=1, 
                    type="n", xaxt="n")
    arglistAxis <- .sarg(.slice.run(.getArgList("axis", arglist)), 
                      side=1, at=(1:10)/10, labels=(1:10)/10, las=2, 
                      col.ticks="grey", col.axis="grey", col=1)
    arglistAxisThreshold <- .sarg(.slice.run(.getArgList("axis", arglist)), 
                      side=1, at=threshold, labels=threshold, las=2, col=1)
    arglistAbline <- .sarg(.slice.run(.getArgList("axis", arglist)), 
                      v=threshold, lty=arglist$lty.threshold, 
                      lwd=arglist$lwd.threshold, 
                      col=arglist$col.threshold)   
    arglistP <- .sarg(.slice.run(.getArgList("points", arglist)), x=FDR, 
                      y=TPR, type="p", bg=bg) 
    arglistL <- .sarg(.slice.run(.getArgList("points", arglist)), x=FDR, 
                      y=TPR, type="l", col=arglist$col.line, 
                      lwd=arglist$lwd.line)
    
    if(!add)
    {
        do.call("plot", arglistPlot)
        do.call("axis", arglistAxis)
        do.call("axis", arglistAxisThreshold)
        suppressWarnings(do.call("abline", arglistAbline))

    } 
    if(pointType %in% c("l","b")) 
    {
        do.call("points", arglistL)
        if(pointType=="b") 
            do.call("points", arglistP)
    }
    if(pointType=="p")    
        do.call("points", arglistP) 
}


setMethod(
##Define plot method
##Xiaobei Zhou
##Sep 2014.  Last modified 19 Oct 2014.
    "plot",
    signature(x="powerFDR", y="ANY"),
    function(x, y, add=FALSE, legend=list(), ...)
    {
         arglist <- list(...)
         object <- x
         l <- length(object@element)
         if(l > 10) stop("the number of method cannot be larger than 10") 
         col <- .preCol(arglist, length(arglist$threshold))
         pch <- .prePch(arglist, l)
         
         arglist$pch <- as.list(pch)
         argSpecial <- list(xlim=c(0,0.5), ylim=c(0,1), col=col, 
                            pch=pch, lwd.threshold=1, xlab="FDR", ylab="power(TPR)",
                            lty.threshold=3, col.threshold=1,lwd.ticks=1, col.line=1,
                            lwd.line=1, add=add)
         argSpecial <- .select.args(argSpecial, names(arglist), complement = T)
         argPlot <- append(arglist, argSpecial)
         argPlot <- lapply(argPlot, .repArgs, len=l)
         #argPlot <- .expandListArgs(argPlot, len=l)
         argPlot$add[-1L] <- TRUE
         #print(argPlot)          
         for (i in 1:l)
         {
             FDR <- object@element[[i]][, "FDR"]
             TPR <- object@element[[i]][, "TPR"] 
             argPloti <- lapply(argPlot, .getSub2, id = i)
             argPloti <- .sarg(argPloti, FDR=FDR, TPR=TPR) 
             do.call(".powerFDRplot", argPloti)

         }
         nms <- names(object@element) 
         if(!is.null(legend) & !is.null(nms))
         {
             if(is.null(argPloti$cex))
                 cex <- 1
             else
                 cex <- 0.6*argPloti$cex  
             preLegend <- list("bottomright", col="black", legend=nms, cex=cex,pch=pch, lwd=argPloti$lwd, lty=NA)
             legend <- .replaceLegend(preLegend, legend)
             do.call("legend", legend)
         } 
    }
)


