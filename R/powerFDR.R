setClass("powerFDR", contains="BenchMetric")
setClass("powerFDRList", contains="BenchMetricList")

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


powerFDR <- function(object, thresholdX=c(0.01, 0.05, 0.1), plot=FALSE, ...)
##Define powerFDR
##Xiaobei Zhou
##Sep 2014.  Last modified 15 Sep 2014.
        {
            stratify=object@stratify[[1L]]
            .powerFDR(object, stratify=stratify, thresholdX=thresholdX, plot=plot, ...)
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
    function(object, stratify, thresholdX, plot=TRUE, ...)
    {
        l <- ncol(object@pval)
        if(l > 10) stop("the number of method cannot be larger than 10")    
        out <- new("powerFDR")
        for (i in 1:l)
            out@element[[i]] <- .powerFDRfun(padj=object@padj[, i], labels=object@labels,
                                              thresholdX=thresholdX) 
            names(out@element) <- colnames(object@pval)
            if(plot)
                plot(out, thresholdX=thresholdX, ...) 
        out
    } 
)




.powerFDRfun <- function(padj, labels, thresholdX)
{ 
## Calculate false discovery rate and true positive rate.
	tpr <- NULL
	fdr <- NULL
	for (i in 1:length(thresholdX)) 
        {
	    pred <- labels[padj<thresholdX[i]]
            fdr <- c(fdr, 1-sum(pred)/length(pred))
	    tpr <- c(tpr, sum(pred)/sum(labels))
	}
        out <- cbind(fdr=fdr, tpr=tpr, thresholdX=thresholdX)
}


.powerFDRplot <- function(fdr, tpr, add=TRUE, thresholdX=c(0.01,0.05,0.1), pointType="b", ...)
{
    oldPar <- par()
    arglist <- c(lapply( as.list(environment()), eval ), list(...) ) 
    colX <- .getArgX("colX", "col", arglist, oldPar)
    pchX <- .getArgX("pchX", "pch", arglist, oldPar)
    lwdX <- .getArgX("lwdX", "lwd", arglist, oldPar)
    ltyX <- .getArgX("ltyX", "lty", arglist, oldPar)
    if(is.null(arglist[["cexX"]]))
         cexX <- .getArgX("cexX", "cex", arglist, oldPar)+1
    else 
         cexX <- arglist[["cexX"]] 
    bgX <- colX
    bgX[fdr>thresholdX] <- "white"
    print(colX)
    print(fdr)
    print(thresholdX)
    pointType <- match.arg(pointType, c("b","p","l")) 
    arglistPlot <- .sarg(.slice.run(.getArgList("plot", arglist)), x=1, 
                    type="n", xaxt="n")
    arglistAxis <- .sarg(.slice.run(.getArgList("axis", arglist)), 
                      side=1, at=(1:10)/10, labels=(1:10)/10, las=2, 
                      col.ticks="grey", col.axis="grey", 
                      lwd.ticks=1, lwd=1)
    arglistAxisX <- .sarg(.slice.run(.getArgList("axis", arglist)), 
                      side=1, at=thresholdX, labels=thresholdX, las=2, 
                      lwd.ticks=1, lwd=1)
    arglistAbline <- .sarg(.slice.run(.getArgList("axis", arglist)), 
                      v=thresholdX, lty=ltyX, col=colX, lwd=lwdX)   
    arglistX <- .sarg(.slice.run(.getArgList("points", arglist)), x=fdr, 
                      y=tpr, col=colX, cex=cexX, pch=pchX, lwd=lwdX, bg=bgX, type="p") 
    arglistL <- .sarg(.slice.run(.getArgList("points", arglist)), x=fdr, 
                      y=tpr, type="l")
    
    if(!add)
    {
        do.call("plot", arglistPlot)
        do.call("axis", arglistAxis)
        do.call("axis", arglistAxisX)
        do.call("abline", arglistAbline)

    } 
    if(pointType %in% c("l","b")) 
    {
        do.call("points", arglistL)
        if(pointType=="b") 
            do.call("points", arglistX)
    }
    if(pointType=="p")    
        do.call("points", arglistX) 
}


setMethod(
##Define plot method
##Xiaobei Zhou
##Sep 2014.  Last modified 21 Sep 2014.
    "plot",
    signature(x="powerFDR", y="ANY"),
    function(x, y, add=FALSE, legend=list(), ...)
    {
         arglist <- list(...)
         object <- x
         l <- length(object@element)
         if(l > 10) stop("the number of method cannot be larger than 10")
         if(is.null(arglist$xlim))
             xlim <- c(0,0.5)
         else 
             xlim <- arglist$xlim
         if(is.null(arglist$ylim))
             ylim <- c(0,1)
         else 
             ylim <- arglist$ylim  
         colX <- .preColX(arglist, length(thresholdX))
         pchX <- as.list(.prePchX(arglist, l))
         argSpecial <- list(colX=colX, pchX=pchX, cexX=NULL, lwdX=NULL, ltyX=3, add=add)
         argSpecial <- append(.select.args(arglist, names(argSpecial), complement = F), .select.args(argSpecial, names(arglist), complement = T))
         #argSpecial <- lapply(argSpecial, .repArgs, len=l)
         #argSpecial$add[-1L] <- TRUE
         argPlot <- append(arglist, argSpecial)
         argPlot <- lapply(argPlot, .repArgs, len=l)
         argPlot$add[-1L] <- TRUE
         print(argPlot)          
         for (i in 1:l)
         {
             fdr <- object@element[[i]][, "fdr"]
             tpr <- object@element[[i]][, "tpr"] 
             argPloti <- lapply(argPlot, .getSub2, id = i)
             argPloti <- .sarg(argPloti, fdr=fdr, tpr=tpr, xlim=xlim, ylim=ylim) 
             do.call(".powerFDRplot", argPloti)

         }
         nms <- names(object@element) 
         if(!is.null(legend) & !is.null(nms))
         {
             preLegend <- list("bottomright", col=colX, legend=nms, pch=unlist(pchX), lwd=argPlot$lwd[[1]], lty=NA, pt.bg=colX)
             legend <- .replaceLegend(preLegend, legend)
             do.call("legend", legend)
         } 
    }
)


