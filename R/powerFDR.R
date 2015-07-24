setClass("powerFDR", contains=".BenchMetric")
#setClass("powerFDRList", contains=".BenchMetricList")

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


setGeneric(
##Define genetric of powerFDR
##Xiaobei Zhou
##March 2014.  Last modified 19 March 2015.
   "powerFDR", 
   function(object, padj, ...)   
   {
       standardGeneric("powerFDR")
   }  
)



setMethod(
##Define powerFDR for "SimResults"
##Xiaobei Zhou
##March 2015.  Last modified 19 March 2015.
    "powerFDR",
    signature(object="SimResults", padj="missing"),
    function(object, threshold=c(0.01, 0.05, 0.1),  transformation="1-x", plot=TRUE, ...)
    {
        stratify <- object@stratify[[1]]
        if(!is.null(stratify))
            stop("Currently, 'powerFDR' only supports 'stratify=NULL'!")
        idNA <- is.na(object@labels)
        if(any(idNA))
        {
            message("remove NA values from labels for powerFDR")
            object <- object[!idNA,]
        }    
        .powerFDR(object, stratify=stratify, threshold=threshold, 
                  plot=plot,  transformation=transformation, ...)

    }
)


setMethod(
##Define powerFDR for "pval"
##Xiaobei Zhou
##March 2015.  Last modified 19 March 2015.
    "powerFDR",
    signature(object="missing", padj="ANY"),
    function(padj, labels, threshold=c(0.01, 0.05, 0.1),  transformation="1-x", plot=TRUE, ...)
    {
        object <- SimResults(pval=padj, padj=padj, labels=labels, stratify=NULL)
        powerFDR(object, threshold=threshold, plot=plot,  transformation= transformation, ...)
    }
)



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
    function(object, stratify, threshold, plot=TRUE,  transformation="1-x", ...)
    {
        l <- ncol(object@pval)
        if(l > 10) stop("the number of method cannot be larger than 10")    
        out <- new("powerFDR")
        for (i in 1:l)
            out@element[[i]] <- .powerFDRfun(padj=object@padj[, i], labels=object@labels,
                                              threshold=threshold, transformation=transformation) 
            names(out@element) <- colnames(object@pval)
            if(plot)
                plot(out, threshold=threshold, ...) 
        invisible(out)
    } 
)



#setMethod(
##Define .powerFDR for "pval" vector
##Xiaobei Zhou
##Sep 2014.  Last modified 19 Oct 2014.
#    .powerFDR,
#    signature(object="numeric", stratify="NULL"),
#    function(object, labels, stratify, threshold, plot=TRUE, ...)
#    {
#        re <- SimResults(pval=object, padj=object, labels=labels)
#        .powerFDR(re, stratify, threshold, plot=TRUE, ...)         
#    } 
#)


#setMethod(
##Define .powerFDR for "pval" matrix
##Xiaobei Zhou
##Sep 2014.  Last modified 19 Oct 2014.
#    .powerFDR,
#    signature(object="matrix", stratify="NULL"),
#    function(object, labels, stratify, threshold, plot=TRUE, ...)
#    {
#        re <- SimResults(pval=object, padj=object, labels=labels)
#        .powerFDR(re, stratify, threshold, plot=TRUE, ...)         
#    } 
#)




.powerFDRfun <- function(padj, labels, threshold, transformation = "1-x")
{ 
## Calculate false discovery rate and true positive rate.
	 tf <- function(x) eval(parse(text=transformation))
	TPR <- NULL
	FDR <- NULL
	padj <- padj+(1e-20)
    score <- tf(padj)
    thresholdX <- tf(threshold)      
	for (i in 1:length(thresholdX)) 
        {
	    pred <- labels[score>thresholdX[i]]
        FDR <- c(FDR, 1-sum(pred)/length(pred))
	    TPR <- c(TPR, sum(pred)/sum(labels))
	}
        out <- cbind(threshold=threshold, FDR=FDR, TPR=TPR)
}


.powerFDRplot <- function(FDR, TPR, add=TRUE, threshold=c(0.01,0.05,0.1), point.type="b", ...)
{
    arglist <- c(lapply( as.list(environment()), eval ), list(...) ) 
    point.type <- match.arg(point.type, c("b","p","l"))
    bg <- rep(arglist$col,length.out=length(FDR))
    bg[FDR>threshold] <- "white" 
    arglistPlot <- .sarg(.slice.run(.getArgList("plot", arglist)), x=1, 
                    type="n", xaxt="n")
    arglistAxis <- .sarg(.slice.run(.getArgList("axis", arglist)), 
                      side=1, at=(1:10)/10, labels=(1:10)/10, las=2, 
                      col.ticks="grey", col.axis="grey", col=1, lwd=0)
    arglistAxisThreshold <- .sarg(.slice.run(.getArgList("axis", arglist)), 
                      side=1, at=threshold, labels=threshold, las=2, 
                      col=1, lwd=0)
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
    if(point.type %in% c("l","b")) 
    {
        do.call("points", arglistL)
        if(point.type=="b") 
            do.call("points", arglistP)
    }
    if(point.type=="p")    
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
         arglist$col <- col <- .preCol(arglist, l) 
         arglist$pch <- pch <- .prePch(arglist, l)
         xlim <- .preXlim(arglist, object)
         ylim <- .preYlim(arglist, object)         
         argSpecial <- list(xlim=xlim, ylim=ylim, lwd.threshold=1, xlab="FDR", ylab="TPR",
                            lty.threshold=3, col.threshold=1,lwd.ticks=1, col.line=1,
                            lwd.line=3, add=add,lwd=3,cex=2)
         argSpecial <- .select.args(argSpecial, names(arglist), complement = T)
         argPlot <- append(arglist, argSpecial)
         argPlot <- .expandListArgs(argPlot, len=l)
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
             cex <- 0.5*argPlot$cex[[1]]  
             preLegend <- list("bottomright", col=col, legend=nms, pch=pch, lwd=argPloti$lwd, 
                                         lty=NA, text.font=2,cex=cex)
             legend <- .replaceLegend(preLegend, legend)
             do.call("legend", legend)
         } 
    }
)


