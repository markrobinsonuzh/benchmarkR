setClass("precREC", contains=".BenchMetric")


setMethod("show","precREC",
function(object) 
##Define show of precREC
##Xiaobei Zhou
##Nov 2015.  Last modified 15 Nov 2015.
{
    cat("An object of class \"",class(object),"\n",sep="")
    cat("..............", "\n")
    cat(slotNames(object), ":", "\n")
    print(object@element)
})


setGeneric(
##Define genetric of precREC
##Xiaobei Zhou
##Nov 2015.  Last modified 15 Nov 2015.
   "precREC", 
   function(object, padj, ...)   
   {
       standardGeneric("precREC")
   }  
)



setMethod(
##Define precREC for "SimResults"
##Xiaobei Zhou
##Nov 2015.  Last modified 15 Nov 2015.
    "precREC",
    signature(object="SimResults", padj="missing"),
    function(object, threshold=c(0.99, 0.95, 0.9),  transformation="1-x", plot=TRUE, ...)
    {
        stratify <- object@stratify[[1]]
        if(!is.null(stratify))
            stop("Currently, 'precREC' only supports 'stratify=NULL'!")
        idNA <- is.na(object@labels)
        if(any(idNA))
        {
            message("remove NA values from labels for precREC")
            object <- object[!idNA,]
        }    
        .precREC(object, stratify=stratify, threshold=threshold, 
                  plot=plot,  transformation=transformation, ...)

    }
)


setMethod(
##Define precREC for "pval"
##Xiaobei Zhou
##Nov 2015.  Last modified 15 Nov 2015.
    "precREC",
    signature(object="missing", padj="ANY"),
    function(padj, labels, threshold=c(0.99, 0.95, 0.9),  transformation="1-x", plot=TRUE, ...)
    {
        object <- SimResults(pval=padj, padj=padj, labels=labels, stratify=NULL)
        precREC(object, threshold=threshold, plot=plot,  transformation= transformation, ...)
    }
)



setGeneric(
##Define genetric of .precREC
##Xiaobei Zhou
##Nov 2015.  Last modified 15 Nov 2015.
   ".precREC", 
   function(object, stratify, ...)   
   {
       standardGeneric(".precREC")
   }  
)


setMethod(
##Define .precREC for "SimResults"
##Xiaobei Zhou
##Nov 2015.  Last modified 15 Nov 2015.
    .precREC,
    signature(object="SimResults", stratify="NULL"),
    function(object, stratify, threshold, plot=TRUE,  transformation="1-x", ...)
    {
        l <- ncol(object@pval)
        if(l > 10) stop("the number of method cannot be larger than 10")    
        out <- new("precREC")
        for (i in 1:l)
            out@element[[i]] <- .precRECfun(padj=object@padj[, i], labels=object@labels,
                                              threshold=threshold, transformation=transformation) 
            names(out@element) <- colnames(object@pval)
            if(plot)
                plot(out, threshold=threshold, ...) 
        invisible(out)
    } 
)



#setMethod(
##Define .precREC for "pval" vector
##Xiaobei Zhou
##Nov 2015.  Last modified 15 Nov 2015.
#    .precREC,
#    signature(object="numeric", stratify="NULL"),
#    function(object, labels, stratify, threshold, plot=TRUE, ...)
#    {
#        re <- SimResults(pval=object, padj=object, labels=labels)
#        .precREC(re, stratify, threshold, plot=TRUE, ...)         
#    } 
#)


#setMethod(
##Define .precREC for "pval" matrix
##Xiaobei Zhou
##Nov 2015.  Last modified 15 Nov 2015.
#    .precREC,
#    signature(object="matrix", stratify="NULL"),
#    function(object, labels, stratify, threshold, plot=TRUE, ...)
#    {
#        re <- SimResults(pval=object, padj=object, labels=labels)
#        .precREC(re, stratify, threshold, plot=TRUE, ...)         
#    } 
#)




.precRECfun <- function(padj, labels, threshold, transformation = "1-x")
{ 
## Calculate false discovery rate and true positive rate.
	 tf <- function(x) eval(parse(text=transformation))
	TPR <- NULL
	FDR <- NULL
	padj <- padj+(1e-20)
    score <- tf(padj)
    thresholdX <- tf(1-threshold)      
	for (i in 1:length(thresholdX)) 
        {
	    pred <- labels[score>thresholdX[i]]
        FDR <- c(FDR, 1-sum(pred)/length(pred))
	    TPR <- c(TPR, sum(pred)/sum(labels))
	}
        out <- cbind(threshold=threshold, PREC=1-FDR, TPR=TPR)
}


.precRECplot <- function(PREC, TPR, add=TRUE, threshold=c(0.99,0.95,0.9), point.type="b", ...)
{
    arglist <- c(lapply( as.list(environment()), eval ), list(...) ) 
    point.type <- match.arg(point.type, c("b","p","l"))
    bg <- rep(arglist$col,length.out=length(PREC))
    bg[PREC>threshold] <- "white" 
    arglistPlot <- .sarg(.slice.run(.getArgList("plot", arglist)), x=1, 
                    type="n", yaxt="n")
    arglistAxis <- .sarg(.slice.run(.getArgList("axis", arglist)), 
                      side=2, at=(1:9)/10, labels=(1:9)/10, las=1, 
                      col.ticks="grey", col.axis="grey", col=1, lwd=0)
    arglistAxisThreshold <- .sarg(.slice.run(.getArgList("axis", arglist)), 
                      side=2, at=threshold, labels=threshold, las=1, 
                      col=1, lwd=0)
    arglistAbline <- .sarg(.slice.run(.getArgList("axis", arglist)), 
                      h=threshold, lty=arglist$lty.threshold, 
                      lwd=arglist$lwd.threshold, 
                      col=arglist$col.threshold)   
    arglistP <- .sarg(.slice.run(.getArgList("points", arglist)), x=TPR, 
                      y=PREC, type="p", bg=bg) 
    arglistL <- .sarg(.slice.run(.getArgList("points", arglist)), x=TPR, 
                      y=PREC, type="l", col=arglist$col.line, 
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
##Nov 2015.  Last modified 15 Nov 2015.
    "plot",
    signature(x="precREC", y="ANY"),
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
         argSpecial <- list(xlim=xlim, ylim=ylim, lwd.threshold=1, xlab="Recall", ylab="Precision",
                            lty.threshold=3, col.threshold=1,lwd.ticks=1, col.line=1,
                            lwd.line=3, add=add,lwd=3,cex=2)
         argSpecial <- .select.args(argSpecial, names(arglist), complement = T)
         argPlot <- append(arglist, argSpecial)
         argPlot <- .expandListArgs(argPlot, len=l)
         argPlot$add[-1L] <- TRUE
         #print(argPlot)          
         for (i in 1:l)
         {
             PREC <- object@element[[i]][, "PREC"]
             TPR <- object@element[[i]][, "TPR"] 
             argPloti <- lapply(argPlot, .getSub2, id = i)
             argPloti <- .sarg(argPloti, PREC=PREC, TPR=TPR) 
             do.call(".precRECplot", argPloti)

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

