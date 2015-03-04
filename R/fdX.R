setClass("fdX", contains="BenchMetric")

fdX <- function(object=NULL, stratify=NULL, thresholdX=0.05, transformation = "1-x", plot=TRUE, ...)
##Define fdX
##Xiaobei Zhou
##December 2014.  Last modified 16 December 2014. 
        {
            object <- SimResults(object=object, stratify=stratify, ...)
            stratify=object@stratify[[1L]]
            if(!is.null(stratify))
                stop("Currently, 'benchmarkR' only supports 'stratify=NULL'!") 
            .fdX(object, stratify=stratify, thresholdX=thresholdX, plot=plot, ...)
        }


setGeneric(
##Define genetric of .fdX
##Xiaobei Zhou
##December 2014.  Last modified 16 December 2014.
   ".fdX", 
   function(object, stratify, ...)   
   {
       standardGeneric(".fdX")
   }  
)

setMethod(
##Define .fdX for "SimResults"
##Xiaobei Zhou
##December 2014.  Last modified 16 December 2014.
    ".fdX",
    signature(object="SimResults", stratify="NULL"),
    function(object, stratify, thresholdX=0.05, plot=TRUE, ...)
    {
        #arglist <- c(lapply( as.list(environment()), eval ), list(...) )
        l <- ncol(object@pval)
        if(l > 10) stop("the number of method cannot be larger than 10")    
        out <- new("fdX")
        for (i in 1:l)
            out@element[[i]] <- .fdXfun(pval=object@pval[, i], labels=object@labels,
                        padj=object@padj[, i], thresholdX=thresholdX) 
            names(out@element) <- colnames(object@pval)
            if(plot)
                plot(out, ...) 
         out
    } 
)



.fdXfun <- function(pval, padj, labels, thresholdX)
{
    id <- which(labels == 1)	
    x <- 1:length(id)
    o <- order(pval)
    y <- !o[x] %in% id
    y <- cumsum(y)
    if(is.null(thresholdX))
    yX <- xX <- NULL
    else
    {   
        yX <- sum(padj < thresholdX & labels==0)
        xX <- approx(y=x, x=y, xout=yX)$y
    }
    list(number=x, fd=y, numberX=xX, fdX=yX)  
}




setMethod(
##Define plot method
##Xiaobei Zhou
##December 2014.  Last modified 16 December 2014.
    "plot",
    signature(x="fdX", y="ANY"),
    function(x, y, add=FALSE, legend=list(), ...)
    {
         arglist <- list(...)
         object <- x
         l <- length(object@element)
         if(l > 10) stop("the number of method cannot be larger than 10")
         col <- .preCol(arglist, l)
         col <- rep(col, length.out=l)
         xlim <- .preXlim(arglist, object)
         ylim <- .preYlim(arglist, object)
         argSpecial <- list(xlim=xlim, ylim=ylim,
                            xlab="Top rank feature", 
                            ylab = "Number of false discoveries", 
                            colX = NULL, cexX = NULL, pchX = 3, 
                            lwdX = NULL,cex=2.5,lwd=3, add=add)
         #argSpecial <- lapply(argSpecial, .repArgs, len=l)
         argSpecial <- .select.args(argSpecial, names(arglist), complement = T)
         #argSpecial$add[-1L] <- TRUE
         argPlot <- append(arglist, argSpecial)
         argPlot <- .expandListArgs(argPlot, len=l)
         argPlot$add[-1L] <- TRUE  
         for (i in 1:l)
         {
             argPloti <- lapply(argPlot, .getSub2, id = i)
             argPloti <- .sarg(argPloti, object = object@element[[i]], col = col[i]) 
             do.call(".fdXPlot", argPloti)

         }
         nms <- names(object@element) 
         if(!is.null(legend) & !is.null(nms))
         {
             preLegend <- list("bottomright", col=col, legend=nms, lty=argPloti$lty, pch=argPloti$pchX, lwd=argPloti$lwd)
             legend <- .replaceLegend(preLegend, legend)
             do.call("legend", legend)
         } 
    }
)



.fdXPlot <- function(object, ...)
##Define built-in function of fdXPlot
##Xiaobei Zhou
##December 2014.  Last modified 16 December 2014.
{        
    oldPar <- par()
    arglist <- c(lapply( as.list(environment()), eval), list(...) )
    if(!arglist$add)
    {
        arglistPar <- .sarg(.slice.run(.getArgList("plot", arglist)), 
                   x=object$number, y=object$fd, type="l")
        do.call("plot", arglistPar)
    }
    else
    {
        arglistPar <- .sarg(.slice.run(.getArgList("lines", arglist)), 
                   x=object$number, y=object$fd)
        do.call("lines", arglistPar)
    } 
    if(!is.null(object$numberX))
    { 
        colX <- .getArgX("colX", "col", arglist, oldPar)
        pchX <- .getArgX("pchX", "pch", arglist, oldPar)
        lwdX <- .getArgX("lwdX", "lwd", arglist, oldPar)
        if(is.null(arglist[["cexX"]]))
            cexX <- .getArgX("cexX", "cex", arglist, oldPar)+1
        else 
            cexX <- arglist[["cexX"]] 
	 arglistX <- .sarg(.slice.run(.getArgList("points", arglist)), x = object$numberX, 
                                     y = object$fdX, col = colX, 
                                     cex = cexX, pch = pchX, lwd = lwdX)
	do.call("points", arglistX)
    }    
} 




