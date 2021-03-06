setClass("fdX", contains=".BenchMetric")


setGeneric(
##Define genetric of fdX
##Xiaobei Zhou
##March 2014.  Last modified 19 March 2015.
   "fdX", 
   function(object, pval, ...)   
   {
       standardGeneric("fdX")
   }  
)



setMethod(
##Define fdX for "SimResults"
##Xiaobei Zhou
##March 2015.  Last modified 19 March 2015.
    "fdX",
    signature(object="SimResults", pval="missing"),
    function(object, thresholdX=0.05, transformation = "1-x", plot=TRUE, ...)
    {
        stratify <- object@stratify[[1]]
        if(!is.null(stratify))
            stop("Currently, 'fdX' only supports 'stratify=NULL'!")
        idNA <- is.na(object@labels)
        if(any(idNA))
        {
            message("remove NA values from labels for fdX")
            object <- object[!idNA,]
        }   
        .fdX(object, stratify=stratify, thresholdX=thresholdX, 
                 transformation = transformation, plot=plot, ...)

    }
)


setMethod(
##Define fdX for "pval"
##Xiaobei Zhou
##March 2015.  Last modified 19 March 2015.
    "fdX",
    signature(object="missing", pval="ANY"),
    function(pval, padj=NULL, labels, thresholdX=0.05, transformation = "1-x", plot=TRUE, ...)
    {
        object <- SimResults(pval=pval, padj=padj, labels=labels, stratify=NULL, ...)
        fdX(object, thresholdX=thresholdX, 
                 transformation = transformation, plot=plot, ...)
    }
)



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
    function(object, stratify, thresholdX=0.05, plot=TRUE, transformation = "1-x", ...)
    {
        #arglist <- c(lapply( as.list(environment()), eval ), list(...) )
        l <- ncol(object@pval)
        if(l > 10) stop("the number of method cannot be larger than 10")    
        out <- new("fdX")
        for (i in 1:l)
            out@element[[i]] <- .fdXfun(pval=object@pval[, i], labels=object@labels,
                        padj=object@padj[, i], thresholdX=thresholdX, transformation=transformation) 
            names(out@element) <- colnames(object@pval)
            if(plot)
                plot(out, ...) 
         invisible(out)
    } 
)



.fdXfun <- function(pval, padj, labels, thresholdX,transformation = "1-x")
{
    tf <- function(x) eval(parse(text=transformation))
    pval <- pval+(1e-20)
    score <- tf(pval)
    id <- which(labels == 1)	
    x <- 1:length(id)
    o <- order(score,decreasing=TRUE)
    y <- !o[x] %in% id
    y <- cumsum(y)
    if(!is.null(padj) & !is.null(thresholdX))
    {
        thresholdX <- thresholdX+(1e-20)
        thresholdX <- tf(thresholdX)
        padj <- padj+(1e-20)
        scoreX <- tf(padj)      
        yX <- sum(scoreX > thresholdX & labels==0)
        xX <- approx(y=x, x=y, xout=yX)$y     	
    	
    	
    }
 
    else
    {   
         yX <- xX <- NULL 
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
                            xlab="Top ranked features", 
                            ylab = "Number of false discoveries", 
                            colX = NULL, cexX = NULL, pchX = 3, 
                            lwdX = NULL,cex=2,lwd=3, lty=1,add=add)
         #argSpecial <- lapply(argSpecial, .repArgs, len=l)
         argSpecial <- .select.args(argSpecial, names(arglist), complement = T)
         #argSpecial$add[-1L] <- TRUE
         argPlot <- append(arglist, argSpecial)
         argPlot <- .expandListArgs(argPlot, len=l)
         argPlot$add[-1L] <- TRUE
         argPloti <- list()
         for (i in 1:l)
         {
             argPloti[[i]] <- lapply(argPlot, .getSub2, id = i)
             argPloti[[i]] <- .sarg(argPloti[[i]], object = object@element[[i]], col = col[i])
             do.call(".fdXPlot", argPloti[[i]])

         }
         nms <- names(object@element) 
         if(!is.null(legend) & !is.null(nms))
         {
             pchX <- unlist(lapply(argPloti, .subset2,"pchX"))
             lwd <- unlist(lapply(argPloti, .subset2,"lwd"))
             lty <- unlist(lapply(argPloti, .subset2,"lty"))
             cex <- 0.5*argPlot$cex[[1]]  
             preLegend <- list("bottomright", col=col, legend=nms, lty=lty, pch=pchX, lwd=lwd,
                                           text.font=2,cex=cex)
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



setMethod(
##Define fdX for "SimResultsList"
##Xiaobei Zhou
##November 2015.  Last modified 13 November 2015.
    "fdX",
    signature(object="SimResultsList", pval="missing"),
    function(object, thresholdX=0.05, transformation="1-x", plot=TRUE, ...)
    {
        pval_a <- do.call("rbind", (lapply(object, function(x) x@pval)))
        padj_a <- do.call("rbind", (lapply(object, function(x) x@padj)))
        labels_a <- unlist(lapply(object, function(x) x@labels))
        re_a <- SimResults(pval=pval_a,padj=padj_a, labels=labels_a)
        n <- length(object)  
            .fdXAve(re_a, thresholdX=thresholdX,
                 transformation="1-x", n=n, plot=plot, ...)
            
    }

)



.fdXAve <- function(object,thresholdX=0.05, transformation="1-x", n, plot=TRUE, ...)
{
   tmp <- fdX(object=object, thresholdX=thresholdX, 
          transformation=transformation, plot=FALSE)
   tmp <- lapply(tmp@element, lapply,function(x) x/n)
   out <- new("fdX")
   out@element <- tmp
   if(plot) 
      plot(out, ...)
   invisible(out)  
}




