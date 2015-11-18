setClass("rocX", contains=".BenchMetric")

setClass("rocXList", contains=".BenchMetricList")



setGeneric(
##Define genetric of rocX
##Xiaobei Zhou
##March 2014.  Last modified 19 March 2015.
   "rocX", 
   function(object, pval, ...)   
   {
       standardGeneric("rocX")
   }  
)



setMethod(
##Define rocX for "SimResults"
##Xiaobei Zhou
##March 2015.  Last modified 19 March 2015.
    "rocX",
    signature(object="SimResults", pval="missing"),
    function(object, thresholdX=0.05, transformation = "1-x", plot=TRUE, ...)
    {
        stratify <- object@stratify[[1]]
        idNA <- is.na(object@labels)
        if(any(idNA))
        {
            message("remove NA values from labels for rocX")
            object <- object[!idNA,]
        }  
            .rocX(object, stratify=stratify, thresholdX=thresholdX, 
                 transformation = transformation, plot=plot, ...)

    }
)


setMethod(
##Define rocX for "pval"
##Xiaobei Zhou
##March 2015.  Last modified 19 March 2015.
    "rocX",
    signature(object="missing", pval="ANY"),
    function(pval, padj=NULL, labels, stratify=NULL, thresholdX=0.05, transformation = "1-x", plot=TRUE, ...)
    {
        object <- SimResults(pval=pval, padj=padj, labels=labels, stratify=stratify, ...)
        rocX(object, thresholdX=thresholdX, 
                 transformation = transformation, plot=plot, ...)
    }
)





rocXList <- function(...)
{
    x <- list(...)
    if(length(x) == 1L & !class(x) == "rocX")
        new("rocXList", x[[1L]])
    else 
        new("rocXList", x)
}

setGeneric(
##Define genetric of .rocX
##Xiaobei Zhou
##June 2014.  Last modified 26 June 2014.
   ".rocX", 
   function(object, stratify, ...)   
   {
       standardGeneric(".rocX")
   }  
)



setMethod(
##Define .rocX for "SimResults"
##Xiaobei Zhou
##June 2014.  Last modified 26 June 2014.
    ".rocX",
    signature(object="SimResults", stratify="NULL"),
    function(object, stratify, thresholdX=0.05, transformation = "1-x", plot=TRUE, ...)
    {
        #arglist <- c(lapply( as.list(environment()), eval ), list(...) )
        l <- ncol(object@pval)
        if(l > 10) stop("the number of method cannot be larger than 10")    
        out <- new("rocX")
        for (i in 1:l)
            out@element[[i]] <- .rocXfun(pval=object@pval[, i], labels=object@labels,
                        padj=object@padj[, i], thresholdX=thresholdX, transformation = transformation) 
            names(out@element) <- colnames(object@pval)
            if(plot)
                plot(out, ...) 
         invisible(out)
    } 
)

setMethod(
##Define .rocX for "factor"
##Xiaobei Zhou
##June 2014.  Last modified 26 June 2014.
    ".rocX",
    signature(object="SimResults", stratify="factor"),
    function(object, stratify, thresholdX=0.05, transformation = "1-x", plot=TRUE, add=FALSE, addFun=NULL, addFunLocation=NULL, legend=list(), ...)
    {
        l <- levels(stratify)
        ll <- length(l)
        out <- list()
        j <- 1 
        for(i in l)
        {
            id <- stratify == i 
            objecti <- object[id,]
            object1 <- initialize(objecti, stratify=NULL)
            out[[j]] <- rocX(objecti, plot=FALSE, thresholdX=thresholdX, transformation = transformation)
            j <- j+1
        }
        names(out) <- paste0("Level:",l)
        out <- rocXList(out)
        if(plot)
            plot(out, add=add, addFun=addFun, addFunLocation=addFunLocation, legend=legend, ...) 
        invisible(out) 
    } 
)



setMethod(
##Define .rocX for "numeric"
##Xiaobei Zhou
##June 2014.  Last modified 26 June 2014.
    ".rocX",
    signature(object="SimResults", stratify="numeric"),
    function(object, stratify, numGroups=4, thresholdX=0.05, transformation = "1-x", plot=TRUE, add=FALSE, addFun=NULL, addFunLocation=NULL, ...)
    {
        stratify <- cut(stratify, numGroups)
        object1 <- initialize(object, stratify=as.data.frame(stratify))
        out <- rocX(object1, plot=FALSE, thresholdX=thresholdX, transformation = transformation)
        if(plot)
            plot(out, add=add, addFun=addFun, addFunLocation=addFunLocation, ...) 
        invisible(out) 
    } 
)


 

setMethod(
##Define plot method
##Xiaobei Zhou
##June 2014.  Last modified 1 July 2014.
    "plot",
    signature(x="rocX", y="ANY"),
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
         argSpecial <- list(xlim = xlim, ylim = ylim, xlab="FPR", 
                           ylab="TPR", colX = NULL, cexX = NULL, 
                           pchX = 3, lwdX = NULL, lwd=3, cex=2, lty=1, add=add)
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
             do.call(".rocXPlot", argPloti[[i]])

         }
         
         nms <- names(object@element) 
         if(!is.null(legend) & !is.null(nms))
         
         {
             pchX <- unlist(lapply(argPloti, .subset2,"pchX"))
             lwd <- unlist(lapply(argPloti, .subset2,"lwd"))
             lty <- unlist(lapply(argPloti, .subset2,"lty"))
             cex <- 0.5*argPlot$cex[[1]]  
             preLegend <- list("bottomright", col=col, legend=nms, lty=lty, 
                                         pch=pchX, lwd=lwd, text.font=2,cex=cex)
             legend <- .replaceLegend(preLegend, legend)
             do.call("legend", legend)
         } 
    }
)


        
setMethod(
##Define plot method
##Xiaobei Zhou
##June 2014.  Last modified 1 July 2014.
    "plot",
    signature(x="rocXList", y="ANY"),
    function(x, y, add=FALSE, addFun=NULL, addFunLocation=NULL, legend=list(), ...)
    {    
         object <- x
         l <- length(object)
         add <- rep(add, l)
         arglist <- list(...)
         arglist <- lapply(arglist, .repArgs, len=l)
         #arglist <- .expandListArgs(arglist, len=l)
         if(is.null(legend$location) & !is.null(legend))
             legend$location <- rep(1, l) 
         for(i in seq(1:l))
         {
              argPloti <- lapply(arglist, .getSub2, id = i)
              maini <- argPloti$main
              if(is.null(maini))
                  maini <- names(object)[i]
              objecti <- object[[i]]
              if(is.null(legend))
                  legendi <- NULL
              else
              {
                  if(legend$location[i] == 0)
                      legendi <- NULL  
                  else
                  {    
                      legendi <- legend
                      legendi$location <- NULL
                  } 
              }
              legendList <- list(legend=legendi)                     
              argPloti <- .sarg(argPloti, x = objecti, add=add[i], main=maini)
              argPloti <- append(argPloti, legendList)
              do.call("plot", argPloti)
              if(!is.null(addFun))
                 .evalFunLocation(addFun, addFunLocation, l, i) 
         }
   }
)




.rocXfun <- function(pval, labels, thresholdX = NULL, padj = NULL, transformation = "1-x")
##Define built-in function of rocX
##Xiaobei Zhou
##June 2014.  Last modified 26 June 2014.
{   
    tf <- function(x) eval(parse(text=transformation))
    pval <- pval+(1e-20)
    score <- tf(pval)
    pred <- prediction(score, labels)
    perf <- performance(pred, "tpr", "fpr")
    #calculate the threshold of tpr and for
    if(!is.null(padj) & !is.null(thresholdX))
    {
        thresholdX <- thresholdX+(1e-20)
        thresholdX <- tf(thresholdX)
        padj <- padj+(1e-20)
        scoreX <- tf(padj)
	thresholdX <- quantile(score, probs = mean(scoreX <= thresholdX, na.rm=TRUE),
                      names = FALSE, na.rm=TRUE)
        fprX <- approx(y = perf@x.values[[1]], x = perf@alpha.values[[1]], xout = thresholdX)$y
	tprX <- approx(y = perf@y.values[[1]], x = perf@x.values[[1]], xout = fprX)$y
	threshold = c(fprX = fprX, tprX = tprX)
    }
    else threshold <- NULL
    out <- list(performance = perf, threshold = threshold)
}

.rocXPlot <- function(object, ...)
##Define built-in function of rocXPlot
##Xiaobei Zhou
##June 2014.  Last modified 26 June 2014.
{        
    oldPar <- par()
    arglist <- c(lapply( as.list(environment()), eval), list(...) )
    #xlim <- c(0, arglist$fprCutoff)
    if(!arglist$add)
    {
        arglistPar <- .sarg(.slice.run(.getArgList("plot", arglist)), 
                   x=object$performance@x.values[[1]], y=object$performance@y.values[[1]], type="l")
        do.call("plot", arglistPar)
    }
    else
    {
        arglistPar <- .sarg(.slice.run(.getArgList("lines", arglist)), 
                   x=object$performance@x.values[[1]], y=object$performance@y.values[[1]])
        do.call("lines", arglistPar)
    } 
    if(!is.null(object$threshold))
    { 
        colX <- .getArgX("colX", "col", arglist, oldPar)
        pchX <- .getArgX("pchX", "pch", arglist, oldPar)
        lwdX <- .getArgX("lwdX", "lwd", arglist, oldPar)
        if(is.null(arglist[["cexX"]]))
            cexX <- .getArgX("cexX", "cex", arglist, oldPar)+1
        else 
            cexX <- arglist[["cexX"]] 
	 arglistX <- .sarg(.slice.run(.getArgList("points", arglist)), x = object$threshold["fprX"], 
                                     y = object$threshold["tprX"], col = colX, 
                                     cex = cexX, pch = pchX, lwd = lwdX)
	do.call("points", arglistX)
    }    
} 





setMethod(
##Define rocX for "SimResultsList"
##Xiaobei Zhou
##November 2015.  Last modified 12 November 2015.
    "rocX",
    signature(object="SimResultsList", pval="missing"),
    function(object, thresholdX=0.05, transformation="1-x", plot=TRUE,
             typeLine="b", percentile=c(0,1), alpha=0.3, ...)
    {
        pval_a <- do.call("rbind", (lapply(object, function(x) x@pval)))
        padj_a <- do.call("rbind", (lapply(object, function(x) x@padj)))
        labels_a <- unlist(lapply(object, function(x) x@labels))
        re_a <- SimResults(pval=pval_a,padj=padj_a, labels=labels_a)  
        typeLine <- match.arg(typeLine, c("average","shape","b"))     
        if(typeLine=="average")
            rocX(re_a, thresholdX=thresholdX,
                 transformation="1-x", plot=plot, ...)
        else if (typeLine=="shape")
        {
            .rocXShape(object, thresholdX=thresholdX, 
                transformation=transformation, alpha=alpha,
                percentile=percentile, ...)
                 
        }
        else
        {
            .rocXShape(object, thresholdX=thresholdX, 
                transformation=transformation, alpha=alpha,
                percentile=percentile, ...)
             rocX(re_a, thresholdX=thresholdX,
                 transformation="1-x", add=TRUE, plot=plot, ...)
        }
    }

)





.rocShapePlot <- function(x1,y1, add=FALSE, col="black",  
            percentile=c(0,1), ...)
##Define built-in function of rocX for SimResultsList
##Xiaobei Zhou
##November 2015.  Last modified 12 November 2015.
{     
    y11 <- do.call("cbind",y1)
    #ymax <- do.call(pmax,y1)
    #ymin <- do.call(pmin,y1)
    ymax <- apply(y11,1, quantile, percentile[2])
    ymin <- apply(y11,1, quantile, percentile[1])
    if(!add)
        plot(x1[[1]],y1[[1]], type="n", ...)
    polygon(c(x1[[1]],rev(x1[[1]])), 
            c(ymax,rev(ymin)),    
            col=col, border=col, ...)
}
 

.rocXShape <- function(object, thresholdX=0.05, transformation="1-x",alpha=0.3,
               percentile=c(0,1), legend=list(), ...)
##Define built-in function of rocX for SimResultsList
##Xiaobei Zhou
##November 2015.  Last modified 12 November 2015.
{ 
    out <- lapply(object, rocX, plot=FALSE,thresholdX=thresholdX, 
                      transformation=transformation)
    nms <- names(out[[1L]]@element) 
    xList <- yList <- list()
    for (j in 1:3)
    {
        xList[[j]] <- lapply(out, function(x) 
                     x@element[[j]]$performance@x.values[[1]])
        yList[[j]] <- lapply(out, function(x) 
                     x@element[[j]]$performance@y.values[[1]])
    }
    approxL <- function(x,y)
    {
        yy <- xx <- list()
        for (i in seq(x))
        {
            yy[[i]] <- approx(y=y[[i]],x=x[[i]],xout=x[[1]])$y
            xx[[i]] <- x[[1]]
        }
        list(x=xx,y=yy)    
    }
    out <- mapply(function(u,v)approxL(u,v), u=xList, v=yList)
    xList <- out[1,]
    yList <- out[2,]
    l <- length(xList)
    arglist <- list(...)
    col <- .preCol(arglist, l)
    #col <- rep(col, length.out=l)
    col <- alpha(col,alpha)
    xlim <- .preXlim(arglist, object)
    ylim <- .preYlim(arglist, object)
    argSpecial <- list(xlim = xlim, ylim = ylim, xlab="FPR", 
                       ylab="TPR", lwd=1, cex=2, lty=1, add=FALSE)
    argSpecial <- .select.args(argSpecial, names(arglist),
                     complement = T)
    argPlot <- append(arglist, argSpecial)
    argPlot <- .expandListArgs(argPlot, len=l)
    argPlot$add[-1L] <- TRUE
    argPloti <- list()
    for (i in 1:l)
    {
        argPloti[[i]] <- lapply(argPlot, .getSub2, id=i)
        argPloti[[i]] <- .sarg(argPloti[[i]], 
              x1=xList[[i]], y1=yList[[i]], col=col[i],
              percentile=percentile)
        do.call(".rocShapePlot", argPloti[[i]])

     }
     if(!is.null(legend) & !is.null(nms))
     {
         cex <- 0.5*argPlot$cex[[1]]
         lwd <- 3*unlist(lapply(argPloti, .subset2,"lwd"))
         pch <- unlist(lapply(argPloti, .subset2,"pch")) 
         lty <- unlist(lapply(argPloti, .subset2,"lty"))
         preLegend <- list("bottomright", col=col, legend=nms, lty=lty, 
                           pch=pch, lwd=lwd, text.font=2, cex=cex)
         legend <- .replaceLegend(preLegend, legend)
         do.call("legend", legend)
     }
} 



