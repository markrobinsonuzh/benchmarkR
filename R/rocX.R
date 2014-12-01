setClass("rocX", contains="BenchMetric")

setClass("rocXList", contains="BenchMetricList")


rocX <- function(object, thresholdX=0.05, transformation = "1-x", plot=TRUE, ...)
##Define rocX
##Xiaobei Zhou
##June 2014.  Last modified 7 July 2014. 
        {
            stratify=object@stratify[[1L]]
            .rocX(object, stratify=stratify, thresholdX=thresholdX, transformation = transformation, plot=plot, ...)
        }

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
         out
    } 
)

setMethod(
##Define .rocX for "SimResults"
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
        out 
    } 
)



setMethod(
##Define .rocX for "SimResults"
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
        out 
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
         argSpecial <- list(xlim = c(0,0.4), xlab="fpr", ylab="tpr", colX = NULL, cexX = NULL, pchX = 3, lwdX = NULL, add=add)
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
             do.call(".rocXPlot", argPloti)

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


#plot.rocX <- function(object, ...)
#Define plot of rocX
##Xiaobei Zhou
##June 2014.  Last modified 26 June 2014.
#{
#     arglist <- c(lapply( as.list(environment()), eval ), list(...) )
#     l <- length(object)
#     if(l > 10) stop("the number of method cannot be larger than 10")
#     pre.col <- c("black", "blue", "purple", "gray", "tan3", "red", "green", "powderblue", "chartreuse4", "yellow")	
#     if(is.null(arglist[["col"]])) col <- pre.col[1:l]
#     else col <- arglist[["col"]]
#     argSpecial <- list(fprCutoff = 0.4, colX = NULL, cexX = NULL, pchX = 3, lwdX = NULL, add = TRUE)
#     argSpecial <- lapply(argSpecial, rep, l)
#     argSpecial <- append(.select.args(arglist, names(argSpecial), complement = F), .select.args(argSpecial, names(arglist), complement = T))
#     argSpecial$add[1] <- FALSE
#     argPlot <- append(list(...), argSpecial) 
#     for (i in 1:l)
#     {
#         argPloti <- lapply(argPlot, .getSub, id = i)
#         argPloti <- .sarg(argPloti, object = object[[i]], col = col[i])  
#         do.call(".rocXPlot", argPloti)
#     }
#}



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
	thresholdX <- quantile(score, probs = mean(scoreX <= thresholdX), names = FALSE)
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





