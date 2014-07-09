setClass("rocX", slots=c(rocXelement="list"))
setClass("rocXList", contains="list")
##Define rocX
##Xiaobei Zhou
##June 2014.  Last modified 26 June 2014.


setMethod("show","rocX",
function(object) 
##Define show of rocX
##Xiaobei Zhou
##June 2014.  Last modified 26 June 2014.
{
    cat("An object of class \"",class(object),"\n",sep="")
    cat("..................", "\n")
    cat(slotNames(object), ":", "\n")
    str(object@rocXelement, max.level = 1)
})



rocX <- function(object, thresholdX=0.05, plot=TRUE, ...)
##Define rocX
##Xiaobei Zhou
##June 2014.  Last modified 7 July 2014. 
        {
            stratify=object@stratify[[1L]]
            .rocX(object, stratify=stratify, thresholdX=thresholdX, plot=plot, ...)
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
    function(object, stratify, thresholdX=0.05, plot=TRUE, ...)
    {
        arglist <- c(lapply( as.list(environment()), eval ), list(...) )
        l <- ncol(object@pval)
        if(l > 10) stop("the number of method cannot be larger than 10")    
        out <- new("rocX")
        for (i in 1:l)
            out@rocXelement[[i]] <- .rocXfun(pval=object@pval[, i], labels=object@labels,
                        padj=object@padj[, i], thresholdX=thresholdX) 
            names(out@rocXelement) <- colnames(object@pval)
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
    function(object, stratify, thresholdX=0.05, plot=TRUE, add=FALSE, addFun=NULL, addFunLocation=NULL, ...)
    {
        l <- levels(stratify)
        ll <- length(l)
        out <- new("rocXList")
        j <- 1 
        for(i in l)
        {
            id <- stratify == i 
            objecti <- object[id,]
            object1 <- initialize(objecti, stratify=NULL)
            out[[j]] <- rocX(objecti, plot=FALSE)
            j <- j+1
        }
        if(plot)
            plot(out, thresholdX=thresholdX, add=add, addFun=addFun, addFunLocation=addFunLocation, ...) 
        out 
    } 
)



setMethod(
##Define .rocX for "SimResults"
##Xiaobei Zhou
##June 2014.  Last modified 26 June 2014.
    ".rocX",
    signature(object="SimResults", stratify="numeric"),
    function(object, stratify, numGroups=4, thresholdX=0.05, plot=TRUE, add=FALSE, addFun=NULL, addFunLocation=NULL, ...)
    {
        stratify <- cut(stratify, numGroups)
        object1 <- initialize(object, stratify=as.data.frame(stratify))
        out <- rocX(object1, plot=FALSE)
        if(plot)
            plot(out, thresholdX=thresholdX, add=add, addFun=addFun, addFunLocation=addFunLocation, ...) 
        out 
    } 
)


 

setMethod(
##Define plot method
##Xiaobei Zhou
##June 2014.  Last modified 1 July 2014.
    "plot",
    signature(x="rocX", y="ANY"),
    function(x, y, add=FALSE, ...)
    {
         arglist <- list(...)
         object <- x
         l <- length(object@rocXelement)
         if(l > 10) stop("the number of method cannot be larger than 10")
         pre.col <- c("black", "blue", "purple", "gray", "tan3", "red", "green", "powderblue", "chartreuse4", "yellow")	
         if(is.null(arglist[["col"]])) col <- pre.col[1:l]
         else col <- arglist[["col"]]
         argSpecial <- list(fprCutoff = 0.4, colX = NULL, cexX = NULL, pchX = 3, lwdX = NULL, add=add)
         argSpecial <- lapply(argSpecial, rep, l)
         argSpecial <- append(.select.args(arglist, names(argSpecial), complement = F), .select.args(argSpecial, names(arglist), complement = T))
         argSpecial$add[-1L] <- TRUE
         argPlot <- append(list(...), argSpecial) 
         for (i in 1:l)
         {
             argPloti <- lapply(argPlot, .getSub, id = i)
             argPloti <- .sarg(argPloti, object = object@rocXelement[[i]], col = col[i])  
             do.call(".rocXPlot", argPloti)
         } 
    }
)


        
setMethod(
##Define plot method
##Xiaobei Zhou
##June 2014.  Last modified 1 July 2014.
    "plot",
    signature(x="rocXList", y="ANY"),
    function(x, y, add=FALSE, addFun=NULL, addFunLocation=NULL, ...)
    {    
         object <- x
         l <- length(object)
         add <- rep(add, l)
         arglist <- list(...)
         arglist <- lapply(arglist, rep, l)
         for(i in seq(1:l))
         {
              argPloti <- lapply(arglist, .getSub, id = i)
              objecti <- object[[i]]
              argPloti <- .sarg(argPloti, x = objecti, add=add[i]) 
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







.rocXfun <- function(pval, labels, thresholdX = NULL, padj = NULL)
##Define built-in function of rocX
##Xiaobei Zhou
##June 2014.  Last modified 26 June 2014.
{   
    score <- 1 - pval
    thresholdX <- 1 - thresholdX
    pred <- prediction(score, labels)
    perf <- performance(pred, "tpr", "fpr")
    #calculate the threshold of tpr and for
    if(!is.null(padj))
    {
        scoreX <- 1 - padj
	thresholdX <- quantile(score, probs = mean(scoreX <= thresholdX), names = FALSE)
    }
    if(!is.null(thresholdX))
    {
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
    arglist <- c(lapply( as.list(environment()), eval ), list(...) )
    arglistPar <- .sarg(.slice.run(.getArgList("plot", arglist)), 
                        xlim = c(0, arglist$fprCutoff), x=object$performance, add = arglist$add)
    do.call("plot", arglistPar)
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


.getSub <- function(x, id)
##this function for plot.roc
##Xiaobei Zhou
##June 2014.  Last modified 26 June 2014.
{
   if(length(x) == 1)
      x
   else
      x[id]
}

.getArgList <- function(fname, arglist) 
##this function borrows some ideas from ROCR
##Define built-in function of get arguments 
##Xiaobei Zhou
##June 2014.  Last modified 26 June 2014.
{
    if (fname=='plot')
    return(.select.args(arglist, union(names(formals(plot.default)), names(par()))))
    else if (fname=='points')
    return(.select.args(arglist,union(names(formals(points.default)), names(par()))))
    else return( .select.args(arglist, names(formals(fname))))
}



.getArgX <- function(argX, arg, from, elseFrom=par())

{
    argXF <- from[[argX]]
    argF <- from[[arg]]      
    if(is.null(argXF) & is.null(argF))
        out <- elseFrom[[arg]] 
    else if(is.null(argXF) & !is.null(argF)) 
        out <- argF
    else
        out <- argXF
    out             
}

 
.evalFunLocation <- function(addFun, addFunLocation=NULL, len, iter)
{
    if(is.null(addFunLocation))
         addFunLocation <- rep(1, len)
    if(!is.list(addFunLocation))
         addFunLocation <- list(addFunLocation)
    if(!is.list(addFun))       
         addFun <- list(addFun)
    ll <- lapply(addFunLocation, function(y) y[iter])
    ids <- which(ll == 1) 
    addFun <- addFun[ids]
    lapply(addFun, function(x) eval(parse(text=x)))

}