setClass("rocX", "list")
##Define rocX
##Xiaobei Zhou
##June 2014.  Last modified 26 June 2014.


setMethod("show","rocX",
function(object) 
##Define show of rocX
##Xiaobei Zhou
##June 2014.  Last modified 26 June 2014.
{
    cat("An object of class \"",class(object),"\"\n",sep="")
    for( i in 1:length(object))
        {
            cat("..................", "\n\n")
            if(!is.null(nm <- names(object[i])))
            cat(nm, ":", "")
            else cat("[[", i, "]] : ", sep = "")     
            str(object[[i]])
        }
})


setGeneric(
##Define genetric of rocX
##Xiaobei Zhou
##June 2014.  Last modified 26 June 2014.
   "rocX", 
   function(object, ...)   
   {
       standardGeneric("rocX")
   }  
)






setMethod(
##Define rocX for "SimResults"
##Xiaobei Zhou
##June 2014.  Last modified 26 June 2014.
    "rocX",
    signature(object = "SimResults"),
    function(object, thresholdX=0.05, plot=TRUE, ...)
    {
        arglist <- c(lapply( as.list(environment()), eval ), list(...) )
        l <- ncol(object@pval)
        if(l > 10) stop("the number of method cannot be larger than 10")
        ind <- object@stratifyBy
        if(length(ind) > 0)
        {
            if(all(ind == 0|1))
            {   
                id <- which(ind == 1)
                object <- object[id,] 
            }
        }     
        out <- new("rocX")
        for (i in 1:l)
            out[[i]] <- .rocX(pval=object@pval[, i], labels=object@labels,
                        padj=object@padj[, i], thresholdX=thresholdX) 
        #names(out) <- colnames(object@pval)
        if(plot)
            plot(out, ...) 
        out
    }
)



setMethod(
##Define rocX for "SimResults"
##Xiaobei Zhou
##June 2014.  Last modified 1 July 2014.
    "plot",
    signature(x="rocX", y="missing"),
    function(x, y, ...)
    {
         arglist <- list(...)
         object <- x
         l <- length(object)
         if(l > 10) stop("the number of method cannot be larger than 10")
         pre.col <- c("black", "blue", "purple", "gray", "tan3", "red", "green", "powderblue", "chartreuse4", "yellow")	
         if(is.null(arglist[["col"]])) col <- pre.col[1:l]
         else col <- arglist[["col"]]
         argSpecial <- list(fprCutoff = 0.4, colX = NULL, cexX = NULL, pchX = 3, lwdX = NULL, add = TRUE)
         argSpecial <- lapply(argSpecial, rep, l)
         argSpecial <- append(.select.args(arglist, names(argSpecial), complement = F), .select.args(argSpecial, names(arglist), complement = T))
         argSpecial$add[1] <- FALSE
         argPlot <- append(list(...), argSpecial) 
         for (i in 1:l)
         {
             argPloti <- lapply(argPlot, .getSub, id = i)
             argPloti <- .sarg(argPloti, object = object[[i]], col = col[i])  
             do.call(".rocXPlot", argPloti)
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







.rocX <- function(pval, labels, thresholdX = NULL, padj = NULL)
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



