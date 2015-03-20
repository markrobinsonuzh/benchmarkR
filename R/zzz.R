.getSub <- function(x, id)
##this function for plot.roc
##Xiaobei Zhou
##June 2014.  Last modified 26 June 2014.
{
   if(is.null(x))
      x
   else if(length(x) == 1)
      .subset(x, 1L)
   else
      .subset(x, id)
   
}

.getSub2 <- function(x, id)
##this function for plot.roc
##Xiaobei Zhou
##June 2014.  Last modified 26 June 2014.
{
   if(is.null(x))
      x    
   else if(length(x) == 1)
      .subset2(x, 1L)
   else
      .subset2(x, id)
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
    else if (fname=='lines')
    return(.select.args(arglist,union(names(formals(lines.default)), names(par())))) 
    else if (fname=='axis')
    return(.select.args(arglist,union(names(formals(axis)), names(par()))))
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



.repArgs <- function(x, len, list=TRUE)
{
   if(is.null(x))
       NULL
   else
   {
       if(!is.list(x))
       {
           if(list)
               x <- list(x)
       }
       rep(x, length.out=len)
   }     

}


.expandListArgs <- function(x, len)
{

   s <- c("col","cex","pch","cex","lwd","pch","bg")
   s <- paste0(s, collapse="|")  
   id <- grep(s, names(x))
   l <- length(id) 
   if(l>0 & l < length(x))
   {     
       x1 <- x[id]
       x1 <- lapply(x1, .repArgs, len=len, list=FALSE)
       x2 <- x[-id]
       x2 <- lapply(x2, .repArgs, len=len)
       out <- append(x1, x2)
   } 
   if(l==0) 
   {
       out <- lapply(x, .repArgs, len=len)   
   } 
   if(l==length(x)) 
       out <- lapply(x, .repArgs, len=len, list=FALSE)
   out
}





 

.preCol <- function(x, len)
{
    pre.col <- c("black", "blue", "darkgreen", "gray", "tan3", "red", "purple", "powderblue", "darkkhaki", "yellow")	
    if(is.null(x[["col"]])) 
        pre.col[1:len]
    else 
        x[["col"]]

}




.prePch <- function(x, len)
{
    pre.pch <- c(21:25,21:25)	
    if(is.null(x[["pch"]])) 
        pre.pch[1:len]
    else 
        x[["pch"]]

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





.replaceLegend <- function(x1, x2)
{
   x0 <- c("bottomright", "bottom", "bottomleft","left", "topleft", "top", "topright", "right", "center")
   if(is.null(names(x2)))
       names(x2) <- rep("", length(x2))
   id1 <- x1 %in% x0
   id2 <- x2 %in% x0
   if(any(id2))
       out <- append(x1[!id1], x2[id2], after=(which(id1)-1))
   else
       out <- x1
   id3 <- names(x1) == "legend"
   id4 <- names(x2) == "" & !id2
   if(any(id4))
       {
        out <- append(out[!id3], x2[id4], after=(which(id3)-1))
        names(out)[which(id3)] <- "legend"
       }
   id5 <- names(out) %in% names(x2) & !names(out) == ""
   id6 <- !names(x2) == ""
   out <- append(out[!id5], x2[id6]) 
   out           
}



.preXlim <- function(x, object)
{
    if(is.null(x[["xlim"]]))
    {
        if(class(object) == "rocX")    
        {
             x <- unlist(lapply(object@element, 
                             function(x) lapply(x[2], 
                                 .subset2,"fprX")))
             if(is.null(x)) x <- NA 
             x1 <- pmax(round(1.5*max(x), 1), 0.4)
             x2 <- pmin(1, x1, na.rm=TRUE) 
             c(0,x2)
        }
        else if(class(object) == "fdX")    
        {
             x <- unlist(lapply(object@element,  
                                 .subset2,"numberX"))
             if(is.null(x)) x <- NA
             xmax <- max(unlist(lapply(object@element,  
                                 .subset2,"number"))) 
             xx <- pmin(xmax, round(1.5*max(x)), na.rm=TRUE) 
             c(0,xx)
        }
        else if(class(object) == "powerFDR")    
        {
             x <- unlist(lapply(object@element,  
                                 function(x) x[,"FDR"]))
             if(is.null(x)) x <- NA
             x1 <- pmax(round(1.5*max(x), 1), 0.4)
             x2 <- pmin(1, x1, na.rm=TRUE) 
             c(0,x2)
        }
    }
    else
        x[["xlim"]]
}




.preYlim <- function(x, object)
{
    if(is.null(x[["ylim"]]))
    {
        if(class(object) == "rocX")    
        {
             y <- unlist(lapply(object@element, 
                             function(x) lapply(x[2], 
                                 .subset2,"tprX")))
             if(is.null(y)) y <- NA
             y1 <- pmax(round(1.5*max(y), 1), 0.4)
             y2 <- pmin(1, y1, na.rm=TRUE) 
             c(0,y2)
        }
        else if(class(object) == "fdX")    
        {
             y <- unlist(lapply(object@element,  
                                 .subset2,"fdX"))
             if(is.null(y)) y <- NA
             ymax <- max(unlist(lapply(object@element,  
                                 .subset2,"fd"))) 
             yy <- pmin(ymax, round(1.5*max(y)), na.rm=TRUE) 
             c(0,yy)
        }
        else if(class(object) == "powerFDR")    
        {
             y <- unlist(lapply(object@element,  
                                 function(x) x[,"TPR"]))
             if(is.null(y)) y <- NA
             y1 <- pmax(round(1.5*max(y), 1), 0.4)
             y2 <- pmin(1, y1, na.rm=TRUE) 
             c(0,y2)
        }
    }
    else
        x[["ylim"]]
}




.printHead <- function (x)
##this function copys from limma printHead
##Define built-in function of get arguments 
##Xiaobei Zhou
##June 2014.  Last modified 26 June 2014. 
{
    if (is.atomic(x)) {
        d <- dim(x)
        if (length(d) < 2) 
            which <- "OneD"
        if (length(d) == 2) 
            which <- "TwoD"
        if (length(d) > 2) 
            which <- "Array"
    }
    else {
        if (inherits(x, "data.frame")) {
            d <- dim(x)
            which <- "TwoD"
        }
        else {
            if (is.call(x)) 
                which <- "Call"
            else {
                if (is.recursive(x)) 
                  which <- "Recursive"
                else which <- "Other"
            }
        }
    }
    switch(which, OneD = {
        n <- length(x)
        if (n > 20) {
            print(x[1:5])
            cat(n - 5, "more elements ...\n")
        } else print(x)
    }, TwoD = {
        n <- d[1]
        if (n > 10) {
            print(x[1:5, ])
            cat(n - 5, "more rows ...\n")
        } else print(x)
    }, Array = {
        n <- d[1]
        if (n > 10) {
            dn <- dimnames(x)
            dim(x) <- c(d[1], prod(d[-1]))
            x <- x[1:5, ]
            dim(x) <- c(5, d[-1])
            if (!is.null(dn[[1]])) dn[[1]] <- dn[[1]][1:5]
            dimnames(x) <- dn
            print(x)
            cat(n - 5, "more rows ...\n")
        } else print(x)
    }, Recursive = {
        n <- length(x)
        if (n) {
            i <- names(x)
            if (is.null(i)) i <- seq_len(n)
            for (what in i) {
                y <- x[[what]]
                cat("$", what, "\n", sep = "")
                Recall(y)
                cat("\n")
            }
        }
    }, Call = , Other = print(x))
}
