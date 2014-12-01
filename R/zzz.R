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
    pre.col <- c("black", "blue", "purple", "gray", "tan3", "red", "green", "powderblue", "chartreuse4", "yellow")	
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
