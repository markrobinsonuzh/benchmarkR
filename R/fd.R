fd<- function(object, ...) 
##Define fd for "SimResults"
##Xiaobei Zhou
##June 2014.  Last modified 8 July 2014.
{
     y <- apply(object@padj, 2,.fd, labels=object@labels)
     id <- which(object@labels == 1)	
     x <- 1:length(id)
     arglist <- list(...)
     l <- ncol(object@padj)
     if(l > 10) 
          stop("the number of method cannot be larger than 10")
     pre.col <- c("black", "blue", "purple", "gray", "tan3", "red", "green", "powderblue", "chartreuse4", "yellow")	 
     col <- pre.col[1:l] 
     argDefault <- list(col = col, xlab = "Number of genes selected", ylab = "Number of false discoveries", type = "l", lty =1)
     argNew <- append(.select.args(arglist, names(argDefault), complement = F), .select.args(argDefault, names(arglist), complement = T))
     argPlot <- .sarg(argNew, y=y, x=x)   
     do.call("matplot", argPlot)
}

.fd <- function(pval, labels)
{
    id <- which(labels == 1)	
    x <- 1:length(id)
    o <- order(pval)
    y <- !o[x] %in% id
    cumsum(y)
}

