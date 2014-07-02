

.SimResults <- setClass("SimResults", slots = c(pval="matrix", padj="matrix", labels="numeric", stratifyBy="data.frame"))
##Define SimResults
##Xiaobei Zhou
##June 2014.  Last modified 26 June 2014.

SimResults <- function(pval=new("matrix"), padj=new("matrix"), labels=new("numeric"), stratifyBy=new("data.frame"), ...)
##Define SimResults constructor
##Xiaobei Zhou
##June 2014.  Last modified 26 June 2014.
{
    pval <- as.matrix(pval)
    padj <- as.matrix(padj)
    stratifyBy <- as.data.frame(stratifyBy) 
    labels <- as.numeric(as.character(labels))
    if(length(pval) == 0)
        .SimResults(pval=pval, padj=padj, labels=labels, stratifyBy=stratifyBy)
    else
    {
        if(length(padj) == 0)
        {
            message("padj is missing, 'BH' method is used to generate padj.")
            padj <- apply(pval, 2, p.adjust, method = "BH")  
        }
        if(!nrow(padj) == length(labels))
            stop("padj and labels must have the same length!")
        if(!ncol(pval) == ncol(padj))
            stop("padj and padj must have the same dimension!")
        if(!all(labels == 0|1))
            stop("labels must only contain 0 or 1!")
        if(length(stratifyBy) > 0)
        {
            if(!nrow(stratifyBy) == length(labels))
                 stop("stratifyBy and labels must have the same dimension!") 
        }
        .SimResults(pval = pval, padj = padj, labels = labels, stratifyBy = stratifyBy)   
    }
}


setMethod("[", c("SimResults", "ANY", "ANY"),
    function(x, i, j)
{
    SimResults(pval = x@pval[i, j, drop=FALSE], padj = x@padj[i, j, drop=FALSE], labels = x@labels[i])
})




setMethod("show", "SimResults",
function(object)
##Define show of SimResults
##Xiaobei Zhou
##June 2014.  Last modified 26 June 2014.
{
    cat("An object of class \"",class(object),"\"\n",sep="")
    snms <- setdiff(slotNames(object),".Data")        
    for(what in snms) 
    {        
	x <- slot(object,what)
	if(length(x) > 0) 
        {
	    cat("@",what,"\n",sep="")
	    printHead(x)
	    cat("\n")
	}
    }

        
})



printHead <- function(x)
##Directly borrow from Gordon
##Print leading 5 elements or rows of atomic object
##Gordon Smyth
##May 2003.  Last modified 14 April 2009.
{
	if(is.atomic(x)) {
		d <- dim(x)
		if(length(d)<2) which <- "OneD"
		if(length(d)==2) which <- "TwoD"
		if(length(d)>2) which <- "Array"
	} else {
		if(inherits(x,"data.frame")) {
			d <- dim(x)
			which <- "TwoD"
		} else {
			if(is.call(x))
				which <- "Call"
			else {
				if(is.recursive(x))
					which <- "Recursive"
				else
					which <- "Other"
			}
		}
	}
	switch(which,
	OneD={
		n <- length(x)
		if(n > 20) {
			print(x[1:5])
			cat(n-5,"more elements ...\n")
		} else
			print(x)
	},
	TwoD={
		n <- d[1]
		if(n > 10) {
			print(x[1:5,])
			cat(n-5,"more rows ...\n")
		} else
			print(x)
	},
	Array={
		n <- d[1]
		if(n > 10) {
			dn <- dimnames(x)
			dim(x) <- c(d[1],prod(d[-1]))
			x <- x[1:5,]
			dim(x) <- c(5,d[-1])
			if(!is.null(dn[[1]])) dn[[1]] <- dn[[1]][1:5]
			dimnames(x) <- dn
			print(x)
			cat(n-5,"more rows ...\n")
		} else
			print(x)
	},
	Recursive={
		n <- length(x)
		if(n) {
			i <- names(x)
			if(is.null(i)) i <- seq_len(n)
			for (what in i) {
				y <- x[[what]]
				cat("$",what,"\n",sep="")
				Recall(y)
				cat("\n")
			}
		}
	},
	Call=,Other=print(x)
	)
}


