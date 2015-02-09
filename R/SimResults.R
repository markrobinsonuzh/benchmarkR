
setClassUnion("pval", c("matrix", "NULL"))
setClassUnion("padj", c("matrix", "NULL"))
setClassUnion("labels", c("numeric", "NULL"))
setClassUnion("stratify", c("data.frame", "NULL"))

.SimResults <- setClass("SimResults", slots = c(pval="pval", padj="padj", labels="labels", stratify="stratify"))
##Define SimResults
##Xiaobei Zhou
##June 2014.  Last modified 26 June 2014.

SimResults <- function(pval=NULL, padj=NULL, labels=new("numeric"), stratify=NULL, ...)
##Define SimResults constructor
##Xiaobei Zhou
##June 2014.  Last modified 26 June 2014.
{
    if(is.null(pval))
        .SimResults(pval=NULL, padj=NULL, labels=NULL, stratify=NULL)
    else
    {
        pval <- as.matrix(pval) 
        if(is.null(padj))
        {
            message("padj is missing, 'BH' method is used to generate padj.")
            padj <- apply(pval, 2, p.adjust, method = "BH")  
        }
        padj <- as.matrix(padj)
        if(is.null(labels)) 
            stop("labels cannot be NULL when pval is not NULL")
        else
            labels <- as.numeric(as.character(labels))
        if(!nrow(pval) == length(labels))
            stop("pval and labels must have the same length!")
        if(!ncol(pval) == ncol(padj))
            stop("padj and padj must have the same dimension!")
        if(!all(labels == 0|1))
            stop("labels must only contain 0 or 1!")
        if(!is.null(stratify))
        {
            stratify <- as.data.frame(stratify)
            if(!nrow(stratify) == length(labels))
                 stop("stratify and labels must have the same dimension!") 
        }
        if(is.na(pval))
        {
            message("pval has NA values, any NA value is replaced by 1.")
            pval[is.na(pval)] <- 1
        }
        if(is.na(padj))
        {
            message("padj has NA values, any NA value is replaced by 1.")
            padj[is.na(padj)] <- 1
        }

        .SimResults(pval = pval, padj = padj, labels = labels, stratify = stratify)
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


