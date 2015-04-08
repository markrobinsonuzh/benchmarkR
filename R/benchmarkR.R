setGeneric(
##Define genetric of benchmarkR
##Xiaobei Zhou
##March 2015.  Last modified 18 March 2015.
   "benchmarkR", 
   function(object, pval, ...)   
   {
       standardGeneric("benchmarkR")
   }  
)



setMethod(
##Define "SimResults" object for benchamrkR
##Xiaobei Zhou
##March 2015.  Last modified 18 March 2015.
    "benchmarkR",
    signature(object="SimResults", pval="missing"),
    function(object, thresholdX=0.05, threshold=c(0.01,0.05,0.1), transformation="1-x", name.panel=c("a", "b", "c"), ..., cex.panel=1.5)
    {
        l <- 3
        #arglist <- list(...)
        arglist <- c(lapply( as.list(environment()), eval), list(...) )
        if(is.null(arglist$legend))
            arglist$legend <- list("bottomright", "topleft", "bottomright")
        arglist <- lapply(arglist, .repArgs, len=l)
        argPloti <- maini <- list()
        for(i in seq(1:l))
        {
            argPloti[[i]] <- lapply(arglist, .getSub2, id = i)
            argPloti[[i]] <- .sarg(argPloti[[i]], object=object)
        } 
        #argPloti[[1]] <- .sarg(argPloti[[1]], thresholdX=thresholdX)
        #argPloti[[2]] <- .sarg(argPloti[[2]], thresholdX=thresholdX)
        #argPloti[[3]] <- .sarg(argPloti[[3]], threshold=threshold)
        old.par <- par(c("mar","mgp","mfrow"))
        par(mar=c(4,5,3,2))
        par(oma=c(0,0,2,0))
        par(mgp = c(2.6, 1, 0))
        on.exit(par(old.par))
        layout(matrix(c(1,3, 2,3), 2, 2 , byrow = TRUE), widths = c(0.6, 1, 1))
        do.call("rocX", argPloti[[1]])
        mtext(name.panel[1], side=3, adj=-.13, padj=-.5, cex=cex.panel)
        do.call("fdX", argPloti[[2]])
        mtext(name.panel[2], side=3, adj=-.13, padj=-.5, cex=cex.panel)
        do.call("powerFDR", argPloti[[3]])
        mtext(name.panel[3], side=3, adj=-.13, padj=-.5, cex=cex.panel)
    }
)



setMethod(
##Define "SimResults" object for benchamrkR
##Xiaobei Zhou
##March 2015.  Last modified 18 March 2015.
    "benchmarkR",
    signature(object="missing", pval="ANY"),
    function(pval, padj=NULL, labels, thresholdX=0.05, threshold=c(0.01,0.05,0.1), transformation="1-x", name.panel=c("a", "b", "c"), ..., cex.panel=1.5)
    {
        object <- SimResults(pval=pval, padj=padj, labels=labels, ...)
        benchmarkR(object, thresholdX=thresholdX, threshold=threshold, transformation=transformation, name.panel=name.panel, cex.panel=cex.panel)
    }
)




