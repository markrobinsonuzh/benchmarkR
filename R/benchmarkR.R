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
##Define "SimResults" object for benchmarkR
##Xiaobei Zhou
##March 2015.  Last modified 18 March 2015.
    "benchmarkR",
    signature(object="SimResults", pval="missing"),
    function(object, thresholdX=0.05, threshold=c(0.01,0.05,0.1), transformation="1-x", name.panel=c("a", "b", "c"), ..., cex.panel=1.5, 
              legend=list("bottomright", "topleft", "bottomright") )
    {
        idNA <- is.na(object@labels)
        if(any(idNA))
        {
            message("remove NA values from labels for benchmarkR")
            object <- object[!idNA,]
        }  
        l <- 3
        arglist <- c(lapply( as.list(environment()), eval), list(...) )
        if(is.null(arglist$add))
             add <- arglist$add <- FALSE           
        else 
            add <- arglist$add
        arglist <- lapply(arglist, .repArgs, len=l)
        argPloti <- maini <- list()
        for(i in seq(1:l))
        {
            argPloti[[i]] <- lapply(arglist, .getSub2, id = i)
            argPloti[[i]] <- .sarg(argPloti[[i]], object=object)
        } 

        old.par <- par(c("mar","mgp","mfrow")) 
        on.exit(par(old.par))
        if((add))
        {
            layout(matrix(c(1,3, 2,3), 2, 2 , byrow = TRUE), widths = c(0.6, 1, 1))
            par1 <- subplotPars[[1]]
            par(par1)          
            do.call("rocX", argPloti[[1]])
            par2 <- subplotPars[[2]]
            par(par2) 
            do.call("fdX", argPloti[[2]])
            par3 <- subplotPars[[3]]
            par(par3)  
            do.call("powerFDR", argPloti[[3]])
        }
        else
        { 
            par(mar=c(4,5,3,2))
            par(oma=c(0,0,2,0))
            par(mgp = c(2.6, 1, 0))
            pars <- c('plt','usr','mar')
            layout(matrix(c(1,3, 2,3), 2, 2 , byrow = TRUE), widths = c(0.6, 1, 1))
            do.call("rocX", argPloti[[1]])
            par1 <- c(list(mfg=c(1,1,2,2)), par(pars))
            mtext(name.panel[1], side=3, adj=-.13, padj=-.5, cex=cex.panel)
            do.call("fdX", argPloti[[2]])
            par2 <- c(list(mfg=c(1,2,2,2)), par(pars))
            mtext(name.panel[2], side=3, adj=-.13, padj=-.5, cex=cex.panel)
            do.call("powerFDR", argPloti[[3]])
            par3 <- c(list(mfg=c(2,1,2,2)), par(pars))
            mtext(name.panel[3], side=3, adj=-.13, padj=-.5, cex=cex.panel)
            subplotPars <<- list(par1=par1, par2=par2, par3=par3)
        }
    }
)



setMethod(
##Define "SimResults" object for benchamrkR
##Xiaobei Zhou
##March 2015.  Last modified 18 March 2015.
    "benchmarkR",
    signature(object="missing", pval="ANY"),
    function(pval, padj=NULL, labels, thresholdX=0.05, threshold=c(0.01,0.05,0.1), transformation="1-x", name.panel=c("a", "b", "c"), ..., cex.panel=1.5, 
              legend=list("bottomright", "topleft", "bottomright"))
    {
        object <- SimResults(pval=pval, padj=padj, labels=labels, ...)
        benchmarkR(object, thresholdX=thresholdX, threshold=threshold, transformation=transformation, name.panel=name.panel, cex.panel=cex.panel, legend=legend)
    }
)




