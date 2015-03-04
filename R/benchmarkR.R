benchmarkR <- function(object=NULL, stratify=NULL, name.panel=c("a", "b", "c"), ..., cex.panel=1.5)
{
    l <- 3
    arglist <- list(...)
    object <- SimResults(object=object, stratify=stratify, ...)
            stratify=object@stratify[[1L]]
    if(!is.null(stratify))
        stop("Currently, 'benchmarkR' only supports 'stratify=NULL'!") 
    if(is.null(arglist$legend))
        arglist$legend <- list("bottomright", "topleft", "bottomright")
    arglist <- lapply(arglist, .repArgs, len=l)
    argPloti <- maini <- list()
    for(i in seq(1:l))
    {
         argPloti[[i]] <- lapply(arglist, .getSub2, id = i)
         maini[[i]] <- argPloti$main
         argPloti[[i]] <- .sarg(argPloti[[i]], object=object, main=maini)
    } 
    old.par <- par(c("mar","mgp","mfrow"))
    par(mar=c(4,5,3,2))
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