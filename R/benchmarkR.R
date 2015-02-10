benchmarkR <- function(object, ..., cex.panel=1.5)
{
    l <- 3
    arglist <- list(...)
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
    mtext("(a)", side=3, adj=-.13, padj=-.5, cex=cex.panel)
    do.call("fdX", argPloti[[2]])
    mtext("(b)", side=3, adj=-.13, padj=-.5, cex=cex.panel)
    do.call("powerFDR", argPloti[[3]])
    mtext("(c)", side=3, adj=-.13, padj=-.5, cex=cex.panel)
}