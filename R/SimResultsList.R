setClass("SimResultsList",
         prototype = prototype(elementType = "SimResults"),
         contains = "list")

SimResultsList <- function(...)
{
    x <- list(...)
    if(length(x) == 1L & !class(x) == "SimResults")
        new("SimResultsList", x[[1L]])
    else 
        new("SimResultsList", x)
}

setMethod("show","SimResultsList",
function(object) 
##Define show of SimResultsList
##Xiaobei Zhou
##Nov 2015.  Last modified 2 Nov 2015.
{
    cat("An object of class \"",class(object),"\n",sep="")
    cat("+++++++++++++++++++++", "\n")
    l <- length(object)
    if(is.null(names(object)))
         nms <- seq_len(l) 
    else nms <- names(object)  
    defnms <- paste0("[[", nms, "]]")
    for (i in seq_len(l))
    {
        cat(defnms[i], "\n")
        show(object[[i]])
    }

})

