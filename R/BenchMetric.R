setClass(".BenchMetric", slots=c(element="list"))

##Define .BenchMetric
##Xiaobei Zhou
##Sep 2014.  Last modified 15 Sep 2014.


setMethod("show",".BenchMetric",
function(object) 
##Define show of .BenchMetric
##Xiaobei Zhou
##Sep 2014.  Last modified 15 Sep 2014.
{
    cat("An object of class \"",class(object),"\n",sep="")
    cat("..............", "\n")
    cat(slotNames(object), ":", "\n")
    str(object@element, max.level = 1)
})




setClass(".BenchMetricList",
         prototype = prototype(elementType = ".BenchMetric"),
         contains = "list")

setMethod("show",".BenchMetricList",
function(object) 
##Define show of .BenchMetricList
##Xiaobei Zhou
##Sep 2014.  Last modified 15 Sep 2014.
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
