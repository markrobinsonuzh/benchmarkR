source("http://130.60.190.4/robinson_lab/edgeR_robust/robust_simulation_v1.R")
method <- c("edgeR", "limma_voom", "DESeq2")
n5 <- 10
g5 <- as.factor(rep(0:1,each=n5/2))

###########5 vs 5########################################################################################################################
###########simulation foldDiff = 3############


#simulation pickrell, 5v5, foldDiff=3, pDiff=0.15, pUp = 0.5, pOutlier = 0.1
data <- NBsim(foldDiff=3, dataset = pickrell, group = g5, add.outlier = TRUE, pOutlier = 0.1, nTags = 4000)
Pickrell <- pval(data, method = method ,count.type = "counts", mc.cores = 4)

pval <- do.call("cbind", Pickrell$pval)
padj <- do.call("cbind", Pickrell$padj)
labels <- rep(0, nrow(pval))
labels[Pickrell$indDE] <- 1
Pickrell$pval <- pval
Pickrell$padj <- padj
Pickrell$labels <- labels
class(Pickrell) <- "list"

#save(Pickrell, file="Pickrell.rda")


###########5 simulation############

source("http://130.60.190.4/robinson_lab/edgeR_robust/robust_simulation_v1.R")
n5 <- 10
g5 <- as.factor(rep(0:1,each=n5/2))
method <- c("edgeR", "limma_voom", "DESeq2")
PickrellList <- list()

for (i in 1:5)
{
   sim <- NBsim(foldDiff=3, dataset=pickrell,nTags=4000, group=g5)
   Pickrell <- pval(sim, method=method ,count.type="counts", mc.cores=8)
   labels <- rep(0, nrow(sim))
   labels[sim$indDE] <- 1
   Pickrell$labels <- labels
   Pickrell$pval <- do.call("cbind", Pickrell$pval)
   Pickrell$padj <- do.call("cbind", Pickrell$padj)
   class(Pickrell) <- "list"
   PickrellList[[i]] <- Pickrell
   cat("i=", i, "\n")
}

names(PickrellList) <- paste0("sim", 1:5)


#save(PickrellList, file="PickrellList.rda")