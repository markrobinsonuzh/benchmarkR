source("http://130.60.190.4/robinson_lab/edgeR_robust/robust_simulation_v1.R")
method <- c("edgeR", "limma_voom", "DESeq2")
n5 <- 10
g5 <- as.factor(rep(0:1,each=n5/2))

###########5 vs 5########################################################################################################################
###########simulation foldDiff = 3############


#simulation pickrell, 5v5, foldDiff=3, pDiff=0.15, pUp = 0.5, pOutlier = 0.1
data <- NBsim(foldDiff=3, dataset = pickrell, group = g5, add.outlier = TRUE, pOutlier = 0.1, nTags = 4000)
Pickrell <- pval(data, method = method ,count.type = "counts", mc.cores = 4)

save(Pickrell, file="Pickrell.rda")