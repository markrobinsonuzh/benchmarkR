



##############preprocessing data#############
load("abrf.read.counts.rda")
tq <- read.table("TaqmanPCR.txt",header=TRUE)
du <- duplicated(tq[,1])
tq <- tq[!du,]
tmp <- tq
tq <- tq[,-1]
row.names(tq) <- tmp[,1]
tq <- as.matrix(tq)


g <- as.factor(rep(0:3, each=4))
levels(g) <- c("A", "B", "C", "D")
mm <- model.matrix(~0+g)
colnames(mm) <- c("A","B","C","D")
cols <- grep("STAR-ILMN-RNA", colnames(abrf.counts))
counts <- abrf.counts[,cols]
#filter remove low-expressed counts#
library(edgeR)
d0 <- DGEList(counts, group= g)
d0 <- calcNormFactors(d0)
keep <- rowSums(cpm(d0)>0.5) >= 2
cnt <- counts[keep,]
#rm duplicated rownames#
nms <- sapply(strsplit( rownames(cnt), "!" ),.subset,2)
du <- duplicated(nms)
cnt <- cnt[!du,]
rownames(cnt) <- nms[!du]

#############taqman gene analysis#############
#########compare with A and B condition####### 
library(limma)
contr1 <- makeContrasts(A-B, A-C, A-D, levels=mm)
f <- lmFit(tq, mm)
f1 <- contrasts.fit(f, contr1)
f1 <- eBayes(f1)
pvalAB_tq <- topTable(f1,coef=1,n=nrow(cnt), sort.by = "none")$P.Value
padjAB_tq = p.adjust(pvalAB_tq, "BH")

#########RNA-seq gene counts DE analysis#######

#DE using edgeR#
library(edgeR)
d <- DGEList(cnt, group=g)
d <- calcNormFactors(d)
d <- estimateGLMCommonDisp(d, mm)
d <- estimateGLMTrendedDisp(d, mm)
d <- estimateGLMTagwiseDisp(d, mm)
f <- glmFit(d, mm)
lrtAB <- glmLRT(f, contrast=c(1,-1,0,0))
pvalAB_edgeR <- lrtAB$table$PValue
padjAB_edgeR = p.adjust(pvalAB_edgeR, "BH")

#DE using DESeq2#
library(DESeq2)

colData <- data.frame(g)
dse <- DESeqDataSetFromMatrix(countData=cnt, colData=colData, design =~g)
dse <- DESeq(dse)
resAB <- results(dse, contrast=c("g","A","B"))
pvalAB_DESeq2 = resAB$pvalue
padjAB_DESeq2 = resAB$padj

#DE using limma voom#
nf <- calcNormFactors(cnt)
y <- voom(cnt, mm, plot=FALSE, lib.size = colSums(cnt)*nf)
fit <- lmFit(y, mm)
fit1 <- contrasts.fit(fit, contr1)
fit1 <- eBayes(fit1)
pvalAB_voom <- topTable(fit1,coef=1,n=nrow(cnt), sort.by = "none")$P.Value
padjAB_voom = p.adjust(pvalAB_voom, "BH")

###########build SimResults object for plots#########
id <- match(rownames(tq), rownames(cnt))
mt <- which(!is.na(id))
id <- id[mt]
pvalAB <- cbind(edgeR=pvalAB_edgeR, DESeq2=pvalAB_DESeq2, voom=pvalAB_voom)[id,]
padjAB <- cbind(edgeR=padjAB_edgeR, DESeq2=padjAB_DESeq2, voom=padjAB_voom)[id,]
#truth setting#
labelAB <- rep(0, length(id))
labelAB[padjAB_tq[mt]<0.1] <- 1

save(pvalAB,padjAB,labelAB, file="ABRF.rda")

library(benchmarkR)
scAB <- SimResults(pval=pvalAB, padj=padjAB, labels=labelAB)

#png("ABRF_AvsB.png", width = 1200, height = 1000, pointsize = 20)
par(mar=c(5,5,3,2))
par(oma=c(0,0,4,0))
layout(matrix(c(1,3, 2,3), 2, 2 , byrow = TRUE), widths = c(0.6, 1, 1))
rocX(scAB, xlim=c(0,1), transformation="-log10(x)", lwd=2, lwdX=4, main="A-B", cex.main=2, cexX=3)
mtext("(a)", side=3, adj=-.13, padj=-.5, cex=2)
fdX(scAB, lwd=2, lwdX=4, main="A-B", cex.main=2, legend=list("topleft"), cexX=3)
mtext("(b)", side=3, adj=-.13, padj=-.5, cex=2)
powerFDR(scAB, ylim=c(0.8,1), xlim=c(0.0,0.2), lwd=3, cex=2, main="A-B", legend=list(col="gray50"), cex.main=2)
mtext("(c)", side=3, adj=-.13, padj=-.5, cex=2)
#dev.off()