\docType{data}
\name{microbiome}
\alias{microbiome}

\title{A remade example for evaluating performance of methods of a simulated ``microbiome'' dataset}
\description{A remade example for evaluating performance of methods of a simulated ``microbiome'' dataset from McMurdie et al [1]. The original figure (Figure 6 doi:10.1371/journal.pcbi.1003531.g006) evaluates AUC, specificity and sensitivity of different methods across different conditions, including effected sizes,  number of samples, number of reads and different normalization methods. We present \code{\link{powerFDR}} curves and modify the original figure focusing on one side (effected sizes=c(2.5,5,10,20), number of samples=10 and number of reads=50000, methods=c(``edge'',``DESeq'')) but not every condition of settings. The details procedures can be found on examples below.)
}
\usage{data(microbiome)
}

\references{

[1]McMurdie PJ, Holmes S (2014). Waste not, want not: why rarefying microbiome data isinadmissible.
\emph{PLoS computational biology} 2014;10(4):e1003531. 
\url{http://journals.plos.org/ploscompbiol/article?id=10.1371/journal.} 
pcbi.1003531.
}


\author{Xiaobei zhou and Mark D. Robinson}

\examples{
# powerFDR curves
library(benchmarkR)
data(microbiome)
powerFDR(microbiome[[3]], xlim=c(0,0.1),ylim=c(0.2,1), pch=1, lwd=5, lwd.line=5, 
         legend=list("bottomright",lty=1,pch=NA, lwd=4,cex=1.5))
powerFDR(microbiome[[4]], add=TRUE, legend=NULL, pch=2, lwd=5, lwd.line=5)
powerFDR(microbiome[[1]], add=TRUE, legend=NULL, pch=0, lwd=5, lwd.line=5)
powerFDR(microbiome[[2]], add=TRUE, legend=NULL, pch=6,lwd=5, lwd.line=5)
legend <- paste0("EffectSize=",c(2.5,5,10,20))
legend("topright",lty=1, pch=c(1,2,0,6), legend=legend, cex=1.5, lwd=2)

# modify Figure 6
##download the link file:	
##\url{http://journals.plos.org/ploscompbiol/article/asset?unique&id=info:doi/10.1371/journal.pcbi.1003531.s001}	
##run the Rcode on ``simulation-differential-abundance/simulation-differential-abundance-server.html"	
##run the following codes

##superlist=list(DESeq=DESeqreslist, edgeR=edgeRreslist)
##nreadskeep <- 50000
##nsampleskeep <- 10
##maindat = transform(dfmeansd, group=paste0(nsamples,Normalization,Method))
##Normalizationkeep <- "Rarefied"
##maindat <- subset(maindat, as.numeric(nreads) %in% nreadskeep)
##maindat <- subset(maindat, as.numeric(nsamples) %in% nsampleskeep)
##maindat <- subset(maindat, !Normalization %in% Normalizationkeep)

##pSpecificity
##pSpecificity=ggplot(maindat, aes(EffectSize,Specificity,color=Normalization, 
##    alpha=factor(nsamples))) + geom_path(size=pathsize, aes(group = group)) + 
##    geom_errorbar(aes(ymax=Specificity + sd.Specificity, ymin=Specificity - 
##        sd.Specificity), width=errorwidth, alpha=erroralpha, size=errorsize, 
##        position="dodge") + geom_point(aes(shape=Normalization), size=pointsize) + 
##    facet_grid(nreads ~ Method, labeller=label_parsed) + scale_alpha_discrete(range=c(1, 
##    0.4), guide = guide_legend(title=nsampleslegtitle)) + 
##    scale_colour_discrete(guide=guide_legend(title=normlegtitle)) + 
##    scale_shape_discrete(guide=guide_legend(title = normlegtitle)) + 
##    scale_y_continuous(breaks = c(0.6,0.9,0.95,0.975,1), limits=c(0.95,1), 
##    oob=function(x, limits) {x}) + ggtitle("Differential Abundance Specificity")

##pSensitivity
##pSensitivity = ggplot(maindat, aes(EffectSize,Sensitivity,color = Normalization, 
##    alpha=factor(nsamples))) + geom_path(size=pathsize, aes(group=group)) + 
##    geom_errorbar(aes(ymax=Sensitivity + sd.Sensitivity, ymin=Sensitivity - 
##        sd.Sensitivity), width=errorwidth, alpha=erroralpha, size=errorsize, 
##        position = "dodge") + geom_point(aes(shape=Normalization), size=pointsize) + 
##    facet_grid(nreads ~ Method, labeller=label_parsed) + scale_alpha_discrete(range = c(1, 
##    0.4), guide = guide_legend(title = nsampleslegtitle)) + 
##    scale_colour_discrete(guide=guide_legend(title=normlegtitle)) + 
##    scale_shape_discrete(guide=guide_legend(title=normlegtitle)) + 
##    scale_y_continuous(breaks=c(0.2,0.5,0.6,0.8,1), limits=c(0.3, 1), 
##    oob=function(x, limits) {x}) + xlab("Effect Size") + 
##    ggtitle("Differential Abundance Sensitivity")

##pDAFP
##pDAFP = ggplot(maindat, aes(EffectSize,FP,color=Normalization)) + geom_path(size=0.5) + 
##    geom_errorbar(aes(ymax=FP+sd.FP, ymin=FP-sd.FP), width=0.2, alpha=0.25, 
##    size=0.35, position="dodge") + geom_point(aes(shape=Normalization),size=1.5) + 
##    facet_grid(nreads  ~ Method) + scale_y_continuous(breaks=c(-0.05,0,0.02,0.05,0.075,0.1)) 
##    + coord_cartesian(ylim=c(-0.1,0.1)) + ylab("False Positive Rate") + 
##    ggtitle("Differential Abundance Detection Performance,False Positives")

##remove main title
##pSpecificity$labels$title <- NULL
##pSensitivity$labels$title <- NULL
##pDApower$labels$title <- NULL
##pDAFP$labels$title <- NULL

##Remove margins
##pSensitivity = pSensitivity + theme(text=element_text(size=plotlabtextsize),
##    plot.margin=unit(c(0,0,0,0),"cm"),legend.position="top", 
##    legend.box="horizontal",legend.margin=unit(-0.5,"cm"))
##pSpecificity = pSpecificity+theme(text=element_text(size=plotlabtextsize), 
##    plot.margin=unit(c(0,0,0,0),"cm"))
##pDAFP = pDAFP+theme(text=element_text(size=plotlabtextsize), 
##    plot.margin=unit(c(0,0,0,0),"cm"))
##pSensitivity = pSensitivity+theme(axis.text.x=element_blank(), 
##    axis.title.x=element_blank(),plot.margin=unit(c(0, 0, -0.4, 0),"cm"))
##pSpecificity = pSpecificity + theme(axis.text.x=element_blank(), 
##    axis.title.x=element_blank(),plot.margin=unit(c(0, 0, -0.4, 0),"cm"))
##Remove redundant x-labels, legends
##pSensitivity = pSensitivity+theme(strip.background=element_blank())
##pSpecificity = pSpecificity+theme(legend.position="none", 
##    strip.text.x=element_blank(),strip.background=element_blank())
##pDAFP = pDAFP + theme(legend.position="none", strip.text.x=element_blank(), 
##    strip.background=element_blank())

##Create multi-plot grid layout
##Layout = grid.layout(3,1,heights=c(1.5, 1, 1))
##grid.show.layout(Layout)

##pdf("Figure_6_modify.pdf",width=6.8, height=4)
##pushViewport(viewport(layout = Layout))
##print(pSensitivity, vp=viewport(layout.pos.row=1,layout.pos.col=1))
##print(pSpecificity, vp=viewport(layout.pos.row=2,layout.pos.col=1))
##print(pDAFP, vp=viewport(layout.pos.row=3,layout.pos.col=1))
##dev.off()

}	
\keyword{datasets}
