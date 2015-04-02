\docType{data}
\name{calibration}
\alias{calibration}

\title{A hypothetical example of a calibrated, liberal and conservative statistical method in genomics simulation}
\description{
  A hypothetical example of a calibrated, liberal and conservative statistical method in genomics simulation, 10000 tags, 6 samples (3 control versus 3 treatment). 1000 are
truly differential.  All features are generated from a normal distribution with mean 0 and variance 1, except for the 1,000 differential features, which had a shifted mean (see codes on examples below). 
}
\usage{data(calibration)
}


\author{Mark D. Robinson}

\examples{

data(calibration)
re <- SimResults(pval=calibration$pval, labels=calibration$labels)
benchmarkR(re)


# generate data
set.seed(100)
nf <- 10000
ntrue <- 500
nsamp <- 6

x <- matrix( rnorm(nf*nsamp), nrow=nf)
x[1:ntrue,1:3] <- x[1:ntrue,1:3]+2.5
rm1 <- rowMeans(x[,1:3])
rm2 <- rowMeans(x[,4:6])
z <- (rm1-rm2)/sqrt(2/3)
zps <- 2*pnorm(abs(z),lower.tail=FALSE)

x <- matrix( rnorm(nf*nsamp), nrow=nf)
x[1:ntrue,1:3] <- x[1:ntrue,1:3]+2.5
rm1 <- rowMeans(x[,1:3])
rm2 <- rowMeans(x[,4:6])
zc <- (rm1-rm2)/sqrt(1.3*2/3)
zcps <- 2*pnorm(abs(zc),lower.tail=FALSE)

x <- matrix( rnorm(nf*nsamp), nrow=nf)
x[1:ntrue,1:3] <- x[1:ntrue,1:3]+2.5
rm1 <- rowMeans(x[,1:3])
rm2 <- rowMeans(x[,4:6])
zl <- (rm1-rm2)/sqrt(.8*2/3)
zlps <- 2*pnorm(abs(zl),lower.tail=FALSE)

true <- rep(1:0,c(ntrue, nf-ntrue))
pval <- padj<- cbind(calibrated=zps,conservative=zcps,liberal=zlps)
labels <- true
calibration <- list(pval=pval, padj=padj, labels=labels)
#save(calibration, file="calibration.rda")



par(mfrow=c(2,3))
hist(zps,50,main="calibrated", xlab="")
mtext("(a)", side=3, adj=-.16, padj=-.5, cex=1.75)
hist(zlps,50,main="liberal", xlab="")
mtext("(b)", side=3, adj=-.16, padj=-.5, cex=1.75)
hist(zcps,50,main="conservative", xlab="")
mtext("(c)", side=3, adj=-.16, padj=-.5, cex=1.75)
true <- rep(1:0,c(ntrue, nf-ntrue))
re <- SimResults(pval=pval, labels=labels)
rocX(re,xlim=c(0,.2))
mtext("(d)", side=3, adj=-.16, padj=-.5, cex=1.75)
fdX(re)
mtext("(e)", side=3, adj=-.16, padj=-.5, cex=1.75)
powerFDR(re,xlim=c(0,.3))
mtext("(f)", side=3, adj=-.16, padj=-.5, cex=1.75)	
	
	
	
}	
\keyword{datasets}
