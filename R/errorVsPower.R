# Charity Law
# 27 August 2014 (Last updated 1 September 2014)

errorVsPower <- function(pvalue, posctrl, id=NULL, cutoff=0.05, plot=FALSE, add.points=FALSE, col="black", pch=21, cex=1.3, lwd=2, xlim=c(0,1), ylim=c(0,1), points.type="b", line.col="black", cutoff.lty=3, print=TRUE, ...){

	if (!is.numeric(pvalue)) stop("pvalue is non-numeric")
	cutoff <- as.numeric(cutoff)
	
	# If posctrl is a logical vector of length length(pvalue), then id not required.
	if (length(posctrl)==length(pvalue) & is.logical(posctrl)) {
		id <- 1:length(pvalue)
		posctrl <- which(posctrl)
	}

	# If posctrl is a vector of ids, then id required.
	if (length(posctrl)!=length(pvalue) & is.null(id)) stop("id required")
	if (length(posctrl)!=length(pvalue)) {
		posctrl <- as.character(posctrl) 
		id <- as.character(id) 
	}
	
	# Warning for ambiguous ids
	if (any(is.na(id))) cat("WARNING: NAs detected in id", "\n")
	if (length(id[!is.na(id)])!=length(unique(id[!is.na(id)]))) cat("WARNING: non-unique ids detected", "\n")
	if (length(id)!=length(pvalue)) cat("WARNING: length(id) different to length(pvalue)", "\n")


	# Remove missing values	from results.
	keep <- !is.na(pvalue) 
	pvalue <- pvalue[keep]
	id <- id[keep]
	
	# Clean up posctrl
	posctrl <- posctrl[!is.na(posctrl)]
	posctrl <- unique(posctrl) 
	
	# Calculate false discovery rate and true positive rate.
	trueposrate <- NULL
	falsediscoveryrate <- NULL
	for (i in 1:length(cutoff)) {
		detected <- id[pvalue<cutoff[i]]
		ndiscoveries <- length(detected)
		nfalsediscoveries <- sum(!detected %in% posctrl)
		ntruepos <- sum(posctrl %in% detected)
		trueposrate <- c(trueposrate, ntruepos/length(posctrl))
		falsediscoveryrate <- c(falsediscoveryrate, nfalsediscoveries/ndiscoveries)	
	}	
		
	# Make plot
	if (any(plot, add.points)) {
		if (!pch %in% 21:25) cat("WARNING: defined pch does not allow fill-unfill option for points (pch = 21, 22, 23, 24, or 25 recommended)", "\n")	
		if (length(cutoff)!=length(col)) col <- palette()[1:length(cutoff)]
		bg <- col
		bg[falsediscoveryrate>cutoff] <- "white"
		if (!points.type %in% c("b", "p", "l")) points.type <- "b"
	}
	
	if (plot) {
		plot(1, type="n",xlim=xlim, ylim=ylim, 
			xlab="False discovery rate", ylab="True positive rate", xaxt="n", ...)
		abline(v=cutoff, lty=cutoff.lty, lwd=lwd, col=col)
		axis(side=1, at=(1:10)/10, labels=(1:10)/10, las=2, col.ticks="grey", col.axis="grey")
		axis(side=1, at=cutoff, labels=cutoff, las=2)
		if (points.type %in% c("l", "b")) {
			points(falsediscoveryrate, trueposrate, type="l", col=line.col, lwd=lwd)
			if (points.type=="b") points(falsediscoveryrate, trueposrate, type="p", col=col, pch=pch, bg=bg, cex=cex)			
		}	
		if (points.type=="p") points(falsediscoveryrate, trueposrate, type="p", col=col, pch=pch, bg=bg, cex=cex)			
	}
	
	# Add points to current plot
	if (add.points) {
		if (points.type %in% c("l", "b")) {
			points(falsediscoveryrate, trueposrate, type="l", col=line.col, lwd=lwd)
			if (points.type=="b") points(falsediscoveryrate, trueposrate, type="p", col=col, pch=pch, bg=bg, cex=cex)			
		}	
		if (points.type=="p") points(falsediscoveryrate, trueposrate, type="p", col=col, pch=pch, bg=bg, cex=cex)			
	}

	out <- data.frame(cutoff=cutoff, FDR=falsediscoveryrate, TPR=trueposrate)
	if (print) out
}
