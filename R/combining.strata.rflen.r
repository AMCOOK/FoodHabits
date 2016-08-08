#@
combining.strata.rflen <- function(x) {
	A <- c('YEAR','RFLEN', 'STRAT','BIOM.mean',  'CLEN.mean',   'BIOM.var',   'CLEN.var', 'strat.areas')
	b <- x[,!colnames(x) %in% A]
	B <- x[1,]
	B <- B[,!colnames(B) %in% A[3:length(A)]]
	B[,2:(ncol(B)-1)] <- NA
	for(i in 2:(ncol(B)-1)) {
		B[,i] <- sum(x[,i+1]*(x$BIOM.mean*x$strat.areas))/sum(x$BIOM.mean*x$strat.areas)		
	}
	return(B)
}

	
