#' @export

combining.strata <- function(x) {
	A <- c('YEAR', 'STRAT','BIOM.mean',  'CLEN.mean',   'BIOM.var',   'CLEN.var', 'strat.areas')
	b <- x[,!colnames(x) %in% A]
	B <- x[1,]
	B <- B[,!colnames(B) %in% A[2:length(A)]]
	B[,2:ncol(B)] <- NA
	for(i in 2:ncol(B)) {
		B[,i] <- sum(x[,i+1]*(x$BIOM.mean*x$strat.areas))/sum(x$BIOM.mean*x$strat.areas)		
	}
	return(B)
}

