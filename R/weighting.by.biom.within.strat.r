#@
weighting.by.biom.within.strat <- function (z) {
		#for combining lengths in stratified diet estimates weighted means and variances
	x <- c('YEAR','STRAT','FLEN', "BIOM.mean" ,"CLEN.mean", "BIOM.var", "CLEN.var",'RFLEN')
	b <- z[1,]
	b <- b[,!colnames(b)=='FLEN']
	b[,!colnames(b) %in% x] <- NA
	d <- z[,!colnames(z) %in% x]
	for(i in 1:ncol(d)){
		b[,i+2] <- sum(d[,i]*z$BIOM.mean)/sum(z$BIOM.mean)
	}
	 f <- which(colnames(b) %in% c(x[4:7]))
	 for(i in 1:length(f)){
		b[,f[i]] <- sum(z[,f[i]+1])	 
	 }
	 return(b)
}

