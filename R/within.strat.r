#' @export

within.strat <- function(data1) {
	#within strata means and variances using clen data within length groups
	#dats are the means
	#dats2 varainces
	data1 <- data1[,!colnames(data1)=='MISSION']
	A <- c('MISSION','SETNO','STRAT','FLEN','YEAR','BIOM','CLEN')
	dat.cols <- data1[,!colnames(data1) %in% A]
	clen.col <- data1[,colnames(data1)=='CLEN']
	dats <- data1[1,c(-1,-(ncol(data1)-1),-(ncol(data1)))]
	dats[,c(4:ncol(dats))] <- NA
	dats2 <- dats
	#weighted means
	colnames(dats2)[c(4:ncol(dats))] <- paste(colnames(dats2)[c(4:ncol(dats))],'-var',sep="")
	for(i in 1:ncol(dat.cols)) {
		dats[,i+3] <- 	sum(dat.cols[i]*clen.col)/sum(clen.col)
		}
	if(nrow(data1)==1) {dats2[,c(4:ncol(dats2))] <- 0 }
	#weighted variances
	if(nrow(data1)>1){
		for(i in 1:ncol(dat.cols)) {
		dats2[,i+3] <- 	1/(nrow(data1)*mean(data1$CLEN)^2)*(sum(clen.col^2*(dat.cols[i]-rep(dats[i+3],times=length(dat.cols[i])))^2)/(nrow(data1)-1)) #var from warren et al. using only fish with sampled sets
		}
	}
		datt <- merge(dats,dats2,by=c('YEAR','FLEN','STRAT'))
		return(datt)
	}


