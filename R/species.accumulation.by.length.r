#' @export

species.accumulation.by.length <- function(data,lengths, cut.p=T) {
	if(cut.p) {
		data$RFLEN <- ifelse(data$FLEN<=lengths,paste("<=",lengths,sep=""),paste(">",lengths,sep=""))
		d1 <- split(data,data$RFLEN)
		par(mfrow=c(2,2),mar=c(4,4,3,1))
		a <- lapply(d1,species.accumulation,cut.p=T)
		}
	else {
		if(lengths==1) data$RFLEN <- round.flens(data$FLEN,grp=5) #5cm
		if(lengths==2) data$RFLEN <- round.flens(data$FLEN,grp=10) #10cm
		if(lengths==3) data$RFLEN <- round.flens(data$FLEN,grp=20) #3cm
		
		d1 <- split(data,data$RFLEN)
		pr <- length(d1)
 		mult.windows(mfrows=c(2,2),mars=c(4,4,3,1))
 		a <- lapply(d1,species.accumulation,cut.p=T)
		}
	}
