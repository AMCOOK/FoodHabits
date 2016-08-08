#@
mean.diet.by.length <- function(data,lengths, cut.p=T, prey.grouping,remove.singletons,remove.influentials,percent.diff) {
	mult.windows()
	if(cut.p) {
	
		data$RFLEN <- ifelse(data$FLEN<=lengths,paste("<=",lengths,sep=""),paste(">",lengths,sep=""))
		d1 <- split(data,data$RFLEN)
		a <- lapply(d1,mean.diet,prey.grouping=prey.grouping,remove.singletons=remove.singletons,remove.influentials=remove.influentials,percent.diff=percent.diff,by.lengths=T)
		}
	else {
		if(lengths==1) data$RFLEN <- round.flens(data$FLEN,grp=5) #5cm
		if(lengths==2) data$RFLEN <- round.flens(data$FLEN,grp=10) #10cm
		if(lengths==3) data$RFLEN <- round.flens(data$FLEN,grp=20) #20cm
		d1 <- split(data,data$RFLEN)
		pr <- length(d1)
		a <- lapply(d1,mean.diet,prey.grouping=prey.grouping,remove.singletons=remove.singletons,remove.influentials=remove.influentials,percent.diff=percent.diff,by.lengths=T)
		}
		return(a)
	}
