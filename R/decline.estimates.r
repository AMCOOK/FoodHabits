#' @export

decline.estimates <- function(da,syear,eyear) {
	dd <- subset(da,da$Year>=syear & da$Year<=eyear)
	plot(dd$Year,log(dd$Total),type='n',ylab='Log Abundance #s',xlab='Year')
	if(any(colnames(dd)=='Flen.Group')) {
		ds <- split(dd,f=dd$Flen.Group)
		dg <- data.frame(Flen.Group=names(ds),Rate.of.Change=NA,Years=paste(syear,eyear,sep="-"))
		for(i in 1:length(ds)) {
		a <- coef(lm(log(Total)~Year,data=ds[[i]]))
		dg[i,2] <- round(100*(1-exp(a[2]*nrow(ds[[i]]))),digits=1)
		with(ds[[i]],points(Year,log(Total),pch=i,col=i))
		abline(a=a[1],b=a[2],lty=i,col=i)
		}
		legend('topright',names(ds),lty=1:length(names(ds)),col=1:length(names(ds)),pch=1:length(names(ds)),bty='n',cex=0.7)
		}
	else{
		with(dd,points(Year,log(Total)))
		a <- coef(lm(log(Total)~Year,data=dd))
		abline(a=a[1],b=a[2],lty=1,col=1,lwd=2)
		dg <- c(Rate.of.Change=-1*round(100*(1-exp(a[2]*nrow(dd))),digits=1),Years=paste(syear,eyear,sep="-"))
		}
	print(dg)
	return(dg)
	}

