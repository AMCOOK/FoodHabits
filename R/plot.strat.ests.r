#@
plot.strat.ests <- function(a=strat.ests) {
	plot(a$Year,a$Mean,type='n',xlab='Year',ylab=expression(Number%.%tow^-1),ylim=c(min(a$Mean-a$SD),max(a$Mean+a$SD)))
	if(ncol(a)>=7) {
		b <- unique(a$Flen.Group)
		for(i in 1:length(b)) {
			w <- subset(a,a$Flen.Group==b[i])
			lines(w$Year,w$Mean,col=i,lty=i,lwd=2)
			arrows(w$Year,w$Mean,w$Year,w$Mean+w$SD,angle=90,length=0.04,col=i)
			arrows(w$Year,w$Mean,w$Year,w$Mean-w$SD,angle=90,length=0.04,col=i)

		}
		legend('topright',b,lty=1:length(b),col=1:length(b),bty='n',cex=0.7)		
	}	
	else {
		w <- a
			lines(w$Year,w$Mean,col=1,lty=1,lwd=2)
			arrows(w$Year,w$Mean,w$Year,w$Mean+w$SD,angle=90,length=0.04,col=1)
			arrows(w$Year,w$Mean,w$Year,w$Mean-w$SD,angle=90,length=0.04,col=1)
	
	}
}

