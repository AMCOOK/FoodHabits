#@
diet.barplot.3 <- function(data,number=10,spps=unique(diet.data1$COMMON),prey.group='FAM') {
	lens <- unique(data$FLEN)
	if(nrow(data)>1) {
	 if(prey.group=='FAM') {
		 aaf <- paste(unique(data$Prey),collapse="','")
		 nna <- sqlQuery(channel,paste("select distinct fam, pretty_f_names from prey_spec_details where fam in ('",aaf,"');",sep=""))
	 	 data$Prey.names <- nna[match(data$Prey,nna$FAM),2]	
 	 	}
 	 else {
	 	 aaf <- paste(unique(data$Prey),collapse="','")
		 nna <- sqlQuery(channel,paste("select distinct speccd cds, common from prey_spec_details where speccd in ('",aaf,"');",sep=""))
	 	 data$Prey.names <- nna[match(data$Prey,nna$CDS),2]	
	 data$Prey.names <- capwords(data$Prey.names, strict=T)	 
 	 }
 	for(i in 1:length(lens)) {
	data1 <- data[data$FLEN==lens[i],] 	 
 if(nrow(data1)<10) number <- nrow(data1[data1$Mean>0,])
 data1 <- data1[rev(order(data1$Mean)),]
 	data1 <- data1[1:number,]
	par(mar=c(9,4,4,2))

	a <- barplot(data1$Mean,ylab='Wt (g)',ylim=c(0,max(data1$Mean+sqrt(data1$Variance))))
	mtext(side=1,at=a,data1$Prey.names,las=2,line=0.7,cex=0.8)
	title(paste(spps,lens[i],sep="-"))
	arrows(x0=a,y0=data1$Mean,y1=data1$Mean+sqrt(data1$Variance),angle=90,length=0.05)
	}
	}
	}	
