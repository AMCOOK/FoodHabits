#' @export
	
diet.barplot <- function(data,number=10,spps=unique(diet.data1$COMMON),error=T,prey.group='FAM',by.lengths=F) {
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
 if(nrow(data)<10) number <- nrow(data)
	 data <- data[1:number,]
	par(mar=c(9,4,4,2))
	if(error) {
		 a <- barplot(data$Mean,ylab='Wt (g)',ylim=c(0,max(data$Mean+sqrt(data$Variance))))
	mtext(side=1,at=a,data$Prey.names,las=2,line=0.7,cex=0.8)
	title(paste(spps))
	arrows(x0=a,y0=data$Mean,y1=data$Mean+sqrt(data$Variance),angle=90,length=0.05)
	}
	
	else {
	 a <- barplot(data$Mean,ylab='Wt (g)',ylim=c(0,max(data$Mean)))
	mtext(side=1,at=a,data$Prey.names,las=2,line=0.7,cex=0.8)
	title(spps)
	}	
	}
	}	

