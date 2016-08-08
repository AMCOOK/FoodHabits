#' @export

stratified.diet <- function (diet.data=diet.data$raw.diet,survey.data=diet.data$gs.survey.data,strata.data=diet.data$strata.data,prey.grouping=pp,
							remove.singletons,by.lengths=F,cut.p=T,lengths,a.b=diet.data$a.b,fillin.missing.lengths=F) {
		diet.data$YEAR <- as.numeric(substr(diet.data$MISSION,4,7))
	diet.data1 <- subset(diet.data,select=c(MISSION:SAMPLE_INDEX,PWT:YEAR))
	diet.data1$PREY <- diet.data[,which(colnames(diet.data)==prey.grouping)]
	da <- as.data.frame(cast(diet.data1,YEAR+MISSION+SETNO+STRAT+FLEN+SAMPLE_INDEX~PREY,value='PWT',fun.aggregate=sum))
	if(remove.singletons==1) pa <- da[,which(colSums(da>0)>1)]
	else pa <- da
	pa$FLEN <- tally.flens(pa$FLEN)
	survey.data$FWT <- a.b[1]*survey.data$FLEN^a.b[2]
	survey.data$BIOM <- survey.data$FWT*survey.data$CLEN
	pa <- aggregate(pa[,7:ncol(pa)],by=c(pa['MISSION'],pa['STRAT'],pa['SETNO'],pa['FLEN'],pa['YEAR']),FUN=mean) #mean for each flen within a set
	survey.data$FLEN <- tally.flens(survey.data$FLEN)

	survey.data <- aggregate(survey.data[c('BIOM','CLEN')],by=c(survey.data['MISSION'],survey.data['STRAT'],survey.data['SETNO'],survey.data['FLEN'],survey.data['YEAR']),FUN=sum)
	surv.data <- survey.data[survey.data$BIOM>0,]
	cdata <- merge(pa,surv.data,by=c('MISSION','SETNO','FLEN','STRAT','YEAR'),all=T) #merge with survey data to get bioms
	
					##the missing length script does not work need to fix later August 24, 2011 10:29:55 AM 
					#if(fillin.missing.lengths) {
					#cdd <- na.omit(cdata)
					#strat.means <- split(cdd,f=list(cdd$YEAR,cdd$STRAT))
					#cd <- split(cdata,f=list(cdata$YEAR,cdata$STRAT))
					#
					#cd <- rm.from.list(cd)
					#cd <- sort.list.amc(cd,3,inc=T)
					##fill in na from within strata
					#cc <- lapply(cd,fillin.mult) 
					#ccc <- do.call('rbind',cc)
					#
					##fill in any na with values from across all strata using similar lengths
					#if(any(is.na(ccc[,7:ncol(ccc)]))) { 
					#	ccd <- ccc[order(ccc$FLEN),]
					#	ccd <- fillin.mult(ccd)
					#	}
					#	}
					
	cdata <- na.omit(cdata) #mean by mission length and set 	
	#within strata mean
	ddd <- split(cdata,f=list(cdata$YEAR,cdata$STRAT,cdata$FLEN))	
	ddd <- rm.from.list(ddd)
	dd.sum <- lapply(ddd,within.strat)
	dd.sum <- do.call('rbind',dd.sum)
	#calculate strata diets based on sets where stomachs were sampled 
	##########this will change when nas are filled from the missing length script
	u <- summaryBy(data=survey.data,BIOM+CLEN~YEAR+STRAT+FLEN,FUN=c(mean,var))
	
	#combining flens using mean biomass data at flen within strat
	cc <- merge(dd.sum,u,by=c('YEAR','FLEN','STRAT'))

	
	#this is where flen groupings comes in 
	if(by.lengths) {
		lengths =as.numeric(lengths)
	if(cut.p) {
		cc$RFLEN <- ifelse(cc$FLEN<=lengths,paste("<=",lengths,sep=""),paste(">",lengths,sep=""))
		da$RFLEN <- ifelse(da$FLEN<=lengths,paste("<=",lengths,sep=""),paste(">",lengths,sep=""))
			}
	else{ #if by flen groups
		if(lengths==1) cc$RFLEN <- round.flens(cc$FLEN,grp=5) #5cm
		if(lengths==2) cc$RFLEN <- round.flens(cc$FLEN,grp=10) #10cm
		if(lengths==3) cc$RFLEN <- round.flens(cc$FLEN,grp=20) #20cm
		
		if(lengths==1) da$RFLEN <- round.flens(da$FLEN,grp=5) #5cm
		if(lengths==2) da$RFLEN <- round.flens(da$FLEN,grp=10) #10cm
		if(lengths==3) da$RFLEN <- round.flens(da$FLEN,grp=20) #20cm

		
		}
	cd1 <- rm.from.list(split(cc,f=list(cc$YEAR,cc$STRAT,cc$RFLEN)))
	cd1 <- lapply(cd1,weighting.by.biom.within.strat)
	cd1 <- do.call('rbind',cd1)
	cd1$strat.areas <- strata.data[match(cd1$STRAT,strata.data$STRAT),2]
	cd1$strat.areas <- cd1$strat.areas/(1.75*41/6080.2) #to trawlable units
	cd2 <- rm.from.list(split(cd1,f=list(cd1$YEAR,cd1$RFLEN)))
	cd2 <- lapply(cd2,combining.strata.rflen)
	cd2 <- do.call('rbind',cd2)
	daa <- aggregate(da[,1],by=c(da['YEAR'],da['RFLEN']),FUN=length)
	colnames(daa)[3] <- 'N'
	cd3 <- merge(cd2,daa,by=c('YEAR','RFLEN'))
	cd4 <- rm.from.list(split(cd3,f=list(cd3$RFLEN)))
	
	dt.by.length <- function(x) {
	z <- x[!colnames(x) %in% c('YEAR','N')]
	z <- z[1,]
	for(i in 2:ncol(z)) {
			z[,i] <- sum(x[,i+1]*x[,ncol(x)])/sum(x[,ncol(x)])
		}
		return(z)	
	}
	cd5 <- do.call('rbind',lapply(cd4,dt.by.length)) #all years combined grouped by length
	cd6 <- reshape(cd5,idvar='RFLEN',varying=list(colnames(cd5[2:ncol(cd5)])),direction='long')
	nn <- data.frame(id=1:(ncol(cd5)-1),names=colnames(cd5)[2:ncol(cd5)])
	cd6$Prey <- nn[match(cd6$time,nn$id),2]
	cd6[,5] <- cd6[grepl('var',cd6$Prey),3]
	cd7 <- cd6[!grepl('var',cd6[,4]),]
	cd7 <- cd7[,c(1,4,3,5)]
	names(cd7) <- c('FLEN','Prey','Mean','Variance')
	cd7 <- cd7[order(cd7$FLEN),]
	mult.windows()
	diet.barplot.3(cd7,10,unique(diet.data1$COMMON),prey.group=prey.grouping)
	return(cd7)
		}
	if(by.lengths==F) {
	cd <- rm.from.list(split(cc,f=list(cc$YEAR,cc$STRAT)))
	cd1 <- lapply(cd,weighting.by.biom)
	cd1 <- do.call('rbind',cd1)
	cd1$strat.areas <- strata.data[match(cd1$STRAT,strata.data$STRAT),2]
	cd1$strat.areas <- cd1$strat.areas/(1.75*41/6080.2) #to trawlable units
	cd2 <- split(cd1,f=list(cd1$YEAR))
	cd2 <- lapply(cd2,combining.strata)
	cd2 <- do.call('rbind',cd2)
	daa <- aggregate(da[,1],by=c(da['YEAR']),FUN=length)
	colnames(daa)[2] <- 'N'
	cd3 <- merge(cd2,daa,by='YEAR')
	
	#combine years weight based on sample size
	daa <- aggregate(da[,1],by=c(da['YEAR']),FUN=length)
	colnames(daa)[2] <- 'N'
	cd3 <- merge(cd2,daa,by='YEAR')
	cd4 <- cd3[1,]
	cd4 <- cd4[!colnames(cd4)%in% c('N','YEAR')]
		for(i in 1:ncol(cd4)){
		cd4[,i] <- sum(cd3[,i+1]*cd3[,ncol(cd3)])/sum(cd3[,ncol(cd3)])
		}
	 cd5 <- as.data.frame(cbind((t(cd4))))
	 cd5[,2] <- cd5[grepl('var',rownames(cd5)),]
	 cd5 <- cd5[!grepl('var',rownames(cd5)),]
	 cd5$Prey <- rownames(cd5)
	 names(cd5)[c(1,2)] <- c('Mean','Variance')
	 cd5 <- data.frame(cd5,row.names=NULL)
	 cd5 <- cd5[rev(order(cd5$Mean)),]
		 aaf <- paste(unique(cd5$Prey),collapse="','")
		 nna <- sqlQuery(channel,paste("select distinct fam, pretty_f_names from prey_spec_details where fam in ('",aaf,"');",sep=""))
		 cd5$Prey.names <- nna[match(cd5$Prey,nna$FAM),2]
	 }
	diet.barplot(cd5,10,unique(diet.data1$COMMON))
	return(cd5)
	}
	
