#@
data.src <- function() {
		srcs <- sqlQuery(channel,paste("select distinct datasource from mfd_stomach.sdinf order by datasource;"))
		a3 <- paste(srcs[,1],sep=",")
	return(a3)
	}
#@
data.year <- function(dat) {
		years <- sqlQuery(channel,paste("select distinct to_char(sdate,'yyyy') from mfd_stomach.sdinf where datasource= '",dat[1],"' and sdate is not null order by to_char(sdate,'yyyy');",sep=""))
		a3 <- paste(years[,1],sep=",")
	return(a3)
	}
#@
data.seasons <- function(dat,year) {
		season <- sqlQuery(channel,paste("select distinct decode(to_char(sdate,'mm'),'01','Winter','02','Winter','03','Winter','04','Spring','05','Spring','06','Summer','07','Summer','08','Summer','09',
			'Autumn','10','Autumn','11','Autumn','12','Winter','other') season from mfd_stomach.sdinf where datasource= '",dat[1],"' and sdate is not null and to_char(sdate,'yyyy') in ('",year,"');",sep=""))
		a3 <- paste(season[,1],sep=",")
	return(a3)
	}
#@
data.region <- function(dat=ds,strat.nafo=areas,seas=season,yrs=year) {
	yrs1 <- paste(yrs,collapse="','")
	mns <- ifelse(seas=='Winter',paste('12','01','02','03',sep="','"),ifelse(seas=='Spring',paste('04','05',sep="','"),ifelse(seas=='Summer',paste('06','07','08',sep="','"),
	ifelse(seas=='Autumn',paste('09','10','11','12',sep="','"),9999))))
	if(strat.nafo==1) {
		reg <- sqlQuery(channel,paste("select distinct strat from mfd_stomach.sdinf where datasource = '",dat[1],"' and to_char(sdate,'mm') in ('",mns,"') and to_char(sdate,'yyyy') in 
			('",yrs1,"') and strat is not null order by strat;",sep=""))
		}
	if(strat.nafo==2) {
		reg <- sqlQuery(channel,paste("select distinct nafo1 from mfd_stomach.sdinf where datasource = '",dat[1],"' and to_char(sdate,'mm') in ('",mns,"') and to_char(sdate,'yyyy') in 
			('",yrs1,"') and nafo1 is not null order by nafo1;",sep=""))		
		}
	a3 <- paste(reg[,1],sep=",")
	return(a3)
	}

#@
data.species <- function(dat=ds, strat.nafo=areas,seas=season,yrs=year,reg=regions) {
	yrs1 <- paste(yrs,collapse="','")
	reg1 <- paste(reg,collapse="','")
	mns <- ifelse(seas=='Winter',paste('12','01','02','03',sep="','"),ifelse(seas=='Spring',paste('04','05',sep="','"),ifelse(seas=='Summer',paste('06','07','08',sep="','"),
	ifelse(seas=='Autumn',paste('09','10','11','12',sep="','"),9999))))
	if(strat.nafo==1) {
		species <- sqlQuery(channel,paste("select common||'-'||count(sample_index) from mfd_stomach.sddet i,mfd_stomach.sdinf d, mflib.species_codes s where i.mission=d.mission and i.setno=d.setno and i.spec=s.research and 
			fullness in (1,2,3,4) and d.datasource='",dat[1],"' and to_char(sdate,'mm') in ('",mns,"') and to_char(sdate,'yyyy') in 
			('",yrs1,"') and strat in ('",reg1,"') group by common order by common;",sep=""))
			}
	
	if(strat.nafo==2) {
		species <- sqlQuery(channel,paste("select common||'-'||count(sample_index) from mfd_stomach.sddet i,mfd_stomach.sdinf d, mflib.species_codes s where i.mission=d.mission and i.setno=d.setno and i.spec=s.research and 
			fullness in (1,2,3,4) and d.datasource='",dat[1],"' and to_char(sdate,'mm') in ('",mns,"') and to_char(sdate,'yyyy') in 
			('",yrs1,"') and nafo1 in ('",reg1,"') group by common order by common;",sep=""))
						}
	a3 <- paste(species[,1],sep=",")
	return(a3)
	}
#@
get.diet.data <- function(dat=ds, strat.nafo=areas,seas=season,yrs=year,reg=regions,specs=spec1) {
	yrs1 <- paste(yrs,collapse="','")
	reg1 <- paste(reg,collapse="','")
	mns <- ifelse(seas=='Winter',paste('12','01','02','03',sep="','"),ifelse(seas=='Spring',paste('04','05',sep="','"),ifelse(seas=='Summer',paste('06','07','08',sep="','"),
	ifelse(seas=='Autumn',paste('09','10','11','12',sep="','"),9999))))
	specs <- strsplit(specs,"-")[[1]][1]
	specs <- sqlQuery(channel,paste("select research from mflib.species_codes where common in ('",specs,"');",sep=""))[,1]
	if(strat.nafo==1) {
		diet <- sqlQuery(channel,paste("select mission,setno,sdate,stime,slat,slong*-1,strat,nafo,depth,bottom_temperature,spec,s.common,fshno,flen,fwt,sample_index,fullness,
		preyspeccd,fam,speccd2,p.common prey_name,pwt ,plen, pnum from mfd_stomach.sdview1 i, mfd_stomach.prey_spec_details p, mflib.species_codes s where i.spec=s.research and i.preyspeccd=speccd and 
			datasource='",dat[1],"' and to_char(sdate,'mm') in ('",mns,"') and to_char(sdate,'yyyy') in ('",yrs1,"') and strat in ('",reg1,"') and spec=",specs," and preyspeccd=speccd;",sep=""))
						}
	if(strat.nafo==2) {
		diet <- sqlQuery(channel,paste("select mission,setno,sdate,stime,slat,slong*-1,strat,nafo,depth,bottom_temperature,spec,s.common,fshno,flen,fwt,sample_index,fullness,
		preyspeccd,fam,speccd2,p.common prey_name,pwt ,plen, pnum from mfd_stomach.sdview1 i, mfd_stomach.prey_spec_details p, mflib.species_codes s where i.spec=s.research and i.preyspeccd=speccd and 
			datasource='",dat[1],"' and to_char(sdate,'mm') in ('",mns,"') and to_char(sdate,'yyyy') in ('",yrs1,"') and nafo in ('",reg1,"') and spec=",specs," and preyspeccd=speccd;",sep=""))
						}
	return(diet)
	}
#@
species.accumulation <- function(data,cut.p=F) {
	
	
			if(nrow(data>5)) {
    		data$P <- 1
    		pdat <- cast(data,SAMPLE_INDEX~SPECCD2,fun.aggregate=sum,value="P")
    		if(ncol(pdat)>3 & nrow(pdat>2)) {
			abcd <- specaccum(pdat[,c(3:ncol(pdat))], method='random')
			if(cut.p) plot(abcd,ylab='N prey items',xlab='N stomachs',main=paste(unique(data$COMMON),unique(data$RFLEN),sep=""))
			else plot(abcd,ylab='N prey items',xlab='N stomachs',main=unique(data$COMMON))
			}
			}
			}
			
#@
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
#@
gs.rv.data <- function(year,area,species,season) {
	#rv numbers per tow by length
	yrs1 <- paste(year,collapse="','")
	reg1 <- paste(area,collapse="','")
	mns <- ifelse(season=='Winter',paste('12','01','02','03',sep="','"),ifelse(season=='Spring',paste('04','05',sep="','"),ifelse(season=='Summer',paste('06','07','08',sep="','"),
	ifelse(season=='Autumn',paste('09','10','11','12',sep="','"),9999))))
	specs <- strsplit(species,"-")[[1]][1]
	specs <- sqlQuery(channel,paste("select research from mflib.species_codes where common in ('",specs,"');",sep=""))[,1]
	dats <- sqlQuery(channel,paste("select year,strat,mission,setno,flen,sum(clen) clen
		 from
		 (SELECT year, icl.strat, icl.mission, icl.setno, icl.flen, DECODE(NVL(sampwgt,0),0,0,totwgt/NVL(sampwgt,0)*nvl(clen,0)*1.75/dist) clen
		 FROM
		 (SELECT mission,setno, flen, SUM(clen) clen, AVG(fwt) avg_fwt
		  FROM  groundfish.gsdet
		  WHERE flen IS NOT NULL AND spec=",specs,"
		  GROUP BY mission,setno, FLEN
		  ) d,
		 (SELECT year, mission, setno, strat, dist, totwgt, sampwgt, flen
		   FROM
		    (SELECT class, flen
		    FROM groundfish.gs_lengths
		    WHERE class=1
		     AND flen <=
		       (SELECT max(flen) + 1
		       FROM groundfish.gsdet
		       WHERE spec=",specs," AND flen IS NOT NULL
		        AND (mission, setno) IN
		          (SELECT DISTINCT i.mission, i.setno
		           FROM groundfish.gsinf i, groundfish.gsmission_list l, mflib.gsmgt m
		           WHERE i.mission=l.pk_mission AND i.strat=m.strat
		           and ( m.unit in ('",reg1,"')) AND to_char(i.sdate,'mm') in ('",mns,"')
		           AND l.year in ('",yrs1,"') AND i.type=1))) l,
		    (SELECT year, i.mission, i.setno, strat, dist, totwgt, sampwgt
		    FROM
		     (SELECT mission, setno, totwgt, sampwgt
		        FROM groundfish.gscat WHERE spec=",specs,") c,
		     (SELECT l.year, i.mission, i.setno, i.strat, dist
		        FROM groundfish.gsinf i, groundfish.gsmission_list l, mflib.gsmgt m
		      WHERE i.mission=l.pk_mission AND i.strat=m.strat
		       and m.unit in ('",reg1,"') AND to_char(i.sdate,'mm') in ('",mns,"')
		       AND l.year in ('",yrs1,"') AND i.type=1) i
		    WHERE i.mission=c.mission(+)
		     AND i.setno=c.setno(+)) ic
		 ) icl
		 WHERE icl.mission=d.mission(+) AND icl.setno=d.setno(+) AND icl.flen=d.flen(+))
		group by year,strat,mission,setno,flen;",sep=""))
		ss <- unique(dats$STRAT)
		strat <- paste(ss,collapse="','")
	wts <- sqlQuery(channel,paste("select distinct strat,area from groundfish.gsstratum where strat in ('",strat,"') order by strat;",sep=""))
	wts$wt <- wts$AREA/sum(wts$AREA)
	dd <- list(survey.data=dats,strata.weights=wts)
	return(dd)
}
#@
mean.diet <- function(diet.data,prey.grouping='FAM',remove.singletons=1,remove.influentials=1,percent.diff=50,by.lengths=F) {
		#mean diet no stratification and no length corrections
	diet.data$YEAR <- as.numeric(substr(diet.data$MISSION,4,7))
	diet.data1 <- subset(diet.data,select=c(MISSION:SAMPLE_INDEX,PWT:YEAR))
	diet.data1$PREY <- diet.data[,which(colnames(diet.data)==prey.grouping)]
	da <- as.data.frame(cast(diet.data1,YEAR+SAMPLE_INDEX~PREY,value='PWT',fun.aggregate=sum))
	if(remove.singletons==1) pa <- da[,which(colSums(da>0)>1)]
	else pa <- da
	if(nrow(pa)>1) {
	if(remove.influentials==1) {
	sa <- split(pa,pa$YEAR)
	outs <- list()
	for(k in 1:length(sa)) {
		a1 <- apply(sa[[k]],2,influential.obs,infl=percent.diff)
			inds <- list()
			for(j in 1:length(a1)) {
				if(a1[[j]]>0) {
						inds[[j]] <- data.frame(MEAN=mean(sa[[k]][-c(a1[[j]]),j]),VAR=var(sa[[k]][-c(a1[[j]]),j]),nobs=length((sa[[k]][-c(a1[[j]]),j])),nfeeding=sum((sa[[k]][-c(a1[[j]]),j]>0)),cols=j,years=k)
						}
				else {
					
					inds[[j]] <- data.frame(MEAN=mean(sa[[k]][,j]),VAR=var(sa[[k]][,j]),nobs=length((sa[[k]][,j])),nfeeding=sum((sa[[k]][,j])>0),cols=j,years=k)
					
			}
		}
		inds <- do.call(rbind,inds)
		outs[[k]] <- inds
	}
	a21 <- do.call(rbind,outs)
	nn <- data.frame(names(sa),1:length(sa))
	nb <- data.frame(colnames(pa),1:ncol(pa))
	a21$grps <- nb[match(a21$cols,nb[,2]),1]
	a21$years <- nn[match(a21$years,nn[,2]),1]
	a21$nobser <- max(a21$nobs)
	a21$nremoved <- a21$nobser-a21$nobs
	a21$nfeeding <- a21$nfeeding+a21$nremoved
	a21 <- a21[-which(a21$grps %in% c('YEAR','SAMPLE_INDEX')),-c(3,5)]
	a21[,c(1,2)] <- round(a21[,c(1,2)],digits=3)
	names(a21) <- c('Mean','Variance','Nfeeding','Year','Prey','Nobs','Nremove')
		}
	else {

		sa <- split(pa,pa$YEAR)
		outs <- list()
		souts <- list()
		louts <- list()
		nfed <- list()
		for(p in 1:length(sa)) {
			outs[[p]] <- apply(sa[[p]],2,mean)
			souts[[p]] <- apply(sa[[p]],2,var)
			louts[[p]] <- apply(sa[[p]],2,length)
			nfed[[p]] <- apply(sa[[p]],2,id.non.0)
		}
		mm <- unlist(outs);ss <- unlist(souts);ll <- unlist(louts);nf <- unlist(nfed)
		a22 <- as.data.frame(cbind(mm,ss,ll,nf,rep(colnames(sa[[1]]),length(sa)),rep(names(sa),each=ncol(sa[[1]]))))
		a22 <- a22[-which(a22$V5 %in% c('YEAR','SAMPLE_INDEX')),]
		colnames(a22) <- c('Mean','Variance','Nobs','Nfeeding','Prey','Year')
		a22 <- to.nums(a22,cols=c(1,2,3,4))
		a22[,c(1,2)] <- round(a22[,c(1,2)],digits=3)
		a21 <- a22
	}
	#overall means weighted by sample size
	ss21 <- sum(unique(a21[c('Year','Nobs')])[2])
	a23 <- a21
	a23[c('Mean.wts','Var.wts')] <- apply(a23[c('Mean','Variance')],2,function(x) x*a23$Nobs/ss21)
	a24 <- aggregate(a23[c('Mean.wts','Var.wts')],by=c(a23['Prey']),FUN=sum)
	aaf <- paste(unique(a24$Prey),collapse="','")
	a24 <- a24[rev(order(a24$Mean.wts)),]
	names(a24) <- c('Prey','Mean','Variance')
	diet.barplot(a24,10,unique(diet.data1$COMMON),error=F,prey.group=prey.grouping, by.lengths=by.lengths)
	aa43 <- a24
	return(aa43)
	}
}

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
#@
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
	
#@
prey.species <- function(group) {
	if(group=='Fin Fish') {spec=paste(10:1000,collapse=",")}
	if(group=='Shrimps') {spec=paste(2100:2415,collapse=",")}
	if(group=='Crabs') {spec=paste(2500:2570,collapse=",")}
	if(group=='Cephalopods') {spec=paste(c(4500,4504,4510,4511,4512,4513,4514,4515,4516,4519,4520,4518,4521,4529,4540,4541),collapse=",")}
	if(group=='Amphipods') {spec=paste(2600:2990,collapse=",")}
	if(group=='All') {spec=10:9000}
		
	dat <- sqlQuery(channel,paste("select distinct common, count(distinct sample_index) N from mfd_stomach.sdsto s, mflib.species_codes m where s.preyspeccd=m.research 
		and preyspeccd in (",spec,")group by common order by count(distinct sample_index) desc;",sep=""))
	dat <- dat[dat$N>1,]
	dat <- dat[order(dat$COMMON),]
	dat <- paste(dat$COMMON,dat$N,sep="-")
		return(dat)
}
#@
predators <- function(species) {
	specs <- strsplit(species,"-")[[1]][1]
	dat <- sqlQuery(channel,paste("Select to_char(sdate,'yyyy') year,nafo ,spec,count(distinct sample_index) n_preds
	 from mfd_stomach.sdview1 w, mflib.species_codes s where s.common in ('",specs,"') and research=preyspeccd group by to_char(sdate,'yyyy'),nafo,spec;",sep=""))
	spp <- unique(dat$SPEC)
	spp <- paste(spp,collapse=",")
	dd <- sqlQuery(channel,paste("select to_char(sdate,'yyyy') year,nafo1 nafo, spec, common, scientif, count(sample_index) n_examined from sddet v, sdinf i,mflib.species_codes g where i.mission=v.mission and
	i.setno=v.setno and v.spec=g.research and v.spec in (",spp,") group by v.datasource,to_char(sdate,'yyyy'),nafo1, spec, common, scientif order by to_char(sdate,'yyyy'),spec;"))
	dat <- merge(dd,dat,by=c('YEAR','NAFO','SPEC'),all=T)
	dat <- na.zero(dat)
	by.spec <- aggregate(dat[,c(6,7)],by=c(dat['SPEC']),FUN=sum)
	by.spec.year <- aggregate(dat[,c(6,7)],by=c(dat['SPEC'],dat['YEAR']),FUN=sum)
	by.spec.area <- aggregate(dat[,c(6,7)],by=c(dat['SPEC'],dat['NAFO']),FUN=sum)
	by.spec.year.area <- aggregate(dat[,c(6,7)],by=c(dat['SPEC'],dat['NAFO'],dat['YEAR']),FUN=sum)
	d1 <- list(Predation.by.Species=by.spec,Predation.by.Species.and.Time=by.spec.year,Predation.by.Species.and.Area=by.spec.area,Predation.by.Species.Area.Time=by.spec.year.area)
	return(d1)
	}
#@
rv.year <- function(season) {
	mns <- ifelse(season=='Winter',paste('12','01','02','03',sep="','"),ifelse(season=='Spring',paste('04','05',sep="','"),ifelse(season=='Summer',paste('06','07','08',sep="','"),
	ifelse(season=='Autumn',paste('09','10','11','12',sep="','"),9999))))
	dats <- sqlQuery(channel,paste("select distinct to_char(sdate,'yyyy') year from groundfish.gsinf where to_char(sdate,'mm') in ('",mns,"') order by to_char(sdate,'yyyy');",sep=""))
	}

#@
rv.data.region <- function(strat.nafo=areas,seas=season,yrs=year) {
	yrs1 <- paste(yrs,collapse="','")
	mns <- ifelse(seas=='Winter',paste('12','01','02','03',sep="','"),ifelse(seas=='Spring',paste('04','05',sep="','"),ifelse(seas=='Summer',paste('06','07','08',sep="','"),
	ifelse(seas=='Autumn',paste('09','10','11','12',sep="','"),9999))))
	if(strat.nafo==1) {
		reg <- sqlQuery(channel,paste("select distinct strat from groundfish.gsinf where to_char(sdate,'mm') in ('",mns,"') and to_char(sdate,'yyyy') in 
			('",yrs1,"') order by strat;",sep=""))
		}
	if(strat.nafo==2) {
		reg <- sqlQuery(channel,paste("select distinct unit from groundfish.gsinf i, mflib.gsmgt m where i.strat=m.strat and to_char(sdate,'mm') in ('",mns,"') and to_char(sdate,'yyyy') in 
			('",yrs1,"') and unit in ('4X','4VS','4VN','4W') order by unit;",sep=""))		
		}
	a3 <- paste(reg[,1],sep=",")
	return(a3)
	}

#@
rv.species <- function(year, season,area)
	{
	mns <- ifelse(season=='Winter',paste('12','01','02','03',sep="','"),ifelse(season=='Spring',paste('04','05',sep="','"),ifelse(season=='Summer',paste('06','07','08',sep="','"),
	ifelse(season=='Autumn',paste('09','10','11','12',sep="','"),9999))))
	yrs1 <- paste(year,collapse="','")
	reg1 <- paste(area,collapse="','")
	
	dat <- sqlQuery(channel,paste("select common from (select distinct common, count(distinct c.mission||','||c.setno) n from groundfish.gscat c,groundfish.gsinf i, mflib.species_codes d where i.mission=c.mission and i.setno=c.setno and 
	d.research=c.spec and to_char(sdate,'yyyy') in ('",yrs1,"') and to_char(sdate,'mm') in ('",mns,"') and strat in (select distinct strat from mflib.gsmgt where unit
	 in ('",reg1,"')) group by common) where n>1 order by common;",sep=""))
	}

#@
rv.data <- function(year,area,specs,season) {
	#rv numbers per tow by length
	yrs1 <- paste(year,collapse="','")
	reg1 <- paste(area,collapse="','")
	mns <- ifelse(season=='Winter',paste('12','01','02','03',sep="','"),ifelse(season=='Spring',paste('04','05',sep="','"),ifelse(season=='Summer',paste('06','07','08',sep="','"),
	ifelse(season=='Autumn',paste('09','10','11','12',sep="','"),9999))))
	specs <- sqlQuery(channel,paste("select research from mflib.species_codes where common in ('",specs,"');",sep=""))[,1]
	dats <- sqlQuery(channel,paste("select year,strat,mission,setno,flen,sum(clen) clen
		 from
		 (SELECT year, icl.strat, icl.mission, icl.setno, icl.flen, DECODE(NVL(sampwgt,0),0,0,totwgt/NVL(sampwgt,0)*nvl(clen,0)*1.75/dist) clen
		 FROM
		 (SELECT mission,setno, flen, SUM(clen) clen, AVG(fwt) avg_fwt
		  FROM  groundfish.gsdet
		  WHERE flen IS NOT NULL AND spec=",specs,"
		  GROUP BY mission,setno, FLEN
		  ) d,
		 (SELECT year, mission, setno, strat, dist, decode(totwgt,0,1,totwgt) totwgt, decode(sampwgt,0,1,sampwgt) sampwgt, flen
		   FROM
		    (SELECT class, flen
		    FROM groundfish.gs_lengths
		    WHERE class=1
		     AND flen <=
		       (SELECT max(flen) + 1
		       FROM groundfish.gsdet
		       WHERE spec=",specs," AND flen IS NOT NULL
		        AND (mission, setno) IN
		          (SELECT DISTINCT i.mission, i.setno
		           FROM groundfish.gsinf i, mflib.gsmgt m
		           WHERE i.strat=m.strat
		           and ( m.unit in ('",reg1,"')) AND to_char(i.sdate,'mm') in ('",mns,"')
		           AND to_char(sdate,'yyyy') in ('",yrs1,"') AND i.type=1))) l,
		    (SELECT year, i.mission, i.setno, strat, dist, totwgt, sampwgt
		    FROM
		     (SELECT mission, setno, totwgt, sampwgt
		        FROM groundfish.gscat WHERE spec=",specs,") c,
		     (SELECT to_char(sdate,'yyyy') year, i.mission, i.setno, i.strat, dist
		        FROM groundfish.gsinf i, mflib.gsmgt m
		      WHERE i.strat=m.strat
		       and m.unit in ('",reg1,"') AND to_char(i.sdate,'mm') in ('",mns,"')
		       AND to_char(sdate,'yyyy') in ('",yrs1,"') AND i.type=1) i
		    WHERE i.mission=c.mission(+)
		     AND i.setno=c.setno(+)) ic
		 ) icl
		 WHERE icl.mission=d.mission(+) AND icl.setno=d.setno(+) AND icl.flen=d.flen(+))
		group by year,strat,mission,setno,flen;",sep=""))
		ss1 <- unique(dats$STRAT)
	strat <- paste(ss1,collapse="','")
	wts <- sqlQuery(channel,paste("select distinct strat,area/((41./1000.0/6080.2)*1.75/1000.0)/1000000 tarea from groundfish.gsstratum where strat in ('",strat,"') order by strat;",sep=""))
	dd <- list(survey.data=dats,strata.weights=wts)
	return(dd)
	
}

#@
stratified.estimates <- function(survey.dat=diet.data$rv.data.by.length$survey.data,strata.dat=diet.data$rv.data.by.length$strata.weights,by.lengths=F,cut.p=F,lengths) {
	survey.dat <- merge(survey.dat,strata.dat,by='STRAT')
	ss <- to.nums(as.data.frame(do.call('rbind',(strsplit(unique(paste(survey.dat$YEAR,survey.dat$STRAT,survey.dat$TAREA,sep="-")),"-")))),c(1,2,3))
	names(ss) <- c("YEAR","STRAT","TAREA")
	wts <- split(ss,f=ss$YEAR)
	sr <- function(wts) {
		wts$wt <- wts$TAREA/sum(wts$TAREA)
		return(wts)
	}
	wts <- lapply(wts,sr)
	wts <- do.call('rbind',wts)
	tots <- aggregate(wts$TAREA,by=wts['YEAR'],sum)
	survey.dat <- merge(survey.dat,wts,by=c('YEAR','STRAT','TAREA'))
	if(by.lengths==F) {
	data <- aggregate(survey.dat['CLEN'],c(survey.dat['YEAR'],survey.dat['STRAT'],survey.dat['SETNO'],survey.dat['wt'],survey.dat['MISSION']),FUN=sum)
	dats <- split(data,f=data$YEAR)
		means <- function(data) {
				dat <- summaryBy(data=data,CLEN~YEAR+STRAT+wt,FUN=c(mean,length,var))
				}
	dat.sum <- lapply(dats,means)
		strat.vals <- function(data) {
			data <- na.omit(data) #remove strata if only one observation
			data$wt <- data$wt/sum(data$wt) #reweight if removed strata
			o.mean <- sum(data$wt*data$CLEN.mean)
			o.sd <- sqrt(sum(data$wt^2*(data$CLEN.var/data$CLEN.length)))
			dd <- c(o.mean=o.mean,o.sd=o.sd)
			}
	o.dat <- lapply(dat.sum,strat.vals)
	}
	if(by.lengths) {
			if(cut.p) {
					survey.dat$RFLEN <- ifelse(survey.dat$FLEN<=lengths,paste("<=",lengths,sep=""),paste(">",lengths,sep=""))
					}
			if(cut.p==F) {
						if(lengths==1) survey.dat$RFLEN <- round.flens(survey.dat$FLEN,grp=3) #3cm
						if(lengths==2) survey.dat$RFLEN <- round.flens(survey.dat$FLEN,grp=5) #5cm
						if(lengths==3) survey.dat$RFLEN <- round.flens(survey.dat$FLEN,grp=10) #10cm
						if(lengths==4) survey.dat$RFLEN <- round.flens(survey.dat$FLEN,grp=20) #20cm
				}
				survey.dat <- aggregate(survey.dat['CLEN'],c(survey.dat['YEAR'],survey.dat['STRAT'],survey.dat['SETNO'],survey.dat['wt'],survey.dat['MISSION'],survey.dat['RFLEN']),FUN=sum)
				dats <- split(survey.dat,f=list(survey.dat$YEAR,survey.dat$RFLEN))
				means <- function(data) {
				dat <- summaryBy(data,CLEN~YEAR+STRAT+wt,FUN=c(mean,length,var))
				}
				dat.sum <- lapply(dats,means)
		strat.vals <- function(data) {
			data <- na.omit(data) #remove strata if only one observation
			data$wt <- data$wt/sum(data$wt) #reweight if removed strata
			o.mean <- sum(data$wt*data$CLEN.mean)
			o.sd <- sqrt(sum(data$wt^2*(data$CLEN.var/data$CLEN.length)))
			dd <- c(o.mean=o.mean,o.sd=o.sd)
			}
	o.dat <- lapply(dat.sum,strat.vals)
		}
		dd <- as.data.frame(list.names.to.columns(o.dat))
		if(ncol(dd)==5) {names(dd) <- c('Mean','SD','Lab','Year','Flen.Group');dd <- to.nums(dd,c(1,2,4))}
		if(ncol(dd)<4) {names(dd) <- c('Mean','SD','Year');dd <- to.nums(dd,c(1,2,3))}
		names(tots) <- c('Year','Area')
		dd <- merge(dd,tots,by='Year')
		dd$Total <- dd$Mean*dd$Area
		dd$Total.SD <- dd$SD*dd$Area
		return(dd)
		}

#@
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

#@
influential.obs <- function(x,infl=50){
	##identify influential observation based on their percent difference in mean from the overall
	#only if there are greater than 2 times this prey is observed as less than that it becomes a problem
  n <- length(x)
  n1 <- sum(x>0) 
  e <- c()
  e[1] <- 0
  if(n1>2) {
  		b <- mean(x)
		a <- c()
  		for(i in 1:n) {
  				a[i] <- mean(x[-i])
  				}
  		cd <- abs(b-a)/mean(a,b)*100 # % diff in mean between leave one out and overall
  	 	if(any(cd>infl)) {
	  			e <- which(cd>infl)
	  		
  		}
	 	}
	 	return(e)
	}

#@
a.b <- function(specs,year=ddd,area,season,stat.num=5,plots=T,diet.analysis=F) {
			if(diet.analysis) {specs <- strsplit(specs,"-")[[1]][1]}
						yrs1 <- paste(year,collapse="','")
				reg1 <- paste(area,collapse="','")
				Qhigh <- 1-(stat.num/200)
				Qlow <- (stat.num/200)
				mns <- ifelse(season=='Winter',paste('12','01','02','03',sep="','"),ifelse(season=='Spring',paste('04','05',sep="','"),ifelse(season=='Summer',paste('06','07','08',sep="','"),
				ifelse(season=='Autumn',paste('09','10','11','12',sep="','"),9999))))
				specs <- sqlQuery(channel,paste("select research from mflib.species_codes where common in ('",specs,"');",sep=""))[,1]
				
				dats <- sqlQuery(channel,paste("select flen,fwt/1000 fwt from groundfish.gsdet where spec=",specs," and mission||','||setno in 
				(select mission||','||setno from groundfish.gsinf where to_char(sdate,'mm') in ('",mns,"') and strat in 
				(select distinct strat from mflib.gsmgt where unit in ('",reg1,"') and to_char(sdate,'yyyy') in ('",yrs1,"')));",sep=""))
				dats <- na.omit(dats)
				#name the species
					nn <- sqlQuery(channel,paste("select common from mflib.species_codes where research=",specs,";",sep=""))
			#nls regression
			
				fit <- nls(FWT~a*FLEN^b,dats,start=list(a=0.01,b=3.3))
				coef.fits <- coef(fit)
			if(diet.analysis) {return(coef.fits)}
			
						#bootstrapped confidence intervals
				boots <- nlsBoot1(fit,dats,Qlow1=Qlow,Qhigh1=Qhigh)
				x <- seq(min(dats[,1]),max(dats[,1]),by=0.1)
				xa <- seq(min(dats[,1]),max(dats[,1]),by=1)
			
			#polygon choice
			pp1 <- data.frame(x1=c(x,rev(x)),y1=c(boots$bootCI[1,2]*x^boots$bootCI[2,2],boots$bootCI[1,3]*rev(x)^boots$bootCI[2,3]))
			pp2 <- data.frame(Length=xa,lower.wt=boots$bootCI[1,2]*xa^boots$bootCI[2,2],upper.wt=boots$bootCI[1,3]*(xa)^boots$bootCI[2,3])
			polys <- paste('stat -',Qlow,',',Qhigh)
			
			#plotting
			if(plots) {
			plot(1,1,xlim=c(min(pp1$x1),max(pp1$x1)),ylim=c(min(pp1$y1),max(pp1$y1)),xlab='Length',ylab='Weight',type='n')
			with(pp1,polygon(x1,y1,col='grey'))
			curve(coef(fit)[1]*x^coef(fit)[2],lwd=3,add=T,col='red')
			points(dats[,1],dats[,2],xlab='Length',ylab='Wt',col='black')
			text(x=min(pp1$x1)+10,y=max(pp1$y1)-max(pp1$y1)*0.05,paste('N=',nrow(dats)))
			xe <- bquote(.(round(coef(fit)[1],3))~Length^.(round(coef(fit)[2],2)))
			mtext(side=3,line=0,xe)
			title(paste(nn[,1],";",year[1],"-",year[length(year)]))
			}
			
			regs <- inout(dats,pp1)
			outs <- length(regs[regs==F])
			abc <- list(Polygon.method.level=polys,N.Measured=nrow(dats),N.Outside.of.Polygon=outs,boot.ests=boots$bootCI,Min.Max.Wts=pp2)
			return(abc)
}

#@
vb.spps <- 	function() {
spp.choose <- sqlQuery(channel,paste("select distinct common from groundfish.gsdet d, mflib.species_codes s where s.research=d.spec and age is not null;"))
	return(spp.choose[,1])
	}

#@
vb.season <- function(species) {
sea <- sqlQuery(channel,paste("select distinct decode(to_char(sdate,'mm'),1,'Winter',2,'Winter',3,'Winter',4,'Spring',5,'Spring',6,'Summer',7,'Summer',8,'Summer',9,'Autumn',10,
'Autumn',11,'Autumn',12,'Winter',9999) ss from groundfish.gsinf i, groundfish.gsdet d, mflib.species_codes c where i.mission=d.mission and i.setno=d.setno and d.spec=c.research and 
c.common='",species,"' and age is not null;",sep=""))
return(sea[,1])
}

#@
vb.year <- function(species,season){
				mns <- ifelse(season=='Winter',paste('12','01','02','03',sep="','"),ifelse(season=='Spring',paste('04','05',sep="','"),ifelse(season=='Summer',paste('06','07','08',sep="','"),
				ifelse(season=='Autumn',paste('09','10','11','12',sep="','"),9999))))
	ss <- sqlQuery(channel,paste("select distinct to_char(sdate,'yyyy') year from groundfish.gsinf i, groundfish.gsdet d, 
	mflib.species_codes c where i.mission=d.mission and i.setno=d.setno and d.spec=c.research and c.common='",species,"' and to_char(sdate,'mm') in ('",mns,"') and age is not null
	order by to_char(sdate,'yyyy');",sep=""))
return(ss[,1])
}	

#@
vb.region <- function(strat.nafo,seas,yrs=year,sp=species) {
	mns <- ifelse(seas=='Winter',paste('12','01','02','03',sep="','"),ifelse(seas=='Spring',paste('04','05',sep="','"),ifelse(seas=='Summer',paste('06','07','08',sep="','"),
	ifelse(seas=='Autumn',paste('09','10','11','12',sep="','"),9999))))
	if(strat.nafo==1) {
		reg <- sqlQuery(channel,paste("select distinct strat from groundfish.gsinf i, groundfish.gsdet d , mflib.species_codes c where i.mission=d.mission and i.setno=d.setno and 
		d.spec=c.research and common ='",sp,"' and to_char(sdate,'mm') in ('",mns,"') and to_char(sdate,'yyyy') between ",yrs[1]," and ",yrs[2]," order by strat;",sep=""))
		}
	if(strat.nafo==2) {
		reg <- sqlQuery(channel,paste("select distinct unit from groundfish.gsinf i, groundfish.gsdet d , mflib.species_codes c, mflib.gsmgt m where i.strat=m.strat and i.mission=d.mission and i.setno=d.setno and 
		d.spec=c.research and common ='",sp,"' and to_char(sdate,'mm') in ('",mns,"') and to_char(sdate,'yyyy') between ",yrs[1]," and ",yrs[2]," and unit in ('4X','4VS','4VN','4W') order by unit;",sep=""))
		}
	a3 <- paste(reg[,1],sep=",")
	return(a3)
	}

#@
LVB <- function(species,area,syear,eyear,season, plot=T,add.plot=F,line.col='blue',init.pars=list(hinf=0, K=0, t0=0), NLS=T,MLE=F, method=c('BFGS','CG','SANN'),cohort=F,
	compare=F,species2,area2,syear2,eyear2,season2, init.pars2=list(hinf=0, K=0, t0=0),cohort2,control = list(maxiter = 10000, minFactor = 1/2024, tol = 1e-05),means=F,error.kimura.c,
	means2=F,error.kimura.c2)
			{
				mns <- ifelse(season=='Winter',paste('12','01','02','03',sep="','"),ifelse(season=='Spring',paste('04','05',sep="','"),ifelse(season=='Summer',paste('06','07','08',sep="','"),
				ifelse(season=='Autumn',paste('09','10','11','12',sep="','"),9999))))
				area <- paste(area,collapse="','")
	
          
          
          species <- sqlQuery(channel,paste("select distinct research from mflib.species_codes where common ='",species,"';",sep=""))[,1]
          if(cohort==F) {
          dat <- sqlQuery(channel,paste("select flen,age from groundfish.gsdet d, groundfish.gsinf i where i.mission=d.mission and i.setno=d.setno and 
          to_char(sdate,'yyyy') between ",syear," and ",eyear," and to_char(sdate,'mm') in ('",mns,"') and strat in (select distinct strat from mflib.gsmgt where unit in ('",area,"')) and spec in ",species," and age is not null;",sep=""))
          }
          if(cohort) {
          dat <- sqlQuery(channel,paste("select to_char(sdate,'yyyy') year, flen,age from groundfish.gsdet d, groundfish.gsinf i 
          where i.mission=d.mission and i.setno=d.setno and strat in (select distinct strat from mflib.gsmgt where unit in ('",area,"')) and to_char(sdate,'mm') in ('",mns,"')
          and spec in ",species," and age is not null;",sep=""))
          yrs <- seq(syear, as.numeric(format(Sys.Date(),'%Y')),by=1)
          ages <- seq(1:length(yrs))
          iids <- paste(yrs,ages,sep="-")
          dat$iids <- paste(dat$YEAR,dat$AGE,sep="-")
          dat <- dat[dat$iids %in% iids,]
          }
          dat <- na.omit(dat)
          if(nrow(dat)<15) {stop('Not enough data points to fit')}
          
	if(nrow(dat)>15) {
  parameters <- c()
  dat$wgts <- 1
  if(means) {
  	#use only mean at length for model
  	a1 <- aggregate(dat$FLEN,by=list(dat$AGE),FUN=mean)
  	a2 <- aggregate(dat$FLEN,by=list(dat$AGE),FUN=var)
  	a3 <- aggregate(dat$FLEN,by=list(dat$AGE),FUN=length)
  	ad1 <- merge(a1,a2,by='Group.1')
  	dat <- merge(ad1,a3,by='Group.1')
  	dat[is.na(dat)] <- 1
  	names(dat) <- c('AGE','FLEN','VAR','N')
  	if(error.kimura.c) dat$wgts <- dat$N/dat$VAR
  	if(error.kimura.c==F) dat$wgts <- 1
  	}
	A <- sort(unique(dat$AGE))
	# LVB model
			
	 	# initial parameters
	plot(dat$AGE,dat$FLEN,xlab='Age',ylab='Fish Length')
 		a <- sort(unique(dat$AGE))
	 	mh <- c(); mi <- c()
	 	for(j in 1:length(a)){mh[j] <- mean(dat$FLEN[dat$AGE==a[j]],na.rm=T)}
	 	for(i in 2:(length(mh))) {mi[i] <- mh[i]-mh[i-1]}
	 	if(any(na.omit(mi<0))) {
	 		aa <- which(mi<0)
	 		for(k in 1:length(aa)) {
	 		if(aa[k]==length(mi)) {
	 		mh <- mh[-length(mh)]
	 		a <- a[-length(a)]
	 		}
	 	else{
	 		mh[aa[k]] <- mean(c(mh[aa[k]-1],mh[aa[k]+1]))
	 		}
	 		}
	 		}
	 	walford <- lm(mh[2:length(mh)]~mh[1:(length(mh)-1)])
	 	hinf <- walford$coefficients[1]/(1-walford$coefficients[2])
	 	K <- (walford$coefficients[2])
	 	t0 <- 0
	 	if(init.pars$hinf==0) init.pars <- list(hinf=hinf, K=K, t0=t0)
		init.pars$K <- ifelse(init.pars$K>.4,0.1,init.pars$K)
		init.pars$hinf <- ifelse(init.pars$hinf>300,300,init.pars$hinf)
		


		AGE <- dat$AGE
		FLEN <- dat$FLEN

 	 	if(NLS) {
		lvb.fit <- summary(nls(FLEN~hinf*(1-exp(-K*(AGE-t0))), data = dat,weights=wgts,start = init.pars,control=control))
		parameters <- as.data.frame(lvb.fit$parameters)
		}
		
		if(MLE) {
		
		 vonb = function(init.pars)
 			 {
 			with(as.list(init.pars),{
 					la = hinf * (1 - exp(-K *(AGE-t0)))
 					epsilon = FLEN - la
 					std = sd(epsilon)
 					nloglike = -sum(dnorm(epsilon,0,std,log=T))
 		 		return(list(nloglike=nloglike,la=la))
 			})
 			}
 
 		solver = function(pars,fn,hess=TRUE)
 				{
					fit = optim(pars,fn,method=method,hessian=hess)
					if(hess){	fit$VarCov = solve(fit$hessian) 			#Variance-Covariance
					fit$SDs = sqrt(diag(fit$V))				#Standard deviations
					fit$Correlations = fit$V/(fit$S %o% fit$S) }	#Parameter correlation
				return(fit)
		}
		fn1 = function(init.pars) vonb(init.pars)$nloglike 

		fit = solver(init.pars,fn1)
		parameters <- as.data.frame(cbind(Estimate=fit$par,Std.Error=fit$SDs))
				}
	if(compare) {
					mns2 <- ifelse(season2=='Winter',paste('12','01','02','03',sep="','"),ifelse(season2=='Spring',paste('04','05',sep="','"),ifelse(season2=='Summer',paste('06','07','08',sep="','"),
				ifelse(season2=='Autumn',paste('09','10','11','12',sep="','"),9999))))
				area2 <- paste(area2,collapse="','")
	
          species2 <- sqlQuery(channel,paste("select distinct research from mflib.species_codes where common ='",species2,"';",sep=""))[,1]
          if(cohort==F) {
          dat2 <- sqlQuery(channel,paste("select flen,age from groundfish.gsdet d, groundfish.gsinf i where i.mission=d.mission and i.setno=d.setno and 
          to_char(sdate,'yyyy') between ",syear2," and ",eyear2," and to_char(sdate,'mm') in ('",mns2,"') and strat in (select distinct strat from mflib.gsmgt where unit in ('",area2,"')) and spec in ",species2," and age is not null;",sep=""))
          }
          if(cohort) {
          dat2 <- sqlQuery(channel,paste("select to_char(sdate,'yyyy') year, flen,age from groundfish.gsdet d, groundfish.gsinf i 
          where i.mission=d.mission and i.setno=d.setno and strat in (select distinct strat from mflib.gsmgt where unit in ('",area2,"')) and to_char(sdate,'mm') in ('",mns2,"')
          and spec in ",species2," and age is not null;",sep=""))
          yrs <- seq(syear, as.numeric(format(Sys.Date(),'%Y')),by=1)
          ages <- seq(1:length(yrs))
          iids <- paste(yrs,ages,sep="-")
          dat2$iids <- paste(dat2$YEAR,dat2$AGE,sep="-")
          dat2 <- dat[dat$iids %in% iids,]
          }
          dat2 <- na.omit(dat2)
          if(nrow(dat2)<15) {stop('Not enough data points to fit')}
          
	if(nrow(dat2)>15) {
  dat2$wgts <- 1
  if(means2) {
	  #use only mean at length for model
  	a1 <- aggregate(dat2$FLEN,by=list(dat2$AGE),FUN=mean)
  	a2 <- aggregate(dat2$FLEN,by=list(dat2$AGE),FUN=var)
  	a3 <- aggregate(dat2$FLEN,by=list(dat2$AGE),FUN=length)
  	ad1 <- merge(a1,a2,by='Group.1')
  	dat2 <- merge(ad1,a3,by='Group.1')
  	dat2[is.na(dat2)] <- 1
  	names(dat2) <- c('AGE','FLEN','VAR','N')
  	if(error.kimura.c2) dat2$wgts <- dat2$N/dat2$VAR
  	if(error.kimura.c2==F) dat2$wgts <- 1
  	}
	A <- sort(unique(dat2$AGE))
	# LVB model
			
	 	# initial parameters
	plot(dat2$AGE,dat2$FLEN,xlab='Age',ylab='Fish Length')
 		a <- sort(unique(dat2$AGE))
	 	mh <- c(); mi <- c()
	 	for(j in 1:length(a)){mh[j] <- mean(dat2$FLEN[dat2$AGE==a[j]],na.rm=T)}
	 	for(i in 2:(length(mh))) {mi[i] <- mh[i]-mh[i-1]}
	 	if(any(na.omit(mi<0))) {
	 		aa <- which(mi<0)
	 		for(k in 1:length(aa)) {
	 		if(aa[k]==length(mi)) {
	 		mh <- mh[-length(mh)]
	 		a <- a[-length(a)]
	 		}
	 	else{
	 		mh[aa[k]] <- mean(c(mh[aa[k]-1],mh[aa[k]+1]))
	 		}
	 		}
	 		}
	 	walford <- lm(mh[2:length(mh)]~mh[1:(length(mh)-1)])
	 	hinf <- walford$coefficients[1]/(1-walford$coefficients[2])
	 	K <- (walford$coefficients[2])
	 	t0 <- 0
	 	if(init.pars2$hinf==0) init.pars2 <- list(hinf=hinf, K=K, t0=t0)
		init.pars2$K <- ifelse(init.pars2$K>.4,0.1,init.pars2$K)
		init.pars2$hinf <- ifelse(init.pars2$hinf>300,300,init.pars2$hinf)

		AGE <- dat2$AGE
		FLEN <- dat2$FLEN
 	 	lvb.fit <- summary(nls(FLEN~hinf*(1-exp(-K*(AGE-t0))), data = dat2,weights=wgts,start = init.pars2,control=control))
		parameters2 <- as.data.frame(lvb.fit$parameters)
		print(parameters2)
	   }

	if(ncol(dat)>4) dat <- dat[,colnames(dat) %in% c('AGE','FLEN','wgts')]
	if(ncol(dat2)>4) dat2 <- dat2[,colnames(dat2) %in% c('AGE','FLEN','wgts')]
		dat$cat <- 1
		dat2$cat <- 0
		dat3 <- as.data.frame(rbind(dat,dat2))
		hinf1 <- parameters[1,1]
		K1 <- parameters[2,1]
		t01 <- parameters[3,1]
		hinf.d <- abs(parameters[1,1]-parameters2[1,1])
		K.d <- abs(parameters[2,1]-parameters2[2,1])
		t0.d <- abs(parameters[3,1]-parameters2[3,1])
		hinf.both <- mean(c(parameters[1,1],parameters2[1,1]))
		K.both <- mean(c(parameters[2,1],parameters2[2,1]))
		t0.both <- mean(c(parameters[3,1],parameters2[3,1]))
	  Ho <- nls(FLEN ~ (hinf + ls * cat) * (1 - exp(-(K + ks * cat) * 
		    (AGE - (t0 + ts * cat)))), data = dat3, start = list(hinf = hinf1, 
		    ls = hinf.d, K = K1, ks = K.d, t0 = t01, ts = t0.d), control = control)
  	resid0 <- residuals(Ho)
  	H1 <- nls(FLEN ~ hinf * (1 - exp(-(K + ks * cat) * (AGE - 
		    (t0 + ts * cat)))), data = dat3, start = list(hinf = hinf.both, 
		    K = K1, ks = K.d, t0 = t01, ts = t0.d), control = control)
  	resid1 <- residuals(H1)
  	H2 <- nls(FLEN ~ (hinf + ls * cat) * (1 - exp(-K * (AGE - 
		    (t0 + ts * cat)))), data = dat3, start = list(hinf = hinf1, 
		    ls = hinf.d, K = K.both, t0 = t01, ts = t0.d), control = control)
	  resid2 <- residuals(H2)
	  H3 <- nls(FLEN ~ (hinf + ls * cat) * (1 - exp(-(K + ks * cat) * (AGE - t0))), data = dat3, start = list(hinf = hinf1, ls = hinf.d, K = K1, ks = K.d, t0 = t0.both), control = control)
	  resid3 <- residuals(H3)
	  H4 <- nls(FLEN ~ hinf * (1 - exp(-K * (AGE - t0))), data = dat3,start = list(hinf = hinf.both, K = K.both, 
	      t0 = t0.both), control = control)
	  resid4 <- residuals(H4)
  RSS <- c(sum(residuals(Ho)^2), sum(residuals(H1)^2), sum(residuals(H2)^2), 
    sum(residuals(H3)^2), sum(residuals(H4)^2))
  N <- length(residuals(Ho))
  X <- round(c(-N * log(RSS[1]/RSS[2]), -N * log(RSS[1]/RSS[3]), 
    -N * log(RSS[1]/RSS[4]), -N * log(RSS[1]/RSS[5])), 2)
  df <- c(length(coef(Ho)) - length(coef(H1)), length(coef(Ho)) - 
    length(coef(H2)), length(coef(Ho)) - length(coef(H3)), 
    length(coef(Ho)) - length(coef(H4)))
  p <- round(1 - pchisq(X, df), 6)
  labs <- c("Ho", "H1", "H2", "H3", "H4")
  hyp <- c("Linf1=Linf2", "K1=K2", "t01=t02", "Linf1=Linf2,K1=K2,t01=t02")
  labels <- c("Ho vs H1", "Ho vs H2", "Ho vs H3", "Ho vs H4")
  compout <- data.frame(tests = labels, hypothesis = hyp, chisq = X, 
    df = df, p = p)
  rss <- as.data.frame(cbind(labs, RSS))
  names(rss) <- c("model", "rss")
  residuals_all <- as.data.frame(cbind(resid0, resid1, resid2, 
    resid3, resid4))
  nlsout <- list(compout, summary(Ho), summary(H1), summary(H2), 
    summary(H3), summary(H4), rss)
  names(nlsout) <- c("results", c(paste("model", labs)), "rss")
  xlims=c(0,max(max(dat$AGE),max(dat2$AGE)))
  ylims=c(0,max(max(dat$FLEN),max(dat2$FLEN)))
  plot(dat$AGE,dat$FLEN,ylab='Fish Length',xlab='Age',col='blue',pch='.',cex=3,xlim=xlims,ylim=ylims)
  points(dat2$AGE,dat2$FLEN,col='red',pch='.',cex=3)
	ht <- parameters[1,1]*(1-exp(-parameters[2,1]*(A-parameters[3,1])))   # von Bertalanffy equation #
	lines(A,ht,lwd=2,col='blue')
	ht <- parameters2[1,1]*(1-exp(-parameters2[2,1]*(A-parameters2[3,1])))   # von Bertalanffy equation #
	lines(A,ht,lwd=2,col='red')
	legend('bottomright',col=c('red','blue'),lty=c(1,1),pch=c(1,1),c('Group 2','Group 1'),bty='n',cex=0.7)
  return(nlsout)
			}	
				

	if(plot){
	if(add.plot) 
	{	points(dat$AGE,dat$FLEN,col=line.col,pch='.',cex=3)
		ht <- parameters[1,1]*(1-exp(-parameters[2,1]*(A-parameters[3,1])))   # von Bertalanffy equation #
		lines(A,ht,lwd=2,col=line.col)
		}
	else {plot(dat$AGE,dat$FLEN,ylab='Fish Length',xlab='Age',col='black',pch='.',cex=3)
	ht <- parameters[1,1]*(1-exp(-parameters[2,1]*(A-parameters[3,1])))   # von Bertalanffy equation #
	lines(A,ht,lwd=2,col='red')
		
		}
	
	return(parameters)
}
}
}

#@
to.nums <- function(x,cols,numerics=T) {
	for(i in 1:length(cols)) {
	if(is.factor(x[,cols[i]])) {
	if(numerics==F){
	x[,cols[i]] <- as.character(levels(x[,cols[i]])[x[,cols[i]]])
	}
	else {
	x[,cols[i]] <- as.numeric(levels(x[,cols[i]])[x[,cols[i]]])
	}
	}
	else {
	x[,cols[i]] <- as.numeric(x[,cols[i]])
	}
	}
	return(x)
}

#@
round.flens <- function(x,grp) {
		if(grp==3) a <- floor(x/3)*3+1    #3cm
		if(grp==5) a <- floor(x/5)*5+2    #5cm
		if(grp==10) a <- floor(x/10)*10+5   #10cm
		if(grp==20) a <- floor(x/20)*20+10  #20cm
		return(a)
				}  
#@
tally.flens <- function(x) {
	#makes the flen groups same as tally sheets from subsampling forms
	a <- cbind(1:200,c(rep(5,10),rep(seq(13,98,by=5),each=5),rep(100,100)))
	b <- a[match(x,a[,1]),2]
	return(b)
	}

#@
id.non.0 <- function(x) {
	#function to find out the number of eating fish needed for apply function
		a <- sum(x>0)
		return(a)	
	}

#@
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


#@
mult.windows <- function(mars=par()$mar,mfrows=par()$mfrow) {
		graphics.off()
		if(exists(".SavedPlots",where=1)==T){rm(.SavedPlots,pos=1)}
		par(mar=mars,mfrow=mfrows)
		windows(record=T)
		}
#@
save.to.csv <- function(data) {
	a <- getwd()
	b <- Sys.time()
	b <- format(b,format="%m-%d %H%M",tz='America/Halifax')
	e <- unique(data$COMMON)
	f <- paste("Diet analysis ",e," ", b,".csv",sep="")
	if(is.list(data) & any(names(data)=='mean.diet')) {
		data <- as.data.frame(data$mean.diet)
		}
	if(is.list(data) & is.list(data)[1] & any(names(data)[1]=='mean.diet')) {
		out <- list()
		ad <- names(data)
		for(i in 1:length(ad)) {
			out[[ad[i]]] <- data[[ad[i]]]$mean.diet	
			}
			out <- list.names.to.columns(out)
			names(out)[ncol(out)] <- 'Length_Group'
			data <- out
			}
	if(!is.data.frame(data)){
		for(i in 1:length(data)) {
			if( !is.null(data[[i]])) {
				if(nrow(data[[i]])>=1) {
 					data[[i]][,ncol(data[[i]])+1] <- names(data[i])
						}
					}
				}
			data <- do.call(rbind,data)
			names(data)[ncol(data)] <- 'Length_Group'
			}		
		write.csv(file=f,data, row.names=F)
	cat("\n")
	cat('Your file is in:\n')
	cat(paste(a,"/",f,sep=""))
	cat("\n")
	cat("------------------------------- \n")
  cat("\n")
}

#@
sort.list.amc <- function(list1,r,inc=T) { #sorts all elements in a list based on the column r
	#r=column number for sort
	a <- length(list1)
	b <- list1
	for(i in 1:a) {
	ee <- list1[[i]]
	if(inc) {b[[i]] <- ee[order(ee[,r]),]}
	else {b[[i]] <- ee[rev(order(ee[,r])),]}
	}
	return(b)
}

#@
dim.list <- function(list1) {
 ab <- matrix(nrow=length(list1),ncol=2)
for(i in 1:length(list1)) {
	ab[i,] <- (dim(list1[[i]]))
	}
	return(ab)
	}

#@
rm.from.list <- function(list1) {
	#removes elements from list that contain no information
	a <- dim.list(list1)
	if(any(a[,1]==0)) {
	list1 <- list1[-which(a[,1]==0)]
		}
	return(list1)
	}

#@
fillin <- function(l,d) {
	#uses the slope of relation bwn pwt and len to fill in missing
	#l=length column
	#d=diet column
	#
	a1 <- as.data.frame(cbind(l,d))
	a1 <- a1[order(a1[,1]),]
	ind <- 0
	ind <- which(!complete.cases(a1))
	ind2 <- which(complete.cases(a1))
		if(length(ind)==0) {
		return(a1)		
		}
		if(length(ind)>0) {
		for(i in 1:length(ind)) {
			if(ind[i]==1) {
				a1[1,2] <- 0+(((a1[min(ind2),2]-0)/(a1[min(ind2),1]-0))*(a1[ind[i],1]-0)) #if first entry replace with lowest available entry regresssed against 0 ie the closer to 0 length the closer to 0 diet
				
			}
			if(ind[i]>max(ind2)) {
				 a1[ind[i],2] <- a1[max(ind2),2] #if last entry replace with last available entry
			 }
		if(ind[i]!=1 & i<length(ind) & ind[i]+1==ind[i+1] & ind[i]<max(ind2)) {
				next.ind <- min(ind2[which(ind2>ind[i])])
				a1[ind[i],2] <- a1[ind[i]-1,2]+(((a1[next.ind,2]-a1[ind[i]-1,2])/(a1[next.ind,1]-a1[ind[i]-1,1]))*(a1[ind[i],1]-a1[ind[i]-1,1])) 
			}
			
		if(ind[i]!=1 & ind[i]!=nrow(a1) & i<length(ind) & ind[i]+1!=ind[i+1]){
					a1[ind[i],2] <- a1[ind[i]-1,2]+(((a1[ind[i]+1,2]-a1[ind[i]-1,2])/(a1[ind[i]+1,1]-a1[ind[i]-1,1]))*(a1[ind[i],1]-a1[ind[i]-1,1])) 
		}
		if(ind[i]!=1 & ind[i]!=nrow(a1) & i==length(ind))
			
		a1[ind[i],2] <- a1[ind[i]-1,2]+(((a1[ind[i]+1,2]-a1[ind[i]-1,2])/(a1[ind[i]+1,1]-a1[ind[i]-1,1]))*(a1[ind[i],1]-a1[ind[i]-1,1])) 
	}
	return(a1)
}	
}

#@
fillin.mult <- function(data) {
	data.return <- data
	data.return[,c(7:ncol(data.return))] <- NA
	for(i in 7:(ncol(data))) {
	data.return[,i] <- fillin(data[,3],data[,i])[,2]
	}
	return(data.return)
}

#@
weighting.number.per.flen.per.tow <- function(data) {
	data1 <- split(data,data$SETNO)
	for(i in 1:length(data1)) {
	data1[[i]]$WTS <- data1[[i]]$CLEN/sum(data1[[i]]$CLEN)
	}
	data1 <- do.call('rbind',data1)
	return(data1)
	}

#@
list.names.to.columns <- function(data) {
	for(i in 1:length(data)) {
			if( !is.null(data[[i]])) {
				data[[i]][length(data[[i]])+1] <- names(data[i])
				if(grepl('\\.',length(data[[i]])+1)) {a <- unlist(strsplit(data[[i]][length(data[[i]])],split="\\."))
				 data[[i]] <- as.vector(c(data[[i]],(a)))
			 }
						}
					}
			data <- do.call(rbind,data)
			return(data)
		}

#@
nlsBoot1 <- function (nls, data1,niter = 999,Qlow1, Qhigh1) 
{
  if (!inherits(nls, "nls")) 
    stop("Use only with 'nls' objects")
      data2 <- data1
  fitted1 <- fitted(nls)
  resid1 <- resid(nls)
  var1 <- all.vars(formula(nls)[[2]])
  l1 <- lapply(1:niter, function(i) {
    data2[, var1] <- fitted1 + sample(scale(resid1, scale = FALSE), 
      replace = TRUE)
    nls2 <- try(update(nls, start = as.list(coef(nls)), data = data2), 
      silent = TRUE)
    if (inherits(nls2, "nls")) 
      return(list(coef = coef(nls2), rse = summary(nls2)$sigma))
  })
  if (sum(sapply(l1, is.null)) > niter/2) 
    stop(paste("Procedure aborted: the fit only converged in", 
      round(sum(sapply(l1, is.null))/niter), "% during bootstrapping"))
  tabboot <- sapply(l1[!sapply(l1, is.null)], function(z) z$coef)
  rseboot <- sapply(l1[!sapply(l1, is.null)], function(z) z$rse)
  recapboot <- t(apply(tabboot, 1, quantile, c(0.5, Qlow1, 
    Qhigh1)))
  colnames(recapboot) <- c("Median", Qlow1*100, Qhigh1*100)
  serr <- sum(sapply(l1, is.null))
  if (serr > 0) 
    warning(paste("The fit did not converge", serr, "times during bootstrapping"))
  listboot <- list(coefboot = t(tabboot), rse = rseboot, bootCI = recapboot)
  class(listboot) <- "nlsBoot"
  return(listboot)
}

#@
jack.kn <- function(data) {
	n <- nrows(data)
	dd <- list()
	for( i in 1:n) {
	dd[[i]] <- data[-i,]
	}	
	return(dd)
	}


#@	
na.zero <- function(x){
 for(i in 1:length(x[1,])){
  if(length(which(is.na(x[,i])))>0){
  x[which(is.na(x[,i])),i] <- 0}
  }
  return(x)
 } 
 
 #@
cor.prob <- function(X, dfr = nrow(X) - 2) {
				 R <- cor(X)
				 above <- row(R) < col(R)
				 r2 <- R[above]^2
				 Fstat <- r2 * dfr / (1 - r2)
				 R[above] <- 1 - pf(Fstat, 1, dfr)
				 R
			}
	#@		
capwords <- function(s, strict = FALSE) {
			  cap <- function(s) paste(toupper(substring(s,1,1)),
         {s <- substring(s,2); if(strict) tolower(s) else s},
              sep = "", collapse = " " )
				  sapply(strsplit(s, split = " "), cap, USE.NAMES = !is.null(names(s)))
					
				}
#@
weighting.by.biom <- function (z) {
	#for combining lengths in stratified diet estimates weighted means and variances
	x <- c('YEAR','STRAT','FLEN', "BIOM.mean" ,"CLEN.mean", "BIOM.var", "CLEN.var")
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

#@
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

#@
combining.strata.rflen <- function(x) {
	A <- c('YEAR','RFLEN', 'STRAT','BIOM.mean',  'CLEN.mean',   'BIOM.var',   'CLEN.var', 'strat.areas')
	b <- x[,!colnames(x) %in% A]
	B <- x[1,]
	B <- B[,!colnames(B) %in% A[3:length(A)]]
	B[,2:(ncol(B)-1)] <- NA
	for(i in 2:(ncol(B)-1)) {
		B[,i] <- sum(x[,i+1]*(x$BIOM.mean*x$strat.areas))/sum(x$BIOM.mean*x$strat.areas)		
	}
	return(B)
}

	
#@	
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
