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

