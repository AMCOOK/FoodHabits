#' @export

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

