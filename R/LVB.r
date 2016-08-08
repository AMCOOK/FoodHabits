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

