#' Weight length relationship
#'
#' This function uses the data collected through the menu command to estimate as and bs
#' @param specs what species are included and this is the Maritimes Region Species Codes
#' @param year what are the year(s) included in the data
#' @param season which season is being analysed? Winter(Dec-Mar), Spring(Apr May), Summer (June to Aug), Autumn (Sept to Dec) 
#' @param stat.num is the percentile to define the bounds of the length weight regression. From this the number of data points outside the percentile polygon (obtained through bootstrapping) are identified.
#' @param plots This will produce your plots of regression and bounding polygon
#' @param diet.analysis Uses the info from your diet analysis script to fill in some of the data
#' @examples diet.analysis()

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

