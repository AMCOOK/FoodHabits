#' @export
#diet script and function 

diet.analysis<-function(path=getwd(),uid = oracle.stomach.user,pwd = oracle.stomach.password) {
		if(!require(RODBC)) { install.packages('RODBC',repos="http://cran.r-project.org")}
		if(!require(vegan)) { install.packages('vegan',repos="http://cran.r-project.org")}
		if(!require(reshape)) { install.packages('reshape',repos="http://cran.r-project.org")}
		if(!require(splancs)) { install.packages('splancs',repos="http://cran.r-project.org")}
		if(!require(plyr)) { install.packages('plyr',repos="http://cran.r-project.org")}
		if(!require(nlstools)) { install.packages('nlstools',repos="http://cran.r-project.org")}
		if(!require(doBy)) { install.packages('doBy',repos="http://cran.r-project.org")}
		
	require(RODBC)
	require(vegan)
	require(reshape)
	require(splancs)
	require(plyr)
	require(nlstools)
	require(doBy)
	
	channel<<-odbcConnect("ptran",uid, pwd) #called globally
	options(stringsAsFactors=F)
	options(warn=-1)

	#source(file.path(path,"diet.functions.R")) 
	    diet.version <- "0.1"
    cat("---------------------------------------------------------- \n")
    cat(paste("Welcome to Diet Analysis in R; version", 
        diet.version, "\n"))
    cat("Author: Adam Cook, PED\n")
    cat("The diet data used here is described in Cook and Bundy 2010\n")
    cat("Please report bugs to: amcook127@gmail.com\n")
    cat("---------------------------------------------------------- \n")
    cat("\n")
    cat("Press Esc to exit at any time. \n \n")
    diet.data <- list()
    diet.data$EXIT <-FALSE
    while (diet.data$EXIT == FALSE) {
        choices <- c("Specify diet data **need to to this before option 2,3,4 or 5**","View the Data","Species Accumulation Plot","Mean diet","Stratified Diet Analysis","Stratifed Numbers per tow","a's and b's","LVB growth curve",
        "Predators", "Save all data compiled during this session to your R workspace?","Save all results to a txt file?","Exit")
        
        title <- "The available options are:"
        choose <- menu(choices, title = title)
if (choose == 1) { #get diet data
               diet.data <- list()
    			diet.data$EXIT <-FALSE
               src<-data.src()
               datasource <- menu(src,title='Choose a DATASOURCE (if for stratified analysis only *GS* works:')
               ds<-src[datasource]
               
               yrs<-data.year(ds)
               year<- select.list(yrs,title='Choose one or multiple YEARS:',multiple=T,graphics=T)
               
               seas<-data.seasons(ds,year)
               seasons <- menu(seas,title='Choose a SEASON:')
               
               season<-seas[seasons]
               
               areas<- menu(c("Strata","NAFO"),title='Choose Area based on:')
               
               region<-data.region(dat=ds,strat.nafo=areas,seas=season,yrs=year)
               
               regions<- select.list(region,title='Choose one or multiple AREAS:',multiple=T,graphics=F)
                              
               spec<-data.species(dat=ds, strat.nafo=areas,seas=season,yrs=year,reg=regions)
               species<-menu(spec,title='Choose a SPECIES (- n of stoms w/ prey):')
               spec1<-spec[species]
               
               
              raw <- get.diet.data(dat=ds, strat.nafo=areas,seas=season,yrs=year,reg=regions,specs=spec1)
             
              diet.data$raw.diet.data <- raw
              
              cat("Do you want to save the data to a .csv? (y/n) \n")
                plotans <- scan(what = "", nlines = 1, quiet = TRUE)
                if (plotans == "yes" || plotans == "y")
              save.to.csv(raw)
              
              if(ds=='GS') {
 				 gs.dat <- gs.rv.data (year=year,area=regions,species=spec1,season=season)
 				 diet.data$strata.weights <-gs.dat$strata.weights
 				 diet.data$gs.survey.data <- gs.dat$survey.data
 				 
 				 diet.data$a.b<-a.b(specs=spec1,year=year,area=regions,season=season,plot=F,diet.analysis=T)
 				 
 				  }
 				 
              
            }
      
if (choose == 2) { #print diet data
             print(diet.data)
       
       		}
       
if (choose ==3) { #do SAC's
       		cat("Do you want SAC by Length? (y/n) \n")
                lens <- scan(what = "", nlines = 1, quiet = TRUE)
                if (lens == "no" || lens == "n") {
       			 		species.accumulation(diet.data$raw.diet.data)
       			}
       			if(lens=='yes' || lens=='y') {
       				ll<-menu(c('5','10','20','Cutpoint'),title='Choose a Length grouping:')
       				if(ll==4) {
       				cat("What is your cutpoint length <= ? \n")
       				l1<-scan(what = "", nlines = 1, quiet = TRUE)
       				species.accumulation.by.length(data=diet.data$raw.diet.data,cut.p=T,lengths=as.numeric(l1))
       					}
       				else {
       				species.accumulation.by.length(data=diet.data$raw.diet.data,cut.p=F,lengths=as.numeric(ll))
       				}
       			}
       			}
if(choose ==4) { #mean diets
       		preys <- c('PREYSPECCD','FAM')
       		a3 <- menu(preys,title='How do you want prey items grouped?')
       		pp <- preys[a3]
       		
       		
       		 cat("Do you want to remove influential observations? (y/n) \n")
       		 ac<- scan(what = "", nlines = 1, quiet = TRUE)
       		ag<-0
       		if(ac=='y' | ac=='yes') {
       		 ag<-1 
       		 cat("Removing influential observations is done by comparing the percent differences\n") 
       		 cat("of the mean diets with and without each observation\n")
       		 cat("\n")
       		 cat("Please provide the percent difference cut off point\n")
       		  cat("(i.e. 30, 40, 50 with no percent sign)\n")
       		 af<- scan(what = "", nlines = 1, quiet = TRUE)
       		}
       		 
       		 cat("Do you want to remove singletons? (y/n) \n")
       		 ad<- scan(what = "", nlines = 1, quiet = TRUE)
			if(ad=='y' || ad=='yes') ah<-1
			 else ah<-0       	
       		 cat("Do you want mean diets by length class? (y/n) \n")
       		 ab<- scan(what = "", nlines = 1, quiet = TRUE)
       		if(ab=='no' || ab =='n') {
       		
       		diet.data$mean.diets<-mean.diet(diet.data=diet.data$raw.diet.data,prey.grouping=pp,remove.singletons=ah,remove.influentials=ag,percent.diff=as.numeric(af))
       		
       		}
       		
       		 if (ab == "yes" || ab == "y") {
       		 ll<-menu(c('5','10','20','Cutpoint'),title='Choose a Length grouping:')
       				if(ll==4) {
       				cat("What is your cutpoint length <= ? \n")
       				l1<-scan(what = "", nlines = 1, quiet = TRUE)
       				diet.data$mean.diets<-mean.diet.by.length(data=diet.data$raw.diet,prey.grouping=pp,remove.singletons=ah,remove.influentials=ag,percent.diff=as.numeric(af),cut.p=T,lengths=as.numeric(l1))       				
       				}
       				else {
       				diet.data$mean.diets<-mean.diet.by.length(data=diet.data$raw.diet,prey.grouping=pp,remove.singletons=ah,remove.influentials=ag,percent.diff=as.numeric(af),cut.p=F,lengths=as.numeric(ll))       				
       				}
       				}
       		cat("Do you want to save this mean diet data? (y/n)\n")
       		at1<- scan(what = "", nlines = 1, quiet = TRUE)
       		if(at1=='yes' || at1=='y') {
       		 save.to.csv(diet.data$mean.diets)
       		}
       		cat("Do you want to view this mean diet data? (y/n)\n")
       		at1<- scan(what = "", nlines = 1, quiet = TRUE)
       		if(at1=='yes' || at1=='y') {
       		 print(diet.data$mean.diets)
       		}
       	}
if(choose ==5) {
       		#only for fam right now
       		# preys <- c('PREYSPECCD','FAM','CAT1','CAT2','CAT3')
       		#a3 <- menu(preys,title='How do you want prey items grouped?')
       		#pp <- preys[a3]
       		
       		       		 
       		 cat("Do you want to remove singletons? (y/n) \n")
       		 ad<- scan(what = "", nlines = 1, quiet = TRUE)
			if(ad=='y' || ad=='yes') ah<-1
			 else ah<-0       	
       		 
			 cat("Do you want stratified diets by length class? (y/n) \n")
       		 ab<- scan(what = "", nlines = 1, quiet = TRUE)
       		 
       		 if (ab == "no" || ab == "n") {
       		  diet.data$stratified.diet<-stratified.diet(diet.data=diet.data$raw.diet,survey.data=diet.data$gs.survey.data,strata.data=diet.data$strata.weights,prey.grouping='FAM',
							remove.singletons=ad, a.b=diet.data$a.b)
		
			cat("Do you want to view your diet data? (y/n) \n")
       		 ab1<- scan(what = "", nlines = 1, quiet = TRUE)			
       		 if(ab1 == 'y') {
       		 print(diet.data$stratified.diet)
       		 
       		 }
       		 }
       		 
       		 
       	   if (ab == "yes" || ab == "y") {
       		 ll<-menu(c('5','10','20','Cutpoint'),title='Choose a Length grouping:')
       				if(ll==4) {
       				cat("What is your cutpoint length <= ? \n")
       				l1<-scan(what = "", nlines = 1, quiet = TRUE)
       				
       				diet.data$stratified.diet<-stratified.diet(diet.data=diet.data$raw.diet,survey.data=diet.data$gs.survey.data,strata.data=diet.data$strata.weights,prey.grouping='FAM',
							remove.singletons=ad, a.b=diet.data$a.b,by.lengths=T,cut.p=T,lengths=l1)
       				
       				}
       				else {
       				diet.data$stratified.diet<-stratified.diet(diet.data=diet.data$raw.diet,survey.data=diet.data$gs.survey.data,strata.data=diet.data$strata.weights,prey.grouping='FAM',
							remove.singletons=ad, a.b=diet.data$a.b,by.lengths=T,cut.p=F,lengths=ll)
       				}
       			cat("Do you want to view your diet data? (y/n) \n")
       		 ab1<- scan(what = "", nlines = 1, quiet = TRUE)			
       		 if(ab1 == 'y') {
       		 print(diet.data$stratified.diet)
       		 }
       		 }
       		 cat("Do you want to save the stratified mean diet data? (y/n)\n")
       		at1<- scan(what = "", nlines = 1, quiet = TRUE)
       		if(at1=='yes' || at1=='y') {
       		 save.to.csv(diet.data$stratified.diet)
       		}
       		 }
if(choose==6) {
       	 aw<-c('Winter','Spring','Summer','Autumn')
       	 d<-menu(aw,title='Choose a season:')
       	 dd<-aw[d]
       	 d1<- paste(c(rv.year(dd))[[1]],sep=",")
       	 ddd<-select.list(d1,multiple=T, graphics=T, title='Choose one or more years:')
       	 areas<- menu(c("Strata","NAFO"),title='Choose Area based on:')
       	 reg <- rv.data.region(strat.nafo=areas,seas=dd,yrs=ddd)
       	 reg<-select.list(reg,multiple=T,graphics=F,title='Choose one or more areas:')
       	 dddd<-c(rv.species(ddd,season=dd, area=reg))[[1]]
       	 ddddd<-menu(dddd,title='Select a species:',graphics=F)
       	 ddddd<-dddd[ddddd]
       	 
       	 diet.data$rv.data.by.length<-rv.data(year=ddd,area=reg,specs=ddddd,season=dd)
       	  cat("Do you want stratified estimates by length class? (y/n) \n")
       		 ab<- scan(what = "", nlines = 1, quiet = TRUE)
       		 
       		 if (ab == "no" || ab == "n") {
       		 diet.data$strat.ests<-stratified.estimates(survey.dat=diet.data$rv.data.by.length$survey.data,strata.dat=diet.data$rv.data.by.length$strata.weights,by.lengths=F,cut.p=F,lengths) 
       		 }
       		 
       		 
       	   if (ab == "yes" || ab == "y") {
       		 ll<-menu(c('3','5','10','20','Cutpoint'),title='Choose a Length grouping:')
       				if(ll==5) {
       				cat("What is your cutpoint length <= ? \n")
       				l1<-scan(what = "", nlines = 1, quiet = TRUE)
       				
       				diet.data$strat.ests<-stratified.estimates(survey.dat=diet.data$rv.data.by.length$survey.data,strata.dat=diet.data$rv.data.by.length$strata.weights,by.lengths=T,cut.p=T,lengths=as.numeric(l1))

       				}
       				else {
       				diet.data$strat.ests<-stratified.estimates(survey.dat=diet.data$rv.data.by.length$survey.data,strata.dat=diet.data$rv.data.by.length$strata.weights,by.lengths=T,cut.p=F,lengths=as.numeric(ll))
       				}
       		 }
       	 cat("Do you want to save to workspace? (y/n) \n")
       		 ab<- scan(what = "", nlines = 1, quiet = TRUE)
       		 if(ab=='yes' || ab=='y') {
       		 
       		 strat.ests<<-diet.data$strat.ests
       		cat("------------------------------------------------------------ \n")

       		 	 cat("Your object is named 'strat.ests'\n\n")       	 
       		cat("------------------------------------------------------------ \n")

       	 }
       	 cat("Plot stratified catch estimates? (y/n) \n")
       	 ab<- scan(what = "", nlines = 1, quiet = TRUE)
       		 if(ab=='yes' || ab=='y') {
       		 plot.strat.ests(diet.data$strat.ests)
       		 cat("Title for plot? (in quotes)\n")
       	 	ab<- scan(what = "", nlines = 1, quiet = TRUE)
       	 	title(ab)
       		 }
       	 cat("Calculate rate of decline/increase? (y/n) \n")
       	 ab<- scan(what = "", nlines = 1, quiet = TRUE)
       		 if(ab=='yes' || ab=='y') {
       		aa<-unique(diet.data$strat.ests$Year)
       		 ab<- select.list(aa,multiple=T,graphics=F,title="Choose both a starting year and ending year for calculating rates:")
       	 	diet.data$Rate.of.Change<-decline.estimates(diet.data$strat.ests,syear=ab[1],eyear=ab[2]) 
       	 	 cat("Title for plot? (in quotes)\n")
       	 	ab<- scan(what = "", nlines = 1, quiet = TRUE)
       	 	title(ab)
       		 }
        }
       	
if(choose==7) {
       	
       	aw<-c('Winter','Spring','Summer','Autumn')
       	 d<-menu(aw,title='Choose a season:')
       	 dd<-aw[d]
       	 d1<- paste(c(rv.year(dd))[[1]],sep=",")
       	 ddd<-select.list(d1,multiple=T, graphics=T, title='Choose one or more years:')
       	 areas<- menu(c("Strata","NAFO"),title='Choose Area based on:')
       	 reg <- rv.data.region(strat.nafo=areas,seas=dd,yrs=ddd)
       	 reg<-select.list(reg,multiple=T,graphics=F,title='Choose one or more areas:')
       	 dddd<-c(rv.species(ddd,season=dd, area=reg))[[1]]
       	 ddddd<-menu(dddd,title='Select a species:',graphics=F)
       	 ddddd<-dddd[ddddd]
       	 cat("What is your alpha level for confidence intervals? (e.g. 1, 5, 10) \n")
       	 ab<- scan(what = "", nlines = 1, quiet = TRUE)
       	 cat("Do you want a plot? (y/n) \n")
       	 ac<- scan(what = "", nlines = 1, quiet = TRUE)
       	 if(ac=='yes' || ac=='y') {
       	diet.data$a.b<-a.b(specs=ddddd,year=ddd,area=reg,stat.num=as.numeric(ab),plots=T,season=dd)
       	}
       	else {
       	diet.data$a.b<-a.b(specs=ddddd,year=ddd,area=reg,stat.num=as.numeric(ab),plots=F,season=dd)
       	}
       	
       	print(diet.data$a.b)

       	
       	}
       		 
if(choose ==8) {
       a1<-vb.spps()
       d<-menu(a1,title='Choose a species:')
       dd<-a1[d]
       a2<-vb.season(dd)
       d1<-menu(a2,title='Choose a season:')
       dd2<-a2[d1]
       cat('Do you want to track a cohort? (y/n) \n')
       aaa<-scan(what = "", nlines = 1, quiet = TRUE)
       aaa<-ifelse(aaa=='y',TRUE,FALSE)
       a3<-vb.year(species=dd,season=dd2)
       d1<-select.list(a3,title='Choose a start year and end year:',graphics=F,multiple=T)
       if(length(d1)==1) d1[2]<-d1[1]
       areas<- menu(c("Strata","NAFO"),title='Choose Area based on:')
       a4<-vb.region(sp=dd,seas=dd2,strat.nafo=areas,yrs=d1)
       d2<-select.list(a4,title='Select one or multiple areas:', multiple=T,graphics=F)
       cat("Enter your starting values for Linf, K and t0 (separated by a space) :\n ---------------------------------------------------------------\n if left blank starting values will be estimated by Walford lines\n Everhart, et al. 1975. Principles of Fishery Science \n---------------------------------------------------------------\n")
       asw<-scan(what = "", nlines = 1, quiet = TRUE)
       if(is.na(asw[1])) { sts<-list(hinf=0,K=0,t0=0) }
       else {sts<-list(hinf=asw[1],K=asw[2],t0=asw[3])}
       meth<-c('Nonlinear least squares regression','Maxmium liklihood estimation')
       cat('Do you want to use means at age for VB curve? (y/n)\n')
       ask1<-scan(what = "", nlines = 1, quiet = TRUE)
       a91<-ifelse(ask1=='y',T,F)
       if(ask1=='y') {
       cat('\n Do you want to weight the means by the length specific variances\n as per Kimura (1970) method-c? (y/n)\n')
       ask2<-scan(what = "", nlines = 1, quiet = TRUE)
       a92<-ifelse(ask2=='y',T,F)
       }
      a6<-menu(meth,title='Choose fitting method')
       if(a6==1) {
       a44<-LVB(species=dd,area=d2,syear=d1[1],season=dd2,eyear=d1[2],plot=T,add.plot=F,line.col='blue',init.pars=sts, NLS=T,MLE=F, method=c('BFGS','CG','SANN'),
		compare=F,species2,area2,syear2,eyear2,control = list(maxiter = 10000, minFactor = 1/2024, tol = 1e-05), cohort=aaa,means=a91,error.kimura.c=a92)
		}
		if(a6==2) {
		metho<-c('BFGS a quasi-Newton method uses function values and gradients to build up a picture of the surface to be optimized','CG conjugate gradients method more fragile than the BFGS method may be successful in much larger probles',
		'SANN simulated annealing uses the Metropolis function for the acceptance probability')
		a7<-menu(metho,title='Select a fitting method: \n for more details on these methods see ?optim and references therein')
		a8<-ifelse(a7==1,'BFGS',ifelse(a7==2,'CG','SANN'))
		 a44<-LVB(species=dd,area=d2,syear=d1[1],eyear=d1[2],season=dd2,plot=T,add.plot=F,line.col='blue',init.pars=sts, NLS=F,MLE=T, method=a8,
		compare=F,species2,area2,syear2,eyear2,control = list(maxiter = 10000, minFactor = 1/2024, tol = 1e-05),cohort=aaa,means=a91)
		}
		print(a44)	
	
		cat('\n Do you want to compare this VB Growth Curve with another? (y/n) \n\n\t from Kimura 1980. U.S.Fish.Bull.77(4):765-776.\nThis method is currently programmed for least squares regression only\n')
		comps<-scan(what = "", nlines = 1, quiet = TRUE)
			if(comps=='y'){
					d<-menu(a1,title='Choose a species:')
			       dd1<-a1[d]
			       a21<-vb.season(dd1)
			       d11<-menu(a2,title='Choose a season:')
			       dd21<-a2[d11]
			       cat('Do you want to track a cohort? (y/n) \n')
			       aaa1<-scan(what = "", nlines = 1, quiet = TRUE)
			       aaa1<-ifelse(aaa1=='y',TRUE,FALSE)
			       a31<-vb.year(species=dd1,season=dd21)
			       d11<-select.list(a31,title='Choose a start year and end year:',graphics=F,multiple=T)
			       if(length(d11)==1) d11[2]<-d11[1]
			       areas<- menu(c("Strata","NAFO"),title='Choose Area based on:')
			       a41<-vb.region(sp=dd1,seas=dd21,strat.nafo=areas,yrs=d11)
			       d21<-select.list(a41,title='Select one or multiple areas:', multiple=T,graphics=F)
			       cat("Enter your starting values for Linf, K and t0 (separated by a space) :\n ---------------------------------------------------------------\n if left blank starting values will be estimated by Walford lines\n Everhart, et al. 1975. Principles of Fishery Science \n---------------------------------------------------------------\n")
			       asw1<-scan(what = "", nlines = 1, quiet = TRUE)
			       if(is.na(asw1[1])) { sts1<-list(hinf=0,K=0,t0=0) }
			       else {sts1<-list(hinf=asw1[1],K=asw1[2],t0=asw1[3])}
			       cat('Do you want to use means at age for VB curve? (y/n)\n')
				ask1<-scan(what = "", nlines = 1, quiet = TRUE)
				a93<-ifelse(ask1=='y',T,F)
				if(ask1=='y') {
				cat('\n Do you want to weight the means by the length specific variances\n as per Kimura (1970) method-c? (y/n)\n')
				ask2<-scan(what = "", nlines = 1, quiet = TRUE)
				a94<-ifelse(ask2=='y',T,F)
				}
       	a45<-LVB(species=dd,area=d2,syear=d1[1],eyear=d1[2],season=dd2,plot=T,add.plot=F,line.col='blue',init.pars=sts, NLS=T,MLE=F, method=c('BFGS','CG','SANN'),cohort=aaa,
		compare=T,species2=dd1,area2=d21,season2=dd21,init.pars2=sts1,syear2=d11[1],eyear2=d11[2],control = list(maxiter = 10000, minFactor = 1/2024, tol = 1e-05), cohort2=aaa1,means=a91,
		error.kimura.c=a92,means2=a93,error.kimura.c2=a94 )
		print(a45)
		}
		
		}
       
if(choose ==9) {
       	
       		cat("Who eats me??\n")
  		    cat("----------------------------------------------- \n")
  		    grps<-c('Fin Fish','Shrimps','Crabs','Cephalopods','Amphipods','All - caution this is a very long list')  		    
  		    abc<-menu(grps,title='Choose a broad classification:')
  		    aaa3<-grps[abc]
  		    next1<-prey.species(aaa3)
  		    next2 <- select.list(next1,multiple=F,title='Choose a species',graphics=F)
  		    diet.data$predation<-predators(next2)
  		    aa<-c('Predation by Species','Predation by Species and Year','Predation by Species and Area','Predation by Species,Area and Year')
  		    dc<-menu(aa,title='How do you want to see the predation data?')
  		    print(diet.data$predation[[dc]])
  		    cat("Do you want to save this predation data? (y/n)\n")
       		at1<- scan(what = "", nlines = 1, quiet = TRUE)
       		if(at1=='yes' || at1=='y') {
       		save.to.csv(diet.data$predation[[dc]])
		}
       	} 
       	
if(choose==10) {
       	
       	 diet.data<<-diet.data
       		cat("------------------------------------------------------------ \n")

       		 	 cat("Your object is named 'diet.data'\n\n")       	 
       		cat("------------------------------------------------------------ \n")
       	
       	} 
if(choose==11) {
	a<-getwd()
	b<-Sys.time()
	b<-format(b,format="%m-%d %H%M",tz='America/Halifax')
	f<-paste("Diet analysis ", b,".txt",sep="")
			capture.output(print(diet.data),file=f)
   cat("\n")
	cat('Your file is in:\n')
	cat(paste(a,"/",f,sep=""))
	cat("\n")
	cat("------------------------------- \n")
    cat("\n")
       	

}
if (choose == 12) {
            cat("Enjoy your diet data...... \n")
            odbcCloseAll()
            gc(reset=T)
            diet.data$EXIT = TRUE			
            }
            }
            }
            
          
