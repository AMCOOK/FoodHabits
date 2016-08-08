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
