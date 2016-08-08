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
