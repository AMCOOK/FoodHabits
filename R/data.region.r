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

