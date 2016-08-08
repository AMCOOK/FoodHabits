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

