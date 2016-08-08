#@
data.seasons <- function(dat,year) {
		season <- sqlQuery(channel,paste("select distinct decode(to_char(sdate,'mm'),'01','Winter','02','Winter','03','Winter','04','Spring','05','Spring','06','Summer','07','Summer','08','Summer','09',
			'Autumn','10','Autumn','11','Autumn','12','Winter','other') season from mfd_stomach.sdinf where datasource= '",dat[1],"' and sdate is not null and to_char(sdate,'yyyy') in ('",year,"');",sep=""))
		a3 <- paste(season[,1],sep=",")
	return(a3)
	}
