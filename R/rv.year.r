#' @export

rv.year <- function(season) {
	mns <- ifelse(season=='Winter',paste('12','01','02','03',sep="','"),ifelse(season=='Spring',paste('04','05',sep="','"),ifelse(season=='Summer',paste('06','07','08',sep="','"),
	ifelse(season=='Autumn',paste('09','10','11','12',sep="','"),9999))))
	dats <- sqlQuery(channel,paste("select distinct to_char(sdate,'yyyy') year from groundfish.gsinf where to_char(sdate,'mm') in ('",mns,"') order by to_char(sdate,'yyyy');",sep=""))
	}

