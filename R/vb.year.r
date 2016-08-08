#' @export

vb.year <- function(species,season){
				mns <- ifelse(season=='Winter',paste('12','01','02','03',sep="','"),ifelse(season=='Spring',paste('04','05',sep="','"),ifelse(season=='Summer',paste('06','07','08',sep="','"),
				ifelse(season=='Autumn',paste('09','10','11','12',sep="','"),9999))))
	ss <- sqlQuery(channel,paste("select distinct to_char(sdate,'yyyy') year from groundfish.gsinf i, groundfish.gsdet d, 
	mflib.species_codes c where i.mission=d.mission and i.setno=d.setno and d.spec=c.research and c.common='",species,"' and to_char(sdate,'mm') in ('",mns,"') and age is not null
	order by to_char(sdate,'yyyy');",sep=""))
return(ss[,1])
}	

