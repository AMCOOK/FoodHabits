#' @export

vb.region <- function(strat.nafo,seas,yrs=year,sp=species) {
	mns <- ifelse(seas=='Winter',paste('12','01','02','03',sep="','"),ifelse(seas=='Spring',paste('04','05',sep="','"),ifelse(seas=='Summer',paste('06','07','08',sep="','"),
	ifelse(seas=='Autumn',paste('09','10','11','12',sep="','"),9999))))
	if(strat.nafo==1) {
		reg <- sqlQuery(channel,paste("select distinct strat from groundfish.gsinf i, groundfish.gsdet d , mflib.species_codes c where i.mission=d.mission and i.setno=d.setno and 
		d.spec=c.research and common ='",sp,"' and to_char(sdate,'mm') in ('",mns,"') and to_char(sdate,'yyyy') between ",yrs[1]," and ",yrs[2]," order by strat;",sep=""))
		}
	if(strat.nafo==2) {
		reg <- sqlQuery(channel,paste("select distinct unit from groundfish.gsinf i, groundfish.gsdet d , mflib.species_codes c, mflib.gsmgt m where i.strat=m.strat and i.mission=d.mission and i.setno=d.setno and 
		d.spec=c.research and common ='",sp,"' and to_char(sdate,'mm') in ('",mns,"') and to_char(sdate,'yyyy') between ",yrs[1]," and ",yrs[2]," and unit in ('4X','4VS','4VN','4W') order by unit;",sep=""))
		}
	a3 <- paste(reg[,1],sep=",")
	return(a3)
	}

