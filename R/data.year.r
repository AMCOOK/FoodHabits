#' @export

data.year <- function(dat) {
		years <- sqlQuery(channel,paste("select distinct to_char(sdate,'yyyy') from mfd_stomach.sdinf where datasource= '",dat[1],"' and sdate is not null order by to_char(sdate,'yyyy');",sep=""))
		a3 <- paste(years[,1],sep=",")
	return(a3)
	}
