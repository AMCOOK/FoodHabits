#@
data.src <- function() {
		srcs <- sqlQuery(channel,paste("select distinct datasource from mfd_stomach.sdinf order by datasource;"))
		a3 <- paste(srcs[,1],sep=",")
	return(a3)
	}
