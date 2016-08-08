#' @export

weighting.number.per.flen.per.tow <- function(data) {
	data1 <- split(data,data$SETNO)
	for(i in 1:length(data1)) {
	data1[[i]]$WTS <- data1[[i]]$CLEN/sum(data1[[i]]$CLEN)
	}
	data1 <- do.call('rbind',data1)
	return(data1)
	}

