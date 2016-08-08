#@
fillin.mult <- function(data) {
	data.return <- data
	data.return[,c(7:ncol(data.return))] <- NA
	for(i in 7:(ncol(data))) {
	data.return[,i] <- fillin(data[,3],data[,i])[,2]
	}
	return(data.return)
}

