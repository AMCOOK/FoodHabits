#@
save.to.csv <- function(data) {
	a <- getwd()
	b <- Sys.time()
	b <- format(b,format="%m-%d %H%M",tz='America/Halifax')
	e <- unique(data$COMMON)
	f <- paste("Diet analysis ",e," ", b,".csv",sep="")
	if(is.list(data) & any(names(data)=='mean.diet')) {
		data <- as.data.frame(data$mean.diet)
		}
	if(is.list(data) & is.list(data)[1] & any(names(data)[1]=='mean.diet')) {
		out <- list()
		ad <- names(data)
		for(i in 1:length(ad)) {
			out[[ad[i]]] <- data[[ad[i]]]$mean.diet	
			}
			out <- list.names.to.columns(out)
			names(out)[ncol(out)] <- 'Length_Group'
			data <- out
			}
	if(!is.data.frame(data)){
		for(i in 1:length(data)) {
			if( !is.null(data[[i]])) {
				if(nrow(data[[i]])>=1) {
 					data[[i]][,ncol(data[[i]])+1] <- names(data[i])
						}
					}
				}
			data <- do.call(rbind,data)
			names(data)[ncol(data)] <- 'Length_Group'
			}		
		write.csv(file=f,data, row.names=F)
	cat("\n")
	cat('Your file is in:\n')
	cat(paste(a,"/",f,sep=""))
	cat("\n")
	cat("------------------------------- \n")
  cat("\n")
}

