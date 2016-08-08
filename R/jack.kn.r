#' @export

jack.kn <- function(data) {
	n <- nrows(data)
	dd <- list()
	for( i in 1:n) {
	dd[[i]] <- data[-i,]
	}	
	return(dd)
	}


