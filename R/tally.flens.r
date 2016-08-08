#@
tally.flens <- function(x) {
	#makes the flen groups same as tally sheets from subsampling forms
	a <- cbind(1:200,c(rep(5,10),rep(seq(13,98,by=5),each=5),rep(100,100)))
	b <- a[match(x,a[,1]),2]
	return(b)
	}

