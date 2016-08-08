#' @export

species.accumulation <- function(data,cut.p=F) {
	
	
			if(nrow(data>5)) {
    		data$P <- 1
    		pdat <- cast(data,SAMPLE_INDEX~SPECCD2,fun.aggregate=sum,value="P")
    		if(ncol(pdat)>3 & nrow(pdat>2)) {
			abcd <- specaccum(pdat[,c(3:ncol(pdat))], method='random')
			if(cut.p) plot(abcd,ylab='N prey items',xlab='N stomachs',main=paste(unique(data$COMMON),unique(data$RFLEN),sep=""))
			else plot(abcd,ylab='N prey items',xlab='N stomachs',main=unique(data$COMMON))
			}
			}
			}
			
