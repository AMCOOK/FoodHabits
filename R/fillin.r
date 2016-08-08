#@
fillin <- function(l,d) {
	#uses the slope of relation bwn pwt and len to fill in missing
	#l=length column
	#d=diet column
	#
	a1 <- as.data.frame(cbind(l,d))
	a1 <- a1[order(a1[,1]),]
	ind <- 0
	ind <- which(!complete.cases(a1))
	ind2 <- which(complete.cases(a1))
		if(length(ind)==0) {
		return(a1)		
		}
		if(length(ind)>0) {
		for(i in 1:length(ind)) {
			if(ind[i]==1) {
				a1[1,2] <- 0+(((a1[min(ind2),2]-0)/(a1[min(ind2),1]-0))*(a1[ind[i],1]-0)) #if first entry replace with lowest available entry regresssed against 0 ie the closer to 0 length the closer to 0 diet
				
			}
			if(ind[i]>max(ind2)) {
				 a1[ind[i],2] <- a1[max(ind2),2] #if last entry replace with last available entry
			 }
		if(ind[i]!=1 & i<length(ind) & ind[i]+1==ind[i+1] & ind[i]<max(ind2)) {
				next.ind <- min(ind2[which(ind2>ind[i])])
				a1[ind[i],2] <- a1[ind[i]-1,2]+(((a1[next.ind,2]-a1[ind[i]-1,2])/(a1[next.ind,1]-a1[ind[i]-1,1]))*(a1[ind[i],1]-a1[ind[i]-1,1])) 
			}
			
		if(ind[i]!=1 & ind[i]!=nrow(a1) & i<length(ind) & ind[i]+1!=ind[i+1]){
					a1[ind[i],2] <- a1[ind[i]-1,2]+(((a1[ind[i]+1,2]-a1[ind[i]-1,2])/(a1[ind[i]+1,1]-a1[ind[i]-1,1]))*(a1[ind[i],1]-a1[ind[i]-1,1])) 
		}
		if(ind[i]!=1 & ind[i]!=nrow(a1) & i==length(ind))
			
		a1[ind[i],2] <- a1[ind[i]-1,2]+(((a1[ind[i]+1,2]-a1[ind[i]-1,2])/(a1[ind[i]+1,1]-a1[ind[i]-1,1]))*(a1[ind[i],1]-a1[ind[i]-1,1])) 
	}
	return(a1)
}	
}

