#@
influential.obs <- function(x,infl=50){
	##identify influential observation based on their percent difference in mean from the overall
	#only if there are greater than 2 times this prey is observed as less than that it becomes a problem
  n <- length(x)
  n1 <- sum(x>0) 
  e <- c()
  e[1] <- 0
  if(n1>2) {
  		b <- mean(x)
		a <- c()
  		for(i in 1:n) {
  				a[i] <- mean(x[-i])
  				}
  		cd <- abs(b-a)/mean(a,b)*100 # % diff in mean between leave one out and overall
  	 	if(any(cd>infl)) {
	  			e <- which(cd>infl)
	  		
  		}
	 	}
	 	return(e)
	}

