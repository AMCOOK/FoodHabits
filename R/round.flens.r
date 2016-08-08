#' @export

round.flens <- function(x,grp) {
		if(grp==3) a <- floor(x/3)*3+1    #3cm
		if(grp==5) a <- floor(x/5)*5+2    #5cm
		if(grp==10) a <- floor(x/10)*10+5   #10cm
		if(grp==20) a <- floor(x/20)*20+10  #20cm
		return(a)
				}  
