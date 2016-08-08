#@
prey.species <- function(group) {
	if(group=='Fin Fish') {spec=paste(10:1000,collapse=",")}
	if(group=='Shrimps') {spec=paste(2100:2415,collapse=",")}
	if(group=='Crabs') {spec=paste(2500:2570,collapse=",")}
	if(group=='Cephalopods') {spec=paste(c(4500,4504,4510,4511,4512,4513,4514,4515,4516,4519,4520,4518,4521,4529,4540,4541),collapse=",")}
	if(group=='Amphipods') {spec=paste(2600:2990,collapse=",")}
	if(group=='All') {spec=10:9000}
		
	dat <- sqlQuery(channel,paste("select distinct common, count(distinct sample_index) N from mfd_stomach.sdsto s, mflib.species_codes m where s.preyspeccd=m.research 
		and preyspeccd in (",spec,")group by common order by count(distinct sample_index) desc;",sep=""))
	dat <- dat[dat$N>1,]
	dat <- dat[order(dat$COMMON),]
	dat <- paste(dat$COMMON,dat$N,sep="-")
		return(dat)
}
