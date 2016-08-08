#' @export

predators <- function(species) {
	specs <- strsplit(species,"-")[[1]][1]
	dat <- sqlQuery(channel,paste("Select to_char(sdate,'yyyy') year,nafo ,spec,count(distinct sample_index) n_preds
	 from mfd_stomach.sdview1 w, mflib.species_codes s where s.common in ('",specs,"') and research=preyspeccd group by to_char(sdate,'yyyy'),nafo,spec;",sep=""))
	spp <- unique(dat$SPEC)
	spp <- paste(spp,collapse=",")
	dd <- sqlQuery(channel,paste("select to_char(sdate,'yyyy') year,nafo1 nafo, spec, common, scientif, count(sample_index) n_examined from sddet v, sdinf i,mflib.species_codes g where i.mission=v.mission and
	i.setno=v.setno and v.spec=g.research and v.spec in (",spp,") group by v.datasource,to_char(sdate,'yyyy'),nafo1, spec, common, scientif order by to_char(sdate,'yyyy'),spec;"))
	dat <- merge(dd,dat,by=c('YEAR','NAFO','SPEC'),all=T)
	dat <- na.zero(dat)
	by.spec <- aggregate(dat[,c(6,7)],by=c(dat['SPEC']),FUN=sum)
	by.spec.year <- aggregate(dat[,c(6,7)],by=c(dat['SPEC'],dat['YEAR']),FUN=sum)
	by.spec.area <- aggregate(dat[,c(6,7)],by=c(dat['SPEC'],dat['NAFO']),FUN=sum)
	by.spec.year.area <- aggregate(dat[,c(6,7)],by=c(dat['SPEC'],dat['NAFO'],dat['YEAR']),FUN=sum)
	d1 <- list(Predation.by.Species=by.spec,Predation.by.Species.and.Time=by.spec.year,Predation.by.Species.and.Area=by.spec.area,Predation.by.Species.Area.Time=by.spec.year.area)
	return(d1)
	}
