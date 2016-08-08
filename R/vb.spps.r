#@
vb.spps <- 	function() {
spp.choose <- sqlQuery(channel,paste("select distinct common from groundfish.gsdet d, mflib.species_codes s where s.research=d.spec and age is not null;"))
	return(spp.choose[,1])
	}

