#' @export

vb.season <- function(species) {
sea <- sqlQuery(channel,paste("select distinct decode(to_char(sdate,'mm'),1,'Winter',2,'Winter',3,'Winter',4,'Spring',5,'Spring',6,'Summer',7,'Summer',8,'Summer',9,'Autumn',10,
'Autumn',11,'Autumn',12,'Winter',9999) ss from groundfish.gsinf i, groundfish.gsdet d, mflib.species_codes c where i.mission=d.mission and i.setno=d.setno and d.spec=c.research and 
c.common='",species,"' and age is not null;",sep=""))
return(sea[,1])
}

