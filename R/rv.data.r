#' @export

rv.data <- function(year,area,specs,season) {
	#rv numbers per tow by length
	yrs1 <- paste(year,collapse="','")
	reg1 <- paste(area,collapse="','")
	mns <- ifelse(season=='Winter',paste('12','01','02','03',sep="','"),ifelse(season=='Spring',paste('04','05',sep="','"),ifelse(season=='Summer',paste('06','07','08',sep="','"),
	ifelse(season=='Autumn',paste('09','10','11','12',sep="','"),9999))))
	specs <- sqlQuery(channel,paste("select research from mflib.species_codes where common in ('",specs,"');",sep=""))[,1]
	dats <- sqlQuery(channel,paste("select year,strat,mission,setno,flen,sum(clen) clen
		 from
		 (SELECT year, icl.strat, icl.mission, icl.setno, icl.flen, DECODE(NVL(sampwgt,0),0,0,totwgt/NVL(sampwgt,0)*nvl(clen,0)*1.75/dist) clen
		 FROM
		 (SELECT mission,setno, flen, SUM(clen) clen, AVG(fwt) avg_fwt
		  FROM  groundfish.gsdet
		  WHERE flen IS NOT NULL AND spec=",specs,"
		  GROUP BY mission,setno, FLEN
		  ) d,
		 (SELECT year, mission, setno, strat, dist, decode(totwgt,0,1,totwgt) totwgt, decode(sampwgt,0,1,sampwgt) sampwgt, flen
		   FROM
		    (SELECT class, flen
		    FROM groundfish.gs_lengths
		    WHERE class=1
		     AND flen <=
		       (SELECT max(flen) + 1
		       FROM groundfish.gsdet
		       WHERE spec=",specs," AND flen IS NOT NULL
		        AND (mission, setno) IN
		          (SELECT DISTINCT i.mission, i.setno
		           FROM groundfish.gsinf i, mflib.gsmgt m
		           WHERE i.strat=m.strat
		           and ( m.unit in ('",reg1,"')) AND to_char(i.sdate,'mm') in ('",mns,"')
		           AND to_char(sdate,'yyyy') in ('",yrs1,"') AND i.type=1))) l,
		    (SELECT year, i.mission, i.setno, strat, dist, totwgt, sampwgt
		    FROM
		     (SELECT mission, setno, totwgt, sampwgt
		        FROM groundfish.gscat WHERE spec=",specs,") c,
		     (SELECT to_char(sdate,'yyyy') year, i.mission, i.setno, i.strat, dist
		        FROM groundfish.gsinf i, mflib.gsmgt m
		      WHERE i.strat=m.strat
		       and m.unit in ('",reg1,"') AND to_char(i.sdate,'mm') in ('",mns,"')
		       AND to_char(sdate,'yyyy') in ('",yrs1,"') AND i.type=1) i
		    WHERE i.mission=c.mission(+)
		     AND i.setno=c.setno(+)) ic
		 ) icl
		 WHERE icl.mission=d.mission(+) AND icl.setno=d.setno(+) AND icl.flen=d.flen(+))
		group by year,strat,mission,setno,flen;",sep=""))
		ss1 <- unique(dats$STRAT)
	strat <- paste(ss1,collapse="','")
	wts <- sqlQuery(channel,paste("select distinct strat,area/((41./1000.0/6080.2)*1.75/1000.0)/1000000 tarea from groundfish.gsstratum where strat in ('",strat,"') order by strat;",sep=""))
	dd <- list(survey.data=dats,strata.weights=wts)
	return(dd)
	
}

