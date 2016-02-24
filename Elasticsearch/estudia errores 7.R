library(elastic)
library(RJSONIO)
library(XML)
connect(es_base="http://52.28.187.57")
today=Sys.Date()
periodo=seq(today, length.out=7, by="-1 day") #Obtengo las fechas desde hoy hasta diez días atrás
periodo=format(periodo,"%Y.%m.%d")
library(data.table)

hb=NULL
for(j in 1){
  db2=Search(index = paste("logstash-",periodo[1],sep=""),type = "has",size=1e5,
             q="message: \"'HotelValuedAvailRQ/CheckInDate/@date' must be at least today\" ")$hits$hits
  for(i in 1:length(db2)){
    cat(i,"de",length(db2),"\n")
      fecha=as.character(db2[[i]]$`_source`$'@timestamp')
      if(!is.null(db2[[i]]$`_source`$'@fields'$request)){
      r=xmlTreeParse(db2[[i]]$`_source`$'@fields'$request)
      r=xmlApply(xmlRoot(r)[[6]],xmlValue)
      codigohotel=sapply(1:length(r),function(i){r[[i]]})
      pais=names(which.max(table(hoteles$country[which(hoteles$prov_id %in% codigohotel & hoteles$prov=="HB")])))
      timezone=names(which.max(table(hoteles$Time_zone[which(hoteles$prov_id %in% codigohotel & hoteles$prov=="HB")])))
      hb=rbind(hb,cbind(fecha,pais,timezone))
      }else{
      hb=rbind(hb,cbind(fecha,NA))
    }

  }
}

franja=hb[which(ymd_hms(hb[,1])>ymd_hms("2016-01-27 00:00:00") & ymd_hms(hb[,1])<ymd_hms("2016-01-27 06:00:00")),]
#franja=franja[sample(1:nrow(franja),1000),]
franja=franja[order(franja[,1],decreasing=F),]
franja=franja[-sample(1:nrow(franja),600),]
