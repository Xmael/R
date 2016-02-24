library(elastic)
library(RJSONIO)
library(XML)
connect(es_base="http://52.28.187.57")
today=Sys.Date()
periodo=seq(today, length.out=7, by="-1 day") #Obtengo las fechas desde hoy hasta diez días atrás
periodo=format(periodo,"%Y.%m.%d")
library(data.table)

hb=NULL
for(j in 1:7){
  db2=Search(index = paste("logstash-",periodo[j],sep=""),type = "has",size=1e5,
             q="message: \"'HotelValuedAvailRQ/CheckInDate/@date' must be at least today\" ",fields=c("@message.raw","@fields.prov","message"))$hits$hits
  
  
  for(i in 1:length(db2)){
    if(i%%100==0){
      cat(periodo[j],i,"de",length(db2),"\n")
    }
    if(isValidJSON(unlist(db2[[i]]$fields$message),asText=T)){
      temp=fromJSON(unlist(db2[[i]]$fields$message))
      fecha=strsplit(as.character(temp$'@timestamp'), "T")[[1]][1]
      hora=strsplit(as.character(temp$'@timestamp'), "T")[[1]][2]
      hora=format(as.POSIXlt(gsub("\\..*","",hora), tz = "", format="%H:%M:%S"),"%H:%M:%S")
      hb=rbind(hb,cbind(fecha,hora))
    }
  }
}

franja=hb[which(hb[,2]>"22:00:00" | hb[,2]<"10:00:00"),]
resumen=as.data.frame(table(franja[,1]))


library(ggplot2)
ggplot(resumen,aes(x=Var1,y=Freq,fill=Freq))+geom_bar(stat="identity")+geom_hline(yintercept=mean(resumen[,2]))+
  scale_fill_continuous(low="blue",high="darkblue")
