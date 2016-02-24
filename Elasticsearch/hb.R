library(elastic)
library(RJSONIO)
library(XML)
connect(es_base="http://52.28.187.57")
today=Sys.Date()
periodo=seq(today, length.out=7, by="-1 day") #Obtengo las fechas desde hoy hasta diez días atrás
periodo=format(periodo,"%Y.%m.%d")
library(data.table)

load("~/Trivago/hoteles(20.1.16).RData")

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
      if(!is.null(temp$'@fields'$request)){
        r=xmlTreeParse(temp$'@fields'$request,encoding = "UTF-8")
        r=xmlRoot(r)
        if(length(r)>=6){
          lista=as.character(xmlApply(r[[6]],xmlValue))
          hb=rbind(hb,cbind(db2[[i]]$fields$'@fields.prov',temp$'@timestamp',lista))
        }
      }
    }
  }
  
}

write.table(hb,file="hb.csv",sep=";")

hbm=matrix(unlist(hb),nrow=nrow(hb),ncol=3,byrow = F)
hbm=cbind(hbm[,1],gsub("T.*","",hbm[,2]),gsub("\\..*","",gsub(".*T","",hbm[,2])),hbm[,3])
hbm=hbm[-which(is.na(hoteles$country[which(hoteles$prov=="HB")][match(hbm[,4],hoteles$prov_id[which(hoteles$prov=="HB")])])),]
hbm=as.data.frame(hbm)
hbm$country=hoteles$country[which(hoteles$prov=="HB")][match(hbm[,4],hoteles$prov_id[which(hoteles$prov=="HB")])]

dia19=hbm[which(days(hbm[,2])==19),]
dia19=cbind(sprintf(hours(times(dia19[,3])),fmt="%02d"),sprintf(minutes(times(dia19[,3])),fmt="%02d"),dia19[,5]) #Quitamos los segundos
dia19=as.data.frame(dia19)

resumen=matrix(NA,nrow=length(unique(dia19$V3)),ncol=1:24)
paises=sort(unique(dia19$V3))
horas=sprintf(c(0:23),fmt="%02d")
for(i in 1:length(horas)){
  resumen[,i]=cbind(table(dia19[which(dia19$V1==horas[]),3]))
}
