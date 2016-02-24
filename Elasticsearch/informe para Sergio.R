library(elastic)
library(RJSONIO)
library(XML)
library(lubridate)
connect(es_base="http://52.28.187.57")
today=Sys.Date()-1
periodo=seq(today, length.out=7, by="-1 day") #Obtengo las fechas desde hoy hasta diez días atrás
periodo=format(periodo,"%Y.%m.%d")
library(data.table)

errores.stack=NULL
for(j in 1:7){
  db2=Search(index = paste("logstash-",periodo[j],sep=""),type = "has",size=1e6,
             q="@fields.levelname:ERROR AND @fields.prov:'RS'" )$hits$hits
  
  pb=txtProgressBar(min = 0, max = length(db2), initial = 0, char = "=",width = NA, title, label, style = 3)
  errores=matrix(NA,nrow=length(db2),ncol=2,byrow = T)
  for(i in 1:length(db2)){
    setTxtProgressBar(pb, i)
    message=ifelse(is.null(db2[[i]]$`_source`$'@message'),NA,db2[[i]]$`_source`$'@message')
    errores[i,]=c(db2[[i]]$`_source`$'@timestamp',message)
  }
  
  errores=errores[order(ymd_hms(errores[,1]),decreasing = F),]
  
  errores.stack=rbind(errores.stack,errores)
}

errores=errores.stack

errores=errores[order(as.POSIXct(paste(errores[,1],errores[,2],sep=" "),tz = "Europe/Madrid"),decreasing = F),]

#  errores=errores[order(errores[,1],decreasing = F),]
#  errores=errores[order(hms(errores[,2])),]
colnames(errores)=c("Timestamp","Message")
errores[is.na(errores)]=0
periodos=c(1:(24*60*60/300))
duracion=as.duration(300)

errores=errores[order(ymd_hms(errores[,1]),decreasing=F),]
horas=ymd_hms(errores[,1])
resumen.full=NULL
for(j in 1:length(periodo)){
#  pb=txtProgressBar(min = 0, max = nrow(resumen), initial = 0, char = "=",width = NA, title, label, style = 3)
  resumen=matrix(NA,nrow=length(periodos),ncol=ncol(errores)-1+length(unique(errores[,2])),byrow = T)
  colnames(resumen)=c("Periodo",c(gsub("Unable To Process Request - ","",gsub("with value.*","",unique(errores[,2])))))
  for(i in 1:length(periodos)){
#    setTxtProgressBar(pb, i)
    cat(j,i,"de",length(periodos),"\n")
    intervalo=interval(as.POSIXct(paste(gsub("\\.","-",periodo[j])," 00:00:00",sep=""),tz = "Europe/Madrid")+(periodos[i]-1)*duracion,
                       as.POSIXct(paste(gsub("\\.","-",periodo[j])," 00:00:00",sep=""),tz = "Europe/Madrid")+periodos[i]*duracion-1)
    
    indices=which(horas %within% intervalo)
    
    #indices=which(errores[,1] == (as.POSIXct("2016-01-20 00:00:00",tz = "UTC")+i))
    #resumen[i,1]=as.Date(gsub("\\.","-",periodo[j])) #Fecha
    resumen[i,1]=as.character(as.POSIXct(paste(gsub("\\.","-",periodo[j])," 00:00:00",sep=""),tz = "Europe/Madrid")+(periodos[i]-1)*duracion,
                              format="%Y-%m-%d %H:%M:%S" ) #Periodo
#     system.time(for(k in 2:ncol(resumen)){
#       resumen[i,k]=length(which(colnames(resumen)[k]==errores[indices,2]))
#     })
    resumen[i,2:ncol(resumen)]<-sapply(2:ncol(resumen),function(k){length(which(colnames(resumen)[k]==errores[indices,2]))})
  }
  resumen.full=rbind(resumen.full,resumen)
}

library(reshape)
library(ggplot2)

resumen.full.bak=resumen.full
resumen.full=resumen.full.bak

resumen.full=data.frame(resumen.full,stringsAsFactors = F)

for(i in 2:ncol(resumen.full)){resumen.full[,i]=as.integer(resumen.full[,i])}
resumen.full=resumen.full[,c(1,2,6,7,9,11,19,21)]
resumen.full[,1]=ymd_hms(resumen.full[,1],tz="Europe/Madrid")
resumen.full=resumen.full[which(resumen.full[,1]>ymd_hms("2016-02-02 00:00:00",tz="Europe/Madrid")),]

save(resumen.full.bak,file="resumen.RData")

temp=melt(data = resumen.full,id="Periodo")
ggplot(temp,aes(x=Periodo,y=value,group=variable,fill=variable))+geom_area(position = "stack")+
  ylim(0,200)+
  scale_x_datetime(date_breaks = "2 hour")+theme(axis.text.x  = element_text(angle=90, vjust=0.5, size=11))

