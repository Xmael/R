library(elastic)
library(RJSONIO)
library(XML)
connect(es_base="http://52.28.187.57")
today=Sys.Date()-1
periodo=seq(today, length.out=7, by="-1 day") #Obtengo las fechas desde hoy hasta diez días atrás
periodo=format(periodo,"%Y.%m.%d")
library(data.table)

load("~/Trivago/hoteles(20.1.16).RData")

parsea=function(mensaje){
  r=xmlTreeParse(mensaje)
  cadena=xmlValue(xmlRoot(r)[[2]])
  cadena=strsplit(cadena,split = "#")
  return(c(length(cadena[[1]])-1,cadena[[1]][length(cadena[[1]])]))
}



db2=Search(index = paste("logstash-",periodo[j],sep=""),type = "has",size=1e6,q="@fields.prov:RS")$hits$hits

pb=txtProgressBar(min = 0, max = length(db2), initial = 0, char = "=",width = NA, title, label, style = 3)
errores=NULL
for(i in 1:length(db2)){
  setTxtProgressBar(pb, i)
  
  if(db2[[i]]$`_source`$'@fields'$levelname=="INFO"){
    errores=rbind(errores,cbind(gsub("T.*","",db2[[i]]$`_source`$'@timestamp'),gsub("\\..*","",gsub(".*T","",db2[[i]]$`_source`$'@timestamp')),
                                db2[[i]]$`_source`$'@fields'$requested_hotels,
                                db2[[i]]$`_source`$'@fields'$responsed_hotels,db2[[i]]$`_source`$'@fields'$msecs,
                                db2[[i]]$`_source`$'@fields'$timeout,db2[[i]]$`_source`$'@message'))
  }else if(db2[[i]]$`_source`$'@fields'$levelname=="ERROR"){
    mensaje=db2[[i]]$`_source`$'@fields'$request
    temp=parsea(mensaje)
    errores=rbind(errores,cbind(gsub("T.*","",db2[[i]]$`_source`$'@timestamp'),gsub("\\..*","",gsub(".*T","",db2[[i]]$`_source`$'@timestamp'))
                                ,temp[1],"NULL",db2[[i]]$`_source`$'@fields'$msecs,
                                "NULL",db2[[i]]$`_source`$'@message'))
  }
}

errores=data.frame(errores,stringsAsFactors = F)
errores[,1]=as.POSIXct(paste(errores[,1],errores[,2],sep=" "),tz = "UTC")
errores=errores[order(errores[,1],decreasing = F),]

errores=errores[order(hms(errores[,2])),]
colnames(errores)=c("Timestamp","Time","Requested","Responsed","Msec","Timeout","Message")

columnas=unique(errores[,7])
periodos=c(1:(24*60*60/5))
duracion=as.duration(5)
resumen=as.data.frame(matrix(NA,nrow=length(periodos),ncol=length(unique(errores[,7]))+5,byrow = T))
colnames(resumen)=c("N peticiones medias por segundo","Requested","Responsed","Msec","Timeout",sort(unique(errores[,7])))
pb=txtProgressBar(min = 0, max = nrow(resumen), initial = 0, char = "=",width = NA, title, label, style = 3)
for(i in 7780:length(periodos)){
  setTxtProgressBar(pb, i)
   indices=which(errores[,1] %within% (new_interval(as.POSIXct("2016-01-20 00:00:00",tz = "UTC")+(periodos[i]-1)*duracion,
                                                   as.POSIXct("2016-01-20 00:00:00",tz="UTC")+periodos[i]*duracion-1)))
  
  #indices=which(errores[,1] == (as.POSIXct("2016-01-20 00:00:00",tz = "UTC")+i))
  resumen$`N peticiones medias por segundo`[i]=mean(table(errores[indices,2]))
  resumen$Requested[i]=sum(as.integer(errores$Requested[indices]))
  resumen$Responsed[i]=suppressWarnings(sum(na.omit(as.integer(errores$Responsed[indices]))))
  resumen$Msec[i]=mean(as.numeric(errores$Msec[indices]))
  temp=table(errores$Timeout[indices])
  resumen$Timeout[i]=ifelse(any(names(temp)=="TRUE"),as.integer(temp[match("TRUE",names(temp))]),0)
  for(k in 6:ncol(resumen)){
    resumen[i,k]= length(which(colnames(resumen)[k]==errores[indices,7]))/5
  }
}

resumen=resumen[-c(1:7780),]

resumen.2=data.frame(id=1:nrow(resumen),resumen[,c(1,7)])[1000:2000,]
resumen.2[which(is.nan(resumen.2[,2])),2]=0
resumen.2=melt(resumen.2,"id")
ggplot(resumen.2,aes(x=id,y=value,group=variable,colour=variable))+geom_line()
