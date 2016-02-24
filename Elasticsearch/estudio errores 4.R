library(elastic)
library(RJSONIO)
library(XML)
library(lubridate)

today=Sys.Date()-1
periodo=seq(today, length.out=7, by="-1 day") #Obtengo las fechas desde hoy hasta diez días atrás
periodo=format(periodo,"%Y.%m.%d")
library(data.table)

#load("~/Trivago/hoteles(20.1.16).RData")

parsea=function(mensaje){
  r=xmlTreeParse(mensaje)
  cadena=xmlValue(xmlRoot(r)[[2]])
  cadena=strsplit(cadena,split = "#")
  return(c(length(cadena[[1]])-1,cadena[[1]][length(cadena[[1]])]))
}

resumen.1=NULL
for(j in 1:length(periodo)){
  connect(es_base="http://52.28.187.57")
  cat("#####",periodo[j],"#####\n")
  db2=Search(index = paste("logstash-",periodo[j],sep=""),type = "has",size=1e6,
             q="@fields.prov:GT")$hits$hits
  
  
  pb=txtProgressBar(min = 0, max = length(db2), initial = 0, char = "=",width = NA, title, label, style = 3)
  errores=matrix(NA,nrow=length(db2),ncol=7,byrow = T)
  for(i in 1:length(db2)){
    setTxtProgressBar(pb, i)
    
    requested=ifelse(is.null(db2[[i]]$`_source`$'@fields'$requested_hotels),NA,db2[[i]]$`_source`$'@fields'$requested_hotels)
    responsed=ifelse(is.null(db2[[i]]$`_source`$'@fields'$responsed_hotels),NA,db2[[i]]$`_source`$'@fields'$responsed_hotels)
    msecs=db2[[i]]$`_source`$'@fields'$msecs
    timeout=ifelse(is.null(db2[[i]]$`_source`$'@fields'$timeout),NA,db2[[i]]$`_source`$'@fields'$timeout)
    message=db2[[i]]$`_source`$'@message'
    errores[i,]=c(gsub("T.*","",db2[[i]]$`_source`$'@timestamp'),gsub(".*T","",db2[[i]]$`_source`$'@timestamp'),
                  requested,responsed,msecs,timeout,message)
  }
  
  #Convierte las cadenas de caracteres a fechas y horas
  #  errores=data.frame(errores,stringsAsFactors = F)
  #  errores[,1]=as.POSIXct(paste(errores[,1],errores[,2],sep=" "),tz = "UTC")
  errores=errores[order(as.POSIXct(paste(errores[,1],errores[,2],sep=" "),tz = "UTC"),decreasing = F),]
  
  #  errores=errores[order(errores[,1],decreasing = F),]
  #  errores=errores[order(hms(errores[,2])),]
  colnames(errores)=c("Timestamp","Hora","Requested","Responsed","Msecs","Timeout","Message")
  errores[is.na(errores)]=0
  
  periodos=c(1:(24*60*60/5))
  duracion=as.duration(5)
  resumen=matrix(NA,nrow=length(periodos),ncol=ncol(errores)-1+length(unique(errores[,7])),byrow = T)
  colnames(resumen)=c("Fecha","Periodo","Requested","Responsed","Msecs","Timeout",c(gsub("Unable To Process Request - ","",gsub("with value.*","",unique(errores[,7])))))
  pb=txtProgressBar(min = 0, max = nrow(resumen), initial = 0, char = "=",width = NA, title, label, style = 3)
  horas=as.POSIXct(paste(errores[,1],errores[,2],sep=" "),tz = "UTC")
  for(i in 1:length(periodos)){
    setTxtProgressBar(pb, i)
    intervalo=interval(as.POSIXct(paste(gsub("\\.","-",periodo[j])," 00:00:00",sep=""),tz = "UTC")+(periodos[i]-1)*duracion,
                       as.POSIXct(paste(gsub("\\.","-",periodo[j])," 00:00:00",sep=""),tz = "UTC")+periodos[i]*duracion-1)
    indices=which(horas %within% intervalo)
    
    #indices=which(errores[,1] == (as.POSIXct("2016-01-20 00:00:00",tz = "UTC")+i))
    resumen[i,1]=as.Date(gsub("\\.","-",periodo[j])) #Fecha
    resumen[i,2]=as.character(intervalo) #Periodo
    resumen[i,3]=sum(as.integer(errores[indices,3])) #Requested
    resumen[i,4]=suppressWarnings(sum(na.omit(as.integer(errores[indices,4])))) #Responsed
    resumen[i,5]=mean(as.numeric(errores[indices,5])) #Msecs
    temp=table(errores[indices,6])
    resumen[i,6]=ifelse(any(names(temp)=="TRUE"),as.integer(temp[match("TRUE",names(temp))]),0)
    for(k in 7:ncol(resumen)){
      resumen[i,k]= length(which(colnames(resumen)[k]==errores[indices,7]))
    }
  }
  
  if(!is.null(colnames(resumen.1))){
    diferencia=setdiff(colnames(resumen.1),colnames(resumen))
    diferencia.1=setdiff(colnames(resumen),colnames(resumen.1))
    if(length(diferencia)!=0 | length(diferencia.1)!=0){
      nombres=colnames(resumen)
      resumen=cbind(resumen,matrix(NA,ncol=length(diferencia),nrow=nrow(resumen),byrow=T))
      colnames(resumen)=c(nombres,diferencia)
      nombres.1=colnames(resumen.1)
      resumen.1=cbind(resumen.1,matrix(NA,ncol=length(diferencia.1),nrow=nrow(resumen.1),byrow=T))
      colnames(resumen.1)=c(nombres.1,diferencia.1)
      resumen=resumen[,match(colnames(resumen.1),colnames(resumen))]
    }
  }
  if(any(is.na(resumen))){
    resumen[is.na(resumen)]=0
  }
  resumen.1=rbind(resumen.1,resumen)
  if(any(is.na(resumen.1))){
    resumen.1[is.na(resumen.1)]=0
  }
}
resumen.bak=resumen.1
resumen=resumen.1



for(i in 3:ncol(resumen)){
  resumen[,i]=as.numeric(resumen[,i])
}
resumen[is.na(resumen)]=0


resumen.2=data.frame(hora=resumen[,1],fecha=as.Date(resumen[,1]),recuento=resumen[,c(2)])
resumen.2[,1]=gsub(" ","",gsub(" --.*","",gsub("UTC","",gsub("2016-01-..","",resumen.2[,1]))))
resumen.2[which(resumen.2[,1]==""),1]="00:00:00"
labels=as.character(resumen.2[seq(1,nrow(resumen.2),360),1])
resumen.2$fecha=as.character(resumen.2$fecha)

ggplot(resumen.2[which(resumen.2[,1]>="12:00:00" & resumen.2[,1]<="15:00:00"),],
       aes(x=hora,y=recuento,group=fecha,colour=fecha))+geom_line()+scale_x_discrete(breaks=labels)