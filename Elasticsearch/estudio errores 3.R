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

resumen.1=NULL
for(j in 1:7){
  cat("#####",periodo[j],"#####\n")
  db2=Search(index = paste("logstash-",periodo[j],sep=""),type = "has",size=100000,q="message: \"Unable To Process Request - GTA Internal Communication Error\" ")$hits$hits
  
  pb=txtProgressBar(min = 0, max = length(db2), initial = 0, char = "=",width = NA, title, label, style = 3)
  errores=NULL
  for(i in 1:length(db2)){
    setTxtProgressBar(pb, i)
    
    if(!is.null(db2[[i]]$`_source`$`@message`)){
      errores=rbind(errores,cbind(gsub("T.*","",db2[[i]]$`_source`$'@timestamp'),gsub(".*T","",db2[[i]]$`_source`$'@timestamp')))
    }
  }
  
  errores=data.frame(errores,stringsAsFactors = F)
  errores[,1]=as.POSIXct(paste(errores[,1],errores[,2],sep=" "),tz = "UTC")
  errores=errores[order(errores[,1],decreasing = F),]
  
  errores=errores[order(hms(errores[,2])),]
  
  
  periodos=c(1:(24*60*60/60))
  duracion=as.duration(60)
  resumen=as.data.frame(matrix(NA,nrow=length(periodos),ncol=5,byrow = T))
  colnames(resumen)=c("Periodo","N.errores","Intervalo medio entre errores","Desviacion","Intervalo mínimo")
  pb=txtProgressBar(min = 0, max = nrow(resumen), initial = 0, char = "=",width = NA, title, label, style = 3)
  for(i in 1:length(periodos)){
    setTxtProgressBar(pb, i)
    intervalo=interval(as.POSIXct(paste(gsub("\\.","-",periodo[j])," 00:00:00",sep=""),tz = "UTC")+(periodos[i]-1)*duracion,
                       as.POSIXct(paste(gsub("\\.","-",periodo[j])," 00:00:00",sep=""),tz = "UTC")+periodos[i]*duracion-1)
    indices=which(errores[,1] %within% intervalo)
    
    #indices=which(errores[,1] == (as.POSIXct("2016-01-20 00:00:00",tz = "UTC")+i))
    resumen$Periodo[i]=as.character(intervalo)
    resumen$N.errores[i]=length(indices)
    cuenta=NULL
    for(k in 2:length(indices)){
      cuenta=c(cuenta,dseconds(interval(errores[indices[k-1],1],errores[indices[k],1])))
    }
    resumen$`Intervalo medio entre errores`[i]=mean(cuenta)
    resumen$Desviacion[i]=sd(cuenta)
    resumen$`Intervalo mínimo`[i]=min(cuenta)
    
  }
  
  resumen[is.na(resumen)]=0
  resumen.1=rbind(resumen.1,resumen)
}



resumen=resumen.1

resumen.2=data.frame(hora=resumen[,1],fecha=as.Date(resumen[,1]),recuento=resumen[,c(2)])
resumen.2[,1]=gsub(" ","",gsub(" --.*","",gsub("UTC","",gsub("2016-01-..","",resumen.2[,1]))))
resumen.2[which(resumen.2[,1]==""),1]="00:00:00"
labels=as.character(resumen.2[seq(1,nrow(resumen.2),360),1])
resumen.2$fecha=as.character(resumen.2$fecha)

ggplot(resumen.2[which(resumen.2[,1]>="12:00:00" & resumen.2[,1]<="15:00:00"),],
       aes(x=hora,y=recuento,group=fecha,colour=fecha))+geom_line()+scale_x_discrete(breaks=labels)
