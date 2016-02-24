library(elastic)
library(RJSONIO)
library(XML)
library(reshape2)
library(ggplot2)
library(lubridate)
library(RMySQL)


connect(es_base="http://52.28.187.57")
today=Sys.Date()
periodo=seq(today, length.out=2, by="-1 day") #Obtengo las fechas desde hoy hasta diez días atrás
today=format(today,"%Y.%m.%d")
periodo=format(periodo,"%Y.%m.%d")
library(data.table)

parsea=function(mensaje){
  r=xmlTreeParse(mensaje)
  cadena=xmlValue(xmlRoot(r)[[2]])
  cadena=strsplit(cadena,split = "#")
  return(c(length(cadena[[1]])-1,cadena[[1]][length(cadena[[1]])]))
}

errores.stack=NULL
for(j in 1:2){
  db2=Search(index = paste("logstash-",periodo[j],sep=""),type = "has",size=1e6,
             q="message:ERROR" )$hits$hits
  
  pb=txtProgressBar(min = 0, max = length(db2), initial = 0, char = "=",width = NA, title, label, style = 3)
  errores=matrix(NA,nrow=length(db2),ncol=6,byrow = T)
  for(i in 1:length(db2)){
    setTxtProgressBar(pb, i)
    
    proveedor=ifelse(is.null(db2[[i]]$`_source`$'@fields'$prov),NA,db2[[i]]$`_source`$'@fields'$prov)
    msecs=ifelse(is.null(db2[[i]]$`_source`$'@fields'$msecs),NA,db2[[i]]$`_source`$'@fields'$msecs)
    timeout=ifelse(is.null(db2[[i]]$`_source`$'@fields'$timeout),NA,db2[[i]]$`_source`$'@fields'$timeout)
    message=ifelse(is.null(db2[[i]]$`_source`$'@message'),NA,db2[[i]]$`_source`$'@message')
    elapsed=ifelse(is.null(db2[[i]]$`_source`$'@fields'$elapsed),NA,db2[[i]]$`_source`$'@fields'$elapsed)
    errores[i,]=c(db2[[i]]$`_source`$'@timestamp',proveedor,msecs,elapsed,timeout,message)
  }
  
  errores=errores[order(ymd_hms(errores[,1]),decreasing = F),]
  
  errores.stack=rbind(errores.stack,errores)
}


errores=errores.stack

colnames(errores)=c("Timestamp","Proveedor","Msecs","Elapsed","Timeout","Message")
errores[is.na(errores)]=0
errores=errores[,-c(4,5)]
errores=errores[which(ymd_hms(errores[,1]) %within% interval(start=paste(gsub("\\.","-",periodo[2]), "10:00:00",sep=" "),end=paste(gsub("\\.","-",periodo[1]), "10:00:00",sep=" "),tzone="UTC")),]

reservas=read.csv("/home/ismael/Trabajo/Errores/reservas hoy.csv",header=T,sep = ";",stringsAsFactors = F)

GT=errores[which(errores[,2]=="GT"),]
HB=errores[which(errores[,2]=="HB"),]
RS=errores[which(errores[,2]=="RS"),]
SH=rbind(errores[which(errores[,2]=="SH"),])

errores.GT=as.data.frame(table(GT[,4]))
errores.GT[which(gsub("with value.*","",errores.GT[,1])=="Parameter \"check in date\" "),2]=round(errores.GT[which(gsub("with value.*","",errores.GT[,1])=="Parameter \"check in date\" "),2]*0.2)
errores.GT[,1]=gsub("Unable To Process Request -.","",errores.GT[,1])
errores.GT[,1]=gsub("Parameter","",gsub("with value","",gsub("is not valid","",errores.GT[,1])))

hits.GT=301325
reservas.GT=length(which(reservas$prov=="GT"))

ggplot(errores.GT,aes(x=Var1,y=Freq,fill=Var1))+geom_bar(stat="identity")+
  theme(axis.text.x = element_blank())+xlab("Errores")+ylab("Cantidad")+
ggtitle(paste("GT",Sys.Date(),sep=" - "))+scale_fill_discrete(name="Errores")+
  theme(legend.title = element_text(colour="Black", size=16, face="bold"),
        legend.text = element_text(size = 12),
        plot.title = element_text(size=20, face="bold"),
        axis.text.y = element_text(size=15),
        axis.title.x =element_text(face="bold",size=15),
        axis.title.y = element_text(face="bold",size=15))+
annotate("text",label=paste("Hits=",hits.GT,sep=""),x=2,y=max(errores.GT[,2]),size=8)+
  annotate("text",label=paste("Reservas=",reservas.GT,sep=""),x=2,y=round(max(errores.GT[,2])*0.9),size=8)+
annotate("text",label=paste("Errores Totales=",sum(errores.GT[,2]),sep=""),x=2,y=round(max(errores.GT[,2])*0.8),size=8)

errores.HB=data.frame(table(HB[,4]),stringsAsFactors = F)
errores.HB=errores.HB[which(errores.HB[,2]>=10),]
errores.HB[,1]=gsub("\n.*","",errores.HB[,1])
errores.HB[which(errores.HB[,1]=="'HotelValuedAvailRQ/CheckInDate/@date' must be at least today."),2]=round(errores.HB[which(errores.HB[,1]=="'HotelValuedAvailRQ/CheckInDate/@date' must be at least today."),2]*0.1)
hits.HB=321473
reservas.HB=length(which(reservas$prov=="HB"))

ggplot(errores.HB,aes(x=Var1,y=Freq,fill=Var1))+geom_bar(stat="identity")+
  theme(axis.text.x = element_blank())+xlab("Errores")+ylab("Cantidad")+
  ggtitle(paste("HB",Sys.Date(),sep=" - "))+scale_fill_discrete(name="Errores")+
  theme(legend.title = element_text(colour="Black", size=16, face="bold"),
        legend.text = element_text(size = 12),
        plot.title = element_text(size=20, face="bold"),
        axis.text.y = element_text(size=15),
        axis.title.x =element_text(face="bold",size=15),
        axis.title.y = element_text(face="bold",size=15))+
annotate("text",label=paste("Hits=",hits.HB,sep=""),x=2,y=max(errores.HB[,2]),size=8)+
annotate("text",label=paste("Reservas=",reservas.HB,sep=""),x=2,y=round(max(errores.HB[,2])*0.9),size=8)+
annotate("text",label=paste("Errores Totales=",sum(errores.HB[,2]),sep=""),x=2,y=round(max(errores.HB[,2])*0.8),size=8)


errores.RS=as.data.frame(table(RS[,4]))
errores.RS=errores.RS[which(errores.RS[,2]>=10),]
hits.RS=328355
reservas.RS=length(which(reservas$prov=="RS"))
ggplot(errores.RS,aes(x=Var1,y=Freq,fill=Var1))+geom_bar(stat="identity")+
  theme(axis.text.x = element_blank())+xlab("Errores")+ylab("Cantidad")+
  ggtitle("RS")+scale_fill_discrete(name="Errores")+
  theme(legend.title = element_text(colour="Black", size=16, face="bold"),
        legend.text = element_text(size = 12),
        plot.title = element_text(size=20, face="bold"),
        axis.text.y = element_text(size=15),
        axis.title.x =element_text(face="bold",size=15),
        axis.title.y = element_text(face="bold",size=15))+
  annotate("text",label=paste("Hits=",hits.RS,sep=""),x=2,y=max(errores.RS[,2]),size=8)+
annotate("text",label=paste("Reservas=",reservas.RS,sep=""),x=2,y=round(max(errores.RS[,2])*0.9),size=8)+
annotate("text",label=paste("Errores Totales=",sum(errores.RS[,2]),sep=""),x=2,y=round(max(errores.RS[,2])*0.8),size=8)


errores.SH=as.data.frame(table(SH[,4]))
hits.SH=196515
reservas.SH=length(which(reservas$prov=="SH"))
ggplot(errores.SH,aes(x=Var1,y=Freq,fill=Var1))+geom_bar(stat="identity")+
  theme(axis.text.x = element_blank())+xlab("Errores")+ylab("Cantidad")+
  ggtitle("SH")+scale_fill_discrete(name="Errores")+
  theme(legend.title = element_text(colour="Black", size=16, face="bold"),
        legend.text = element_text(size = 12),
        plot.title = element_text(size=20, face="bold"),
        axis.text.y = element_text(size=15),
        axis.title.x =element_text(face="bold",size=15),
        axis.title.y = element_text(face="bold",size=15))+
  annotate("text",label=paste("Hits=",hits.SH,sep=""),x=2,y=max(errores.SH[,2]),size=8)+
  annotate("text",label=paste("Reservas=",reservas.RS,sep=""),x=1,y=round(max(errores.SH[,2])*0.9),size=8)+
annotate("text",label=paste("Errores Totales=",sum(errores.SH[,2]),sep=""),x=2,y=round(max(errores.SH[,2])*0.8),size=8)

errores.totales=data.frame(Proveedor=c("GT","RS","HB"),Errores=c(sum(errores.GT[,2]),sum(errores.RS[,2]),sum(errores.HB[,2])))
hits.totales=sum(c(hits.GT,hits.HB,hits.RS,hits.SH))
reservas.totales=nrow(reservas)
ggplot(errores.totales,aes(x=Proveedor,y=Errores,fill=Proveedor))+geom_bar(stat="identity")+
  theme(axis.text.x = element_blank())+xlab("Proveedor")+ylab("Cantidad")+
  ggtitle("Totales Proveedor")+scale_fill_discrete(name="Errores")+
  theme(legend.title = element_text(colour="Black", size=16, face="bold"),
        legend.text = element_text(size = 12),
        plot.title = element_text(size=20, face="bold"),
        axis.text.y = element_text(size=15),
        axis.title.x =element_text(face="bold",size=15),
        axis.title.y = element_text(face="bold",size=15))+
  annotate("text",label=paste("Hits=",hits.totales,sep=""),x=2,y=max(errores.totales[,2]),size=8)+
  annotate("text",label=paste("Reservas=",reservas.totales,sep=""),x=1,y=max(errores.totales[,2]),size=8)
