library(elastic)
library(chron)
library(parallel)
library(dplyr)
library(lubridate)
library(ggplot2)

connect(es_base="http://52.28.187.57")


today=Sys.Date()-1
periodo=seq(today, length.out=30, by="-1 day")
periodo=format(periodo,"%Y.%m.%d")

db=NULL
for(i in 1:length(periodo)){#Obtengo las fechas desde hoy hasta diez días atrás
  print(periodo[i])
  db2=Search(index = paste("logstash-",periodo[i],sep=""),type = "has",size=400000,
             q="_exists_:@fields.hotels_len",fields=c("@timestamp","@fields.hotels_len"))$hits$hits


  temp=mclapply(1:length(db2),function(i){
    hora=parse_date_time(db2[[i]]$fields$'@timestamp',orders = "ymd hms")
    lista_peticiones=db2[[i]]$fields$'@fields.hotels_len'
    c(Hora=as.character(hora),Peticiones=as.character(as.integer(lista_peticiones)))
  },mc.preschedule = TRUE, mc.set.seed = TRUE,
  mc.silent = FALSE, mc.cores = getOption("mc.cores", 6L),
  mc.cleanup = TRUE, mc.allow.recursive = TRUE)

  temp=as.data.frame(matrix(unlist(temp),ncol=2,byrow=T))
  temp[,1]=parse_date_time(temp[,1],order="ymd hms")
  temp[,2]=as.integer(levels(temp[,2]))[temp[,2]]
  colnames(temp)=c("Hora","Peticiones")

  db=rbind(db,temp[order(temp$Hora,decreasing = F),])
}

peticiones=db
periodo=seq(today, length.out=30, by="-1 day")
recuento=NULL
for(i in 1:length(periodo)){
  print(periodo[i])
  for(k in 0:23){
    cat(k,"\n")
    mes=month(periodo[i])
    dia=day(periodo[i])
    recuento=rbind(recuento,(data.frame(Hora= parse_date_time(paste("2015-",mes,"-",dia," ",k,sep=""),orders="ymd_h"),
                                           Peticiones=length(peticiones[month(peticiones$Hora)==mes & day(peticiones$Hora)==dia & 
                                                                        hour(peticiones$Hora)==k,2]))))
  }
}

save(recuento,file="recuento_noviembre.RData")

#load("peticiones.RData")
#load("recuento_noviembre.RData")


recuento[periodo[1]==as.Date(recuento[,1]),]

ggplot(recuento[periodo[6]==as.Date(recuento[,1]),],aes(x=Hora,y=Peticiones,fill=as.integer(Peticiones)))+geom_bar(stat="identity",position="stack")+
  scale_fill_gradient(low = "#56B1F7", high = "#132B43", space = "Lab", na.value = "grey50", 
                      guide = "colourbar")


porhoras=as.data.frame(t(sapply(0:23,function(i){
  media=mean(recuento[hour(recuento[,1])==i,2])
  desviacion=sd(recuento[hour(recuento[,1])==i,2])
  return(c(paste(i,sep=""),media,desviacion))
  })))
porhoras[,2]=round(as.integer(levels(porhoras[,2]))[porhoras[,2]])
porhoras[,1]=as.integer(levels(porhoras[,1]))[porhoras[,1]]
porhoras[,3]=round(as.numeric(levels(porhoras[,3]))[porhoras[,3]])
ggplot(porhoras,aes(x=V1,y=V2,fill=as.factor(V1)))+geom_bar(stat="identity",position="stack")+
  geom_errorbar(aes(ymin=V2-V3,ymax=V2+V3,col=as.factor(V1)), width=1)+scale_fill_discrete(h.start=180)+scale_colour_discrete(h.start=180,c=30)


pordias=as.data.frame(t(sapply(1:length(periodo),function(i){
 media=mean(recuento[as.Date(recuento[,1])==periodo[i],2])
 desviacion=sd(recuento[hour(recuento[,1])==periodo[i],2])
 return(c(periodo[i],media,desviacion))
})))