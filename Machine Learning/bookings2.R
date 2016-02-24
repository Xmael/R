library(chron)
library(elastic)

connect(es_base="http://52.28.187.57")

today=Sys.Date()-5
periodo=seq(today, length.out=90, by="-1 day") #Obtengo las fechas desde hoy hasta diez días atrás
periodo=format(periodo,"%Y-%m-%d")


# db1=Search(index = "bookings",type = "booking"
#           ,size=100000,fields=c("Time","source","prov","city","clientemail","country","empowerdebug.HotelName",
#                                 "hotel.gcity","i_persons","empowerdebug.Price","netprice","riskScore","user_agent"),
#           q=paste("date :[",periodo[2],"T00:00:00 TO ",periodo[1],"T23:59:59]",sep=""))$hits$hits


db2=Search(index = "bookings",type = "booking"
          ,size=10000,
          q=paste("date :[",periodo[2],"T00:00:00 TO ",periodo[1],"T23:59:59]",sep=""))$hits$hits

db=NULL
for(i in 1:100){
  if(is.null(db2[[i]]$`_source`$source)){db2[[i]]$`_source`$source=NA}
  if(is.null(db2[[i]]$`_source`$minfraud_debug.ip_city)){db2[[i]]$`_source`$minfraud_debug.ip_city=NA}
  
  db=rbind(db,cbind(db2[[i]]$`_source`$created,
                    db2[[i]]$`_source`$clientemail,
                    db2[[i]]$`_source`$uniq.name,
                    db2[[i]]$`_source`$source,
                    db2[[i]]$`_source`$i_persons,
                    db2[[i]]$`_source`$nights,
                    db2[[i]]$`_source`$hotel.gcountry,
                    db2[[i]]$`_source`$prov,
                    db2[[i]]$`_source`$city,
                    db2[[i]]$`_source`$minfraud_debug.ip_city, #Ciudad del cliente por IP
                    db2[[i]]$`_source`$price,
                    db2[[i]]$`_source`$netprice,
                    db2[[i]]$`_source`$riskScore,
                    db2[[i]]$`_source`$user_agent))
}

colnames(db)=c("Date","Clientemail","Hotel","Source","Nº Pers","Noches","País Hotel","Proveedor","Ciudad"
               ,"Ciudad cliente","Precio","NetPrice","RiskScore","UserAgent")
db=data.frame(db)

#Extreaemos fecha, día de la semana, hora...
fechas=t(sapply(1:nrow(db),function(i) strsplit(as.character(db$Date[i]), "T")[[1]]))
Fecha=as.Date(chron(dates=fechas[,1],format="y-m-d"))
Times=chron(times=fechas[,2])
Diasemana=weekdays(Fecha,abbreviate = T)
Dias=days(Fecha)
Mes=months(Fecha,abbreviate=T)
Hora=hours(Times)
Mediahora=ifelse(minutes(Times)<30,1,2)

#Convertimos datos a las clases correspondientes
db$Nº.Pers=as.integer(levels(db$Nº.Pers))[db$Nº.Pers]
db$Noches=as.integer(levels(db$Noches))[db$Noches]
db$Precio=as.numeric(levels(db$Precio))[db$Precio]
db$NetPrice=as.numeric(levels(db$NetPrice))[db$NetPrice]
db$RiskScore=as.numeric(levels(db$RiskScore))[db$RiskScore]

#Extraemos el modelo de móvil
texto=t(sapply(1:nrow(db),function(i){unlist(db2[[i]]$`_source`$user_agent)}))
phrases=lapply(texto,function(x) gsub(";","",x))
phrases=lapply(phrases,function(x) gsub("\\(","",x))
phrases=lapply(phrases,function(x) gsub("\\)","",x))
phrases=lapply(phrases,function(x) gsub(",","",x))

dbp=NULL
for(i in 1:nrow(db)){
  if(phrases[[i]]!=""){
    words=strsplit(phrases[[i]], " ")[[1]]
    if(words[3]=="Android"){
      dbp=rbind(dbp,words[5])
    }else if(words[2]=="iPhone"){
      dbp=rbind(dbp,words[2])
    }else if(words[2]=="iPad"){
      dbp=rbind(dbp,words[2])
    }else if(words[2]=="Mobile"){
      dbp=rbind(dbp,"Windows Phone")
    }else if(words[2]=="BB10"){
      dbp=rbind(dbp,"BlackBerry")
    }else{
      dbp=(rbind(dbp,"Others"))
    }
  }
}

#Construimos el data.frame final
dbf=data.frame(Fecha,Times,Diasemana,Mes,Hora,Mediahora,db[,-match(c("Date","UserAgent"),colnames(db))],Telfono=dbp)
write.table(dbf,file="bookings",append=TRUE,sep=";",na="NA",row.names=F,col.names=T)