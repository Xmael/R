library(chron)
library(elastic)

connect(es_base="http://52.28.187.57")

#Crea la cabecera del archivo
nombres=data.frame(matrix(ncol=20))
colnames(nombres)=c("Fecha","Diasemana","Mes","Chekin","Diasemana.c","Mes.c","ID","Clientemail","Hotel",
                    "Source","Nº.Pers","Noches","País.Hotel","Proveedor","Ciudad","Ciudad.cliente","Precio","NetPrice",
                    "RiskScore","Telefono")
write.table(nombres[-1,],file="bookings",append=FALSE,sep=";",na="NA",row.names=F,col.names=T)

today=Sys.Date()
periodo=seq(today, length.out=4*365, by="-1 day") #Obtengo las fechas desde hoy hasta diez días atrás
periodo=format(periodo,"%Y-%m-%d")


# db1=Search(index = "bookings",type = "booking"
#           ,size=100000,fields=c("Time","source","prov","city","clientemail","country","empowerdebug.HotelName",
#                                 "hotel.gcity","i_persons","empowerdebug.Price","netprice","riskScore","user_agent"),
#           q=paste("date :[",periodo[2],"T00:00:00 TO ",periodo[1],"T23:59:59]",sep=""))$hits$hits

for(j in 1:length(periodo)){ #length(periodo)-1
  cat("Iteracion numero ",j,"\n")
  db2=Search(index = "bookings",type = "booking",size=10000,
             q=paste("date :[",periodo[j],"T00:00:00 TO ",periodo[j],"T23:59:59]",sep=""))$hits$hits
  print(paste("date :[",periodo[j],"T00:00:00 TO ",periodo[j],"T23:59:59]",sep=""))
  cat("Nº Reservas:", length(db2),"\n")
  if(length(db2)!=0){
    db=NULL
    for(i in 1:length(db2)){
      if(is.null(db2[[i]]$`_id`)){db2[[i]]$`_id`=NA}
      if(is.null(db2[[i]]$`_source`$source)){db2[[i]]$`_source`$source=NA}
      if(is.null(db2[[i]]$`_source`$minfraud_debug.ip_city)){db2[[i]]$`_source`$minfraud_debug.ip_city=NA}
      if(is.null(db2[[i]]$`_source`$hotel.gcountry)){db2[[i]]$`_source`$hotel.gcountry=NA}
      if(is.null(db2[[i]]$`_source`$uniq.id)){db2[[i]]$`_source`$uniq.id=NA} #Utilizar uniq.id
      if(is.null(db2[[i]]$`_source`$city)){db2[[i]]$`_source`$city=NA}
      if(is.null(db2[[i]]$`_source`$user_agent)){db2[[i]]$`_source`$user_agent=NA}
      if(is.null(db2[[i]]$`_source`$riskScore)) {db2[[i]]$`_source`$riskScore=NA}
      if(is.null(db2[[i]]$`_source`$clientemail)) {db2[[i]]$`_source`$clientemail=NA}
      if(is.null(db2[[i]]$`_source`$netprice)) {db2[[i]]$`_source`$netprice=NA} 
      if(is.null(db2[[i]]$`_source`$prov) & is.null(db2[[i]]$`_source`$empowerdebug.Supplier)) {db2[[i]]$`_source`$prov=NA} 
      if(db2[[i]]$`_source`$prov=="" & !is.null(db2[[i]]$`_source`$empowerdebug.Supplier)){db2[[i]]$`_source`$prov=db2[[i]]$`_source`$empowerdebug.Supplier} 
      
      db=rbind(db,cbind(db2[[i]]$`_source`$created, #1
                        db2[[i]]$`_id`, #2
                        db2[[i]]$`_source`$clientemail,  #3
                        db2[[i]]$`_source`$uniq.id,  #4
                        db2[[i]]$`_source`$source,  #5
                        db2[[i]]$`_source`$i_persons,  #6
                        db2[[i]]$`_source`$nights,  #7
                        db2[[i]]$`_source`$hotel.gcountry,  #8
                        db2[[i]]$`_source`$CheckinDateFix,  #9
                        db2[[i]]$`_source`$prov,  #10
                        db2[[i]]$`_source`$city,  #11
                        db2[[i]]$`_source`$minfraud_debug.ip_city,  #12 #Ciudad del cliente por IP
                        db2[[i]]$`_source`$price,  #13
                        db2[[i]]$`_source`$netprice,  #14
                        db2[[i]]$`_source`$riskScore,  #15
                        db2[[i]]$`_source`$user_agent)) #16 columnas
    }
    
    if(!is.null(db)){
      colnames(db)=c("Date","ID","Clientemail","Hotel","Source","Nº Pers","Noches","País Hotel","Chekin","Proveedor","Ciudad"
                     ,"Ciudad cliente","Precio","NetPrice","RiskScore","UserAgent")
      db=data.frame(db)
      
      #Extraemos fecha, día de la semana, hora...
      fechas=t(sapply(1:nrow(db),function(i) strsplit(as.character(db$Date[i]), "T")[[1]]))
      Fecha=as.Date(chron(dates=fechas[,1],format="y-m-d"))
      Diasemana=weekdays(Fecha,abbreviate = T)
      Dias=days(Fecha)
      Mes=months(Fecha,abbreviate=T)
      
      fechas=t(sapply(1:nrow(db),function(i) strsplit(as.character(db$Chekin[i]), "T")[[1]]))
      Chekin=as.Date(chron(dates=fechas[,1],format="y-m-d"))
      Diasemana.c=weekdays(Chekin,abbreviate = T)
      Dias.c=days(Chekin)
      Mes.c=months(Chekin,abbreviate=T)
      
      
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
        if(phrases[[i]]!="" & !is.na(phrases[[i]])){
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
        }else{
          dbp=rbind(dbp,NA)
        }
      }
      
      #Construimos el data.frame final
      dbf=data.frame(Fecha,Diasemana,Mes,Chekin,Diasemana.c,Mes.c,db[,-match(c("Date","UserAgent","Chekin"),colnames(db))],Telefono=dbp)
      write.table(dbf,file="bookings",append=TRUE,sep=";",na="NA",row.names=F,col.names=F)
    }
  }
}
