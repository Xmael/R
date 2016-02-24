library(elastic)
connect(es_base="http://52.28.187.57")

today=Sys.Date()
periodo=seq(today, length.out=90, by="-1 day") #Obtengo las fechas desde hoy hasta diez días atrás
periodo=format(periodo,"%Y.%m.%d")

for(j in 1:length(periodo)){ #length(periodo)-1
  cat("Iteracion numero ",j,"\n")
  db2=Search(index = paste("logstash-",periodo[j],sep=""),type = "has",size=1e5,
             q="api",fields=c("@fields.query.channel"))$hits$hits
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
      
      #Construimos el data.frame final
      dbf=data.frame(Fecha,Diasemana,Mes,Chekin,Diasemana.c,Mes.c,db[,-match(c("Date","UserAgent","Chekin"),colnames(db))],Telefono=dbp)
      write.table(dbf,file="bookings",append=TRUE,sep=";",na="NA",row.names=F,col.names=F)
    }
  }
}