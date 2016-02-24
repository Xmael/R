library(rredis)
library(rPython)
library(RSQLite)
setwd("/home/ismael/Trabajo/")

proveedores=read.csv("codigos de proveedores.csv",sep=";",header=T)
proveedores$pro_id=as.character(proveedores$pro_id)
hoteles=read.csv("hoteles.csv",sep=";",header=T)
redisConnect(host="localhost",port=6379,nodelay = F)
print("Conexión establecida")
redisCmd("monitor")

t1=proc.time()[[3]]
keys=NULL
while(proc.time()[[3]] - t1 < 240) #segundos en escucha
{
  texto=redisMonitorChannels()
  if("MGET" %in% strsplit(texto,"\"")[[1]][2]){
    for(i in 3:length(strsplit(texto,"\"")[[1]])){
      if(strsplit(texto,"\"")[[1]][i]!=" "){
        keys=c(keys,strsplit(texto,"\"")[[1]][i])
      }
    }
  }
}
redisClose()

python.exec("import redis")
python.exec("import msgpack as m")
python.exec("import numpy as np")
python.exec("from operator import is_not")
python.exec("from functools import partial")
python.exec("r = redis.StrictRedis(host=\"localhost\",port=6379)")
python.assign("keys",keys)
python.exec("df=r.mget(keys)")
python.exec("af=filter(partial(is_not, None), df)")
longitud=python.get("len(af)")
temp=list()
for(i in 0:(longitud-1)){
  python.assign("i",i)
  temp[[i+1]]=python.get("m.unpackb(af[i])")
} 

#Obtengo un precio medio a partir de todas las habitaciones que oferta

db=NULL
for(i in 1:length(temp)){
  precio=NULL
  for(j in 1:length(temp[[i]]$Rooms)){
    for(k in 1:length(temp[[i]]$Rooms[[j]]$Regimes)){
      precio=rbind(precio,cbind(Precio=temp[[i]]$Rooms[[j]]$Regimes[[k]]$RoomCostPrice,Tipo=temp[[i]]$Rooms[[j]]$RoomType,Nombre=temp[[i]]$Rooms[[j]]$RoomName))
    }
  }
  db=rbind(db,cbind(Hotel=temp[[i]]$hotel_id,ID.Proveedor=temp[[i]]$hotel_id,Proveedor=temp[[i]]$prov_code,Timestamp=temp[[i]]$timestamp,
      Chekin=temp[[1]]$CheckinDate,Noches=temp[[1]]$nights,precio))
}

db=as.data.frame(db)
aux=proveedores$uniqid[match(db[,1],proveedores$pro_id)]
indices=which(is.na(aux))

for(i in 1:length(indices)){
  id.proveedor=as.numeric(as.character(db[indices[i],2]))
  proveedor=as.character(db[indices[i],3])
  aux[indices[i]]=proveedores$uniqid[which(id.proveedor==proveedores$pro_id & proveedor==as.character(proveedores$prov))]
}

db$Hotel=aux
db[,2]=as.character(db[,2])
db[,3]=as.character(db[,3])
db[,4]=as.numeric(levels(db[,4]))[db[,4]]
db[,5]=format(as.POSIXct(as.character(levels(db[,5]))[db[,5]],origin="1970-01-01"),"%Y-%m-%d")
db[,6]=as.integer(levels(db[,6]))[db[,6]]
db[,7]=as.numeric(levels(db[,7]))[db[,7]]
db[,8]=as.character(db[,8])
db[,9]=as.character(db[,9])
db[,4]=format(as.POSIXct(as.numeric(db[,4]),origin="1970-01-01"),"%Y-%m-%d %H:%M:%S")

dblite=dbConnect(SQLite(),dbname="precios.sqlite")
dbSendQuery(conn=dblite,"CREATE TABLE precios (Hotel INTEGER, IDProveedor CHAR, Proveedor CHAR, Timestamp DATETIME, 
            Chekin DATE, Noches INTEGER, Precio NUMERIC, Tipo CHAR, Nombre CHAR)")
#dbListFields(dblite, "precios")

dbWriteTable(conn = dblite, name = "precios",db,append=T)
#dump=as.data.frame(dbGetQuery(conn=dblite,"SELECT * FROM precios "))
dbDisconnect(dblite)
