library(RCurl)
library(RJSONIO)
library(dplyr)
library(stringdist)
library(stringr)
library(RANN)
library(readr)


fallos=read_csv("fallos mapeo.csv")
cities=read.csv("cities1000.txt",sep="\t",quote="",header=F)
colnames(cities)=c("geonameid","name","asciiname","alternate.names","latitude","longitude","feature.class",
                   "feature.code","country_code","cc2","admin1.code","admin2.code","admin3.code","admin4.code",
                   "population","elevation","dem","timezone","modification.date")
cities=merge(countries,cities)

trivago=read.csv("trivago.csv",sep=";",header=T,stringsAsFactors = F)
hotelbeds=read.csv("hotelbeds.csv")
hotelbeds_direcciones=read.csv("hotelbeds_direcciones.csv",sep=";",header=T)
cprov=read.csv("ciudadesprovincias.csv",sep=",",header=F,stringsAsFactors = F)

hotelbeds=merge(hotelbeds,hotelbeds_direcciones)

hoteles=read.csv("hoteles_full.csv",sep=";",header=T)

restel=read.csv("restel.csv",sep=";",header=T)

restel=select(restel,pais,codigo_hotel,nombre_h,direccion,provincia,poblacion,cp)
giata=read.csv("giata.csv",sep=";",header=T)

colnames(restel)=c("Codigo.hotel","Country","Name","Province","City")

candidatos=hoteles[which(stringdist(str_to_lower(gsub("Hotel","",trivago$Partner.Hotel.Name[i])),str_to_lower(gsub("Hotel","",hoteles$name)),method="cosine")<=0.1 & 
          str_to_lower(trivago$Partner.City[i])==str_to_lower(hoteles$city)),]

candidatos=candidatos[order(stringdist(str_to_lower(gsub("Hotel","",trivago$Partner.Hotel.Name[i])),str_to_lower(gsub("Hotel","",candidatos$name)),method="cosine")),]
candidatos=rbind(candidatos,hoteles[which(hoteles$giata_id==candidatos$giata_id[1]),])

trivago[i,]
trivago$Partner.Address[i]=which.max(table(candidatos$address))




pais=as.character(ciudades[nn2(query=hotelbeds[hotelbeds$HotelCode==fallos$prov_id[i],8:9],
                             data=cities[,5:6],k=1,search='radius',radius=100)$nn.idx,9])

hotelbeds[which(stringdist(str_to_lower(trivago$Partner.Hotel.Name[i]),str_to_lower(hotelbeds$Name),method="cosine")<=0.05),]

candidatos_hb=hoteles[which(stringdist(str_to_lower(trivago$Partner.Hotel.Name[i]),str_to_lower(hoteles$name),method="jaccard")<=0.05),]
uniqid=candidatos_hb$uniqid[as.character(candidatos_hb$country_code)==as.character(trivago$Partner.Country[i])]



pais=as.character(cities[nn2(query=hotelbeds[hotelbeds$HotelCode==fallos$prov_id[i],8:9],
                             data=cities[,5:6],k=1,search='radius',radius=100)$nn.idx,9])

restel[which(stringdist(str_to_lower(trivago$Partner.Hotel.Name[i]),str_to_lower(restel$nombre_h),method="jaccard")<=0.05),]

giata[which(stringdist(str_to_lower(trivago$Partner.Hotel.Name[4]),str_to_lower(giata$name),method="cosine")<=0.1 & 
              str_to_lower(trivago$Partner.City[4])==str_to_lower(giata$cityName)),]
