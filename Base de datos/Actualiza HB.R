library(RMySQL)
library(stringr)
library(stringdist)
con <- dbConnect(MySQL(), user="root", password="papidorl", 
                 dbname="hoteles", host="localhost",client.flag=CLIENT_MULTI_STATEMENTS)

dbListTables(con)

ciudadesp1=ciudades[which(ciudades$feature.code=="ADM3"),]
ciudadesp2=ciudades[which(ciudades$feature.class=="P"),]

contacts=dbGetQuery(con, "select * from contacts")
giata=dbGetQuery(con, "select * from giataproviders")
giatahotels=dbGetQuery(con, "select * from giatahotels")
countries_int=read.csv("/home/ismael/Trivago/countries.csv",sep=";",stringsAsFactors = F,header=F,na="NULL")
colnames(countries_int)=c("country_code","country")
destid=read.csv(file="DestinationIDs.csv",sep="|",header=F,stringsAsFactors = F,na="NULL")
dest=read.csv(file="Destinations.csv",sep="|",header=F,stringsAsFactors = F,na="NULL")
hoteldest=read.csv(file="HotelDestinations.csv",sep="|",header=F,stringsAsFactors = F,na="NULL")
hotels=read.csv(file="Hotels.csv",sep="|",header=F,stringsAsFactors = F,na="NULL")
phones=read.csv(file="Phones.csv",sep="|",header=F,stringsAsFactors = F,na="NULL")
mapped_provinces=read.csv("mapped_provinces.csv",header=T,sep=",",stringsAsFactors = F,na="NULL")
i18n=read.csv("mapped_provinces_i18n.csv",header=T,sep=",",stringsAsFactors = F,na="NULL")
i18n=i18n[which(i18n[,2]=="en"),c(1,3)]
i18n[i18n[,3]=="UK",3]="GB"
i18n[i18n[,3]=="AB",3]="SA"
estrellas=data.frame(Cod=sort(unique(hotels$category)),Num=c(1,1,2,2,3,3,4,4,4,5,5,5,0,0,1,2,3,4,5,1,2,3,1,3,4,5,0,1,2,0,1,2,2,3,3,4,5,0,1,2,3,4,5,6,1,2,3,4,5,1,2,0,0,0,0,0,0,0,0,0,0,0))

countryid=read.csv(file="CountryIDs.csv",sep="|",header=F,stringsAsFactors = F,na="NULL")
countryid=countryid[countryid[,2]=="CAS",c(1,3)]
colnames(countryid)=c("country_code","country")
#Modifica algunos códigos de países que están mal
countryid[which(countryid$country_code=="BK"),1]="BQ"
countryid[which(countryid$country_code=="C0"),1]="CW"
countryid=countryid[-which(countryid$country_code=="NY"),] #Eliminamos el norte de Chipre
countryid[which(countryid$country_code=="SF"),1]="SX"
countryid[which(countryid$country_code=="UK"),1]="GB"


temp=countries_int[na.omit(match(countryid[,1],countries_int[,1])),]
# temp.2=countryid[match(countryid[,1],temp[,1]),]
countryid[,2]=str_to_upper(temp[,2])
colnames(hotels)=c("prov_id","name","category","city_code","V5","chains","blank","lat","lng")
colnames(destid)=c("code","c3c","region")
destid=destid[which(destid[,2]=="ENG"),c(1,3)] #Nombres de regiones en inglés
colnames(dest)=c("c3c","country_code")
colnames(hoteldest)=c("country_code","c3c")
colnames(phones)=c("prov_id","type","number")

#Cambiamos los códigos de país en el data.frame dest
dest[which(dest[,2]=="UK"),2]="GB"
dest[which(dest[,2]=="BK"),2]="BQ"
dest[which(dest[,2]=="NY"),2]="CY"
dest[which(dest[,2]=="C0"),2]="CW"
dest[which(dest[,2]=="SF"),2]="SX"

hotelbeds=data.frame(matrix(NA,ncol=36,nrow=nrow(hotels),byrow = T))
colnames(hotelbeds)=colnames(mapped_hotels)
colnames(hotelbeds)[35]="timezone"
colnames(hotelbeds)[36]="dst"
colnames(mapped_hotels)[35]="timezone"
colnames(mapped_hotels)[36]="dst"
hotelbeds$prov="HB" #Inserta codigo de proveedor
hotelbeds$prov_id=hotels$prov_id #Inserta codigos de hotel de proveedor
hotelbeds$name=str_to_upper(hotels$name) #Inserta nombre del hotel
hotelbeds$lat=hotels$lat #Inserta latitud
hotelbeds$lng=hotels$lng #Inserta longitud
hotelbeds$country_code=dest[match(hotels$city_code,dest$c3c),2] #Inserta código de país
hotelbeds$country=countryid[match(hotelbeds$country_code,countryid$country_code),2] #Inserta nombre de país
hotelbeds[,c(23,24,12)]=contacts[match(hotelbeds$prov_id,contacts$prov_id),c(2,3,4)] #Inserta dirección, código postal y ciudad
giata=giata[which(giata$providerCode=="hotelbeds"),] #Crea subconjunto de giata del proveedor
hotelbeds$giata_id=giata$giata_id[match(hotelbeds$prov_id,giata$value)] #Inserta giata id
hotelbeds[,c(20,17)]=giatahotels[match(hotelbeds$giata_id,giatahotels$id),c(5,6)] #Insertas gcity y gcountry_code
hotelbeds$gcountry=countryid$country[match(hotelbeds$gcountry_code,countryid$country_code)] #Inserta gcountry a partir del gcountry_code usando nuestra lista de países
hotelbeds$uniqid=mapped_hotels$uniqid[which(mapped_hotels$prov=="HB")][match(hotelbeds$prov_id,mapped_hotels$prov_id[which(mapped_hotels$prov=="HB")])]
hotelbeds$stars=estrellas[match(hotels$category,estrellas[,1]),2]
phones=phones[which(phones$type=="phoneHotel"),]
hotelbeds$phone=phones$number[match(hotelbeds$prov_id,phones$prov_id)]

#########
ciudadestemp=cbind(hotelbeds$city,mapped_hotels$city[match(hotelbeds$prov_id,mapped_hotels$prov_id)]) #Reutilizamos los nombres de ciudades que tenemos ya en la base de datos
indices=which(hotelbeds$country_code=="ES" & is.na(ciudadestemp[,2])) #Se corresponde con aquellos indices de hoteles en España que no tenemos en la base de datos para buscarlos a continuación
candidatos_nombre=NULL
for(i in 1:nrow(hotelbeds[indices,])){
  cat(i,"de",length(excluye),hotelbeds$city[indices[i]]," => ")
  coordenadas=hotelbeds[indices[i],c(14,13)]
  if(any(!is.na(coordenadas))){
    distancia=distGeo(rbind(apply(ciudadesp1[,c(6,5)],2,as.numeric)),coordenadas,a=6378137, f=1/298.257223563)/1000
    lista=ciudadesp1$name[order(distancia)[1:10]]
    nombre=gsub("/.*|\\(.*|-.*","",iconv(hotelbeds$city[indices[i]],to="ASCII//TRANSLIT"))
    candidatos_nombre=rbind(candidatos_nombre,cbind(hotelbeds$city[indices[i]],
        lista[order(stringdist(iconv(str_to_upper(lista),to="ASCII//TRANSLIT"),nombre,method="jw",weight = c(0.5,0.5,1)),decreasing=F)[1]]))
  }else{
    jw=ciudadesp1$name[order(stringdist(iconv(str_to_upper(ciudadesp1$name),to="ASCII//TRANSLIT"),iconv(str_to_upper(hotelbeds$city[indices[i]]),to="ASCII//TRANSLIT"),method="jw",weight = c(0.5,0.5,1)),decreasing=F)[1]]
    candidatos_nombre=rbind(candidatos_nombre,cbind(hotelbeds$city[indices[i]],jw))
  }
  cat(candidatos_nombre[i,2],"\n")
}

candidatos_nombre=cbind(candidatos_nombre,NA)
for(i in 1:nrow(hotelbeds[indices,])){
  cat(i,"de",length(excluye),hotelbeds$city[indices[i]]," => ")
  coordenadas=hotelbeds[indices[i],c(14,13)]
  if(any(!is.na(coordenadas))){
    distancia=distGeo(rbind(apply(ciudadesp2[,c(6,5)],2,as.numeric)),coordenadas,a=6378137, f=1/298.257223563)/1000
    lista=ciudadesp2$name[order(distancia)[1:10]]
    nombre=gsub("/.*|\\(.*|-.*","",iconv(hotelbeds$city[indices[i]],to="ASCII//TRANSLIT"))
    candidatos_nombre[i,3]=lista[order(stringdist(iconv(str_to_upper(lista),to="ASCII//TRANSLIT"),nombre,method="jw",weight = c(0.5,0.5,1)),decreasing=F)[1]]
  }else{
    jw=ciudadesp2$name[order(stringdist(iconv(str_to_upper(ciudadesp2$name),to="ASCII//TRANSLIT"),iconv(str_to_upper(hotelbeds$city[indices[i]]),to="ASCII//TRANSLIT"),method="jw",weight = c(0.5,0.5,1)),decreasing=F)[1]]
    candidatos_nombre[i,3]=jw
  }
  cat(candidatos_nombre[i,3],"\n")
}



confirma=function(k){
  for(i in k:nrow(candidatos_nombre)){
    if(ncol(candidatos_nombre)!=4){
      candidatos_nombre=cbind(candidatos_nombre,NA)
    }
    cat("\014")
    cat(i,"de",nrow(candidatos_nombre),"\n")
    cat(candidatos_nombre[i,1]," => 1. ",candidatos_nombre[i,2], " | 2.", candidatos_nombre[i,3],"\n")
    
    if(any(str_to_upper(iconv(candidatos_nombre[i,1],to="ASCII//TRANSLIT"))!=iconv(str_to_upper(candidatos_nombre[i,2]),to="ASCII//TRANSLIT"),
           str_to_upper(iconv(candidatos_nombre[i,1],to="ASCII//TRANSLIT"))!=iconv(str_to_upper(candidatos_nombre[i,3]),to="ASCII//TRANSLIT"))){
      if(candidatos_nombre[i,2]==hotelbeds[indices[i],20] & !is.na(hotelbeds[indices[i],20])){
        candidatos_nombre[i,4]=candidatos_nombre[i,2]
      }else if(candidatos_nombre[i,3]==hotelbeds[indices[i],20] & !is.na(hotelbeds[indices[i],20])){
        candidatos_nombre[i,4]=candidatos_nombre[i,3]
      }else{
        coordenadas=rbind(apply(ciudadesp2[which(ciudadesp2$name==candidatos_nombre[i,3]),c(6,5)],2,as.numeric))
        distancia=distGeo(rbind(apply(ciudadesp1[,c(6,5)],2,as.numeric)),coordenadas,a=6378137, f=1/298.257223563)/1000
        propuesta=cbind(ciudadesp1$name[order(distancia)[1:10]],sort(distancia)[1:10])
        colnames(propuesta)=c("Ciudades Cercanas","Distancia")
        print(propuesta)
        print(kable(hotelbeds[indices[i],c(3,5,6,12,23,20)]))
        #  cat("Proveedor#",raw_text(aux),"\n")
        lee=readline(prompt = "Opcion [1]/[2]/Nombre: ")
        if(lee==1){
          candidatos_nombre[i,4]=candidatos_nombre[i,2]
        }else if(lee==2){
          candidatos_nombre[i,4]=candidatos_nombre[i,3]
        }else{
          candidatos_nombre[i,4]=str_to_upper(lee)
        }
      }
    }else{
      candidatos_nombre[i,4]=str_to_upper(candidatos_nombre[i,3])
      cat("\n\n")
    }
    assign('candidatos_nombre',candidatos_nombre,envir=.GlobalEnv)
  }
}
  
hotelbeds$city[indices]=str_to_upper(candidatos_nombre[,4]) #Introducimos los nombres que hemos corregido en el paso anterior

hotelbeds$city[-indices]=str_to_upper(ciudadestemp[-indices,2]) #Introducimos los que teníamos en la base de datos sin pisar los que hemos buscado antes
hotelbeds$city[which(is.na(hotelbeds$city))]=contacts[match(hotelbeds$prov_id,contacts$prov_id),4][which(is.na(hotelbeds$city))] #Metemos el resto de lo que nos ha facilitado el proveedor


#####Buscamos códigos giata######
candidatos=hotelbeds[which(is.na(hotelbeds$giata_id)),]
resumen=data.frame(matrix(NA,nrow = nrow(candidatos),ncol=7))
colnames(resumen)=c("Candidato","city","name","gcity","giata_id","prov","distancia")

pb=txtProgressBar(min = 0, max = nrow(resumen), initial = 0, char = "=", width = NA, title, label, style = 3)
for(i in 1:nrow(resumen)){
  setTxtProgressBar(pb, i)
  subcandidatos=hoteles[which(hoteles$country_code==candidatos$country_code[i] & hoteles$city==candidatos$city[i] & hoteles$prov!="HB"),]
  
  resultado.1=sort.int(stringdist(str_to_upper(gsub(" \\(.*| - NON REFUNDABLE ROOM","",candidatos$name[i])),str_to_upper(subcandidatos$name),method="jw",weight = c(0.1,0.5,1)),index.return = T)
  if(candidatos$lng[i]>-360 & !is.na(candidatos$lng[i])){
  distancia=distGeo(rbind(apply(subcandidatos[resultado.1$ix[which(resultado.1$x<=0.5)][1],c(14,13)],2,as.numeric)),rbind(apply(candidatos[i,c(14,13)],2,as.numeric)),a=6378137, f=1/298.257223563)/1000
  }else{
    distancia=NA
  }
  resumen[i,]=c(Candidatos=candidatos$name[i],candidatos$city[i],subcandidatos[resultado.1$ix[which(resultado.1$x<=0.5)][1],c(6,12,5,2)],distancia)
  # subcandidatos[resultado.1$ix[1],]
  direccion=subcandidatos$address[resultado.1$ix[which(resultado.1$x<=0.5)]][1]
  score=stringdist(str_to_upper(gsub(" \\(.*| - NON REFUNDABLE ROOM","",resumen[i,1])),str_to_upper(resumen[i,3]),method="cosine")
  score2=sort.int(stringdist(str_to_upper(candidatos$address[i]),str_to_upper(direccion),method="cosine",weight = c(0.1,0.5,1)),index.return=T)$x[1]
  if(score>0.1 & !is.na(score) & str_to_upper(resumen$city[i])!=str_to_upper(resumen$gcity[i])){
    resumen$giata_id[i]=NA
  }else if(score>0.3 & !is.na(score) & str_to_upper(resumen$city[i])==str_to_upper(resumen$gcity[i]) & score2>0.1){
    resumen$giata_id[i]=NA
  }
}


resumen.2=NULL
#colnames(resumen.2)=c("Candidato","city","name","gcity","giata_id","prov")
pb=txtProgressBar(min = 0, max = nrow(resumen), initial = 0, char = "=", width = NA, title, label, style = 3)
hoteles2=hoteles[which(hoteles$prov!="HB" & !is.na(hoteles$giata_id)),]
for(i in 1:nrow(candidatos)){
  cat(i,"\n")
  temp=hoteles2[which(str_to_upper(hoteles2$name) %in% str_to_upper(candidatos$name[i]) & hoteles2$city==candidatos$city[i]),c("name","city","giata_id","prov")][1,]
  if(all(!is.na(temp))){
    resumen.2=rbind(resumen.2,cbind(i,candidatos$name[i],candidatos$city[i],temp))
  }
}
resumen=resumen[-which(row.names(resumen) %in% resumen.2[,1]),]
candidatos[as.integer(rownames(resumen)),"giata_id"]=resumen$giata_id
candidatos[as.integer(rownames(resumen)),"gcountry_code"]=giatahotels$country[match(candidatos[as.integer(rownames(resumen)),"giata_id"],giatahotels$id)]
candidatos[as.integer(rownames(resumen)),"gcountry"]=str_to_upper(countries_int$country[match(candidatos[as.integer(rownames(resumen)),"gcountry_code"],countries_int$country_code)])

candidatos[resumen.2[,1],"giata_id"]=resumen.2$giata_id
candidatos[resumen.2[,1],"gcountry_code"]=giatahotels$country[match(candidatos[resumen.2[,1],"giata_id"],giatahotels$id)]
candidatos[resumen.2[,1],"gcountry"]=str_to_upper(countries_int$country[match(candidatos[resumen.2[,1],"gcountry_code"],countries_int$country_code)])

hotelbeds[match(row.names(candidatos),row.names(hotelbeds)),]=candidatos
########

####Provincias

####Buscamos primero los que tienen coordenadas

candidatos=hotelbeds[which(!is.na(hotelbeds$lat)),]
ciudades1000=read.csv2(file="/home/ismael/Trivago/cities1000.txt",sep="\t",header=F,stringsAsFactors = F,quote="",na="NULL")
colnames(ciudades1000)=c("geonameid","name","asciiname","alternate.names","latitude","longitude","feature.class",
                         "feature.code","country_code","cc2","admin1.code","admin2.code","admin3.code","admin4.code",
                         "population","elevation","dem","timezone","modification.date")
admincodes1=read.csv(file="/home/ismael/Trivago/admin2Codes.txt",sep="\t",header=F,stringsAsFactors = F)
admincodes2=read.csv(file="/home/ismael/Trivago/admin1CodesASCII.txt",sep="\t",header=F,stringsAsFactors = F)
reconstruye=function(admincodes){
  temp=matrix(data=NA,nrow=nrow(admincodes),ncol=6,byrow=T)
  for(i in 1:nrow(admincodes)){
    aux=rbind(unlist(strsplit(admincodes[i,1],split="\\.")))
    for(j in 1:length(aux)){temp[i,j]=aux[j]}
    temp[i,4]=admincodes[i,2]
    temp[i,5]=admincodes[i,3]
    temp[i,6]=admincodes[i,4]
  }
  return(temp)
}
admincodes1=reconstruye(admincodes1)
admincodes2=reconstruye(admincodes2)

#Busco primero las provincias por coincidencia directa en la base de datos de ciudades con más de 1000 habitantes
for(i in 8012:nrow(hotelbeds)){
  cat(i,"de",nrow(hotelbeds),"\n")
  codigo<-ciudades1000[which(ciudades1000$country_code==hotelbeds$country_code[i] & 
                               str_to_upper(iconv(ciudades1000$name,to="ASCII//TRANSLIT"))==str_to_upper(iconv(hotelbeds$city[i],to="ASCII//TRANSLIT"))),
                       c("country_code","admin1.code","admin2.code","admin3.code")]
  if(nrow(codigo)!=1){
    cat("No se encuentra",hotelbeds$city[i],"\n")
  }else{
      if(codigo[,"admin2.code"]==""){
        temp=admincodes2[which(codigo[,1]==admincodes2[,1] & codigo[,2]==admincodes2[,2]),4]
        if(length(temp)==1){
          hotelbeds$province[i]=temp
          cat(hotelbeds$province[i],"\n")
          #     }else if(length(temp)>1){
          #       temp=admincodes[which(codigo[,1]==admincodes[,1] & codigo[,2]==admincodes[,2] & codigo[,3]==admincodes[,3]),4]
          #       hotelbeds$province[i]=temp
          #       cat(hotelbeds$province[i],"\n")
        }else if(length(temp)==0){
          cat("No se encuentra",hotelbeds$city[i],"\n")
        }
      }else if(codigo[,"admin2.code"]!=""){
        temp=admincodes1[which(codigo[,1]==admincodes1[,1] & codigo[,2]==admincodes1[,2] & codigo[,3]==admincodes1[,3]),4]
        if(length(temp)==1){
          hotelbeds$province[i]=temp
          cat(hotelbeds$province[i],"\n")
        }else if(length(temp)==0){
          cat("No se encuentra",hotelbeds$city[i],"\n")
        }
      }else if(nrow(codigo)==0){
        cat("No se encuentra",hotelbeds$city[i],"\n")
      }
  }
}
  
#Busco el resto mediante coordenadas y por proximidad a ciudades con más de 1000 habitantes
#En regiones con poca densidad puede ubicar las poblaciones en provincias equivocadas
indices=which(is.na(hotelbeds$province) & !is.na(hotelbeds$lat) & hotelbeds$lng>-360 & hotelbeds$lat>=-90 & hotelbeds$lat<=90)
for (i in indices){
  coordenadas=hotelbeds[i,c(14,13)]
  candidatos=ciudades1000[which(ciudades1000$country_code==hotelbeds$country_code[i]),]
  distancias=distGeo(rbind(apply(coordenadas,2,as.numeric)),rbind(apply(candidatos[,c("longitude","latitude")],2,as.numeric)),a=6378137, f=1/298.257223563)
  codigo=candidatos[order(distancias)[1],c("country_code","admin1.code","admin2.code","admin3.code")]
  if(codigo[,2]==""){
    codigo=candidatos[order(distancias)[2],c("country_code","admin1.code","admin2.code")]
  }
  codigo=cbind(names(which.max(table(codigo[,1]))),names(which.max(table(codigo[,2]))),names(which.max(table(codigo[,3]))))
  provincia=admincodes[which(codigo[,1]==admincodes[,1] & codigo[,2]==admincodes[,2]),4]
  if(length(provincia)==1){
    hotelbeds$province[i]=provincia
  }else if(length(provincia>1)){
    hotelbeds$province[i]=admincodes[which(codigo[,1]==admincodes[,1] & codigo[,2]==admincodes[,2] & codigo[,3]==admincodes[,3]),4]
  }else if(length(provincia)==0){
    hotelbeds$province[i]=NA
  }
  cat(match(i,indices),"de",length(indices),hotelbeds$province[i],"\n")
}

#Busco las provincias que quedan en los datos facilitados por HB (meten regiones y barrios como provincias)
indices=which(is.na(hotelbeds$province))
provincias=destid[match(hotels$city_code[match(hotelbeds$prov_id[indices],hotels$prov_id)],destid$code),2]
for(i in 1:length(provincias)){
  if(nchar(gsub(".* - ","",provincias[i]))==2){
    provincias[i]=admincodes[which(admincodes[,1]=="US"),][(match(gsub(".* - ","",provincias[i]),admincodes[which(admincodes[,1]=="US"),2])),4]
  }
}
hotelbeds$province[indices]=provincias

###########
i18n=cbind(i18n,code1=NA,code2=NA)
admincodes1[admincodes1[,1]==i18n[1,3],2:3][which(sapply(1:length(prueba),function(i) (muestra[1,2] %in% str_to_upper(strsplit(admincodes1[admincodes1[,1]==muestra[1,3],4],split=" ")[[i]])))),]
